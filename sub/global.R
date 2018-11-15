library(shiny)
library(shinyWidgets)
library(tidyverse)
library(viper)
library(ggrepel)
library(DT)
library(furrr)
library(pheatmap)
library(shinyjs)
library(ggExtra)
library(tidygraph)
library(ggraph)
library(broom)

# shiny options
enableBookmarking(store = "server")
options(shiny.maxRequestSize=30*1024^2)

# load data
rwth_colors_df = get(load("data/rwth_colors.rda"))

# dorothea
load("data/models/dorothea_regulon_human_v1.rda")
load("data/models/dorothea_regulon_mouse_v1.rda")
dorothea_regulon_mouse_coverage_v1 = get(load("data/models/dorothea_regulon_human_coverage_v1.rda")) %>%
  mutate(tf = str_to_title(tf),
         target = str_to_title(target))
dorothea_regulon_human_coverage_v1 = get(load("data/models/dorothea_regulon_human_coverage_v1.rda"))

# progeny
load("data/models/progeny_matrix_mouse_v1.rda")
load("data/models/progeny_matrix_human_v1.rda")

# kinact
kinact_regulon_human = readRDS("data/models/kinact_regulon_human.rds")

# functions
run_progeny = function (E, M, gene_name = "gene", value_name = "expression",
                        id_name = "sample", permutation = 10000, ...) {
  plan(multiprocess)
  E = E %>% mutate_if(is.factor, as.character)
  
  if (permutation > 0) {
    null_model = future_map_dfr(1:permutation, .progress = T, function(p) {
      E %>%
        group_by(!!!syms(id_name)) %>%
        sample_frac() %>%
        ungroup() %>%
        mutate(!!gene_name := E[[gene_name]]) %>%
        run_progeny(M, gene_name = gene_name, value_name = value_name,
                    id_name = id_name, permutation = 0)
    }) %>%
      group_by(!!!syms(id_name), pathway) %>%
      summarise(m = mean(activity),
                s = sd(activity)) %>%
      ungroup()
  }
  
  meta_data = E %>%
    select(-c(!!gene_name, !!value_name)) %>%
    distinct()
  
  emat = E %>%
    select(!!gene_name, !!id_name, !!value_name) %>%
    spread(!!id_name, !!value_name, fill = 0) %>%
    drop_na() %>%
    data.frame(row.names = 1, stringsAsFactors = F, check.names = F)
  
  model = M %>%
    spread(pathway, weight, fill = 0) %>%
    data.frame(row.names = 1, check.names = F, stringsAsFactors = F)
  
  common_genes = intersect(rownames(emat), rownames(model))
  emat_matched = emat[common_genes, , drop = FALSE] %>%
    t()
  model_matched = model[common_genes, , drop = FALSE] %>%
    data.matrix()
  
  stopifnot(names(emat_matched) == rownames(model_matched))
  
  progeny_scores = emat_matched %*% model_matched %>%
    data.frame(stringsAsFactors = F, check.names = F) %>%
    rownames_to_column(id_name) %>%
    gather(key = pathway, value = activity, -!!id_name) %>%
    as_tibble() %>%
    inner_join(meta_data, by = id_name)
  
  if (permutation > 0) {
    progeny_z_scores = progeny_scores %>%
      inner_join(null_model, by = c(id_name, "pathway")) %>%
      mutate(activity = (activity - m)/s) %>%
      select(!!id_name, pathway, activity)
    return(progeny_z_scores)
  }
  else {
    return(progeny_scores)
  }
}

run_viper = function(E, regulon, gene_name = "gene", value_name = "expression",
                     id_name = "sample", regulator_name = "tf",  ...) {
  meta_data = E %>%
    select(-c(!!gene_name, !!value_name)) %>%
    distinct()
  
  meta_regulon_data = regulon %>%
    select(-c(target, mor, likelihood)) %>%
    distinct()
  
  emat = E %>%
    select(!!gene_name, !!id_name, !!value_name) %>%
    spread(!!id_name, !!value_name, fill=0) %>%
    drop_na() %>%
    data.frame(row.names = 1, stringsAsFactors = F, check.names = F)
  
  viper_regulon = regulon %>%
    df2regulon(regulator_name = regulator_name)
  
  activity_scores = viper(eset = emat, regulon = viper_regulon, nes = T,
                          method = 'none', minsize = 4, eset.filter = F,
                          adaptive.size = F) %>%
    data.frame(stringsAsFactors = F, check.names = F) %>%
    rownames_to_column(var = regulator_name) %>%
    gather(key=!!id_name, value="activity", -!!regulator_name) %>%
    as_tibble() %>%
    inner_join(., meta_data, by=id_name) %>%
    inner_join(., meta_regulon_data, by = regulator_name)
  
  return(activity_scores)
}

df2regulon = function(df, regulator_name = "tf") {
  regulon = df %>%
    split(.[regulator_name]) %>%
    map(function(dat) {
      targets = setNames(dat$mor, dat$target)
      likelihood = dat$likelihood
      list(tfmode = targets, likelihood = likelihood)
    })
  return(regulon)
}

rwth_color = function(colors) {
  if (!all(colors %in% rwth_colors_df$query)) {
    wrong_queries = tibble(query = colors) %>%
      anti_join(rwth_colors_df, by="query") %>%
      pull(query)
    warning(paste("The following queries are not available:",
                  paste(wrong_queries, collapse = ", ")))
  }
  tibble(query = colors) %>%
    inner_join(rwth_colors_df, by="query") %>%
    pull(hex)
}

plot_lollipop = function(df, top_n_hits, var, var_label) {
  var = enquo(var)
  title = paste("Contrast:", unique(df$contrast))
  df %>% 
    arrange(activity) %>%
    mutate(!!var := as_factor(!!var),
           effect = factor(sign(activity)),
           abs_activity = abs(activity)) %>%
    group_by(effect) %>%
    top_n(top_n_hits, abs_activity) %>%
    ungroup() %>%
    ggplot(aes(x=!!var, y=activity, color=effect)) +
    geom_segment(aes(x=!!var, xend=!!var, y=0, yend=activity), color="grey") +
    geom_point(size=4) +
    coord_flip() +
    theme_light() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    labs(x = var_label, y="Activity (z-score)") +
    scale_color_manual(values = rwth_color(c("magenta", "green"))) +
    theme(legend.position = "none") +
    theme(aspect.ratio = c(1)) + 
    ggtitle(title)
}

plot_heatmap = function(df, var="tf") {
  mat = df %>%
    select(!!var, contrast, activity) %>% 
    spread(contrast, activity) %>%
    data.frame(row.names = var, check.names = F)
  
  if (ncol(mat) > 1) {
    pheatmap(mat, show_rownames = F)
  } else if (ncol(mat) == 1) {
    pheatmap(mat, cluster_rows = F, cluster_cols = F, show_rownames = F)
  }
}

plot_network = function(network, num_nodes, var="tf", var_label = "TF") {
  var_of_interest = unique(network[[var]])
  reg_var_of_interest = unique(network$regulation)
  
  feature_name = network %>% pull(!!var) %>% unique()
  contrast_name = network %>% pull(contrast) %>% unique()
  title = paste0("Contrast: ", contrast_name, ", ", var_label,": ", feature_name)
  
  sub_network = network %>% 
    drop_na() %>%
    filter(effect != "not regulated") %>%
    arrange(-importance) %>%
    mutate(n = row_number()) %>%
    filter(n <= num_nodes) %>%
    select(!!var, target, mor, effect, regulation)
  
  if (nrow(sub_network) > 0) {
    nodes_df = sub_network %>%
      distinct_(var, "target", "effect", "regulation") %>%
      gather(class, name, -effect, -regulation) %>%
      mutate(effect = case_when(class != var ~ effect,
                                class == var ~ regulation)) %>%
      mutate(effect = factor(effect, c("upregulated", "downregulated"))) %>%
      distinct(class, name, effect) %>%
      mutate(id = row_number())
    
    edges_df = sub_network %>%
      select(!!var, target, mor) %>%
      inner_join(rename(nodes_df, !!var:=name, from=id), by=var) %>%
      select(-c(class,effect)) %>%
      inner_join(rename(nodes_df, target=name, to=id), by="target") %>%
      select(from, to, mor)
    
    g = tbl_graph(nodes = nodes_df, edges = edges_df) %>%
      ggraph(layout = "nicely") + 
      geom_edge_link(arrow = arrow(), aes(edge_colour=mor)) + 
      geom_node_point(aes(color = effect, shape=class), size=10) +
      geom_node_text(aes(label = name), vjust = 0.4) + 
      theme_graph() +
      scale_color_manual(values = rwth_color(c("green50", "bordeaux50")), drop=F) +
      scale_edge_color_manual(values = rwth_color(c("bordeaux", "green")), drop=F) +
      scale_shape_manual(values = c(16,15)) +
      theme(legend.position = "none",
            aspect.ratio = c(1), 
            plot.title = element_text(size = 14, face="plain")) +
      ggtitle(title)
  } else {
    nodes_df = tribble(
      ~effect, ~class, ~name, ~id,
      reg_var_of_interest, "tf", var_of_interest, 1
    )
    # nodes_df = sub_network %>%
    #   distinct_(var, "target", "effect", "regulation") %>%
    #   gather(class, name, -effect, -regulation) %>%
    #   mutate(effect = case_when(class != var ~ effect,
    #                             class == var ~ regulation)) %>%
    #   mutate(effect = factor(effect, c("upregulated", "downregulated"))) %>%
    #   distinct(class, name, effect) %>%
    #   filter(class == "tf") %>%
    #   mutate(id = row_number())
    edges_df = NULL
    
    g = tbl_graph(nodes = nodes_df, edges = edges_df) %>%
      ggraph(layout = "nicely") + 
      geom_node_point(aes(color = effect, shape=class), size=10) +
      geom_node_text(aes(label = name), vjust = 0.4) + 
      theme_graph() +
      scale_color_manual(values = rwth_color(c("green50", "red50")), drop=F) +
      scale_edge_color_manual(values = rwth_color(c("red", "green")), drop=F) +
      scale_shape_manual(values = c(15)) +
      theme(legend.position = "none",
            aspect.ratio = c(1)) +
      ggtitle(title)
  }
  return(g)
}

plot_volcano = function(df, interactome, selected_top_n_labels, var, var_label = "TF") {
  var = enquo(var)
  
  feature_name = interactome %>% pull(!!var) %>% unique()
  contrast_name = df %>% pull(contrast) %>% unique()
  
  title = paste0("Contrast: ", contrast_name, ", ", var_label,": ", feature_name)
  
  interactome %>% 
    inner_join(df, by=c("target")) %>%
    mutate(effect = factor(effect, c("downregulated", "not regulated", "upregulated")),
           regulated = case_when(effect %in% c("downregulated", "upregulated") ~ "yes",
                                 effect %in% c("not regulated") ~ "no")) %>%
    arrange(desc(regulated), -importance) %>%
    mutate(n = row_number(),
           label = case_when(n <= selected_top_n_labels & effect != "not regulated" ~ target,
                             TRUE ~ "")) %>%
    ggplot(aes(x=logFC, y=-log10(adj.p.value), label=label, color=effect)) +
    geom_point() +
    geom_label_repel() +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_color_manual(values = rwth_color(c("bordeaux", "black50","green")),
                       drop=F) +
    theme(aspect.ratio = c(1)) +
    ggtitle(title)
}