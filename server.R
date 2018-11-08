# # load contrast
# limma_result = readRDS("data/limma_result.rds") %>%
#   ungroup() %>%
#   filter(contrast == "CTRvsTNFinWT")
# 
# dorothea_input =  limma_result %>%
#   select(gene, contrast, t = statistic)

#install.packages("~/Projects/utils/", repos = NULL, type="source")



dorothea_regulon_mouse_coverage_v1 = get(load("data/dorothea_regulon_human_coverage_v1.rda")) %>%
  mutate(tf = str_to_title(tf),
         target = str_to_title(target))
  
dorothea_regulon_human_coverage_v1 = get(load("data/dorothea_regulon_human_coverage_v1.rda"))

kinact_regulon_human = readRDS("data/kinact_regulon_human.rds")
  
# df = read_csv("data/phospho.csv") %>%
#   rename(protein = X1, logFC = fc, adj.p.value = fdr) %>%
#   mutate(contrast = "contrast_1") %>%
#   write_csv("data/phospho_clean.csv")

plot_lollipop = function(df, top_n_hits, var, var_label) {
  var = enquo(var)
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
    labs(x = var_label, y="Activity") +
    scale_color_manual(values = rwth_color(c("magenta", "green"))) +
    theme(legend.position = "none") +
    theme(aspect.ratio = c(1))
}

plot_heatmap = function(df, var="tf") {
  mat = df %>%
    select(!!var, contrast, activity) %>% 
    spread(contrast, activity) %>%
    data.frame(row.names = var, check.names = F)
  
  if (ncol(mat) > 1) {
    pheatmap(mat)
  } else if (ncol(mat) == 1) {
    pheatmap(mat, cluster_rows = F, cluster_cols = F)
  }
}

plot_network = function(network, num_nodes, var="tf") {
  var_of_interest = unique(network[[var]])
  sub_network = network %>% 
    drop_na() %>%
    filter(effect != "not regulated") %>%
    arrange(-importance) %>%
    mutate(n = row_number()) %>%
    filter(n <= num_nodes) %>%
    select(!!var, target, mor, effect)
  
  if (nrow(sub_network) > 0) {
    nodes_df = sub_network %>%
      distinct_(var, "target", "effect") %>%
      gather(class, name, -effect) %>%
      mutate(effect = case_when(class != var ~ effect,
                                class == var ~ "NA")) %>%
      mutate(effect = factor(effect, c("NA", "upregulated", "downregulated"))) %>%
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
      geom_node_point(aes(color = effect), size=10) +
      geom_node_text(aes(label = name), vjust = 0.4) + 
      theme_graph() +
      scale_color_manual(values = rwth_color(c("black50", "green50", "bordeaux50")), drop=F) +
      scale_edge_color_manual(values = rwth_color(c("bordeaux", "green")), drop=F) +
      theme(legend.position = "none",
            aspect.ratio = c(1))
  } else {
    nodes_df = tibble(class = var, name=var_of_interest, effect="NA")
    edges_df = NULL
    
    g = tbl_graph(nodes = nodes_df, edges = edges_df) %>%
      ggraph(layout = "nicely") + 
      geom_node_point(aes(color = effect), size=10) +
      geom_node_text(aes(label = name), vjust = 0.4) + 
      theme_graph() +
      scale_color_manual(values = rwth_color(c("black50", "green50", "red50")), drop=F) +
      scale_edge_color_manual(values = rwth_color(c("red", "green")), drop=F) +
      theme(legend.position = "none",
            aspect.ratio = c(1))
  }
  return(g)
}

plot_volcano = function(df, interactome, selected_top_n_labels, var) {
  var = enquo(var)
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
    theme(aspect.ratio = c(1))
}
  
options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {
  source("sub/02_server_upload.R", local=T)
  source("sub/03_server_dorothea.R", local=T)
  source("sub/04_server_progeny.R", local=T)
  source("sub/05_server_kinact.R", local=T)
  source("sub/06_server_integration.R", local=T)
}
