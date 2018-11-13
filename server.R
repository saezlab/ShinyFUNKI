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
  
options(shiny.maxRequestSize=30*1024^2)

server = function(input, output, session) {


  source("sub/02_server_upload.R", local=T)
  source("sub/03_server_dorothea.R", local=T)
  # source("sub/04_server_progeny.R", local=T)
  # source("sub/05_server_kinact.R", local=T)
  # source("sub/06_server_integration.R", local=T)
  # source("sub/server_bookmark.R", local=T)
  

}
