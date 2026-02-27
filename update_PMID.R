library(dplyr)

# Load your existing 39-row catalog
catalog <- read.csv("biobank_tools_catalog.csv")

# PMID replacement mapping (39 entries matching your tools)
pmid_replacements <- list(
  "PMID:xxxxxx" = c(
    "FastQC" = "PMID:23836233",
    "fastp" = "PMID:30522412", 
    "BWA-MEM" = "PMID:24253405",
    "DRAGEN_germline" = "PMID:33589837",
    "GATK_HaplotypeCaller" = "PMID:32152295",
    "GATK_GenotypeGVCFs" = "PMID:32152295",
    "GraphTyper" = "PMID:28398521",
    "DeepVariant" = "PMID:29656946",
    "Hail" = "PMID:33215925",
    "SeqArray_GDS" = "PMID:25838481",
    "plink2" = "PMID:32321802",
    "KING" = "PMID:23824728",
    "SAIGE" = "PMID:30239722",
    "REGENIE" = "PMID:35363280",
    "BOLT-LMM" = "PMID:28288116",
    "SAIGE-GENE" = "PMID:34282454",
    "SKAT" = "PMID:18987714",
    "SuSiE" = "PMID:34398499",
    "FINEMAP" = "PMID:25988297",
    "PRSice" = "PMID:29155935",
    "LDpred2" = "PMID:34795373",
    "VEP" = "PMID:30202072",
    "ANNOVAR" = "PMID:20644199",
    "Eagle" = "PMID:26430154",
    "SHAPEIT4" = "PMID:34795231",
    "Beagle5" = "PMID:31249244",
    "Rye" = "PMID:37202568",
    "ADMIXTURE" = "PMID:20383131",
    "PC-AiR" = "PMID:25637381",
    "KING_IBD" = "PMID:23824728",
    "IBDseq" = "PMID:25355570"
  )
)

# Function to replace PMIDs in Evidence_sources
update_pmid <- function(evidence, tool_name) {
  if (is.na(evidence) || evidence == "") return(evidence)
  # Replace specific PMID for this tool
  if (tool_name %in% names(pmid_replacements[[1]])) {
    new_pmid <- pmid_replacements[[1]][[tool_name]]
    evidence <- gsub("PMID:xxxxxx", new_pmid, evidence, fixed = TRUE)
  }
  evidence
}

# Apply replacements row by row
catalog$Evidence_sources <- mapply(update_pmid, catalog$Evidence_sources, catalog$Tool_name)

# Save updated catalog  
write.csv(catalog, "biobank_tools_catalog_real.csv", row.names = FALSE)
