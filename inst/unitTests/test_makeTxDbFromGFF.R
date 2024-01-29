gffFile <- system.file("extdata", "GFF3_files", "a.gff3",
                       package="txdbmaker")

gtfFile <- system.file("extdata", "GTF_files",
                       "GCA_002204515.1_AaegL5.0_genomic.gtf.gz",
                       package="txdbmaker")

flyFile <- system.file("extdata","GFF3_files","dmel-1000-r5.11.filtered.gff",
                       package="txdbmaker")

## bad bacterial GFFs require use of special argument to ignore most of data.
gffB <- system.file("extdata", "GFF3_files",
                    "GCF_000020065.1_ASM2006v1_genomic.gff",
                    package="txdbmaker")

## Test that outputs match what is expected. ## BOOM
test_makeTxDbFromGFF <- function(){  
  ## wanted
  gffDBFile <- system.file("extdata", "GFF3_files", "a.sqlite",
                           package="txdbmaker")
  txdb_gff <- loadDb(gffDBFile)

  ## generated
  txdb1 <- makeTxDbFromGFF(file=gffFile,
               dataSource="partial GFF file for Tomatoes for testing",
               organism="Solanum lycopersicum",
               circ_seqs=character(0))

  ## test
  checkTrue(GenomicFeatures:::compareTxDbs(txdb1, txdb_gff))

  
  ## wanted
  gtfDBFile <- system.file("extdata", "GTF_files",
                           "GCA_002204515.1_AaegL5.0_genomic.sqlite",
                           package="txdbmaker")
  txdb_gtf <- loadDb(gtfDBFile)

  ## generated
  chrominfo <- data.frame(chrom="MF194022.1", length=16790, is_circular=TRUE)
  dataSource <- paste0("https://ftp.ncbi.nlm.nih.gov/genomes/all/",
                       "GCA/002/204/515/GCA_002204515.1_AaegL5.0/",
                       "GCA_002204515.1_AaegL5.0_genomic.gtf.gz")
  organism <- "Aedes aegypti"
  metadata <- data.frame(name="Genome", value="AaegL5.0")

  txdb2 <- makeTxDbFromGFF(gtfFile, dataSource=dataSource, organism=organism,
                           chrominfo=chrominfo, metadata=metadata)

  ## test
  checkTrue(GenomicFeatures:::compareTxDbs(txdb2, txdb_gtf))


  ## wanted
  flyDBFile <- system.file("extdata", "GFF3_files",
                           "dmel-1000-r5.11.filtered.sqlite",
                           package="txdbmaker")
  txdb_fly <- loadDb(flyDBFile)

  txdb3 <- makeTxDbFromGFF(file=flyFile,
                           dataSource="gff file from flybase",
                           organism="Drosophila melanogaster",
                           circ_seqs=character(0))
  
  checkTrue(GenomicFeatures:::compareTxDbs(txdb3, txdb_fly))


  ## test for broken NCBI bacterial GFFs (that only seem to have
  ## reliable gene info and little else)
  chrominfoBac <- data.frame(chrom = c('NC_011025.1'),
                          length=c(830000), ## placeholder = iow it big enough
                          is_circular=c(TRUE))

  ## mostly I want to see if if can run this:
  txdb_bac <- makeTxDbFromGFF(file = gffB,
                              chrominfo = chrominfoBac,
                              dataSource = "NCBI",
                              organism = "Metamycoplasma arthritidis")

  ## Tests
  checkTrue(class(txdb_bac) == "TxDb")
  checkEquals(length(transcripts(txdb_bac)), 672)
}

