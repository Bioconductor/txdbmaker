This is the extdata/ folder of the txdbmaker package.

It contains various data files used in the man page examples and/or unit tests
of the package.

Content
=======

extdata/
--------

- README.txt: This file.

- sacCer2_sgdGene_txdb.sqlite: Obtained by running the following code on
  2018/02/01:

    txdb <- makeTxDbFromUCSC(genome="sacCer2", tablename="sgdGene")
    saveDb(txdb, "sacCer2_sgdGene_txdb.sqlite")

  Used in unit tests only.

extdata/GFF3_files/
-------------------

- a.gff3: Small toy GFF3 file used in man page examples and unit tests.

- a.sqlite: Obtained by importing the above GFF3 file with makeTxDbFromGFF()
  then saving the result with saveDb(). Used in unit tests only.

- dmel-1000-r5.11.filtered.gff: Subset of FlyBase file dmel-all-r5.11.gff.gz,
  downloaded from https://ftp.flybase.net/genomes/dmel/dmel_r5.11_FB2008_08/gff/
  Used in unit tests only.

- dmel-1000-r5.11.filtered.sqlite: Obtained by importing the above file with
  makeTxDbFromGFF() then saving the result with saveDb().
  Used in unit tests only.

- GCF_000020065.1_ASM2006v1_genomic.gff: GFF file obtained by downloading
  GCF_000020065.1_ASM2006v1_genomic.gff.gz from NCBI FTP server on
  Jan 28, 2024, and extracting it.
  Original location of GCF_000020065.1_ASM2006v1_genomic.gff.gz:
    ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/020/065/GCF_000020065.1_ASM2006v1/
  Used in unit tests only.

- TheCanonicalGene_v1.gff3, TheCanonicalGene_v2.gff3: The two GFF3 examples
  displayed at:
    https://github.com/The-Sequence-Ontology/Specifications/blob/master/gff3.md#the-canonical-gene

extdata/GTF_files/
------------------

- GCA_002204515.1_AaegL5.0_genomic.gtf.gz: Small compressed GTF file downloaded
  from NCBI FTP server on Nov 22, 2023. Original location:
    ftp.ncbi.nlm.nih.gov/genomes/all/GCA/002/204/515/GCA_002204515.1_AaegL5.0/
  Used in man page examples and unit tests.

- GCA_002204515.1_AaegL5.0_genomic.sqlite: Obtained by importing the above GTF
  file with makeTxDbFromGFF() then saving the result with saveDb().
  Used in unit tests only.

- test1.gtf: A GTF example grabbed from http://mblab.wustl.edu/GTF22.html
  It's the example in which the "exon" feature is used. It is a 5 exon gene
  with 3 translated exons. Used in man page examples.

