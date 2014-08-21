from Bio import SeqIO

recs = list(SeqIO.parse("rosalind_tfsq.txt", "fastq"))

for x in recs:
    print x.format("fasta")
