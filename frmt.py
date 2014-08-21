from Bio import Entrez, SeqIO

ids = raw_input().split()

Entrez.email = 'kuzux92@gmail.com'
print "fetching..."

handle = Entrez.efetch(db="nucleotide", id=ids, rettype="fasta")

print "fetched"

records = list(SeqIO.parse(handle, "fasta"))
shortest = sorted(records, cmp=lambda a, b: cmp(len(a), len(b)))[0]

print shortest.format("fasta")
