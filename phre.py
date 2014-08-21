from Bio import SeqIO
import sys


def avg(xs):
    return float(sum(xs))/float(len(xs))

threshold = int(raw_input())
recs = list(SeqIO.parse(sys.stdin, "fastq"))

#print threshold
#print map(lambda x: x.letter_annotations["phred_quality"], recs)

print len(filter(lambda x: avg(x.letter_annotations["phred_quality"]) < threshold, recs))
