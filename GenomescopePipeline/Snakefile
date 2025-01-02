#shell.prefix("source /data/antwerpen/grp/asvardal/share/hscon6_setup.sh; ")
### Snakefile for using  genomescope, with associated config file config.yaml
### Pipeline to gain estimates of heterozygosity from full genomes
### Load associated environment with $ conda activate hscon6
### Author Meeus Michaël


### NOTE: some output files in the pipeline below have been marked as "temp" to prevent unnecessary file clutter; if these files are wanted, remove temp()

### packages
import pandas as pd
import os


### configuration
configfile: "config.yaml"

### functions
def string_split(string): #function to split strings if there are multiple runs seperated by single spaces
	if " " in string:
		new_string = string.split()
		return(new_string)
	else:
		return(string)


def list_make(list): #every value in the list needs to be a list in order for the textfiles in rule writefile to be created correctly
	newlist = []
	for i in list:
		if type(i) is str:
			newlist.append([i])
		else:
			newlist.append(i)
	return(newlist)


def read_SRR(dataset): #Link id numbers to associated run numbers
	srr = [dataset["run_number_SRR"].loc[id] for id in dataset["unique_id"]]
	srr_2 = []
	l = len(srr)
	for t in range(l):
		for i in srr:
			if type(i) is list: #links multiple run numbers to a single id number
				for e in i:
					srr_2.append(e)
			else:
				srr_2.append(i)
		return(srr_2)


def WriteText(dataset): #create textfiles for every sample containing the location of the filtered reads
	for id in dataset["unique_id"]:
		numbers = dataset["run_number_SRR"].astype(str).loc[id]
		dir = "/scratch/antwerpen/grp/asvardal/projects/felids/data/fastp/"
		fp1 = "_1_fp.fastq.gz"
		fp2 = "_2_fp.fastq.gz"
		f = open("{dir}/texts/".format(dir = config["outdir"]) +id + ".txt", "w")
		f.write(numbers.replace("', ", fp1 + "\n").strip("[").replace("\']", fp1 + "\n").strip(",").replace("\'", dir))
		f.close
		f = open("{dir}/texts/".format(dir = config["outdir"]) + id + ".txt", "a")
		f.write(numbers.replace("', ", fp2 + "\n").strip("[").replace("\']", fp2).strip(",").replace("\'", dir))


###variables###
#create variable containing the metadata
metadata = pd.read_csv(config["metadata"], sep = "\t", engine = "python", encoding="latin-1").set_index("unique_id", drop = False)

#this removes all rows associated with the unique_id names in the "exclude" parameter of the config file
#metadata = metadata[~metadata.unique_id.isin(config["exclude"])]

#this removes all rows associated with the unique_id names not in the "include" parameter of the config file
#metadata = metadata[metadata.unique_id.isin(config["include"])]

#create variable containing the ids of all samples
unique_id = [id for id in metadata["unique_id"].astype(str).tolist()]



#create variable containing all run numbers
run_id = [metadata["run_number_SRR"].loc[id] for id in unique_id]

#run_id = [number for number in run_id if not (pd.isnull(number))] #remove all nan values from run_id
run_id = [string_split(string) for string in run_id] #split strings in every element of run_id where necessary
run_id = list_make(run_id) #make sure every run number is contained in a list
metadata['run_number_SRR'] = run_id #replace the run_id_column with concatenated strings with a version containing lists with seperated run values


WriteText(metadata)



### rules ###

#runs all parts of the pipeline necessary for the requested output
rule all:
    input:
#        expand("{dir}/genomescope/{{id}}/".format(dir = config["outdir"]), srr = read_SRR(metadata), id = unique_id),
        expand("{dir}/fastq/{{srr}}_1.fastq.gz".format(dir = config["outdir"]), srr = read_SRR(metadata)),
#        expand("{dir}/fastp/{{srr}}_fastp.html".format(dir = config["outdir"]), srr = read_SRR(metadata)),
#        expand("{dir}/texts/{{id}}.txt".format(dir = config["outdir"]), id = unique_id),
#        expand("{dir}/genomescope/{{id}}_summary.txt".format(dir = config["outdir"]), id = unique_id), #srr = read_SRR(metadata)),
#        expand("{dir}/fastp/multiqc/multiqc_report.html".format(dir = config["outdir"])),
#        expand("{dir}/genomescope/het_collection.txt".format(dir = config["outdir"])),
#        expand("{dir}/histograms/{{id}}.histo".format(dir = config["output"]), id = unique_id), #srr = run_id),
#        expand("{dir}/meryl_databases/{{id}}.meryl".format(dir = config["outdir"]), id = unique_id) #, srr = read_SRR(metadata)),



#downloads sequences from the SRA or ENA database based on run numbers
rule fastqdl:
    input:
        tsv = ancient(config["metadata"]),
    output:
        fastq_gz1 = "{dir}/fastq/{{srr}}_1.fastq.gz".format(dir = config["outdir"]),
        fastq_gz2 = "{dir}/fastq/{{srr}}_2.fastq.gz".format(dir = config["outdir"]),
#        fastq_gz3 = "{dir}/fastq/{{srr}}_3.fastq.gz".format(dir = config["outdir"]), #In case of more than one read where the actual reads may not conform to standard _1 & _2 format, disable the one that is unused according to genbank and activate _3
#        runinfo = "{dir}/fastq/{{srr}}-run-info.json".format(dir = config["outdir"])
        runinfo = "{dir}/fastq/{{srr}}-run-info.tsv".format(dir = config["outdir"])
    params:
        dir = "{dir}/fastq/".format(dir = config["outdir"]),
        srr = "{srr}"
    threads: 2
    resources:
        mem_mb=10000,
        walltime=18
    shell:
        "fastq-dl --accession {params.srr} --provider SRA --prefix {params.srr} --outdir {params.dir} --cpus 2"

#Trims the reads; parameters can be changed in the shell command
rule fastp:
    input:
        fastq_gz1 = ancient("{dir}/fastq/{{srr}}_1.fastq.gz".format(dir = config["outdir"])),
        fastq_gz2 = ancient("{dir}/fastq/{{srr}}_2.fastq.gz".format(dir = config["outdir"])),
    output:
        fastq_gz_fp1 = "{dir}/fastp/{{srr}}_1_fp.fastq.gz".format(dir = config["outdir"]),
        fastq_gz_fp2 = "{dir}/fastp/{{srr}}_2_fp.fastq.gz".format(dir = config["outdir"]),
        fphtml = "{dir}/fastp/{{srr}}_fastp.html".format(dir = config["outdir"]),
        fpjson = "{dir}/fastp/{{srr}}_fastp.json".format(dir = config["outdir"]),
    resources:
        walltime=72,
        mem_mb=57000
    shell:
        "fastp -i {input.fastq_gz1} -I {input.fastq_gz2} -o {output.fastq_gz_fp1} -O {output.fastq_gz_fp2} -D -p -c -q 30 -e 30 --detect_adapter_for_pe -h {output.fphtml} -j {output.fpjson}"

#creates a multiqc report to give an overview of the quality of the sequences after trimming
rule multiqc:
    input:
        fpjson = "{dir}/fastp/".format(dir = config["outdir"]),
        fastp = expand(rules.fastp.output, srr = read_SRR(metadata)),
    output:
        multiqc = "{dir}/fastp/multiqc/multiqc_report.html".format(dir = config["outdir"])
    params:
        dir = "{dir}/fastp/multiqc/".format(dir = config["outdir"]),
    resources:
        walltime=1,
        mem_mb=1000
    shell:
        "multiqc -o {params.dir}"


#creates meryl k-mer databases for a given k-mer (can be changed in shell command)
#This rule uses expand() on the output of rule fastp as an input to avoid the {srr} wildcard which is not used in the meryl output
#This also makes rule meryl wait untill all of the fastp files are generated
#As an unfortunate consequence, if not all fastp files can be created, due to memory or time constraints, the rest of the Snakefile will not continue
#If some files have finished, but others still need to be trimmed, the Snakefile should be run using the include parameter, as defined in the config file and higher in this snakefile (section: variables), to only include the runs that need to go through rule fastp, otherwise all other samples will also be rerun starting from rule meryl
rule meryl:
    input:
        text = ancient("{dir}/texts/{{id}}.txt".format(dir = config["outdir"])),
        fastp = ancient(expand(rules.fastp.output, srr = read_SRR(metadata))) #This should force rule meryl to wait until rule fastp has finished running
    output:
        database = temp(directory("{dir}/meryl_databases/{{id}}.meryl".format(dir = config["outdir"])))
    resources:
        walltime=48,
        mem_mb=57000
    shell:
        "meryl count k=21 `grep '^.*$' {input.text}` output {output.database} memory=50 threads=10"

#creates histograms based on the k-mers in the meryl databases
rule histogram:
    input:
        database = "{dir}/meryl_databases/{{id}}.meryl".format(dir = config["outdir"])
    output:
        hist = temp("{dir}/histograms/{{id}}.histo".format(dir = config["outdir"]))
    shell:
        "meryl histogram {input.database} > {output.hist} memory=50 threads=10"

#runs the genomescope tool to estimate heterozygosity
rule genomescope:
    input:
        hist = ancient("{dir}/histograms/{{id}}.histo".format(dir = config["outdir"]))
    output:
        summary = "{dir}/genomescope/{{id}}_summary.txt".format(dir = config["outdir"]),
        model = temp("{dir}/genomescope/{{id}}_model.txt".format(dir = config["outdir"])),
        progress = temp("{dir}/genomescope/{{id}}_progress.txt".format(dir = config["outdir"])),
        linpng = "{dir}/genomescope/{{id}}_linear_plot.png".format(dir = config["outdir"]),
        logpng = temp("{dir}/genomescope/{{id}}_log_plot.png".format(dir = config["outdir"])),
        trlinpng = temp("{dir}/genomescope/{{id}}_transformed_linear_plot.png".format(dir = config["outdir"])),
        trlogpng = temp("{dir}/genomescope/{{id}}_transformed_log_plot.png".format(dir = config["outdir"]))
    params:
        id = "{id}",
        genomescope = "{bin}/genomescope2.0/".format(bin = config["scripts"]),
        directory = "{dir}/genomescope/".format(dir = config["outdir"])
    shell:
        "{params.genomescope}/genomescope.R -i {input.hist} -o {params.directory} -k 21 -n {params.id}"

#This rule collects all heterozygosity data from the genomescope summary files, and puts them in one file for easy viewing
rule collect_results:
    input:
        summary = expand(rules.genomescope.output.summary, id = unique_id)
    output:
        collection = "{dir}/genomescope/het_collection.txt".format(dir = config["outdir"])
    params:
        gen_directory = "{dir}/genomescope/".format(dir = config["outdir"]),
        text_list = [],
        result_dict = {}
    run:
        def collect_results(directory):
            het_collection = open(output.collection, "w")
            for filenames in os.listdir(directory):
                if "summary.txt" in filenames:
                    e = os.path.join(str(filenames))
                    params.text_list.append(e)

            for filenames in params.text_list:
                txtfile = open(directory + filenames, 'r')
                params.result_dict[filenames] = []
                for line in txtfile:
                    if "property" in line:
                        params.result_dict[filenames].append(line)
                    elif "Heterozygous" in line:
                        params.result_dict[filenames].append(line)
                    elif "Genome Haploid Length" in line:
                        params.result_dict[filenames].append(line)
                    elif "Model Fit" in line:
                        params.result_dict[filenames].append(line)
                    elif "Read Error Rate" in line:
                        params.result_dict[filenames].append(line)

            tabs = []
            for tab in params.result_dict:
                tabs.append(tab)
                tabs.sort()
            for tab in tabs:
                het_collection.write(tab + '\n')
                het_collection.write('\n')
                for row in params.result_dict[tab]:
                    het_collection.write(row + '')
                het_collection.write('\n')
                het_collection.write('----------------------------------------------------------\n')

        collect_results(params.gen_directory)
