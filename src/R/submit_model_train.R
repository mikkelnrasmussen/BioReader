library("tidyverse")

submit <- function(command, runtime, cores, ram, directory = "", modules = "",
                   job_name = "bioreader", email = "", queue = "hpc",
                   jobscript = "jobscript", output = "log/bioreader_%J.out",
                   error = "log/bioreader_%J.err", host = 1, depend = NULL) {
  # Convert inputs to integers
  runtime <- as.integer(runtime)
  cores <- as.integer(cores)
  ram <- as.integer(ram)
  max_ram <- ram + ram * 0.05

  # Checks
  if (cores > 32) {
    stop("Can't use more than 32 cores on a node")
  }
  if (ram > 22.5) {
    stop("Can't use more than 22.5 GB memory per node. There is 32 * 22.5 = 720 GB on a node")
  }
  if (runtime < 1) {
    stop("Must allocate at least 1 minute runtime")
  }

  # Calculate walltime
  hours <- floor(runtime / 60)
  minutes <- runtime %% 60
  walltime <- sprintf("%d:%02d", hours, minutes)

  # Set directory
  if (directory == "") {
    directory <- getwd()
  }

  # Construct the job script
  script <- paste0(
    "#!/bin/sh\n",
    "### General options\n",
    "### -- specify queue -- \n",
    "#BSUB -q ", queue, "\n",
    "### -- set the job Name --\n",
    "#BSUB -J ", job_name, "\n",
    "### -- ask for number of cores -- \n",
    "#BSUB -n ", cores, "\n",
    "### -- specify that the cores must be on the same host -- \n",
    "#BSUB -R \"span[hosts=", host, "]\"\n",
    "### -- specify that we need XGB of memory per core/slot -- \n",
    "#BSUB -R \"rusage[mem=", ram, "GB]\"\n",
    "### -- specify that we want the job to get killed if it exceeds X GB per core/slot -- \n",
    "#BSUB -M ", max_ram, "GB\n",
    "### -- set walltime limit: hh:mm -- \n",
    "#BSUB -W ", walltime, "\n",
    "### -- set the email address -- \n",
    "# please uncomment the following line and put in your e-mail address,\n",
    "# if you want to receive e-mail notifications on a non-default address\n",
    "#BSUB -u ", email, "\n",
    "### -- send notification at start -- \n",
    "#BSUB -B\n",
    "### -- send notification at completion -- \n",
    "#BSUB -N \n",
    "### -- Specify the output and error file. %J is the job-id -- \n",
    "### -- -o and -e mean append, -oo and -eo mean overwrite -- \n",
    "#BSUB -e ", error, "\n",
    "#BSUB -o ", output, "\n"
  )
  if (!is.null(depend)) {
    script <- paste0(script, "#BSUB -w \"done(", depend, ")\"\n")
  }
  if (modules != "") {
    script <- paste0(script, "module load ", modules, "\n")
  }
  # Add R stuff
  script <- paste0(
    script,
    "export R_BATCH_OPTIONS=\"--no-save\"\n",
    "mkdir -p log/\n",
    "mkdir -p time/\n"
  )
  script <- paste0(script, command, "\n")

  # Write job script to file
  if (!startsWith(jobscript, "/")) {
    jobscript <- file.path(directory, jobscript)
  }
  writeLines(script, jobscript)

  # Submit the job
  job <- system(paste("bsub <", jobscript), intern = TRUE)
  jobid <- unlist(strsplit(job, " "))[2]
  gsub("\\[.*\\]", "", jobid) # return the job ID
}

###################################################################
######################### Load Data ###############################
###################################################################

df_class_label <- read_csv("data/class_info.csv")

# for (target in unique(df_class_label$category)) {
for (target in c("Cancer")) {
  command <- paste(
    "/usr/bin/time -v -o time/bioreader.time Rscript src/R/main_PRIAT.R -a",
    target,
    "-p --hpc -k"
  )
  job_name <- paste0("bioreader_", target)
  jobscript <- paste0("log/jobscript_", target)

  # Submit trainig of model usage
  job_id <- submit(
    command = command,
    runtime = 1800,
    cores = 10,
    ram = 7,
    job_name = job_name,
    jobscript = jobscript,
    email = "s193518@dtu.dk",
    modules = "R/4.3.1-mkl2023update1",
  )
}
