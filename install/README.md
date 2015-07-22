I use these scripts to install HLearn on AWS Ubuntu Precise machines (e.g. using [this AMI](https://us-west-2.console.aws.amazon.com/ec2/home?region=us-west-2#LaunchInstanceWizard:ami=ami-17b0b127)) and for testing with Travis CI (which also uses Ubuntu Precise).
The `ubuntu-precise.sh` script installs just the componenets needed for HLearn, whereas the `ubuntu-precise-extras.sh` script installs all the related packages needed to run the benchmarks and tests.
It should be pretty straightforward to adapt these scripts to whatever your preferred OS is.

To install on an Ubuntu Precise machine, pipe the script to a shell instance using:
```
curl https://raw.githubusercontent.com/mikeizbicki/HLearn/travis/install/ubuntu-precise.sh | sh
```
