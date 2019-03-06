# Test for ludwig package for tensorflow

## To build the docker image
`docker build --rm -t jupyter/ludwig .`
## To run the Jupyter notebook
`docker run --rm -p 8888:8888 -e JUPYTER_ENABLE_LAB=yes -v "$PWD":/home/jovyan/work jupyter/ludwig`
