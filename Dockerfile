FROM opencpu/base
RUN R -e 'remotes::install_github("resplab/chdwilson")'
RUN R -e 'remotes::install_github("resplab/chdwilsonPrism")'
RUN echo "opencpu:opencpu" | chpasswd
