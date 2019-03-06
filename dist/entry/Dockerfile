FROM dbms-checker:latest
MAINTAINER lierdakil

ENV USER hsapp
ENV DATA /var/lib/$USER
#VOLUME $DATA

RUN useradd -rm -d $DATA -s /bin/false -N -u 199 $USER

RUN mkdir $DATA/data && chown -R $USER $DATA/data

ENV DBMS_CHECKER_DATA_DIR $DATA/data
ENV DBMS_CHECKER_FRONTEND_DIR /usr/local/share/dbms-checker-client

WORKDIR $DATA
USER $USER
CMD ["/usr/local/bin/dbms-checker-server"]
