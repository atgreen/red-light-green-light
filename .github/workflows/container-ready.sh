#!/bin/bash

CONTAINER_HEALTH=$(docker inspect --format='{{json .State.Health.Status}}' $1)
until test $CONTAINER_HEALTH = "\"healthy\""; do
    echo "Waiting $1"
    sleep 1
    CONTAINER_HEALTH=$(docker inspect --format='{{json .State.Health.Status}}' $1)
    if [ -z $CONTAINER_HEALTH ]; then
        echo "Container $1 crashed"
        exit 1
    fi;
    if [ $CONTAINER_HEALTH = "\"unhealthy\"" ]; then
        echo "Container $1 is not ready!"
        docker container ls
        exit 1
    fi
done
echo "Container $1 is ready!"
