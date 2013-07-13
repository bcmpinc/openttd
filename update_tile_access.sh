#!/bin/bash

perl -pi -e "s/_m\[([^\]]+)\]\./GetTile(\1)->/g; s/_me\[([^\]]+)\]\./GetTileEx(\1)->/g" src/*.*
