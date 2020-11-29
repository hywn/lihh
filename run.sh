#!/usr/bin/env bash
sed -e 's/--.*//' $1 | runghc -Wno-tabs Cool.hs | runghc -Wno-tabs Lihh.hs