#!/bin/sh
        for file in `ls *cdk90`
          do
#                echo `ls $file`
                fichier=`ls $file |cut -d"." -f1 `
                echo $fichier
#                echo `ls $file |cut -d"." -f1  `
                mv $file ${fichier}.f90             
          done

