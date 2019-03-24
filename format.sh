#!/bin/sh

for file in $(ls src/*.elm)
do
	elm-impfix -r $file
	elm-format --yes $file
done
