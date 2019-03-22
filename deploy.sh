#!/bin/sh

set -ex

elm make src/Main.elm --output public/index.html
firebase deploy
