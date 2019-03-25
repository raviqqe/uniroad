#!/bin/sh

set -ex

elm make src/Main.elm --optimize --output public/index.html
firebase deploy
