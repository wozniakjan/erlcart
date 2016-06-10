#!/bin/bash

ssh 556f9860e0b8cdaf36000016@erlang-wozj.rhcloud.com

rm -rf $OPENSHIFT_DIY_DIR/test/*
rm -rf $OPENSHIFT_DIY_DIR/deploy/erlang/*
rm -rf $OPENSHIFT_DIY_DIR/deploy/elixir/*

cd diy/otp
git update
git checkout OTP-18.2.1

./otp_build autoconf
./configure #--prefix=$OPENSHIFT_ERL_DIR/usr/bin/  #maybe prefix is needed, will see later
make
make release
cp -r release/x86_64-unknown-linux-gnu/* $OPENSHIFT_DIY_DIR/deploy/erlang/

cd release/x86_64-unknown-linux-gnu
./Install "`pwd`"
export PATH="`pwd`/bin:$PATH"

cd ~/diy/elixir
git update
git checkout v1.2.1
export MIX_ARCHIVES="$OPENSHIFT_DIY_DIR/mix"
make clean
make test

cp -r $OPENSHIFT_DIY_DIR/elixir/bin $OPENSHIFT_DIY_DIR/deploy/elixir/
cp -r $OPENSHIFT_DIY_DIR/elixir/lib $OPENSHIFT_DIY_DIR/deploy/elixir/

cp --symbolic-link $OPENSHIFT_DIY_DIR/otp/release/x86_64-unknown-linux-gnu/bin/* $OPENSHIFT_DIY_DIR/test/
cp --symbolic-link $OPENSHIFT_DIY_DIR/deploy/elixir/bin/* $OPENSHIFT_DIY_DIR/test/

export PATH="$OPENSHIFT_DIY_DIR/test:$PATH"

scp 556f9860e0b8cdaf36000016@erlang-wozj.rhcloud.com:~/diy/deploy/erlang.zip .
//TODO
