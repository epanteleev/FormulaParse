#!/bin/bash
string="//------------------------------------------------------------------------------
// $1
// 
// Copyright (c) 2020 Afti
// All rights reserved.
// $2
// Date:       $(date +%F)
// Author:     $(git config user.name)
//------------------------------------------------------------------------------"

{ echo "$string" ; cat $1; } > temp.txt
mv temp.txt $1