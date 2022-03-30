#!/usr/bin/env bash
set -euo pipefail

just clippy_arguments="-D warnings" check-all
