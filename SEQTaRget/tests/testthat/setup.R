# Cap data.table to 2 threads during tests to comply with CRAN policy, which
# allows at most 2 cores in package checks. Without this, data.table defaults to
# half the available cores and the check machine reports a high CPU/elapsed ratio.
data.table::setDTthreads(2)
