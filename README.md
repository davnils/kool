This is a reduced testcase triggering a blocking bug when invoking "safeCall".

Install by doing git clone, stack setup, stack install, and:

stack exec kool-server


.. and repeatedly (from the base directory):

for i in $(seq 1 10); do stack exec -- kool-client; done

Eventually there will be a case where it prints "Found service on nid://127.0.0.1:9999:0, calling" but doesn't terminate.
