mkdir pgcluster
initdb pgcluster
# TODO: Instead of sleep, use something that checks that PostgreSQL is online.
postgres -D pgcluster & sleep 0.1
createuser deet
createdb -O deet deet
