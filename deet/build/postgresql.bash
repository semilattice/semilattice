mkdir pgcluster
initdb pgcluster
# TODO: Instead of sleep, use something that checks that PostgreSQL is online.
postgres -D pgcluster & sleep 0.1

# TODO: db/setup has a shebang line; use that instead.
bash db/setup

(
    export HOME=$PWD
    cd db
    sqitch deploy build
)
