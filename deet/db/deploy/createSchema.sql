START TRANSACTION;

CREATE SCHEMA deet;

GRANT USAGE
    ON SCHEMA deet
    TO deet_application;

COMMIT WORK;