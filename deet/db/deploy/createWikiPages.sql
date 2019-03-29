START TRANSACTION;

CREATE TABLE deet.wiki_pages (
    title               CHARACTER VARYING NOT NULL,
    body                CHARACTER VARYING NOT NULL,

    CONSTRAINT wiki_pages_pk
        PRIMARY KEY (title)
);

GRANT SELECT, INSERT, UPDATE, DELETE
    ON TABLE deet.wiki_pages
    TO deet_application;

COMMIT WORK;
