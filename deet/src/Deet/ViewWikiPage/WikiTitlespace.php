<?php
declare(strict_types = 1);
namespace Deet\ViewWikiPage;

use Deet\Support\Database;
use Deet\ViewPage\Page;
use Deet\ViewPage\Titlespace;

# Finding wiki pages by their titles.

final class WikiTitlespace implements Titlespace
{
    /** @var Database */
    private $database;

    public function __construct(Database $database)
    {
        $this->database = $database;
    }

    public function retrievePage(string $title): ?Page
    {
        $row = $this->database->queryOne(/* sql */ '
            SELECT body
            FROM deet.wiki_pages
            WHERE title = $1
        ', [$title]);

        if ($row === NULL)
        {
            return NULL;
        }
        else
        {
            assert($row[0] !== NULL);
        }

        return new WikiPage($row[0]);
    }
}
