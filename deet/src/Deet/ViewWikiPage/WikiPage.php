<?php
declare(strict_types = 1);
namespace Deet\ViewWikiPage;

use Deet\ViewPage\Page;

# A wiki page renders wikitext.

final class WikiPage implements Page
{
    public function body(): void
    {
        echo 'wiki page lol';
    }
}
