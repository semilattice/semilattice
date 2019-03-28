<?php
declare(strict_types = 1);
namespace Deet\ViewWikiPage;

use Deet\ViewPage\Page;

# A wiki page renders wikitext.

final class WikiPage implements Page
{
    public function body(): void
    {
        echo '<p>wiki page lol</p>';
    }

    /** @return iterable<array{0:string,1:string}> */
    public function parents(): iterable
    {
        return [['Wiki', 'Example 1'], ['Wiki', 'Example 2']];
    }
}
