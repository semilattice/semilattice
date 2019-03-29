<?php
declare(strict_types = 1);
namespace Deet\ViewWikiPage;

use Deet\ViewPage\Page;
use Deet\Support\Wikitext;

# A wiki page renders wikitext.

final class WikiPage implements Page
{
    public function body(): void
    {
        $wikitext = '<b><i>Lorem ipsum</em></b> <s>dolor</s> `sit amet.';
        Wikitext::render($wikitext);
    }

    /** @return iterable<array{0:string,1:string}> */
    public function parents(): iterable
    {
        return [['Wiki', 'Example 1'], ['Wiki', 'Example 2']];
    }
}
