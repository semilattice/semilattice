<?php
declare(strict_types = 1);
namespace Deet\ViewWikiPage;

use Deet\ViewPage\Page;
use Deet\Support\Wikitext;

# A wiki page renders wikitext.

final class WikiPage implements Page
{
    /** @var string */
    private $wikitext;

    public function __construct(string $wikitext)
    {
        $this->wikitext = $wikitext;
    }

    public function body(): void
    {
        Wikitext::render($this->wikitext);
    }

    /** @return iterable<array{0:string,1:string}> */
    public function parents(): iterable
    {
        return [['Wiki', 'Example 1'], ['Wiki', 'Example 2']];
    }
}
