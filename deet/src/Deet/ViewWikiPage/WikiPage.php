<?php
declare(strict_types = 1);
namespace Deet\ViewWikiPage;

use Deet\Support\Wikitext;
use Deet\Support\WikitextFactory;
use Deet\ViewPage\Page;
use Deet\ViewPage\Titlespaces;

# A wiki page renders wikitext.

final class WikiPage implements Page
{
    /** @var string */
    private $body;

    public function __construct(string $body)
    {
        $this->body = $body;
    }

    public function body(Titlespaces $titlespaces): void
    {
        Wikitext::render($titlespaces, $this->body);
    }

    /** @return iterable<array{0:string,1:string}> */
    public function parents(): iterable
    {
        return [['Wiki', 'Example 1'], ['Wiki', 'Example 2']];
    }
}
