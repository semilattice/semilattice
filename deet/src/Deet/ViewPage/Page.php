<?php
declare(strict_types = 1);
namespace Deet\ViewPage;

# A page knows how to render itself and can provide metadata about itself.

interface Page
{
    # TODO: The titlespaces are given for Wikitext inclusion. Instead, create a
    # TODO: class for rendering contexts, so that we can easily add more fields
    # TODO: to it later.
    public function body(Titlespaces $titlespaces): void;

    /** @return iterable<array{0:string,1:string}> */
    public function parents(): iterable;
}
