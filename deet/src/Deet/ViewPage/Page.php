<?php
declare(strict_types = 1);
namespace Deet\ViewPage;

# A page knows how to render itself and can provide metadata about itself.

interface Page
{
    public function body(): void;

    /** @return iterable<array{0:string,1:string}> */
    public function parents(): iterable;
}
