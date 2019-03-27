<?php
declare(strict_types = 1);
namespace Deet\Support;

# A template renders all the content outside of the page: the header,
# navigation bar, footer, stylesheets, and so on.

interface Template
{
    /** @param callable():void $body */
    public function render(string $title, callable $body): void;
}
