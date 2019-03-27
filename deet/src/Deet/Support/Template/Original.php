<?php
declare(strict_types = 1);
namespace Deet\Support\Template;

use Deet\Support\Template;

class Original implements Template
{
    /** @param callable():void $body */
    public function render(string $title, callable $body): void
    {
        echo '<!DOCTYPE html>';
        echo '<meta charset="utf-8">';
        echo '<title>' . \htmlentities($title) . '</title>';
        echo '<h1>' . \htmlentities($title) . '</h1>';
        $body();
    }
}
