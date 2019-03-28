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
        echo '<link rel="stylesheet" href="/static/style.css">';
        echo '<title>' . \htmlentities($title) . '</title>';
        echo '<div class="deet--container">';
        $this->renderHeader();
        $this->renderNavigation();
        $this->renderContent($title, $body);
        $this->renderFooter();
        echo '</div>';
    }

    private function renderHeader(): void
    {
        echo '<header class="deet--header">';
        echo '<p>header</p>';
        echo '</header>';
    }

    private function renderNavigation(): void
    {
        echo '<nav class="deet--navigation">';
        echo '<p>navigation</p>';
        echo '</nav>';
    }

    /** @param callable():void $body */
    private function renderContent(string $title, callable $body): void
    {
        echo '<section class="deet--content">';
        echo '<h1>' . \htmlentities($title) . '</h1>';
        $body();
        echo '</section>';
    }

    private function renderFooter(): void
    {
        echo '<footer class="deet--footer">';
        echo '<p>footer</p>';
        echo '</footer>';
    }
}
