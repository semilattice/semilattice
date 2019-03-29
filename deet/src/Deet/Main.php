<?php
declare(strict_types = 1);
namespace Deet;

use Deet\Support\Database;
use Deet\Support\Template;

final class Main
{
    /** @var Template */
    private $template;

    /** @var ViewPage\Titlespaces */
    private $viewPageTitlespaces;

    public function __construct()
    {
        $database = new Database('');
        $this->template = new Template\Original();
        $this->viewPageTitlespaces = new ViewPage\Titlespaces([
            'Wiki' => new ViewWikiPage\WikiTitlespace($database),
        ]);
    }

    public function viewPage(string $titlespace, string $title): void
    {
        # TODO: Move this code to the ViewPage directory.

        $page = $this->viewPageTitlespaces->retrievePage($titlespace, $title);
        if ($page === NULL)
        {
            # TODO: Render a 404 Not Found page.
            die();
        }

        $templateTitle = "$titlespace: $title";
        $templateBody = function() use($page): void {
            $page->body();
            echo '<nav class="deet--page-parents">';
            foreach ($page->parents() as list($parentTitlespace, $parentTitle))
            {
                echo '<a href="/viewPage.php';
                echo '?titlespace=' . \urlencode($parentTitlespace);
                echo '&amp;title=' . \urlencode($parentTitle);
                echo '">';
                echo \htmlentities("$parentTitlespace: $parentTitle");
                echo '</a>';
            }
            echo '</nav>';
        };
        $this->template->render($templateTitle, $templateBody);
    }
}
