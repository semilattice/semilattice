<?php
declare(strict_types = 1);
namespace Deet;

use Deet\Support\Template;

final class Main
{
    /** @var Template */
    private $template;

    /** @var ViewPage\Titlespaces */
    private $viewPageTitlespaces;

    public function __construct()
    {
        $this->template = new Template\Original();
        $this->viewPageTitlespaces = new ViewPage\Titlespaces([
            'Wiki' => new ViewWikiPage\WikiTitlespace(),
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
        $templateBody = function() use($page): void { $page->body(); };
        $this->template->render($templateTitle, $templateBody);
    }
}
