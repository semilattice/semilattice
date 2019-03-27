<?php
declare(strict_types = 1);
namespace Deet;

use Deet\Support\Template;

final class Main
{
    /** @var Template */
    private $template;

    public function __construct()
    {
        $this->template = new Template\Original();
    }

    public function viewPage(string $titlespace, string $title): void
    {
        $templateTitle = "$titlespace: $title";
        $templateBody = function(): void { echo 'Hello, world!'; };
        $this->template->render($templateTitle, $templateBody);
    }
}
