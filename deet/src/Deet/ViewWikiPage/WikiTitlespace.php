<?php
declare(strict_types = 1);
namespace Deet\ViewWikiPage;

use Deet\ViewPage\Page;
use Deet\ViewPage\Titlespace;

# Finding wiki pages by their titles.

final class WikiTitlespace implements Titlespace
{
    public function retrievePage(string $title): ?Page
    {
        return new WikiPage();
    }
}
