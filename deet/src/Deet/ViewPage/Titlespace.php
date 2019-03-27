<?php
declare(strict_types = 1);
namespace Deet\ViewPage;

# Retrieving a page by its title, assuming a titlespace.

interface Titlespace
{
    public function retrievePage(string $title): ?Page;
}
