<?php
declare(strict_types = 1);
namespace Deet\ViewPage;

# Collection of titlespaces, by name.

final class Titlespaces
{
    /** @var array<string,Titlespace> */
    private $titlespaces;

    /** @param array<string,Titlespace> $titlespaces */
    public function __construct(array $titlespaces)
    {
        $this->titlespaces = $titlespaces;
    }

    public function retrievePage(string $titlespace, string $title): ?Page
    {
        if (\array_key_exists($titlespace, $this->titlespaces))
        {
            return $this->titlespaces[$titlespace]->retrievePage($title);
        }
        else
        {
            return NULL;
        }
    }
}
