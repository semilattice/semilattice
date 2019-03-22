<?php
declare(strict_types = 1);
namespace Deet;

final class Main
{
    private function __construct()
    {
    }

    public static function viewPage(string $title): void
    {
        echo \htmlentities($title);
    }
}
