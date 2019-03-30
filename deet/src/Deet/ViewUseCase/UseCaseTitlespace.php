<?php
declare(strict_types = 1);
namespace Deet\ViewUseCase;

use Deet\ViewPage\Page;
use Deet\ViewPage\Titlespace;

final class UseCaseTitlespace implements Titlespace
{
    public function retrievePage(string $title): ?Page
    {
        return new UseCase(['Administrator', 'Author'], ['A', 'B', 'C']);
    }
}
