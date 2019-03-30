<?php
declare(strict_types = 1);
namespace Deet\ViewUseCase;

use Deet\ViewPage\Page;
use Deet\ViewPage\Titlespaces;

final class UseCase implements Page
{
    /** @var string[] */
    private $actors;

    /** @var string[] */
    private $procedure;

    /**
     * @param string[] $actors
     * @param string[] $procedure
     */
    public function __construct(array $actors, array $procedure)
    {
        $this->actors = $actors;
        $this->procedure = $procedure;
    }

    public function body(Titlespaces $titlespaces): void
    {
        echo '<table>';
        echo '<tbody>';

        echo '<tr>';
        echo '<th>Actors</th>';
        echo '<td><ul>';
        foreach ($this->actors as $actor)
            echo '<li>' . \htmlentities($actor) . '</li>';
        echo '</ul></td>';
        echo '</tr>';

        echo '<tr>';
        echo '<th>Procedure</th>';
        echo '<td><ol>';
        foreach ($this->procedure as $step)
            echo '<li>' . \htmlentities($step) . '</li>';
        echo '</ol></td>';
        echo '</tr>';

        echo '</tbody>';
        echo '</table>';
    }

    /** @return iterable<array{0:string,1:string}> */
    public function parents(): iterable
    {
        return [];
    }
}
