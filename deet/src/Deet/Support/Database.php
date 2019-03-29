<?php
declare(strict_types = 1);
namespace Deet\Support;

final class Database
{
    /** @var resource */
    private $handle;

    public function __construct(string $dsn)
    {
        $this->handle = \pg_connect($dsn);
    }

    /**
     * @param array<?string> $arguments
     * @return iterable<int,array<?string>>
     */
    public function query(string $sql, array $arguments): iterable
    {
        $result = \pg_query_params($this->handle, $sql, $arguments);
        for (;;) {
            /** @var false|array<?string> */
            $row = \pg_fetch_array($result, NULL, \PGSQL_NUM);
            if ($row === FALSE) {
                break;
            }
            yield $row;
        }
    }

    /**
     * @param array<?string> $arguments
     * @return ?array<?string>
     */
    public function queryOne(string $sql, array $arguments): ?array
    {
        foreach ($this->query($sql, $arguments) as $row)
        {
            return $row;
        }
        return NULL;
    }

    /** @param array<?string> $arguments */
    public function execute(string $sql, array $arguments): void
    {
        # It is important that the generator is advanced, otherwise it will not
        # perform the side-effects. There are probably no rows, so this should
        # be efficient.
        $rows = $this->query($sql, $arguments);
        foreach ($rows as $row) {
        }
    }
}
