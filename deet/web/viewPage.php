<?php
require_once __DIR__ . '/bootstrap.php';
(new Deet\Main())->viewPage($_GET['titlespace'], $_GET['title']);
