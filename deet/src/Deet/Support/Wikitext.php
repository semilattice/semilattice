<?php
declare(strict_types = 1);
namespace Deet\Support;

# Conversion from Wikitext to HTML.

# TODO: Write a generative test that we always generate valid HTML, no matter
# TODO: how horribly broken the Wikitext is.

final class Wikitext
{
    public static function render(string $input): void
    {
        $wikitext = new Wikitext($input);
        while ($wikitext->chunk());
    }

    /** @var string */
    private $input;

    /** @var int */
    private $offset;

    /** @var string[] */
    private $elementStack;

    public function __construct(string $input)
    {
        $this->input = $input;
        $this->offset = 0;
        $this->elementStack = [];
    }

    # Render one chunk of Wikitext and return whether it was the last one. Call
    # this repeatedly to render all Wikitext.
    public function chunk(): bool
    {
        if ($this->offset === 0)
        {
            echo '<p>'; # TODO: More principled paragraph handling.
        }

        # End of input.
        if ($this->offset >= \strlen($this->input))
        {
            echo '</p>'; # TODO: More principled paragraph handling.

            # TODO: Error at still open elements.
            # TODO: Render end tags for still open elements.
            return FALSE;
        }

        # Start tag.
        $matches = $this->match('/\G<([a-z]+)>/');
        if ($matches !== NULL)
        {
            $this->startTag($matches[1]);
            $this->offset += \strlen($matches[0]);
            return TRUE;
        }

        # End tag.
        $matches = $this->match('/\G<\/([a-z]+)>/');
        if ($matches !== NULL)
        {
            $this->endTag($matches[1]);
            $this->offset += \strlen($matches[0]);
            return TRUE;
        }

        # Backtick.
        $matches = $this->match('/\G`/');
        if ($matches !== NULL)
        {
            $this->backtick();
            $this->offset += 1;
            return TRUE;
        }

        # Plain text.
        echo \htmlentities(\substr($this->input, $this->offset, 1));
        $this->offset += 1;
        return TRUE;
    }

    # This function is called by chunk when encountering a start tag.
    private function startTag(string $element): void
    {
        # Open the element.
        $this->elementStack[] = $element;

        # Render the start tag.
        # TODO: Map from Wikitext elements to HTML elements.
        echo '<' . $element . '>';
    }

    # This function is called by chunk when encountering an end tag.
    private function endTag(string $element): void
    {
        # Check if element is open at all.
        if (!\in_array($element, $this->elementStack))
        {
            $this->error("dangling end tag: $element");
            return;
        }

        # Close any children that were not yet closed.
        for (;;)
        {
            $elementStackTop = \array_pop($this->elementStack);
            if ($elementStackTop === $element)
            {
                break;
            }
            else
            {
                $this->error("unclosed element: $elementStackTop");
                # TODO: Map from Wikitext elements to HTML elements.
                echo '</' . $elementStackTop . '>';
            }
        }

        # Render the end tag.
        # TODO: Map from Wikitext elements to HTML elements.
        echo '</' . $element . '>';
    }

    # Backticks are prohibited.
    private function backtick(): void
    {
        $this->error("backtick");
    }

    # This function renders a Wikitext syntax error.
    private function error(string $message): void
    {
        echo '<span class="deet--wikitext-error">';
        echo "Invalid Wikitext at offset $this->offset: ";
        echo \htmlentities($message);
        echo '.</span>';
    }

    /** @return ?array<string> */
    private function match(string $pattern): ?array
    {
        if (\preg_match($pattern, $this->input, $matches, 0, $this->offset))
        {
            return $matches;
        }
        else
        {
            return NULL;
        }
    }
}
