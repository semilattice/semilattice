<?php
declare(strict_types = 1);
namespace Deet\Support;

# Conversion from Wikitext to HTML.
#
# Wikitext resembles HTML, but there are differences:
#
#  - The handling of whitespace is much more trivial in Wikitext than it is in
#    HTML. For example, whitespace inside a tag is prohibited.
#
#  - There is a different set of elements. There are simple elements, which
#    translate to HTML 1:1. For example, b becomes strong and does nothing more
#    than that. And there are complex elements, which have more interesting
#    behavior. For example, math renders LaTeX mathematics.
#
#  - A blank line introduces a new paragraph.
#
#  - Backticks are prohibited in normal text.
#
# Methods that deal with Wikitext have names
# that begin with «process». Methods that deal with HTML have names that begin
# with «render».

# TODO: Write a generative test that we always generate valid HTML, no matter
# TODO: how horribly broken the Wikitext is.

final class Wikitext
{
    public static function render(string $input): void
    {
        $wikitext = new Wikitext($input);
        while ($wikitext->processChunk());
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

    # Process one chunk of Wikitext and return whether it was the last one.
    # Call this repeatedly to render all Wikitext.
    public function processChunk(): bool
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
            $this->processStartTag($matches[1]);
            $this->offset += \strlen($matches[0]);
            return TRUE;
        }

        # End tag.
        $matches = $this->match('/\G<\/([a-z]+)>/');
        if ($matches !== NULL)
        {
            $this->processEndTag($matches[1]);
            $this->offset += \strlen($matches[0]);
            return TRUE;
        }

        # Backtick.
        $matches = $this->match('/\G`/');
        if ($matches !== NULL)
        {
            $this->processBacktick();
            $this->offset += 1;
            return TRUE;
        }

        # Plain text.
        echo \htmlentities(\substr($this->input, $this->offset, 1));
        $this->offset += 1;
        return TRUE;
    }

    # This function is called by chunk when encountering a start tag.
    private function processStartTag(string $element): void
    {
        # Open the element.
        $this->elementStack[] = $element;

        # Render the start tag.
        $this->renderStartTag($element);
    }

    # This function is called by chunk when encountering an end tag.
    private function processEndTag(string $element): void
    {
        # Check if element is open at all.
        if (!\in_array($element, $this->elementStack))
        {
            $this->renderError("dangling end tag: $element");
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
                $this->renderError("unclosed element: $elementStackTop");
                $this->renderEndTag($elementStackTop);
            }
        }

        # Render the end tag.
        $this->renderEndTag($element);
    }

    # Backticks are prohibited.
    private function processBacktick(): void
    {
        $this->renderError("backtick");
    }

    # For every simple Wikitext element, the corresponding HTML element.
    private static function htmlElement(string $element): ?string
    {
        switch ($element)
        {
        case 'b': return 'strong';
        case 'i': return 'em';
        case 's': return 's';
        default: return NULL;
        }
    }

    # Render an HTML start tag for a Wikitext element.
    private function renderStartTag(string $element): void
    {
        $htmlElement = self::htmlElement($element);
        if ($htmlElement === NULL)
        {
            $this->renderError("invalid element in start tag: $element");
        }
        else
        {
            echo "<$htmlElement>";
        }
    }

    # Render an HTML end tag for a Wikitext element.
    private function renderEndTag(string $element): void
    {
        $htmlElement = self::htmlElement($element);
        if ($htmlElement === NULL)
        {
            $this->renderError("invalid element in end tag: $element");
        }
        else
        {
            echo "</$htmlElement>";
        }
    }

    # This function renders a Wikitext syntax error.
    private function renderError(string $message): void
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
