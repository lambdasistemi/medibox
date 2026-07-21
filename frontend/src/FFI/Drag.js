export const trackVerticalDrag = (startClientY) => (onDelta) => (onEnd) => () => {
  let active = true;
  const body = document.body;
  const previousCursor = body ? body.style.cursor : "";
  const previousUserSelect = body ? body.style.userSelect : "";

  const cleanup = () => {
    if (!active) {
      return;
    }

    active = false;
    window.removeEventListener("mousemove", handleMouseMove);
    window.removeEventListener("mouseup", handleMouseUp);

    if (body) {
      body.style.cursor = previousCursor;
      body.style.userSelect = previousUserSelect;
    }
  };

  const handleMouseMove = (event) => {
    if (!active) {
      return;
    }

    event.preventDefault();
    onDelta(event.clientY - startClientY)();
  };

  const handleMouseUp = (event) => {
    if (!active) {
      return;
    }

    event.preventDefault();
    cleanup();
    onEnd();
  };

  if (body) {
    body.style.cursor = "ns-resize";
    body.style.userSelect = "none";
  }

  window.addEventListener("mousemove", handleMouseMove);
  window.addEventListener("mouseup", handleMouseUp);

  return cleanup;
};
