export const connect = (url) => (onMessage) => (onOpen) => (onClose) => () => {
  const socket = new WebSocket(url);

  socket.addEventListener("message", (event) => {
    if (typeof event.data === "string") {
      onMessage(event.data)();
    }
  });

  socket.addEventListener("open", () => {
    onOpen();
  });

  socket.addEventListener("close", () => {
    onClose();
  });

  return socket;
};

export const send = (socket) => (message) => () => {
  if (socket.readyState === WebSocket.OPEN) {
    socket.send(message);
  }
};
