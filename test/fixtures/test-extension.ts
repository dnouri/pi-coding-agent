/**
 * Test extension for pi-coding-agent integration tests.
 * Load with: pi --mode rpc -e test/fixtures/test-extension.ts --no-extensions
 */

import type { ExtensionAPI } from "@anthropic/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.registerCommand("test-noop", {
    description: "No-op command that returns immediately",
    handler: async () => {},
  });

  pi.registerCommand("test-message", {
    description: "Sends a custom message via pi.sendMessage()",
    handler: async () => {
      pi.sendMessage({
        customType: "test-message",
        content: "Test message from extension",
        display: true,
      });
    },
  });

  pi.registerCommand("test-notify", {
    description: "Shows a notification via ctx.ui.notify()",
    handler: async (_args, ctx) => {
      ctx.ui.notify("Test notification from extension", "info");
    },
  });

  pi.registerCommand("test-confirm", {
    description: "Confirm dialog with follow-up message",
    handler: async (_args, ctx) => {
      const result = await ctx.ui.confirm("Test Confirm", "Do you confirm?");
      pi.sendMessage({
        customType: "test-confirm-response",
        content: result ? "CONFIRMED" : "CANCELLED",
        display: true,
      });
    },
  });

  pi.registerCommand("test-select", {
    description: "Select dialog with follow-up message",
    handler: async (_args, ctx) => {
      const result = await ctx.ui.select("Test Select", ["Option A", "Option B", "Option C"]);
      pi.sendMessage({
        customType: "test-select-response",
        content: result !== undefined ? `SELECTED: ${result}` : "CANCELLED",
        display: true,
      });
    },
  });

  pi.registerCommand("test-input", {
    description: "Input dialog with follow-up message",
    handler: async (_args, ctx) => {
      const result = await ctx.ui.input("Test Input", "placeholder");
      pi.sendMessage({
        customType: "test-input-response",
        content: result !== undefined ? `INPUT: ${result}` : "CANCELLED",
        display: true,
      });
    },
  });
}
