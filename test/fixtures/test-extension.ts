/**
 * Test Extension for pi-coding-agent integration tests
 *
 * Provides commands that exercise the extension API without triggering LLM turns.
 * Used to verify that extension commands properly return to idle state.
 *
 * Load with: pi --mode rpc -e test/fixtures/test-extension.ts --no-extensions
 */

import type { ExtensionAPI } from "@anthropic/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  /**
   * /test-noop - Does nothing, returns immediately
   *
   * Tests that extension commands without LLM turns return to idle state.
   * This is the minimal test case for the spinner/status bug.
   */
  pi.registerCommand("test-noop", {
    description: "Test: no-op command (returns immediately)",
    handler: async () => {
      // Intentionally empty - tests command lifecycle
    },
  });

  /**
   * /test-message - Sends a custom message via pi.sendMessage()
   *
   * Tests that pi.sendMessage() works correctly and returns to idle.
   */
  pi.registerCommand("test-message", {
    description: "Test: sends a custom message",
    handler: async () => {
      pi.sendMessage({
        customType: "test-message",
        content: "Test message from extension",
        display: true,
      });
    },
  });

  /**
   * /test-notify - Shows a notification via ctx.ui.notify()
   *
   * Tests that ctx.ui.notify() works in RPC mode.
   */
  pi.registerCommand("test-notify", {
    description: "Test: shows notification",
    handler: async (_args, ctx) => {
      ctx.ui.notify("Test notification from extension", "info");
    },
  });

  /**
   * /test-confirm - Shows a confirm dialog and sends a follow-up message
   *
   * Tests that ctx.ui.confirm() works and that follow-up messages are displayed.
   */
  pi.registerCommand("test-confirm", {
    description: "Test: confirm dialog with follow-up message",
    handler: async (_args, ctx) => {
      const result = await ctx.ui.confirm("Test Confirm", "Do you confirm?");
      pi.sendMessage({
        customType: "test-confirm-response",
        content: result ? "CONFIRMED" : "CANCELLED",
        display: true,
      });
    },
  });

  /**
   * /test-select - Shows a select dialog and sends a follow-up message
   *
   * Tests that ctx.ui.select() works and that follow-up messages are displayed.
   */
  pi.registerCommand("test-select", {
    description: "Test: select dialog with follow-up message",
    handler: async (_args, ctx) => {
      const result = await ctx.ui.select("Test Select", ["Option A", "Option B", "Option C"]);
      pi.sendMessage({
        customType: "test-select-response",
        content: result !== undefined ? `SELECTED: ${result}` : "CANCELLED",
        display: true,
      });
    },
  });

  /**
   * /test-input - Shows an input dialog and sends a follow-up message
   *
   * Tests that ctx.ui.input() works and that follow-up messages are displayed.
   */
  pi.registerCommand("test-input", {
    description: "Test: input dialog with follow-up message",
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
