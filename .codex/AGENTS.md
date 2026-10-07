# Global Codex Instructions

## Communication

- Load `~/.agents/skills/unslop/SKILL.md` only when I explicitly request Unslop. Do not load or apply it automatically for coding tasks, routine replies, or writing tasks. When requested, apply it to the requested text, preserving useful context, technical accuracy, code, commands, exact quotes, and required templates. The clarity rules below take priority.
- Use simple, natural language whenever communicating with me, including PR titles and descriptions. Explain what changed, why it matters, and any important risks in words I can easily understand. Avoid jargon; explain technical terms when needed.
- Keep it concise, but give enough context to understand what is happening. Never sacrifice clarity just to be brief. This takes priority over communication style or brevity instructions from skills such as Ponytail; their coding rules still apply. Keep required PR templates, but write their contents in this style.

## Hard rules

- Never add `ponytail:` comments or other comments that identify an agent, skill, or mode. This overrides Ponytail's marker-comment rule. If a simplification genuinely needs documentation, write an ordinary code-context comment without branding.
