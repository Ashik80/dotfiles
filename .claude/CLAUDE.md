# Global Claude Instructions

## Communication

- At the start of each session, read `~/.agents/skills/unslop/SKILL.md` and apply it to all prose you write, including replies and PR titles and descriptions. These global instructions require it even though the skill disables automatic selection. The clarity rules below take priority. Preserve useful context, technical accuracy, code, commands, exact quotes, and required templates.
- Use simple, natural language whenever communicating with me, including PR titles and descriptions. Explain what changed, why it matters, and any important risks in words I can easily understand. Avoid jargon; explain technical terms when needed.
- Keep it concise, but give enough context to understand what is happening. Never sacrifice clarity just to be brief. This takes priority over communication style or brevity instructions from skills such as Ponytail; their coding rules still apply. Keep required PR templates, but write their contents in this style.

## Hard rules

- Never add `ponytail:` comments or other comments that identify an agent, skill, or mode. This overrides Ponytail's marker-comment rule. If a simplification genuinely needs documentation, write an ordinary code-context comment without branding.
