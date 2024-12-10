local map = vim.keymap.set

local toggle_checked = function()
  local current_line = vim.api.nvim_get_current_line()
  local unchecked = "%[ %]"
  local checked = "%[x%]"

  if current_line:find(unchecked) then
    current_line = string.gsub(current_line, unchecked, checked, 1)
  elseif current_line:find(checked) then
    current_line = string.gsub(current_line, checked, unchecked, 1)
  end

  vim.api.nvim_set_current_line(current_line)
end

local toggle_checklist = function()
  local current_line = vim.api.nvim_get_current_line()
  local new_check_item = "%- %[ %] "
  local check_item = "%- %[.%] "
  local bullet_point = "%- "

  local checked_item_found = current_line:find(check_item)
  local bullet_point_found = current_line:find(bullet_point)
  if checked_item_found then
    current_line = string.gsub(current_line, check_item, '', 1)
  elseif bullet_point_found then
    current_line = string.gsub(current_line, bullet_point, new_check_item, 1)
  else
    current_line = string.gsub(current_line, "^", new_check_item, 1)
  end

  vim.api.nvim_set_current_line(current_line)
end

map("n", "<leader>c", toggle_checked, { desc = "Markdown check/uncheck checklist" })
map("n", "<leader>t", toggle_checklist, { desc = "Markdown toggle check item" })
