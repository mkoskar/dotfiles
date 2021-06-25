" vim: fdm=marker

if !has('nvim') | finish | endif

packadd! nvim-lspconfig

" ----------------------------------------

lua << EOF

local lspconfig = require('lspconfig')

local function on_attach(client, bufnr)
  local function set(...) vim.api.nvim_buf_set_option(bufnr, ...) end
  local function map(mode, lhs, rhs, opts)
    if opts == nil then opts = {noremap=true, silent=true} end
    vim.api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, opts)
  end

  -- Enable completion triggered by <C-X><C-O>
  set('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings
  map('n', '<C-Space>', '<Cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>')
  map('n', '<M-Space>', '<Cmd>lua vim.lsp.buf.clear_references()<CR>' ..
                        '<Cmd>lua vim.lsp.buf.document_highlight()<CR>')
  map('n', '<Space>', '<Cmd>nohlsearch | echo<CR>' ..
                      '<Cmd>lua vim.lsp.buf.clear_references()<CR>')
  map('n', '<LocalLeader>;', '<Cmd>lua vim.lsp.buf.hover()<CR>')
  map('n', '<LocalLeader>ca', '<Cmd>lua vim.lsp.buf.code_action()<CR>')
  map('n', '<LocalLeader>ci', '<Cmd>lua vim.lsp.buf.incoming_calls()<CR>')
  map('n', '<LocalLeader>cl', '<Cmd>lua vim.lsp.codelens.display()<CR>')
  map('n', '<LocalLeader>co', '<Cmd>lua vim.lsp.buf.outgoing_calls()<CR>')
  map('n', '<LocalLeader>ds', '<Cmd>lua vim.lsp.buf.document_symbol()<CR>')
  map('n', '<LocalLeader>fo', '<Cmd>lua vim.lsp.buf.formatting()<CR>')
  map('n', '<LocalLeader>i', '<Cmd>lua vim.lsp.buf.implementation()<CR>')
  map('n', '<LocalLeader>j', '<Cmd>lua vim.lsp.diagnostic.goto_next()<CR>')
  map('n', '<LocalLeader>k', '<Cmd>lua vim.lsp.diagnostic.goto_prev()<CR>')
  map('n', '<LocalLeader>ll', '<Cmd>lua vim.lsp.diagnostic.set_loclist()<CR>')
  map('n', '<LocalLeader>rf', '<Cmd>lua vim.lsp.buf.references()<CR>')
  map('n', '<LocalLeader>rn', '<Cmd>lua vim.lsp.buf.rename()<CR>')
  map('n', '<LocalLeader>s', '<Cmd>lua vim.lsp.buf.signature_help()<CR>')
  map('n', '<LocalLeader>td', '<Cmd>lua vim.lsp.buf.type_definition()<CR>')
  map('n', '<LocalLeader>wa', '<Cmd>lua vim.lsp.buf.add_workspace_folder()<CR>')
  map('n', '<LocalLeader>wl', '<Cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  map('n', '<LocalLeader>wr', '<Cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>')
  map('n', '<LocalLeader>ws', '<Cmd>lua vim.lsp.buf.workspace_symbol()<CR>')
  map('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>')
  map('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>')
end

vim.lsp.handlers['textDocument/publishDiagnostics'] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    --virtual_text = false,
    virtual_text = {
      spacing = 1,
    },
  }
)

lspconfig['pyright'].setup {
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 500,
  }
}

EOF
