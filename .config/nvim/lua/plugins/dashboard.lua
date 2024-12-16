return {
  'goolord/alpha-nvim',
  config = function()
    require('alpha').setup(require('config.alpha').config)
  end
}
