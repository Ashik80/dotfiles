1. Clone the repository with `git clone --depth 1 https://github.com/be5invis/Iosevka`
2. Place the `private-build-plans.toml` file in the repository
3. Run `npm install`
4. Run `npm run build -- contents::IosevkaCustom`
5. Copy the *.ttf files from `build/` directory to `~/.fonts/`
6. Refresh the font cache with `fc-cache -f -v`
7. The font that we want to use is `Iosevka Custom`
