_theme_complete() {
  local line
  COMPREPLY=()

  while IFS= read -r line; do
    COMPREPLY+=("$line")
  done < <(
    THEME_COMPLETION=1 \
      COMP_WORD="${COMP_WORDS[$COMP_CWORD]}" \
      theme
  )
}
complete -F _theme_complete theme
