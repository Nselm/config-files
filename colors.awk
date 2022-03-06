BEGIN { end = ")\\]"; start = "\\[$(tput setaf " 
				sop = "\\[$(tput "
				for (x=0; x<=255; x++){
								c[x]=start x end
				}
								}

{ gsub(/ORANGE/, c[166])
				gsub(/GREEN/, c[2])


				gsub(/BLINK/, sop "blink" end) 		
				gsub(/DIM/, sop "dim" end)	
				gsub(/ITALIC/, sop "sitm" end)	
				gsub(/UNDERLINE/, sop "smul" end) 
				gsub(/HIGHLIGHT/, sop "smso" end)

				gsub(/BOLD/, sop "bold" end)
				gsub(/CLR/, sop "sgr0" end)
				gsub(/\|/, "") 
}

{ print }

