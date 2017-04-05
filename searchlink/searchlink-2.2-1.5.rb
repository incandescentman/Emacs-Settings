#!/Usr/bin/env ruby
# encoding: utf-8

SILENT = ENV['SL_SILENT'] =~ /false/i ? false : true || true
VERSION = '2.2.5'
# SearchLink by Brett Terpstra 2015 &lt;http://brettterpstra.com/projects/searchlink/&gt;
# MIT License, please maintain attribution
require 'net/https'
require 'uri'
require 'rexml/document'
require 'shellwords'
require 'yaml'
require 'cgi'
require 'fileutils'
require 'time'

if RUBY_VERSION.to_f &gt; 1.9
  Encoding.default_external = Encoding::UTF_8
  Encoding.default_internal = Encoding::UTF_8
end

class String
  def clean
    gsub(/\n+/,' ').gsub(/"/,"&amp;quot").gsub(/\|/,"-").gsub(/([&amp;\?]utm_[scm].+=[^&amp;\s!,\.\)\]]++?)+(&amp;.*)/, '\2').sub(/\?&amp;/,'').strip
  end

  def to_am # convert itunes to apple music link
    input = self.dup
    input.sub!(/\/itunes\.apple\.com/,'geo.itunes.apple.com')
    append = input =~ /\?[^\/]+=/ ? '&amp;app=music' : '?app=music'
    input + append
  end
end

class SearchLink
  attr_reader :originput, :output, :clipboard
  attr_accessor :cfg
  # You can optionally copy any config variables to a text file
  # at "~/.searchlink" to make them permanent during upgrades
  # Values found in ~/.searchlink will override defaults in
  # this script

  def initialize(opt={})

    @printout = opt[:echo] || false
    unless File.exists? File.expand_path("~/.searchlink")
      default_config =&lt;&lt;ENDCONFIG
# set to true to have an HTML comment included detailing any errors
debug: true
# set to true to have an HTML comment included reporting results
report: true

# use Notification Center to display progress
notifications: false

# when running on a file, back up original to *.bak
backup: true

# change this to set a specific country for search (default US)
country_code: US

# set to true to force inline Markdown links
inline: false

# set to true to include a random string in reference titles.
# Avoids conflicts if you're only running on part of a document
# or using SearchLink multiple times within a document
prefix_random: true

# set to true to add titles to links based on the page title
# of the search result
include_titles: false

# confirm existence (200) of generated links. Can be disabled
# per search with `--v`, or enabled with `++v`.
validate_links: false

# append affiliate link info to iTunes urls, empty quotes for none
# example:
# itunes_affiliate = "&amp;at=10l4tL&amp;ct=searchlink"
itunes_affiliate: "&amp;at=10l4tL&amp;ct=searchlink"

# to create Amazon affiliate links, set amazon_partner to your amazon
# affiliate tag
#    amazon_partner: "bretttercom-20"
amazon_partner: "bretttercom-20"

# To create custom abbreviations for Google Site Searches,
# add to (or replace) the hash below.
# "abbreviation" =&gt; "site.url",
# This allows you, for example to use [search term](!bt)
# as a shortcut to search brettterpstra.com (using a site-specific
# Google search). Keys in this list can override existing
# search trigger abbreviations.
#
# If a custom search starts with "http" or "/", it becomes
# a simple replacement. Any instance of "$term" is replaced
# with a URL-escaped version of your search terms.
# Use $term1, $term2, etc. to replace in sequence from
# multiple search terms. No instances of "$term" functions
# as a simple shortcut. "$term" followed by a "d" lowercases
# the replacement. Use "$term1d," "$term2d" to downcase
# sequential replacements (affected individually).
# Long flags (e.g. --no-validate_links) can be used after
# any url in the custom searches.
custom_site_searches:
  bt: brettterpstra.com
  btt: http://brettterpstra.com/$term1d/$term2d
  bts: /search/$term --no-validate_links
  md: www.macdrifter.com
  ms: macstories.net
  dd: www.leancrew.com
  spark: macsparky.com
  man: http://man.cx/$term
  dev: developer.apple.com
  nq: http://nerdquery.com/?media_only=0&amp;query=$term&amp;search=1&amp;category=-1&amp;catid=&amp;type=and&amp;results=50&amp;db=0&amp;prefix=0
  gs: http://scholar.google.com/scholar?btnI&amp;hl=en&amp;q=$term&amp;btnG=&amp;as_sdt=80006
# Remove or comment (with #) history searches you don't want
# performed by `!h`. You can force-enable them per search, e.g.
# `!hsh` (Safari History only), `!hcb` (Chrome Bookmarks only),
# etc. Multiple types can be strung together: !hshcb (Safari
# History and Chrome bookmarks).
history_types:
- chrome_history
- chrome_bookmarks
- safari_bookmarks
- safari_history
ENDCONFIG
      File.open(File.expand_path("~/.searchlink"), 'w') do |f|
        f.puts default_config
      end
    end

    @cfg = YAML.load_file(File.expand_path("~/.searchlink"))

    # set to true to have an HTML comment inserted showing any errors
    @cfg['debug'] ||= false

    # set to true to get a verbose report at the end of multi-line processing
    @cfg['report'] ||= false

    @cfg['backup'] = true unless @cfg.has_key? 'backup'

    # set to true to force inline links
    @cfg['inline'] ||= false

    # set to true to add titles to links based on site title
    @cfg['include_titles'] ||= false

    # change this to set a specific country for search (default US)
    @cfg['country_code'] ||= "US"

    # set to true to include a random string in ref titles
    # allows running SearchLink multiple times w/out conflicts
    @cfg['prefix_random'] = false unless @cfg['prefix_random']

    # append affiliate link info to iTunes urls, empty quotes for none
    # example:
    # $itunes_affiliate = "&amp;at=10l4tL&amp;ct=searchlink"
    @cfg['itunes_affiliate'] ||= "&amp;at=10l4tL&amp;ct=searchlink"

    # to create Amazon affiliate links, set amazon_partner to your amazon
    # affiliate tag
    #    amazon_partner: "bretttercom-20"
    @cfg['amazon_partner'] ||= ''

    # To create custom abbreviations for Google Site Searches,
    # add to (or replace) the hash below.
    # "abbreviation" =&gt; "site.url",
    # This allows you, for example to use [search term](!bt)
    # as a shortcut to search brettterpstra.com. Keys in this
    # hash can override existing search triggers.
    @cfg['custom_site_searches'] ||= {
      "bt" =&gt; "brettterpstra.com",
      "md" =&gt; "www.macdrifter.com"
    }

    # confirm existence of links generated from custom search replacements
    @cfg['validate_links'] ||= false

    # use notification center to show progress
    @cfg['notifications'] ||= false

    @line_num = nil;
    @match_column = nil;
    @match_length = nil;
  end

  def available_searches
    searches = [
      ["a", "Amazon"],
      ["g", "Google"],
      ["b", "Bing"],
      ["wiki", "Wikipedia"],
      ["s", "Software search (Google)"],
      ["@t", "Twitter user link"],
      ["@adn", "App.net user link"],
      ["am", "Apple Music"],
      ["amart", "Apple Music Artist"],
      ["amalb", "Apple Music Album"],
      ["amsong", "Apple Music Song"],
      ["ampod", "Apple Music Podcast"],
      ["ipod", "iTunes podcast"],
      ["isong", "iTunes song"],
      ["iart", "iTunes artist"],
      ["ialb", "iTunes album"],
      ["lsong", "Last.fm song"],
      ["lart", "Last.fm artist"],
      ["mas", "Mac App Store"],
      ["masd", "Mac App Store developer link"],
      ["itu", "iTunes App Store"],
      ["itud", "iTunes App Store developer link"],
      ["def", "Dictionary definition"],
      ["sp", "Spelling"],
      ["h", "Web history"],
      ["hs[hb]", "Safari [history, bookmarks]"],
      ["hc[hb]", "Chrome [history, bookmarks]"]
    ]
    out = ""
    searches.each {|s|
      out += "!#{s[0]}#{spacer(s[0])}#{s[1]}\n"
    }
    out
  end

  def spacer(str)
    len = str.length
    str.scan(/[mwv]/).each do |tt|
      len += 1
    end
    str.scan(/[t]/).each do |tt|
      len -= 1
    end
    spacer = case len
    when 0..3
      "\t\t"
    when 4..12
      " \t"
    end
    spacer
  end

  def get_help_text
    help_text =&lt;&lt;EOHELP
-- [Available searches] -------------------
#{available_searches}
EOHELP

    if @cfg['custom_site_searches']
      help_text += "\n-- [Custom Searches] ----------------------\n"
      @cfg['custom_site_searches'].each {|label, site|
        help_text += "!#{label}#{spacer(label)} #{site}\n"
      }
    end
    help_text
  end

  def help_dialog
    help_text = "[SearchLink v#{VERSION}]\n\n"
    help_text += get_help_text
    help_text += "\nClick \\\"More Help\\\" for additional information"
    res = %x{osascript &lt;&lt;'APPLESCRIPT'
set _res to display dialog "#{help_text.gsub(/\n/,'\\\n')}" buttons {"OK", "More help"} default button "OK" with title "SearchLink Help"

return button returned of _res
APPLESCRIPT
    }.strip
    if res == "More help"
      %x{open http://brettterpstra.com/projects/searchlink}
    end
  end

  def help_cli
    $stdout.puts get_help_text
  end

  def parse(input)
    @output = ''
    return false unless input &amp;&amp; input.length &gt; 0
    parse_arguments(input, {:only_meta =&gt; true})
    @originput = input.dup

    if input.strip =~ /^help$/i
      if SILENT
        help_dialog # %x{open http://brettterpstra.com/projects/searchlink/}
      else
        $stdout.puts "SearchLink v#{VERSION}"
        $stdout.puts "See http://brettterpstra.com/projects/searchlink/ for help"
      end
      print input
      Process.exit
    end

    @cfg['inline'] = true if input.scan(/\]\(/).length == 1 &amp;&amp; input.split(/\n/).length == 1
    @errors = {}
    @report = []

    links = {}
    @footer = []
    counter_links = 0
    counter_errors = 0

    input.sub!(/\n?&lt;!-- Report:.*?--&gt;\n?/m, '')
    input.sub!(/\n?&lt;!-- Errors:.*?--&gt;\n?/m, '')

    input.scan(/\[(.*?)\]:\s+(.*?)\n/).each {|match|
      links[match[1].strip] = match[0]
    }

    if @cfg['prefix_random']
      if input =~ /\[(\d{4}-)\d+\]: \S+/
        prefix = $1
      else
        prefix = ("%04d" % rand(9999)).to_s + "-"
      end
    else
      prefix = ""
    end

    highest_marker = 0
    input.scan(/^\s{,3}\[(?:#{prefix})?(\d+)\]: /).each do |match|
      highest_marker = $1.to_i if $1.to_i &gt; highest_marker
    end

    footnote_counter = 0
    input.scan(/^\s{,3}\[\^(?:#{prefix})?fn(\d+)\]: /).each do |match|
      footnote_counter = $1.to_i if $1.to_i &gt; footnote_counter
    end

    if input =~ /\[(.*?)\]\((.*?)\)/
      lines = input.split(/\n/)
      out = []

      total_links = input.scan(/\[(.*?)\]\((.*?)\)/).length
      in_code_block = false
      line_difference = 0
      lines.each_with_index {|line, num|
        @line_num = num - line_difference
        cursor_difference = 0
        # ignore links in code blocks
        if line =~ /^( {4,}|\t+)[^\*\+\-]/
          out.push(line)
          next
        end
        if line =~ /^[~`]{3,}/
          if in_code_block
            in_code_block = false
            out.push(line)
            next
          else
            in_code_block = true
          end
        end
        if in_code_block
          out.push(line)
          next
        end

        # line.gsub!(/\(\$ (.*?)\)/) do |match|
        #   this_match = Regexp.last_match
        #   match_column = this_match.begin(0)
        #   match_string = this_match.to_s
        #   match_before = this_match.pre_match
        #   match_after = this_match.post_match
        #   # todo: inline searches in larger context
        # end

        delete_line = false

        line.gsub!(/\[(.*?)\]\((.*?)\)/) do |match|
          this_match = Regexp.last_match
          @match_column = this_match.begin(0) - cursor_difference
          match_string = this_match.to_s
          @match_length = match_string.length
          match_before = this_match.pre_match

          invalid_search = false
          ref_title = false

          if match_before.scan(/(^|[^\\])`/).length % 2 == 1
            add_report("Match '#{match_string}' within an inline code block")
            invalid_search = true
          end

          counter_links += 1
          $stderr.print("\033[0K\rProcessed: #{counter_links} of #{total_links}, #{counter_errors} errors. ") unless SILENT

          link_text = this_match[1] || ''
          link_info = parse_arguments(this_match[2].strip).strip || ''

          if link_text == '' &amp;&amp; link_info =~ /".*?"/
            link_info.gsub!(/\"(.*?)\"/) {|q|
              link_text = $1 if link_text == ''
              $1
            }
          end

          if link_info.strip =~ /:$/ &amp;&amp; line.strip == match
            ref_title = true
            link_info.sub!(/\s*:\s*$/,'')
          end

          unless link_text.length &gt; 0 || link_info.sub(/^[!\^]\S+/,'').strip.length &gt; 0
            add_error('No input', match)
            counter_errors += 1
            invalid_search = true
          end

          if link_info =~ /^!(\S+)/
            search_type = $1
            unless valid_search?(search_type) || search_type =~ /^(\S+\.)+\S+$/
              add_error('Invalid search', match)
              invalid_search = true
            end
          end


          if invalid_search
            match
          elsif link_info =~ /^\^(.+)/
            if $1.nil? || $1 == ''
              match
            else
              note = $1.strip
              footnote_counter += 1
              if link_text.length &gt; 0 &amp;&amp; link_text.scan(/\s/).length == 0
                ref = link_text
              else
                ref = prefix + "fn" + ("%04d" % footnote_counter).to_s
              end
              add_footer "[^#{ref}]: #{note}"
              res = %Q{[^#{ref}]}
              cursor_difference = cursor_difference + (@match_length - res.length)
              @match_length = res.length
              add_report("#{match_string} =&gt; Footnote #{ref}")
              res
            end
          elsif (link_text == "" &amp;&amp; link_info == "") || is_url?(link_info)
            add_error('Invalid search', match) unless is_url?(link_info)
            match
          else

            if link_text.length &gt; 0 &amp;&amp; link_info == ""
              link_info = link_text
            end

            search_type = ''
            search_terms = ''
            link_only = false
            @clipboard = false


            if link_info =~ /^(?:[!\^](\S+))?\s*(.*)$/

              if $1.nil?
                search_type = 'g'
              else
                search_type = $1
              end

              search_terms = $2.gsub(/(^["']|["']$)/, '')
              search_terms.strip!

              search_terms = link_text if search_terms == ''

              # if the input starts with a +, append it to the link text as the search terms
              search_terms = link_text + " " + search_terms.strip.sub(/^\+\s*/, '') if search_terms.strip =~ /^\+[^\+]/

              # if the end of input contain "^", copy to clipboard instead of STDOUT
              @clipboard = true if search_terms =~ /(!!)?\^(!!)?$/

              # if the end of input contains "!!", only print the url
              link_only = true if search_terms =~ /!!\^?$/

              search_terms.sub!(/(!!)?\^?(!!)?$/,"")

            elsif link_info =~ /^\!/
              search_word = link_info.match(/^\!(\S+)/)

              if search_word &amp;&amp; valid_search?(search_word[1])
                search_type = search_word[1] unless search_word.nil?
                search_terms = link_text
              elsif search_word &amp;&amp; search_word[1] =~ /^(\S+\.)+\S+$/
                search_type = 'g'
                search_terms = "site:#{search_word[1]} #{link_text}"
              else
                add_error('Invalid search', match)
                search_type = false
                search_terms = false
              end

            elsif link_text &amp;&amp; link_text.length &gt; 0 &amp;&amp; (link_info.nil? || link_info.length == 0)
              search_type = 'g'
              search_terms = link_text
            else
              add_error('Invalid search', match)
              search_type = false
              search_terms = false
            end

            @cfg['custom_site_searches'].each {|k,v|
              if search_type == k
                link_text = search_terms if link_text == ''
                v = parse_arguments(v, {:no_restore =&gt; true})
                if v =~ /^(\/|http)/i
                  search_type = 'r'
                  tokens = v.scan(/\$term\d+d?/).sort.uniq

                  if tokens.length &gt; 0
                    highest_token = 0
                    tokens.each {|token|
                      if token =~ /(\d+)d?$/
                        highest_token = $1.to_i if $1.to_i &gt; highest_token
                      end
                    }
                    terms_p = search_terms.split(/ +/)
                    if terms_p.length &gt; highest_token
                      remainder = terms_p[highest_token-1..-1].join(" ")
                      terms_p = terms_p[0..highest_token - 2]
                      terms_p.push(remainder)
                    end
                    tokens.each {|t|
                      if t =~ /(\d+)d?$/
                        int = $1.to_i - 1
                        replacement = terms_p[int]
                        if t =~ /d$/
                          replacement.downcase!
                          re_down = ""
                        else
                          re_down = "(?!d)"
                        end
                        v.gsub!(/#{Regexp.escape(t)+re_down}/, CGI.escape(replacement))
                      end
                    }
                    search_terms = v


                  else
                    search_terms = v.gsub(/\$termd?/i) {|m|
                      search_terms.downcase! if m =~ /d$/i
                      CGI.escape(search_terms)
                    }
                  end

                else
                  search_type = 'g'
                  search_terms = "site:#{v} #{search_terms}"
                end

                break
              end
            } if search_type &amp;&amp; search_terms &amp;&amp; search_terms.length &gt; 0

            if search_type &amp;&amp; search_terms
              url = false
              title = false
              # $stderr.puts "Searching #{search_type} for #{search_terms}"
              url, title, link_text = do_search(search_type, search_terms, link_text)

              if url
                link_text = title if link_text == '' &amp;&amp; title

                if link_only || search_type =~ /sp(ell)?/ || url == 'embed'
                  url = title if url == 'embed'
                  cursor_difference = cursor_difference + (@match_length - url.length)
                  @match_length = url.length
                  add_report("#{match_string} =&gt; #{url}")
                  url
                elsif ref_title
                  unless links.has_key? url
                    links[url] = link_text
                    add_footer make_link('ref_title', link_text, url, title)
                  end
                  delete_line = true
                elsif @cfg['inline']
                  res = make_link('inline', link_text, url, title)
                  cursor_difference = cursor_difference + (@match_length - res.length)
                  @match_length = res.length
                  add_report("#{match_string} =&gt; #{url}")
                  res
                else
                  unless links.has_key? url
                    highest_marker += 1
                    links[url] = prefix + ("%04d" % highest_marker).to_s
                    add_footer make_link('ref_title', links[url], url, title)
                  end

                  type = @cfg['inline'] ? 'inline' : 'ref_link'
                  res = make_link(type, link_text, links[url], false)
                  cursor_difference = cursor_difference + (@match_length - res.length)
                  @match_length = res.length
                  add_report("#{match_string} =&gt; #{url}")
                  res
                end
              else
                add_error('No results', "#{search_terms} (#{match_string})")
                counter_errors += 1
                match
              end
            else
              add_error('Invalid search', match)
              counter_errors += 1
              match
            end
          end
        end
        line_difference += 1 if delete_line
        out.push(line) unless delete_line
        delete_line = false
      }
      $stderr.puts("\n") unless SILENT

      input = out.delete_if {|l|
        l.strip =~ /^&lt;!--DELETE--&gt;$/
      }.join("\n")

      if @cfg['inline']
        add_output input + "\n"
        add_output "\n" + print_footer unless @footer.empty?
      else
        if @footer.empty?
          add_output input
        else
          last_line = input.strip.split(/\n/)[-1]
          if last_line =~ /^\[.*?\]: http/
            add_output input.rstrip + "\n"
          elsif last_line =~ /^\[\^.*?\]: /
            add_output input.rstrip
          else
            add_output input + "\n\n"
          end
          add_output print_footer + "\n\n"
        end
      end
      @line_num = nil
      add_report("Processed: #{total_links} links, #{counter_errors} errors.")
      print_report
      print_errors
    else
      link_only = false
      @clipboard = false

      input = parse_arguments(input.strip!).strip

      # if the end of input contain "^", copy to clipboard instead of STDOUT
      @clipboard = true if input =~ /\^[!~:]*$/

      # if the end of input contains "!!", only print the url
      link_only = true if input =~ /!![\^~:]*$/

      reference_link = input =~ /:([!\^\s~]*)$/

      # if end of input contains ~, pull url from clipboard
      if input =~ /~[:\^!\s]*$/
        input.sub!(/[:!\^\s~]*$/,'')
        clipboard = %x{__CF_USER_TEXT_ENCODING=$UID:0x8000100:0x8000100 pbpaste}.strip
        if is_url?(clipboard)
          type = reference_link ? 'ref_title' : 'inline'
          print make_link(type, input.strip, clipboard, false)
        else
          print @originput
        end
        Process.exit
      end

      input.sub!(/[:!\^\s~]*$/,'')

      # check for additional search terms in parenthesis
      additional_terms = ''
      if input =~ /\((.*?)\)/
        additional_terms = " " + $1.strip
        input.sub!(/\(.*?\)/,'')
      end

      link_text = false

      if input =~ /"(.*?)"/
        link_text = $1
        input.gsub!(/"(.*?)"/, '\1')
      end

      # remove quotes from terms, just in case
      # input.sub!(/^(!\S+)?\s*(["'])(.*?)\2([\!\^]+)?$/, "\\1 \\3\\4")

      if input =~ /^!(\S+)\s+(.*)$/
        type = $1
        link_info = $2.strip
        link_text = link_info unless link_text
        terms = link_info  + additional_terms
        terms.strip!

        if valid_search?(type) || type =~ /^(\S+\.)+\S+$/
          @cfg['custom_site_searches'].each {|k,v|
            if type == k
              link_text = terms if link_text == ''
              v = parse_arguments(v, {:no_restore =&gt; true})
              if v =~ /^(\/|http)/i
                type = 'r'
                tokens = v.scan(/\$term\d+d?/).sort.uniq

                if tokens.length &gt; 0
                  highest_token = 0
                  tokens.each {|token|
                    if token =~ /(\d+)d?$/
                      highest_token = $1.to_i if $1.to_i &gt; highest_token
                    end
                  }
                  terms_p = terms.split(/ +/)
                  if terms_p.length &gt; highest_token
                    remainder = terms_p[highest_token-1..-1].join(" ")
                    terms_p = terms_p[0..highest_token - 2]
                    terms_p.push(remainder)
                  end
                  tokens.each {|t|
                    if t =~ /(\d+)d?$/
                      int = $1.to_i - 1
                      replacement = terms_p[int]
                      if t =~ /d$/
                        replacement.downcase!
                        re_down = ""
                      else
                        re_down = "(?!d)"
                      end
                      v.gsub!(/#{Regexp.escape(t)+re_down}/, CGI.escape(replacement))
                    end
                  }
                  terms = v


                else
                  terms = v.gsub(/\$termd?/i) {|m|
                    terms.downcase! if m =~ /d$/i
                    CGI.escape(terms)
                  }
                end

              else
                type = 'g'
                terms = "site:#{v} #{terms}"
              end

              break
            end
          } if type &amp;&amp; terms &amp;&amp; terms.length &gt; 0

          if type =~ /^(\S+\.)+\S+$/
            terms = "site:#{type} #{terms}"
            type = 'g'
          end

          url, title, link_text = do_search(type, terms, link_text)
        else
          add_error('Invalid search', input)
          counter_errors += 1
        end
      elsif input =~ /^@(\S+)\s*$/
        link_text = input
        url, title = social_handle('twitter', link_text)
      else
        link_text = input unless link_text
        url, title = ddg(input)
      end

      if url
        if type =~ /sp(ell)?/
          add_output(url)
        elsif link_only
          add_output(url)
        else
          type = reference_link ? 'ref_title' : 'inline'
          add_output make_link(type, link_text, url, title)
          print_errors
        end
      else
        add_error('No results', title)
        add_output @originput.chomp
        print_errors
      end

      if @clipboard
        if @output == @originput
          $stderr.puts "No results found"
        else
          %x{echo #{Shellwords.escape(@output)}|tr -d "\n"|pbcopy}
          $stderr.puts "Results in clipboard"
        end
      end
    end
  end

  private

  def parse_arguments(string, opt={})
    input = string.dup
    skip_flags = opt[:only_meta] || false
    no_restore = opt[:no_restore] || false
    restore_prev_config unless no_restore

    input.gsub!(/(\+\+|--)([dirtv]+)\b/) do |match|
      bool = $1 == "++" ? "" : "no-"
      output = " "
      $2.split('').each {|arg|
        output += case arg
        when 'd'
          "--#{bool}debug "
        when 'i'
          "--#{bool}inline "
        when 'r'
          "--#{bool}prefix_random "
        when 't'
          "--#{bool}include_titles "
        when 'v'
          "--#{bool}validate_links "
        else
          ""
        end
      }
      output
    end unless skip_flags

    options = %w{ debug country_code inline prefix_random include_titles validate_links }
    options.each {|o|
      if input =~ /^#{o}:\s+(.*?)$/
        val = $1.strip
        val = true if val =~ /true/i
        val = false if val =~ /false/i
        @cfg[o] = val
        $stderr.print "\r\033[0KGlobal config: #{o} = #{@cfg[o]}\n" unless SILENT
      end

      unless skip_flags
        while input =~ /^#{o}:\s+(.*?)$/ || input =~ /--(no-)?#{o}/ do

          if input =~ /--(no-)?#{o}/ &amp;&amp; !skip_flags
            unless @prev_config.has_key? o
              @prev_config[o] = @cfg[o]
              bool = $1.nil? || $1 == '' ? true : false
              @cfg[o] = bool
              $stderr.print "\r\033[0KLine config: #{o} = #{@cfg[o]}\n" unless SILENT
            end
            input.sub!(/\s?--(no-)?#{o}/, '')
          end
        end
      end
    }
    @clipboard ? string : input
  end

  def restore_prev_config
    @prev_config.each {|k,v|
      @cfg[k] = v
      $stderr.print "\r\033[0KReset config: #{k} = #{@cfg[k]}\n" unless SILENT
    } if @prev_config
    @prev_config = {}
  end

  def make_link(type, text, url, title=false)
    title = title &amp;&amp; cfg['include_titles'] ? %Q{ "#{title.clean}"} : ""
    case type
    when 'ref_title'
      %Q{\n[#{text.strip}]: #{url}#{title}}
    when 'ref_link'
      %Q{[#{text.strip}][#{url}]}
    when 'inline'
      %Q{[#{text.strip}](#{url}#{title})}
    end
  end

  def add_output(str)
    if @printout &amp;&amp; !@clipboard
      print str
    end
    @output += str
  end

  def add_footer(str)
    @footer ||= []
    @footer.push(str.strip)
  end

  def print_footer
    unless @footer.empty?

      footnotes = []
      @footer.delete_if {|note|
        note.strip!
        if note =~ /^\[\^.+?\]/
          footnotes.push(note)
          true
        elsif note =~ /^\s*$/
          true
        else
          false
        end
      }

      output = @footer.sort.join("\n").strip
      output += "\n\n" if output.length &gt; 0 &amp;&amp; !footnotes.empty?
      output += footnotes.join("\n\n") unless footnotes.empty?
      return output.gsub(/\n{3,}/,"\n\n")
    end
    return ""
  end

  def add_report(str)
    if @cfg['report']
      unless @line_num.nil?
        position = @line_num.to_s + ':'
        position += @match_column.nil? ? "0:" : "#{@match_column}:"
        position += @match_length.nil? ? "0" : @match_length.to_s
      end
      @report.push("(#{position}): #{str}")
      $stderr.puts "(#{position}): #{str}" unless SILENT
    end
  end

  def add_error(type, str)
    if @cfg['debug']
      unless @line_num.nil?
        position = @line_num.to_s + ':'
        position += @match_column.nil? ? "0:" : "#{@match_column}:"
        position += @match_length.nil? ? "0" : @match_length.to_s
      end
      @errors[type] ||= []
      @errors[type].push("(#{position}): #{str}")
    end
  end

  def print_report
    return if (@cfg['inline'] &amp;&amp; @originput.split(/\n/).length == 1) || @clipboard
    unless @report.empty?
      out = "\n&lt;!-- Report:\n#{@report.join("\n")}\n--&gt;\n"
      add_output out
    end
  end

  def print_errors(type = 'Errors')
    return if @errors.empty?
    out = ''
    if @originput.split(/\n/).length &gt; 1
      inline = false
    else
      inline = @cfg['inline'] || @originput.split(/\n/).length == 1
    end

    @errors.each {|k,v|
      unless v.empty?
        v.each_with_index {|err, i|
          out += "(#{k}) #{err}"
          out += inline ? i == v.length - 1 ? " | " : ", " : "\n"
        }
      end
    }
    unless out == ''
      sep = inline ? " " : "\n"
      out.sub!(/\| /, '')
      out = "#{sep}&lt;!-- #{type}:#{sep}#{out}--&gt;#{sep}"
    end
    if @clipboard
      $stderr.puts out
    else
      add_output out
    end
  end

  def print_or_copy(text)
    # Process.exit unless text
    if @clipboard
      %x{echo #{Shellwords.escape(text)}|tr -d "\n"|pbcopy}
      print @originput
    else
      print text
    end
  end

  def notify(str, sub)
    return unless @cfg['notifications']
    %x{osascript -e 'display notification "SearchLink" with title "#{str}" subtitle "#{sub}"'}
  end

  def valid_link?(uri_str, limit = 5)
    begin
      notify("Validating", uri_str)
      return false if limit == 0
      url = URI(uri_str)
      return true unless url.scheme
      if url.path == ""
        url.path = "/"
      end
      # response = Net::HTTP.get_response(URI(uri_str))
      response = false

      Net::HTTP.start(url.host, url.port, :use_ssl =&gt; url.scheme == 'https') {|http| response = http.request_head(url.path) }

      case response
      when Net::HTTPMethodNotAllowed, Net::HTTPServiceUnavailable then
        unless /amazon\.com/ =~ url.host
          add_error('link validation', "Validation blocked: #{uri_str} (#{e})")
        end
        notify("Error validating", uri_str)
        true
      when Net::HTTPSuccess then
        true
      when Net::HTTPRedirection then
        location = response['location']
        valid_link?(location, limit - 1)
      else
        notify("Error validating", uri_str)
        false
      end
    rescue =&gt; e
      notify("Error validating", uri_str)
      add_error('link validation', "Possibly invalid =&gt; #{uri_str} (#{e})")
      return true
    end
  end

  def is_url?(input)
    input =~ /^(https?:\/\/\S+|\/\S+|\S+\/|[^!]\S+\.\S+)(\s+".*?")?$/
  end

  def valid_search?(term)
    valid = false
    valid = true if term =~ /(^h(([sc])([hb])?)*|^a$|^g$|^b$|^wiki$|^def$|^masd?$|^itud?$|^s$|^(i|am)(art|alb|song|pod)e?$|^lart|^@(t|adn|fb)|^r$|^sp(ell)?$)/
    valid = true if @cfg['custom_site_searches'].keys.include? term
    notify("Invalid search", term) unless valid
    valid
  end

  def search_chrome_history(term)
    # Google history
    if File.exists?(File.expand_path('~/Library/Application Support/Google/Chrome/Default/History'))
      notify("Searching Chrome History", term)
      tmpfile = File.expand_path('~/Library/Application Support/Google/Chrome/Default/History.tmp')
      FileUtils.cp(File.expand_path('~/Library/Application Support/Google/Chrome/Default/History'), tmpfile)

      terms = []
      terms.push("(url NOT LIKE '%search/?%' AND url NOT LIKE '%?q=%' AND url NOT LIKE '%?s=%')")
      terms.concat(term.split(/\s+/).map {|t| "(url LIKE '%#{t.strip.downcase}%' OR title LIKE '%#{t.strip.downcase}%')" })
      query = terms.join(" AND ")
      most_recent = %x{sqlite3 -separator ' ;;; ' '#{tmpfile}' "select title,url,datetime(last_visit_time / 1000000 + (strftime('%s', '1601-01-01')), 'unixepoch') from urls where #{query} AND NOT (url LIKE '%?s=%' OR url LIKE '%/search%') order by last_visit_time limit 1 COLLATE NOCASE;"}.strip
      FileUtils.rm_f(tmpfile)
      return false if most_recent.strip.length == 0
      title, url, date = most_recent.split(/\s*;;; /)
      date = Time.parse(date)
      [url, title, date]
    else
      false
    end
  end

  def search_chrome_bookmarks(term)
    out = false
    if File.exists?(File.expand_path('~/Library/Application Support/Google/Chrome/Default/Bookmarks'))
      notify("Searching Chrome Bookmarks", term)
      chrome_bookmarks = JSON.parse(IO.read(File.expand_path('~/Library/Application Support/Google/Chrome/Default/Bookmarks')))
      if chrome_bookmarks
        terms = term.split(/\s+/)
        roots = chrome_bookmarks['roots']
        urls = extract_chrome_bookmarks(roots)
        urls.sort_by! {|bookmark| bookmark["date_added"]}
        urls.select {|u|
          found = false
          terms.each {|t|
            if u["url"] =~ /#{t}/i || u["title"] =~ /#{t}/
              found = true
            end
          }
          found
        }
        unless urls.empty?
          lastest_bookmark = urls[-1]
          out = [lastest_bookmark['url'], lastest_bookmark['title'], lastest_bookmark['date']]
        end
      end
    end
    out
  end

  def search_history(term,types = [])
    if types.empty?
      if @cfg['history_types']
        types = @cfg['history_types']
      else
        return false
      end
    end


    results = []

    if types.length &gt; 0
      types.each {|type|

        url, title, date = case type
        when 'chrome_history'
          search_chrome_history(term)
        when 'chrome_bookmarks'
          search_chrome_bookmarks(term)
        when 'safari_bookmarks'
          search_safari_urls(term, 'bookmark')
        when 'safari_history'
          search_safari_urls(term, 'history')
        when 'safari_all'
          search_safari_urls(term)
        else
          false
        end
        if url
          results &lt;&lt; {'url' =&gt; url, 'title' =&gt; title, 'date' =&gt; date}
        end
      }

      unless results.empty?
        out = results.sort_by! {|r| r['date'] }[-1]
        [out['url'], out['title']]
      else
        false
      end
    else
      false
    end
  end

  # Search Safari Bookmarks and/or history using spotlig
  #
  # @param (String) tern
  # @param (String) type ['history'|'bookmark'|(Bool) false]
  #
  # @return (Array) [url, title, access_date] on success
  # @return (Bool) false on error
  def search_safari_urls(term,type = false)
    notify("Searching Safari History", term)
    onlyin = "~/Library/Caches/Metadata/Safari"
    onlyin += type ? "/"+type.capitalize : "/"
    type = type ? ".#{type}" : "*"
    # created:&gt;10/13/13 kind:safari filename:.webbookmark
    # Safari history/bookmarks
    terms = term.split(/\s+/).delete_if {|t| t.strip =~ /^\s*$/ }.map{|t|
      %Q{kMDItemTextContent = "*#{t}*"cdw}
    }.join(" &amp;&amp; ")

    date = type == ".history" ? "&amp;&amp; kMDItemContentCreationDate &gt; $time.today(-182) " : ""

    avoid_results = ["404", "not found", "chrome-extension"].map {|q|
      %Q{ kMDItemDisplayName != "*#{q}*"cdw }
    }.join(" &amp;&amp; ")
    query = %Q{((kMDItemContentType = "com.apple.safari#{type}") #{date}&amp;&amp; (#{avoid_results}) &amp;&amp; (#{terms}))}
    search = %x{mdfind -onlyin #{onlyin.gsub(/ /,'\ ')} '#{query}'}
    if search.length &gt; 0
      res = []
      search.split(/\n/).each {|file|
        url = %x{mdls -raw -name kMDItemURL "#{file}"}
        date = %x{mdls -raw -name kMDItemDateAdded "#{file}"}
        date = Time.parse(date)
        title = %x{mdls -raw -name kMDItemDisplayName "#{file}"}
        res &lt;&lt; {'url' =&gt; url, 'date' =&gt; date, 'title' =&gt; title}
      }
      res.delete_if {|k,el| el =~ /\(null\)/ }

      latest = res.sort_by! {|r| r["date"] }[-1]
      [latest['url'], latest['title'], latest['date']]
    else
      false
    end
  end

  def extract_chrome_bookmarks(json,urls = [])

    if json.class == Array
      json.each {|item|
        urls = extract_chrome_bookmarks(item, urls)
      }
    elsif json.class == Hash
      if json.has_key? "children"
        urls = extract_chrome_bookmarks(json["children"],urls)
      elsif json["type"] == "url"
        date = Time.at(json["date_added"].to_i / 1000000 + (Time.new(1601,01,01).strftime('%s').to_i))
        urls &lt;&lt; {'url' =&gt; json["url"], 'title' =&gt; json["name"], 'date' =&gt; date}
      else
        json.each {|k,v|
          urls = extract_chrome_bookmarks(v,urls)
        }

      end
    else
      return urls
    end
    urls
  end



  def wiki(terms)
    ## Hack to scrape wikipedia result
    body = %x{/usr/bin/curl -sSL 'https://en.wikipedia.org/wiki/Special:Search?search=#{CGI.escape(terms)}&amp;go=Go'}
    if body
      if RUBY_VERSION.to_f &gt; 1.9
        body = body.force_encoding('utf-8')
      end

      begin
        title = body.match(/"wgTitle":"(.*?)"/)[1]
        url = body.match(/&lt;link rel="canonical" href="(.*?)"/)[1]
      rescue
        return false
      end
      return [url, title]
    end
    ## Removed because Ruby 2.0 does not like https connection to wikipedia without using gems?
    # uri = URI.parse("https://en.wikipedia.org/w/api.php?action=query&amp;format=json&amp;prop=info&amp;inprop=url&amp;titles=#{CGI.escape(terms)}")
    # req = Net::HTTP::Get.new(uri.path)
    # req['Referer'] = "http://brettterpstra.com"
    # req['User-Agent'] = "SearchLink (http://brettterpstra.com)"

    # res = Net::HTTP.start(uri.host, uri.port,
    #   :use_ssl =&gt; true,
    #   :verify_mode =&gt; OpenSSL::SSL::VERIFY_NONE) do |https|
    #     https.request(req)
    #   end



    # if RUBY_VERSION.to_f &gt; 1.9
    #   body = res.body.force_encoding('utf-8')
    # else
    #   body = res.body
    # end

    # result = JSON.parse(body)

    # if result
    #   result['query']['pages'].each do |page,info|
    #     unless info.key? "missing"
    #       return [info['fullurl'],info['title']]
    #     end
    #   end
    # end
    # return false
  end

  def zero_click(terms)
    url = URI.parse("http://api.duckduckgo.com/?q=#{CGI.escape(terms)}&amp;format=json&amp;no_redirect=1&amp;no_html=1&amp;skip_disambig=1")
    res = Net::HTTP.get_response(url).body
    res = res.force_encoding('utf-8') if RUBY_VERSION.to_f &gt; 1.9

    result = JSON.parse(res)
    if result
      definition = result['Definition'] || false
      definition_link = result['DefinitionURL'] || false
      wiki_link = result['AbstractURL'] || false
      title = result['Heading'] || false
      return [title, definition, definition_link, wiki_link]
    else
      return false
    end
  end

  # Search apple music
  # terms =&gt; search terms (unescaped)
  # media =&gt; music, podcast
  # entity =&gt; optional: artist, song, album, podcast
  # returns {:type=&gt;,:id=&gt;,:url=&gt;,:title}
  def applemusic(terms, media='music', entity='')
    aff = @cfg['itunes_affiliate']
    output = {}

    url = URI.parse("http://itunes.apple.com/search?term=#{CGI.escape(terms)}&amp;country=#{@cfg['country_code']}&amp;media=#{media}&amp;entity=#{entity}")
    res = Net::HTTP.get_response(url).body
    res = res.force_encoding('utf-8') if RUBY_VERSION.to_f &gt; 1.9
    res.gsub!(/(?mi)[\x00-\x08\x0B-\x0C\x0E-\x1F]/,'')
    json = JSON.parse(res)
    if json['resultCount'] &amp;&amp; json['resultCount'] &gt; 0
      result = json['results'][0]

      case result['wrapperType']
      when 'track'
        if result['kind'] == 'podcast'
          output[:type] = 'podcast'
          output[:id] = result['collectionId']
          output[:url] = result['collectionViewUrl'].to_am + aff
          output[:title] = result['collectionName']
        else
          output[:type] = 'song'
          output[:id] = result['trackId']
          output[:url] = result['trackViewUrl'].to_am + aff
          output[:title] = result['trackName'] + " by " + result['artistName']
        end
      when 'collection'
        output[:type] = 'album'
        output[:id] = result['collectionId']
        output[:url] = result['collectionViewUrl'].to_am + aff
        output[:title] = result['collectionName'] + " by " + result['artistName']
      when 'artist'
        output[:type] = 'artist'
        output[:id] = result['artistId']
        output[:url] = result['artistLinkUrl'].to_am + aff
        output[:title] = result['artistName']
      end
      return false if output.empty?
      output
    else
      return false
    end
  end

  def itunes(entity, terms, dev, aff='')
    aff = @cfg['itunes_affiliate']

    url = URI.parse("http://itunes.apple.com/search?term=#{CGI.escape(terms)}&amp;country=#{@cfg['country_code']}&amp;entity=#{entity}")
    res = Net::HTTP.get_response(url).body
    res = res.force_encoding('utf-8').encode # if RUBY_VERSION.to_f &gt; 1.9

    begin
      json = JSON.parse(res)
    rescue =&gt; e
      add_error('Invalid response', "Search for #{terms}: (#{e})")
      return false
    end
    return false unless json
    if json['resultCount'] &amp;&amp; json['resultCount'] &gt; 0
      result = json['results'][0]
      case entity
      when /(mac|iPad)Software/
        output_url = dev &amp;&amp; result['sellerUrl'] ? result['sellerUrl'] : result['trackViewUrl']
        output_title = result['trackName']
      when /(musicArtist|song|album)/
        case result['wrapperType']
        when 'track'
          output_url = result['trackViewUrl']
          output_title = result['trackName'] + " by " + result['artistName']
        when 'collection'
          output_url = result['collectionViewUrl']
          output_title = result['collectionName'] + " by " + result['artistName']
        when 'artist'
          output_url = result['artistLinkUrl']
          output_title = result['artistName']
        end
      when /podcast/
        output_url = result['collectionViewUrl']
        output_title = result['collectionName']
      end
      return false unless output_url and output_title
      if dev
        return [output_url, output_title]
      else
        return [output_url + aff, output_title]
      end
    else
      return false
    end
  end

  def lastfm(entity, terms)
    url = URI.parse("http://ws.audioscrobbler.com/2.0/?method=#{entity}.search&amp;#{entity}=#{CGI.escape(terms)}&amp;api_key=2f3407ec29601f97ca8a18ff580477de&amp;format=json")
    res = Net::HTTP.get_response(url).body
    res = res.force_encoding('utf-8') if RUBY_VERSION.to_f &gt; 1.9
    json = JSON.parse(res)
    if json['results']
      begin
        case entity
        when 'track'
          result = json['results']['trackmatches']['track'][0]
          url = result['url']
          title = result['name'] + " by " + result['artist']
        when 'artist'
          result = json['results']['artistmatches']['artist'][0]
          url = result['url']
          title = result['name']
        end
        return [url, title]
      rescue
        return false
      end
    else
      return false
    end
  end

  def bing(terms, define = false)
    uri = URI.parse(%Q{https://api.datamarket.azure.com/Data.ashx/Bing/Search/v1/Web?Query=%27#{CGI.escape(terms)}%27&amp;$format=json})
    req = Net::HTTP::Get.new(uri)
    req.basic_auth '2b0c04b5-efa5-4362-9f4c-8cae5d470cef', 'M+B8HkyFfCAcdvh1g8bYST12R/3i46zHtVQRfx0L/6s'

    res = Net::HTTP.start(uri.hostname, uri.port, :use_ssl =&gt; true) {|http|
      http.request(req)
    }

    if res
      begin
        json = res.body
        json.force_encoding('utf-8') if json.respond_to?('force_encoding')
        data = JSON.parse(json)
        result = data['d']['results'][0]
        return [result['Url'], result['Title']]
      rescue
        return false
      end
    else
      return false
    end
  end

  def google(terms, define = false)
    begin
      uri = URI.parse("http://ajax.googleapis.com/ajax/services/search/web?v=1.0&amp;filter=1&amp;rsz=small&amp;q=#{CGI.escape(terms)}")
      req = Net::HTTP::Get.new(uri.request_uri)
      req['Referer'] = "http://brettterpstra.com"
      res = Net::HTTP.start(uri.host, uri.port) {|http|
        http.request(req)
      }
      if RUBY_VERSION.to_f &gt; 1.9
        body = res.body.force_encoding('utf-8')
      else
        body = res.body
      end

      json = JSON.parse(body)
      if json['responseData']
        result = json['responseData']['results'][0]
        return false if result.nil?
        output_url = result['unescapedUrl']
        if define &amp;&amp; output_url =~ /dictionary/
          output_title = result['content'].gsub(/&lt;\/?.*?&gt;/,'')
        else
          output_title = result['titleNoFormatting']
        end
        return [output_url, output_title]
      else
        return bing(terms, define)
      end
    rescue
      return bing(terms, define)
    end
  end

  def ddg(terms,type=false)

    prefix = type ? "#{type.sub(/^!?/,'!')} " : "%5C"

    begin
      body = %x{/usr/bin/curl -sSL 'http://duckduckgo.com/?q=#{prefix}#{CGI.escape(terms)}&amp;t=hn&amp;ia=web'}

      url = body.match(/uddg=(.*?)'/)

      if url &amp;&amp; url[1]
        result = url[1] rescue false
        return false unless result
        output_url = URI.unescape(result)
        if @cfg['include_titles']
          output_title = titleize(output_url) rescue ''
        else
          output_title = ''
        end
        return [output_url, output_title]
      else
        return false
      end
    end
  end

  def titleize(url)

    whitespace = url.match(/(\s*$)/)[0] || ''
    title = nil
    begin
      source = %x{/usr/bin/curl -sSL '#{url.strip}'}
      title = source.match(/&lt;title&gt;(.*)&lt;\/title&gt;/im)

      title = title.nil? ? nil : title[1].strip

      orig_title = false

      if title.nil? || title =~ /^\s*$/
        $stderr.puts "Warning: missing title for #{url.strip}" if $cfg['debug']
        title = url.gsub(/(^https?:\/\/|\/.*$)/,'').strip
      else
        title = title.gsub(/\n/, ' ').gsub(/\s+/,' ').strip # .sub(/[^a-z]*$/i,'')
      end

      title
    rescue Exception =&gt; e
      $stderr.puts "Error retrieving title for #{url.strip}"
      raise e
    end
  end

  def spell(terms)
    caps = []
    terms.split(" ").each {|w|
      caps.push(w =~ /^[A-Z]/ ? true : false)
    }

    uri = URI.parse("https://api.datamarket.azure.com/Data.ashx/Bing/Search/v1/SpellingSuggestions?Query=%27#{CGI.escape(terms)}%27&amp;$format=json")
    req = Net::HTTP::Get.new(uri)

    req.basic_auth '2b0c04b5-efa5-4362-9f4c-8cae5d470cef', 'M+B8HkyFfCAcdvh1g8bYST12R/3i46zHtVQRfx0L/6s'

    res = Net::HTTP.start(uri.hostname, uri.port, :use_ssl =&gt; true) {|http|
      http.request(req)
    }
    if res
      begin
        json = res.body
        json.force_encoding('utf-8') if json.respond_to?('force_encoding')
        data = JSON.parse(json)
        return terms if data['d']['results'].empty?
        result = data['d']['results'][0]['Value']
        output = []
        result.split(" ").each_with_index {|w, i|
          output.push(caps[i] ? w.capitalize : w)
        }
        return output.join(" ")
      rescue
        return false
      end
    else
      return false
    end
  end

  def amazon_affiliatize(url, amazon_partner)
    return url if amazon_partner.nil? || amazon_partner.length == 0

    if url =~ /https?:\/\/(?:.*?)amazon.com\/(?:(.*?)\/)?([dg])p\/([^\?]+)/
      title = $1
      type = $2
      id = $3
      az_url = "http://www.amazon.com/#{type}p/product/#{id}/ref=as_li_ss_tl?ie=UTF8&amp;linkCode=ll1&amp;tag=#{amazon_partner}"
      return [az_url, title]
    else
      return [url,'']
    end
  end

  def social_handle(type, term)
    handle = term.sub(/^@/,'').strip
    case type
    when /twitter/
      url = "https://twitter.com/#{handle}"
      title = "@#{handle} on Twitter"
    when /adn/
      url = "https://alpha.app.net/#{handle}"
      title = "@#{handle} on App.net"
    when /fb/
      url = "https://www.facebook.com/#{handle}"
      title = "@#{handle} on Facebook"
    else
      [false, term, link_text]
    end
    [url, title]
  end

  def do_search(search_type, search_terms, link_text='')
    notify("Searching", search_terms)
    return [false, search_terms, link_text] unless search_terms.length &gt; 0

    case search_type
    when /^r$/ # simple replacement
      if @cfg['validate_links']
        unless valid_link?(search_terms)
          return [false, "Link not valid: #{search_terms}", link_text]
        end
      end
      link_text = search_terms if link_text == ''
      return [search_terms, link_text, link_text]
    when /^@t/ # twitterify username
      if search_terms.strip =~ /^@?[0-9a-z_$]+$/i
        url, title = social_handle('twitter', search_terms)
        link_text = search_terms
      else
        return [false, "#{search_terms} is not a valid Twitter handle", link_text]
      end
    when /^@adn/ # adnify username
      if search_terms.strip =~ /^@?[0-9a-z_]+$/i
        url, title = social_handle('adn', search_terms)
        link_text = search_terms if link_text == ''
      else
        return [false, "#{search_terms} is not a valid App.net handle", link_text]
      end
    when /^@fb/ # adnify username
      if search_terms.strip =~ /^@?[0-9a-z_]+$/i
        url, title = social_handle('fb', search_terms)
        link_text = search_terms if link_text == ''
      else
        return [false, "#{search_terms} is not a valid Facebook username", link_text]
      end
    when /^sp(ell)?$/ # replace with spelling suggestion
      res = spell(search_terms)
      if res
        return [res, res, ""]
      else
        url = false
      end
    when /^h(([sc])([hb])?)*$/
      str = $1
      types = []
      if str =~ /s([hb]*)/
        if $1.length &gt; 1
          types.push('safari_all')
        elsif $1 == 'h'
          types.push('safari_history')
        elsif $1 == 'b'
          types.push('safari_bookmarks')
        end
      end

      if str =~ /c([hb]*)/
        if $1.length &gt; 1
          types.push('chrome_bookmarks')
          types.push('chrome_history')
        elsif $1 == 'h'
          types.push('chrome_history')
        elsif $1 == 'b'
          types.push('chrome_bookmarks')
        end
      end
      url, title = search_history(search_terms, types)
    when /^a$/
      az_url, title = ddg(%Q{site:amazon.com #{search_terms}})
      url, title = amazon_affiliatize(az_url, @cfg['amazon_partner'])

    when /^g$/ # google lucky search
      url, title = ddg(search_terms)

    when /^b$/ # bing
      url, title = bing(search_terms)

    when /^wiki$/
      url, title = wiki(search_terms)

    when /^def$/ # wikipedia/dictionary search
      # title, definition, definition_link, wiki_link = zero_click(search_terms)
      # if search_type == 'def' &amp;&amp; definition_link != ''
      #   url = definition_link
      #   title = definition.gsub(/'+/,"'")
      # elsif wiki_link != ''
      #   url = wiki_link
      #   title = "Wikipedia: #{title}"
      # end
      fix = spell(search_terms)

      if fix &amp;&amp; search_terms.downcase != fix.downcase
        add_error('Spelling', "Spelling altered for '#{search_terms}' to '#{fix}'")
        search_terms = fix
        link_text = fix
      end

      url, title = google("define " + search_terms, true)

    when /^masd?$/ # Mac App Store search (mas = itunes link, masd = developer link)
      dev = search_type =~ /d$/
      url, title = itunes('macSoftware',search_terms, dev, @cfg['itunes_affiliate'])
    # Stopgap:
    #when /^masd?$/
    # url, title = google("site:itunes.apple.com Mac App Store #{search_terms}")
    # url += $itunes_affiliate

    when /^itud?$/ # iTunes app search
      dev = search_type =~ /d$/
      url, title = itunes('iPadSoftware',search_terms, dev, @cfg['itunes_affiliate'])

    when /^s$/ # software search (google)
      url, title = ddg(%Q{(software OR app OR mac) #{search_terms}})
      link_text = title if link_text == ''

    when /^am/ # apple music search
      stype = search_type.downcase.sub(/^am/,'')
      otype = 'link'
      if stype =~ /e$/
        otype = 'embed'
        stype.sub!(/e$/,'')
      end
      case stype
      when /^pod$/
        result = applemusic(search_terms, 'podcast')
      when /^art$/
        result = applemusic(search_terms, 'music', 'musicArtist')
      when /^alb$/
        result = applemusic(search_terms, 'music', 'album')
      when /^song$/
        result = applemusic(search_terms, 'music', 'musicTrack')
      else
        result = applemusic(search_terms)
      end

      # {:type=&gt;,:id=&gt;,:url=&gt;,:title=&gt;}
      if otype == 'embed' &amp;&amp; result[:type] =~ /(album|song)/
        url = 'embed'
        title = %Q{&lt;iframe src="//tools.applemusic.com/embed/v1/#{result[:type]}/#{result[:id]}?country=#{@cfg['country_code']}#{@cfg['itunes_affiliate']}" height="500px" width="100%" frameborder="0"&gt;&lt;/iframe&gt;}
      else
        url = result[:url]
        title = result[:title]
      end

    when /^ipod$/
      url, title = itunes('podcast', search_terms, false)

    when /^isong$/ # iTunes Song Search
      url, title = itunes('song', search_terms, false)

    when /^iart$/ # iTunes Artist Search
      url, title = itunes('musicArtist', search_terms, false)

    when /^ialb$/ # iTunes Album Search
      url, title = itunes('album', search_terms, false)

    when /^lsong$/ # Last.fm Song Search
      url, title = lastfm('track', search_terms)

    when /^lart$/ # Last.fm Artist Search
      url, title = lastfm('artist', search_terms)
    else
      if search_terms
        if search_type =~ /.+?\.\w{2,4}$/
          url, title = ddg(%Q{site:#{search_type} #{search_terms}})
        else
          url, title = ddg(search_terms)
        end
      end
    end
    link_text = search_terms if link_text == ''
    if url &amp;&amp; @cfg['validate_links'] &amp;&amp; !valid_link?(url) &amp;&amp; search_type !~ /^sp(ell)?/
      [false, "Not found: #{url}", link_text]
    elsif !url
      [false, "No results: #{url}", link_text]
    else
      [url, title, link_text]
    end
  end

end

## Stupid small pure Ruby JSON parser &amp; generator.
#
# Copyright © 2013 Mislav Marohnić
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this
# software and associated documentation files (the “Software”), to deal in the Software
# without restriction, including without limitation the rights to use, copy, modify,
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to the following
# conditions:
#
# The above copyright notice and this permission notice shall be included in all copies or
# substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
# PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
# OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.

require 'strscan'
require 'forwardable'

# Usage:
#
#   JSON.parse(json_string) =&gt; Array/Hash
#   JSON.generate(object)   =&gt; json string
#
# Run tests by executing this file directly. Pipe standard input to the script to have it
# parsed as JSON and to display the result in Ruby.
#
class JSON
  def self.parse(data) new(data).parse end

  WSP = /\s+/
  OBJ = /[{\[]/;    HEN = /\}/;  AEN = /\]/
  COL = /\s*:\s*/;  KEY = /\s*,\s*/
  NUM = /-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?/
  BOL = /true|false/;  NUL = /null/

  extend Forwardable

  attr_reader :scanner
  alias_method :s, :scanner
  def_delegators :scanner, :scan, :matched
  private :s, :scan, :matched

  def initialize data
    @scanner = StringScanner.new data.to_s
  end

  def parse
    space
    object
  end

  private

  def space() scan WSP end

  def endkey() scan(KEY) or space end

  def object
    matched == '{' ? hash : array if scan(OBJ)
  end

  def value
    object or string or
      scan(NUL) ? nil :
      scan(BOL) ? matched.size == 4:
      scan(NUM) ? eval(matched) :
      error
  end

  def hash
    obj = {}
    space
    repeat_until(HEN) { k = string; scan(COL); obj[k] = value; endkey }
    obj
  end

  def array
    ary = []
    space
    repeat_until(AEN) { ary &lt;&lt; value; endkey }
    ary
  end

  SPEC = {'b' =&gt; "\b", 'f' =&gt; "\f", 'n' =&gt; "\n", 'r' =&gt; "\r", 't' =&gt; "\t"}
  UNI = 'u'; CODE = /[a-fA-F0-9]{4}/
  STR = /"/; STE = '"'
  ESC = '\\'

  def string
    if scan(STR)
      str, esc = '', false
      while c = s.getch
        if esc
          str &lt;&lt; (c == UNI ? (s.scan(CODE) || error).to_i(16).chr : SPEC[c] || c)
          esc = false
        else
          case c
          when ESC then esc = true
          when STE then break
          else str &lt;&lt; c
          end
        end
      end
      str
    end
  end

  def error
    raise "parse error at: #{scan(/.{1,10}/m).inspect}"
  end

  def repeat_until reg
    until scan(reg)
      pos = s.pos
      yield
      error unless s.pos &gt; pos
    end
  end

  module Generator
    def generate(obj)
      raise ArgumentError unless obj.is_a? Array or obj.is_a? Hash
      generate_type(obj)
    end
    alias dump generate

    private

    def generate_type(obj)
      type = obj.is_a?(Numeric) ? :Numeric : obj.class.name
      begin send(:"generate_#{type}", obj)
      rescue NoMethodError; raise ArgumentError, "can't serialize #{type}"
      end
    end

    ESC_MAP = Hash.new {|h,k| k }.update \
      "\r" =&gt; 'r',
      "\n" =&gt; 'n',
      "\f" =&gt; 'f',
      "\t" =&gt; 't',
      "\b" =&gt; 'b'

    def quote(str) %("#{str}") end

    def generate_String(str)
      quote str.gsub(/[\r\n\f\t\b"\\]/) { "\\#{ESC_MAP[$&amp;]}"}
    end

    def generate_simple(obj) obj.inspect end
    alias generate_Numeric generate_simple
    alias generate_TrueClass generate_simple
    alias generate_FalseClass generate_simple

    def generate_Symbol(sym) generate_String(sym.to_s) end

    def generate_Time(time)
      quote time.strftime(time.utc? ? "%F %T UTC" : "%F %T %z")
    end
    def generate_Date(date) quote date.to_s end

    def generate_NilClass(*) 'null' end

    def generate_Array(ary) '[%s]' % ary.map {|o| generate_type(o) }.join(', ') end

    def generate_Hash(hash)
      '{%s}' % hash.map { |key, value|
        "#{generate_String(key.to_s)}: #{generate_type(value)}"
      }.join(', ')
    end
  end

  extend Generator
end

sl = SearchLink.new({:echo =&gt; false})
overwrite = true
backup = sl.cfg['backup']

if ARGV.length &gt; 0
  files = []
  ARGV.each {|arg|
    if arg =~ /^(--?)?(h(elp)?|v(ersion)?)$/
      $stdout.puts "SearchLink v#{VERSION}"
      sl.help_cli
      $stdout.puts "See http://brettterpstra.com/projects/searchlink/ for help"
      Process.exit
    elsif arg =~ /^--?(stdout)$/
      overwrite = false
    elsif arg =~ /^--?no[\-_]backup$/
      backup = false
    else
      files.push(arg)
    end
  }
  files.each {|file|
    if File.exists?(file) &amp;&amp; %x{file -b "#{file}"|grep -c text}.to_i &gt; 0
      if RUBY_VERSION.to_f &gt; 1.9
        input = IO.read(file).force_encoding('utf-8')
      else
        input = IO.read(file)
      end
      FileUtils.cp(file,file+".bak") if backup &amp;&amp; overwrite

      sl.parse(input)

      if overwrite
        File.open(file, 'w') do |f|
          f.puts sl.output
        end
      else
        puts sl.output
      end
    else
      $stderr.puts "Error reading #{file}"
    end
  }
else
  if RUBY_VERSION.to_f &gt; 1.9
    input = STDIN.read.force_encoding('utf-8').encode
  else
    input = STDIN.read
  end

  sl.parse(input)
  if sl.clipboard
    print input
  else
    print sl.output
  end
end

# Workflow: [/Users/ttscoff/Library/Services/SearchLink.workflow, 348C55FD-D93F-4CDE-A8A8-DC31888F8448]
# Workflow: [/Users/ttscoff/Library/Services/SearchLink File.workflow, D9899AB4-AC1C-444F-A0A2-660F782655FE]
