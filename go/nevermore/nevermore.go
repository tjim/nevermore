package main

/* nevermore.go

   Search interface to notmuch for the Nevermore emacs mail client.
   Nevermore search by default is message based, not thread based.
   Notmuch does not sort messages by date, it sorts by thread first.
   This program searches for messages and sorts them properly.
   Output is appropriate for emacs lisp.

   Install with
       go install github.com/tjim/nevermore/go/nevermore

*/

import (
	"fmt"
	"git.notmuchmail.org/git/notmuch.git/bindings/go/src/notmuch"
	"github.com/msbranco/goconfig"
	"os"
	"path"
	"strings"
	"time"
)

func main() {
	args := os.Args[1:]
	query := strings.Join(args, " ")
	SearchMessages(query)
}

func SearchMessages(query string) {
	db, status := getNotmuchDb()
	if status != notmuch.STATUS_SUCCESS {
		panic("notmuch error")
	}
	dbQuery := db.CreateQuery(query)
	fmt.Printf("(")
	firstMsg := true
	for msgs := dbQuery.SearchMessages(); msgs.Valid(); msgs.MoveToNext() {
		if !firstMsg {
			fmt.Printf("\n")
		}
		firstMsg = false
		msg := msgs.Get()
		timestamp, _ := msg.GetDate()
		fmt.Printf("(:subject %s :authors %s :date_relative %s :id %s",
			elispQuote(msg.GetHeader("Subject")),
			elispQuote(msg.GetHeader("From")),
			elispQuote(timeRelativeDate(timestamp)),
			elispQuote(msg.GetMessageId()))
		fmt.Printf(" :tags (")
		firstTag := true
		for tags := msg.GetTags(); tags.Valid(); tags.MoveToNext() {
			if !firstTag {
				fmt.Printf(" ")
			}
			firstTag = false
			tag := tags.Get()
			fmt.Printf("%s", elispQuote(tag))
		}
		fmt.Printf("))")
	}
	fmt.Printf(")\n")
}

func getNotmuchDb() (*notmuch.Database, notmuch.Status) {
	home := os.Getenv("NOTMUCH_CONFIG")
	if home == "" {
		home = os.Getenv("HOME")
	}

	cfg, err := goconfig.ReadConfigFile(path.Join(home, ".notmuch-config"))
	if err != nil {
		return nil, notmuch.STATUS_FILE_ERROR
	}

	dbPath, err := cfg.GetString("database", "path")
	if err != nil {
		return nil, notmuch.STATUS_FILE_ERROR
	}

	return notmuch.OpenDatabase(dbPath, notmuch.DATABASE_MODE_READ_ONLY)
}

func elispQuote(s string) string {
	buf := make([]byte, 0, 3*len(s)/2)
	dquote := `"`[0]
	buf = append(buf, dquote)

	for i := 0; i < len(s); i++ { // not range b/c that iterates over runes not bytes
		b := s[i]
		switch {
		case b < " "[0] || b > "~"[0]:
			// use octal, not hex, because in elisp hex escapes don't have a limited number of digits
			// and can run on into the next byte, e.g., "\x09Academic" is (unintentionally) malformed
			buf = append(buf, fmt.Sprintf("\\%03o", b)...)
		case b == `"`[0]:
			buf = append(buf, `\"`...)
		case b == `\`[0]:
			buf = append(buf, `\\`...)
		default:
			buf = append(buf, b)
		}
	}
	buf = append(buf, dquote)
	return string(buf)
}

const (
	MINUTE = 60
	HOUR   = 60 * MINUTE
	DAY    = 24 * HOUR
)

// port of notmuch_time_relative_date(), using am/pm instead of 24hr time
func timeRelativeDate(then int64) string {
	thenTime := time.Unix(then, 0)
	nowTime := time.Now()
	now := nowTime.Unix()
	delta := now - then

	if then > now {
		return "the future"
	}
	if delta > 180*DAY {
		return thenTime.Format("2006-01-02")
	}
	if delta < 3600 {
		return fmt.Sprintf("%d mins. ago", delta/60)
	}
	if delta <= 7*DAY {
		if thenTime.Weekday() == nowTime.Weekday() && delta < DAY {
			return thenTime.Format("Today 3:04pm")
		} else if (nowTime.Weekday()+7-thenTime.Weekday())%7 == 1 {
			return thenTime.Format("Yest. 3:04pm")
		} else if thenTime.Weekday() != nowTime.Weekday() {
			return thenTime.Format("Mon. 3:04pm")
		}
	}
	return thenTime.Format("January 2")
}
