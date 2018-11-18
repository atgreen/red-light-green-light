package main

import (
  "fmt"
  "log"
  "os"
  "time"

  "github.com/urfave/cli"
)

func main() {
  app := cli.NewApp()

  app.Commands = []cli.Command{
    {
      Name:    "login",
      Aliases: []string{"l"},
      Usage:   "login to Red Light Green Light server",
      Action:  func(c *cli.Context) error {
        fmt.Println("login task: ", c.Args().First())
        return nil
      },
    },
    {
      Name:    "start",
      Aliases: []string{"s"},
      Usage:   "create a Player ID",
      Action:  func(c *cli.Context) error {
        fmt.Println("start task: ", c.Args().First())
        return nil
      },
    },
    {
      Name:    "test",
      Aliases: []string{"t"},
      Usage:   "evaluate test results",
      Action:  func(c *cli.Context) error {
        fmt.Println("test task: ", c.Args().First())
        return nil
      },
    },
  }

  app.Name = "rlgl"
  app.Copyright = "(c) 2018 Anthony Green"
  app.Compiled = time.Now()
  app.Authors = []cli.Author{
  	  cli.Author{
	    Name: "Anthony Green",
	    Email: "green@moxielogic.com",
	    },
	    }
  app.Usage = "Red Light Green Light"
  app.Action = func(c *cli.Context) error {
    fmt.Println("boom! I say!")
    return nil
  }

  err := app.Run(os.Args)
  if err != nil {
    log.Fatal(err)
  }
}
