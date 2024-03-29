// rlgl.go - Copyright 2018-2022  Anthony Green <green@moxielogic.com>
//
// This program is free software: you can redistribute it and/or
// modify it under the terms of the GNU Affero General Public License
// as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.

package main

import (
	"bytes"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"mime/multipart"
	"net/http"
	"net/url"
	"os"
	"regexp"
	"strings"
	"time"

	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/x509"
	"encoding/pem"

	"github.com/fatih/color"
	"github.com/naoina/toml"
	"github.com/urfave/cli/v2"
)

var (
	// FIXME: get version from git, as it done in rlgl-server.lisp
	VERSION = "undefined"
	red     = color.New(color.FgRed).SprintFunc()
	cyan    = color.New(color.FgCyan).SprintFunc()
)

type Config struct {
	Host      string
	Key       string
	Proxy     string
	ProxyAuth string
}

func output(s string) {
	fmt.Printf("%s %s\n", cyan("rlgl"), s)
}

func exitErr(err error) {
	output(red(err.Error()))
	os.Exit(2)
}

func make_keys(path string) {

	privateKey, _ := ecdsa.GenerateKey(elliptic.P384(), rand.Reader)
	publicKey := &privateKey.PublicKey

	x509bytes, _ := x509.MarshalECPrivateKey(privateKey)
	var pemPrivateBlock = &pem.Block{
		Type:  "EC PRIVATE KEY",
		Bytes: x509bytes,
	}

	x509bytesPub, _ := x509.MarshalPKIXPublicKey(publicKey)
	var pemPublicBlock = &pem.Block{
		Type:  "PUBLIC KEY",
		Bytes: x509bytesPub,
	}

	cfgdir := basedir(path)
	pemPrivateFile, err := os.Create(cfgdir + "/private_key.pem")
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	err = pem.Encode(pemPrivateFile, pemPrivateBlock)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	pemPrivateFile.Close()
	err = os.Chmod(cfgdir+"/private_key.pem", 0600)
	if err != nil {
		log.Fatal(err)
	}

	pemPublicFile, err := os.Create(cfgdir + "/public_key.pem")
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	err = pem.Encode(pemPublicFile, pemPublicBlock)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	pemPublicFile.Close()
	err = os.Chmod(cfgdir+"/public_key.pem", 0644)
	if err != nil {
		log.Fatal(err)
	}
}

func (c *Config) Write(path string) {
	cfgdir := basedir(path)

	// create config dir if not exist
	if _, err := os.Stat(cfgdir); err != nil {
		err = os.MkdirAll(cfgdir, 0755)
		if err != nil {
			exitErr(fmt.Errorf("failed to initialize config dir [%s]: %s", cfgdir, err))
		}
	}

	file, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0600)
	if err != nil {
		exitErr(fmt.Errorf("failed to open config for writing: %s", err))
	}

	writer := toml.NewEncoder(file)
	err = writer.Encode(c)
	if err != nil {
		exitErr(fmt.Errorf("failed to write config: %s", err))
	}
}

// determine config path from environment
func getConfigPath() (path string, exists bool) {
	userHome, ok := os.LookupEnv("HOME")
	if !ok {
		exitErr(fmt.Errorf("$HOME not set"))
	}

	path = fmt.Sprintf("%s/.rlgl/config", userHome) // default path

	if xdgSupport() {
		xdgHome, ok := os.LookupEnv("XDG_CONFIG_HOME")
		if !ok {
			xdgHome = fmt.Sprintf("%s/.config", userHome)
		}
		path = fmt.Sprintf("%s/rlgl/config", xdgHome)
	}

	if _, err := os.Stat(path); err == nil {
		exists = true
	}

	return path, exists
}

func basedir(path string) string {
	parts := strings.Split(path, "/")
	return strings.Join((parts[0 : len(parts)-1]), "/")
}

// Test for environment supporting XDG spec
func xdgSupport() bool {
	re := regexp.MustCompile("^XDG_*")
	for _, e := range os.Environ() {
		if re.FindAllString(e, 1) != nil {
			return true
		}
	}
	return false
}

func setProxy(proxy string, auth string) {

	var transport *http.Transport

	if proxy != "" {
		proxyUrl, err := url.Parse(proxy)
		if err != nil {
			log.Fatal(err)
		}
		if auth != "" {
			basicAuth := "Basic " + base64.StdEncoding.EncodeToString([]byte(auth))
			hdr := http.Header{}
			hdr.Add("Proxy-Authorization", basicAuth)
			transport = &http.Transport{
				Proxy:              http.ProxyURL(proxyUrl),
				ProxyConnectHeader: hdr,
			}
		} else {
			transport = &http.Transport{
				Proxy: http.ProxyURL(proxyUrl),
			}
		}

		http.DefaultTransport = transport
	}
}

func SendPostRequest(config *Config, url string, filename string, filetype string) []byte {
	file, err := os.Open(filename)

	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	body := &bytes.Buffer{}
	writer := multipart.NewWriter(body)
	part, err := writer.CreateFormFile(filetype, file.Name())

	if err != nil {
		log.Fatal(err)
	}

	_, err = io.Copy(part, file)
	if err != nil {
		log.Fatal(err)
	}

	writer.Close()
	request, err := http.NewRequest("POST", url, body)

	if err != nil {
		log.Fatal(err)
	}

	var bearer = "Bearer " + config.Key
	request.Header.Add("Authorization", bearer)

	request.Header.Add("Content-Type", writer.FormDataContentType())
	client := &http.Client{}
	response, err := client.Do(request)

	if err != nil {
		log.Fatal(err)
	}
	defer response.Body.Close()

	content, err := ioutil.ReadAll(response.Body)

	if err != nil {
		log.Fatal(err)
	}

	return content
}

func loadPrivateKey(cfgdir string) (*ecdsa.PrivateKey, error) {

	priv, err := ioutil.ReadFile(cfgdir + "/private_key.pem")
	if err != nil {
		log.Fatal(err)
	}

	block, _ := pem.Decode(priv)
	if block == nil {
		log.Fatal("Failed to decode PEM private key")
	}

	var parsedKey interface{}
	parsedKey, err = x509.ParseECPrivateKey(block.Bytes)
	if err != nil {
		log.Fatal(err)
	}

	switch parsedKey := parsedKey.(type) {
	case *ecdsa.PrivateKey:
		return parsedKey, nil
	}
	log.Fatal("Unsupported private key type")
	return nil, nil
}

func main() {
	var policy string
	var key string
	var proxy string
	var proxyauth string
	var signingkey string
	var title string
	var config Config

        labels := cli.NewStringSlice()

	cfgPath, cfgExists := getConfigPath()
	if !cfgExists {
		config.Write(cfgPath)
	} else {
		f, err := os.Open(cfgPath)
		if err != nil {
			exitErr(err)
		}
		defer f.Close()
		if err := toml.NewDecoder(f).Decode(&config); err != nil {
			exitErr(err)
		}
	}

	app := cli.NewApp()

	app.Commands = []*cli.Command{
		{
			Name:    "login",
			Aliases: []string{"l"},
			Usage:   "login to Red Light Green Light server",
			Flags: []cli.Flag{
				&cli.StringFlag{
					Name:        "key",
					Value:       "",
					Usage:       "API key",
					Destination: &key,
				},
				&cli.StringFlag{
					Name:        "proxy",
					Value:       "",
					Usage:       "proxy URL (eg. http://HOST:PORT)",
					Destination: &proxy,
				},
				&cli.StringFlag{
					Name:        "proxy-auth",
					Value:       "",
					Usage:       "proxy basic authentication (eg. USERNAME:PASSWORD)",
					Destination: &proxyauth,
				},
				&cli.StringFlag{
					Name:        "signing-key",
					Value:       "",
					Usage:       "optional private signing key",
					Destination: &signingkey,
				},
			},

			Action: func(c *cli.Context) error {

				if c.NArg() == 0 {
					exitErr(fmt.Errorf("Missing server URL"))
				}

				if key == "" {
					var slash string
					if strings.HasSuffix(c.Args().First(), "/") {
						slash = ""
					} else {
						slash = "/"
					}
					exitErr(fmt.Errorf("Missing API key.  Generate a new one at %s%sget-api-key",
						c.Args().First(),
						slash))
				}

				setProxy(proxy, proxyauth)

				response, err := http.Get(fmt.Sprintf("%s/login", c.Args().First()))

				if err != nil {
					exitErr(err)
				}

				responseData, err := ioutil.ReadAll(response.Body)
				if err != nil {
					log.Fatal(err)
				}
				fmt.Println(string(responseData))
				config.Host = c.Args().First()
				config.Key = key
				config.Proxy = proxy
				config.ProxyAuth = proxyauth
				config.Write(cfgPath)

				if signingkey == "" {
					make_keys(cfgPath)
				} else {
					data, err := ioutil.ReadFile(signingkey)
					if err != nil {
						log.Fatal(err)
					}
					err = ioutil.WriteFile(basedir(cfgPath)+"/private_key.pem", data, 0400)
					if err != nil {
						log.Fatal(err)
					}
				}

				return nil
			},
		},
		{
			Name:  "log",
			Usage: "log evaluations",
			Flags: []cli.Flag{
				&cli.StringSliceFlag{
					Name:        "label",
                                        Value:       labels,
                                        Aliases:     []string{"l"},
					Usage:       "set label `KEY=VALUE`",
				},
			},

			Action: func(c *cli.Context) error {

				labels := make(map[string]string)
                                for _, s := range c.StringSlice("label") {
                                    x := strings.Split(s, "=")
                                    labels[x[0]] = x[1]
                                    }

                                if config.Host == "" {
					exitErr(fmt.Errorf("Login to server first"))
				}

				if c.NArg() != 0 {
					exitErr(fmt.Errorf("Too many arguments"))
				}

				setProxy(config.Proxy, config.ProxyAuth)

                                labelsValue, _ := json.Marshal(labels);
				response, err := http.Get(fmt.Sprintf("%s/report-log?labels=\"%s\"", config.Host, url.QueryEscape(base64.StdEncoding.EncodeToString([]byte(string(labelsValue))))))

				if err != nil {
					exitErr(err)
				}

				responseData, err := ioutil.ReadAll(response.Body)
				if err != nil {
					log.Fatal(err)
				}
				fmt.Print(string(responseData))

				return nil
			},
		},
		{
			Name:    "verify",
			Aliases: []string{"v"},
			Usage:   "verify document sigstore record",

			Action: func(c *cli.Context) error {

				if config.Host == "" {
					exitErr(fmt.Errorf("Login to server first"))
				}

				if c.NArg() == 0 {
					exitErr(fmt.Errorf("Missing report argument"))
				} else if c.NArg() > 1 {
					exitErr(fmt.Errorf("Too may arguments"))
				}

				setProxy(config.Proxy, config.ProxyAuth)

				if !strings.HasPrefix(c.Args().First(), "RLGL-") {
					exitErr(fmt.Errorf("expecting a document ID, but got %s", c.Args().First()))
				}

				request, err := http.NewRequest("GET", fmt.Sprintf("%s/verify?id=%s", config.Host, c.Args().First()), nil)
				if err != nil {
					log.Fatal(err)
				}
				var bearer = "Bearer " + config.Key
				request.Header.Add("Authorization", bearer)
				request.Header.Add("Content-Type", "text")
				client := &http.Client{}
				response, err := client.Do(request)

				if err != nil {
					log.Fatal(err)
				}
				defer response.Body.Close()

				responseData, err := ioutil.ReadAll(response.Body)
				if err != nil {
					log.Fatal(err)
				}

				fmt.Print(string(responseData))

				return nil
			},
		},
		{
			Name:  "new-policy-bound-api-key",
			Usage: "create a new policy bound API key",

			Action: func(c *cli.Context) error {

				if config.Host == "" {
					exitErr(fmt.Errorf("Login to server first"))
				}

				if c.NArg() == 0 {
					exitErr(fmt.Errorf("Missing policy argument"))
				} else if c.NArg() > 1 {
					exitErr(fmt.Errorf("Too may arguments"))
				}

				setProxy(config.Proxy, config.ProxyAuth)

				values := map[string]string{"policy": c.Args().First()}

				jsonValue, _ := json.Marshal(values)

				request, err := http.NewRequest("POST", fmt.Sprintf("%s/new-policy-bound-api-key", config.Host), bytes.NewBufferString(string(jsonValue)))
				if err != nil {
					log.Fatal(err)
				}
				var bearer = "Bearer " + config.Key
				request.Header.Add("Authorization", bearer)
				request.Header.Add("Content-Type", "text")

				client := &http.Client{}
				response, err := client.Do(request)

				if err != nil {
					log.Fatal(err)
				}
				defer response.Body.Close()

				responseData, err := ioutil.ReadAll(response.Body)
				if err != nil {
					log.Fatal(err)
				}

				fmt.Print(string(responseData))

				return nil
			},
		},
		{
			Name:    "baseline",
			Aliases: []string{"b"},
			Usage:   "generate baseline XFAIL regression policy",

			Action: func(c *cli.Context) error {

				if config.Host == "" {
					exitErr(fmt.Errorf("Login to server first"))
				}

				if c.NArg() == 0 {
					exitErr(fmt.Errorf("Missing report argument"))
				} else if c.NArg() > 1 {
					exitErr(fmt.Errorf("Too may arguments"))
				}

				setProxy(config.Proxy, config.ProxyAuth)

				if !strings.HasPrefix(c.Args().First(), "RLGL-") {
					exitErr(fmt.Errorf("expecting a document ID, but got %s", c.Args().First()))
				}

				request, err := http.NewRequest("GET", fmt.Sprintf("%s/get-baseline-xfail-policy?id=%s", config.Host, c.Args().First()), nil)
				if err != nil {
					log.Fatal(err)
				}
				var bearer = "Bearer " + config.Key
				request.Header.Add("Authorization", bearer)
				request.Header.Add("Content-Type", "text")
				client := &http.Client{}
				response, err := client.Do(request)

				if err != nil {
					log.Fatal(err)
				}
				defer response.Body.Close()

				responseData, err := ioutil.ReadAll(response.Body)
				if err != nil {
					log.Fatal(err)
				}

				fmt.Print(string(responseData))

				return nil
			},
		},
		{
			Name:    "evaluate",
			Aliases: []string{"e"},
			Usage:   "evaluate test results",
			Flags: []cli.Flag{
				&cli.StringFlag{
					Name:        "policy",
					Value:       "",
					Usage:       "evaluation policy",
					Destination: &policy,
				},
				&cli.StringSliceFlag{
					Name:        "label",
                                        Value:       labels,
                                        Aliases:     []string{"l"},
					Usage:       "set label `KEY=VALUE`",
				},
				&cli.StringFlag{
					Name:        "title",
					Value:       "",
					Usage:       "report title",
					Destination: &title,
				},
			},

			Action: func(c *cli.Context) error {

				labels := make(map[string]string)
                                for _, s := range c.StringSlice("label") {
                                    x := strings.Split(s, "=")
                                    labels[x[0]] = x[1]
                                    }

				if (config.Host == "") || (config.Key == "") {
					exitErr(fmt.Errorf("Login to server first"))
				}

				if policy == "" {
					exitErr(fmt.Errorf("Missing policy"))
				}

				if c.NArg() == 0 {
					exitErr(fmt.Errorf("Missing report"))
				}

				setProxy(config.Proxy, config.ProxyAuth)

				var n string
				var name string

				message := SendPostRequest(&config, fmt.Sprintf("%s/upload", config.Host), c.Args().Get(0), "bin")
				n = string(message)

				f, err := os.Open(c.Args().Get(0))
				if err != nil {
					log.Fatal(err)
				}
				defer f.Close()

                                labelsValue, _ := json.Marshal(labels);
				values := map[string]string{"policy": policy, "name": name, "ref": n, "labels": string(labelsValue)}
				if title != "" {
					values["title"] = title
				}

				jsonValue, _ := json.Marshal(values)

				request, err := http.NewRequest("POST", fmt.Sprintf("%s/evaluate", config.Host), bytes.NewBufferString(string(jsonValue)))
				if err != nil {
					log.Fatal(err)
				}
				var bearer = "Bearer " + config.Key
				request.Header.Add("Authorization", bearer)
				request.Header.Add("Content-Type", "application/json")
				client := &http.Client{}
				response, err := client.Do(request)

				if err != nil {
					log.Fatal(err)
				}
				defer response.Body.Close()

				responseData, err := ioutil.ReadAll(response.Body)
				if err != nil {
					log.Fatal(err)
				}
				if responseData[0] != '{' {
					log.Fatal(string(responseData))
				}
				var result map[string]interface{}
				err = json.Unmarshal([]byte(responseData), &result)
				if err != nil {
					log.Fatal(err)
				}

				cfgdir := basedir(cfgPath)
				var key, _ = loadPrivateKey(cfgdir)

				var data []byte
				data, err = hex.DecodeString(fmt.Sprintf("%s", result["digest"]))
				if err != nil {
					log.Fatal(err)
				}

				var r []byte
				r, err = key.Sign(rand.Reader, data, nil)
				if err != nil {
					log.Fatal(err)
				}

				values = map[string]string{"signature": base64.StdEncoding.EncodeToString(r),
					"id": fmt.Sprintf("%s", result["callback"])}
				jsonValue, _ = json.Marshal(values)

				request, err = http.NewRequest("POST", fmt.Sprintf("%s/callback", config.Host), bytes.NewBufferString(string(jsonValue)))
				if err != nil {
					log.Fatal(err)
				}
				bearer = "Bearer " + config.Key
				request.Header.Add("Authorization", bearer)
				request.Header.Add("Content-Type", "text")
				client = &http.Client{}
				response, err = client.Do(request)

				if err != nil {
					log.Fatal(err)
				}
				defer response.Body.Close()

				_, err = ioutil.ReadAll(response.Body)
				if err != nil {
					log.Fatal(err)
				}

				fmt.Printf("%s: %s (sha3/256: %s)\n", result["colour"], result["url"], result["digest"])

				if result["colour"] == "GREEN" {
					os.Exit(0)
				} else {
					if result["colour"] == "RED" {
						os.Exit(1)
					} else {
						os.Exit(2)
					}
				}
				return nil
			},
		},
	}

	app.Name = "rlgl"
	app.Version = VERSION
	app.Copyright = "(c) 2018-2022 Anthony Green"
	app.Compiled = time.Now()
	app.Authors = []*cli.Author{
		&cli.Author{
			Name:  "Anthony Green",
			Email: "green@moxielogic.com",
		},
	}
	app.Usage = "Red Light Green Light"
	app.Action = func(c *cli.Context) error {

		err := cli.ShowAppHelp(c)
		if err != nil {
			log.Fatal(err)
		}

		return nil
	}

	err := app.Run(os.Args)
	if err != nil {
		log.Fatal(err)
	}
}
