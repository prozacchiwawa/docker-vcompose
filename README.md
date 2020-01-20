# vcompose

A tool for building docker compose files in layers, and using ascii art drawings.

## Usage

vcompose [-Dvar=val ...] file.dml

- Dvar=val -- Defines a replacement variable for use in ```${...}``` in template yaml.

- Paths in dml and .machine template files are relative to the file containing them.

- See example.

Basically, you can define machines by template, specify variables to replace either on the
command line or in the glyphs of the drawing.  A protocol hierarchy is specified so that
connections between machines are type checked.  Variable replacements are checked as well.

Glyphs in the drawing have letters in their frames, which correspond to ports and outbound
connections in the machine description.  You can specify how they're connected in the drawing.
Specifying a .port.external value in the drawing causes the resulting docker-compose yaml to
contain a ports entry.

This is very experimental WIP but I like it, and I feel like the ability to declutter docker
compose into layers is valuable.

    ,--------------------------.
    | id: main                 |
    | system-yaml: system.yaml |
    | networks:                |
    |   basic:                 |
    `--------------------------'
    
    ,-----------------------------------.
    | id: couchdb0                      |
    | machine: couchdb                  |
    | http-listener.port.external: 5985 |
    `---------------------------------C-'
                                      | 
    ,--------------------------.      |
    | id: orderer.mydomain.com |      |
    | machine: orderer         |      |
    `------P-------------------'      |
           |                          |
           +-----+                    |
                 |                    |
    ,------------o----------------.   |
    | id: peer0.org1.mydomain.com |   |
    P machine: peer               c---+
    | peer.port.external: 7051    |
    `-----------------------------'

And (peer.machine)

    mdyBaseYaml: 'peer.template.yaml'
    
    mdyListenPorts:
      - nlpyName: peer
        nlpyLabel: P
        nlpyPort: 7051
        nlpyProtocol: hyperledger-peer
    
    mdyConnectPorts:
      - ncpyName: couchdb
        ncpyLabel: c
        ncpyProtocol: couchdb
    
      - ncpyName: orderer
        ncpyLabel: o
        ncpyProtocol: hyperledger-peer
