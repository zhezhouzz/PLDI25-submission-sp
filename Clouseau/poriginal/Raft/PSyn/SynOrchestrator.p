machine SynOrchestrator {
    start state Init {
      entry {
        var initnode: machine;
        var domain_bool: set[bool];
        var domain_tNode: set[tNode];
        var domain_tVal: set[tVal];
        var nodes: machine;
        var setting: setting;
        domain_bool += (true);
        domain_bool += (false);
        domain_tNode += (Node1);
        domain_tNode += (Node2);
        domain_tVal += (0);
        domain_tVal += (1);
        nodes = new Node();
        initnode = new InitNode();
        setting = (nodes = nodes, initnode = initnode);
        send nodes, eInit, setting;
        send initnode, eInit, setting;
        new SynClient((setting = setting, domain_bool = domain_bool, domain_tVal = domain_tVal, domain_tNode = domain_tNode));
      }
    }
  }

  machine Orchestrator {
    start state Init {
      entry {
        var initnode: machine;
        var domain_bool: set[bool];
        var domain_tNode: set[tNode];
        var domain_tVal: set[tVal];
        var nodes: machine;
        var setting: setting;
        domain_bool += (true);
        domain_bool += (false);
        domain_tNode += (Node1);
        domain_tNode += (Node2);
        domain_tVal += (0);
        domain_tVal += (1);
        nodes = new Node();
        initnode = new InitNode();
        setting = (nodes = nodes, initnode = initnode);
        send nodes, eInit, setting;
        send initnode, eInit, setting;
        new Client((setting = setting, domain_bool = domain_bool, domain_tVal = domain_tVal, domain_tNode = domain_tNode));
      }
    }
  }
  
  module M = { SynOrchestrator, SynClient, Orchestrator, Client, InitNode, Node, Timer};
  
  test Syn [main=SynOrchestrator]: assert leanerConsistentView in M;
  test Manual [main=Orchestrator]: assert leanerConsistentView in M;