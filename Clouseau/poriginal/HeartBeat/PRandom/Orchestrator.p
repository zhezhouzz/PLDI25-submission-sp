machine SynOrchestrator {
    start state Init {
      entry {
        var node: machine;
        var fd: machine;
        var domain_int: set[int];
        var domain_bool: set[bool];
        node = new Node();
        fd = new FailureDetector((node = node,));
        domain_int += (0);
        domain_int += (1);
        domain_int += (2);
        domain_bool += (true);
        domain_bool += (false);
        new SynClient((node = node, setting = fd, domain_int = domain_int));
      }
    }
  }

  machine Orchestrator {
    start state Init {
      entry {
        var node: machine;
        var fd: machine;
        var domain_int: set[int];
        var domain_bool: set[bool];
        node = new Node();
        fd = new FailureDetector((node = node,));
        domain_int += (0);
        domain_int += (1);
        domain_int += (2);
        domain_bool += (true);
        domain_bool += (false);
        new Client((node = node, setting = fd, domain_int = domain_int));
      }
    }
  }
  
  module M = { Orchestrator, Client, SynOrchestrator, SynClient, Node, FailureDetector, Timer };
  
  test Syn [main=SynOrchestrator]: assert ReliableFailureDetector in M;
  test Manual [main=Orchestrator]: assert ReliableFailureDetector in M;