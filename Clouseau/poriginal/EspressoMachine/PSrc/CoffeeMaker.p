
/* Requests or operations from the panel to coffee maker */

// event: warmup request when the coffee maker starts or resets
event eWarmUpReq;
// event: grind beans request before making coffee
event eGrindBeansReq;
// event: start brewing coffee
event eStartEspressoReq;
// event start steamer
event eStartSteamerReq;
// event: stop steamer
event eStopSteamerReq;

/* Responses from the coffee maker to the panel */
// event: completed grinding beans
event eGrindBeansCompleted;
// event: completed brewing and pouring coffee
event eEspressoCompleted;
// event: warmed up the machine and read to make coffee
event eWarmUpCompleted;

/* Error messages from the coffee maker to control panel or panel*/
// event: no water for coffee, refill water!
event eNoWaterError;
// event: no beans for coffee, refill beans!
event eNoBeansError;
// event: the heater to warm the machine is broken!
event eWarmerError;

/*****************************************************
EspressoCoffeeMaker receives requests from the control panel of the coffee machine and
based on its state e.g., whether heater is working, or it has beans and water, the maker responds
back to the panel if the operation succeeded or errored.
*****************************************************/
machine EspressoCoffeeMaker
{
  // control panel of the coffee machine that sends inputs to the coffee maker
  var panel: CoffeeMakerControlPanel;

  start state WaitForRequests {
    entry (_panel: CoffeeMakerControlPanel) {
      panel = _panel;
    }

    on syn_eWarmUpReq do (input: tsyn_eWarmUpReq) {
        RealSend(panel, syn_eWarmUpCompleted, (controller = input.controller, dst = panel));
    }

    on syn_eGrindBeansReq do (input: tsyn_eGrindBeansReq) {
      if (!HasBeans()) {
        RealSend(panel, syn_eNoBeansError, (controller = input.controller, dst = panel));
      } else {
        RealSend(panel, syn_eGrindBeansCompleted, (controller = input.controller, dst = panel));
      }
    }

    on syn_eStartEspressoReq do (input: tsyn_eStartEspressoReq) {
      if (!HasWater()) {
        RealSend(panel, syn_eNoWaterError, (controller = input.controller, dst = panel));
      } else {
        RealSend(panel, syn_eEspressoCompleted, (controller = input.controller, dst = panel));
      }
    }
    on eStartSteamerReq do {
      if (!HasWater()) {
        send panel, eNoWaterError;
      }
    }
    on eStopSteamerReq do { /* do nothing, steamer stopped */ }
  }

  // nondeterministic functions to trigger different behaviors
  fun HasBeans() : bool { return $; }
  fun HasWater() : bool { return $; }
}


