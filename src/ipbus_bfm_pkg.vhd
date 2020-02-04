--========================================================================================================================
-- This VVC was generated with Bitvis VVC Generator
--========================================================================================================================


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library std;
use std.textio.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library ipbus;
use ipbus.ipbus.all;

--========================================================================================================================
--========================================================================================================================
package ipbus_bfm_pkg is

  --========================================================================================================================
  -- Types and constants for IPBUS BFM
  --========================================================================================================================
  constant C_SCOPE : string := "IPBUS BFM";

  type t_ipbus_response_status is (ACK, ERR);

  type t_ipbus_if is record
    -- to dut
    addr   : std_logic_vector(31 downto 0);
		wdata  : std_logic_vector(31 downto 0);
		strobe : std_logic;
    wr     : std_logic;
    -- from dut
    rdata: std_logic_vector(31 downto 0);
		ack: std_logic;
		err: std_logic;
  end record;

  -- Configuration record to be assigned in the test harness.
  type t_ipbus_bfm_config is
  record
    max_wait_cycles             : integer;
    max_wait_cycles_severity    : t_alert_level;
    clock_period                : time;                    -- Period of the clock signal.
    clock_period_margin         : time;                    -- Input clock period margin to specified clock_period
    clock_margin_severity       : t_alert_level;           -- The above margin will have this severity
    setup_time                  : time;                    -- Setup time for generated signals, set to clock_period/4
    hold_time                   : time;                    -- Hold time for generated signals, set to clock_period/4
    expected_response           : t_ipbus_response_status; -- Sets the expected response for both read and write transactions.
    expected_response_severity  : t_alert_level;           -- A response mismatch will have this severity.
    id_for_bfm                  : t_msg_id;
    id_for_bfm_wait             : t_msg_id;
    id_for_bfm_poll             : t_msg_id;
  end record;

  -- Define the default value for the BFM config
  constant C_IPBUS_BFM_CONFIG_DEFAULT : t_ipbus_bfm_config := (
    max_wait_cycles             => 10,
    max_wait_cycles_severity    => TB_FAILURE,
    clock_period                => 10 ns,
    clock_period_margin         => 0 ns,
    clock_margin_severity       => TB_ERROR,
    setup_time                  => 2.5 ns,
    hold_time                   => 2.5 ns,
    expected_response           => ACK,
    expected_response_severity  => TB_FAILURE,
    id_for_bfm                  => ID_BFM,
    id_for_bfm_wait             => ID_BFM_WAIT,
    id_for_bfm_poll             => ID_BFM_POLL
  );


  --========================================================================================================================
  -- BFM procedures
  --========================================================================================================================

  function init_ipbus_if_signals return t_ipbus_if;

  procedure ipbus_write (
    constant addr_value       : in  unsigned;
    constant data_value       : in  std_logic_vector;
    constant msg              : in  string;
    signal clk                : in  std_logic;
    signal ipbus_if           : inout t_ipbus_if;
    constant scope            : in  string                    := C_SCOPE;
    constant msg_id_panel     : in  t_msg_id_panel            := shared_msg_id_panel;
    constant config           : in  t_ipbus_bfm_config        := C_IPBUS_BFM_CONFIG_DEFAULT
    );

  procedure ipbus_read (
    constant addr_value       : in  unsigned;
    variable data_value       : out std_logic_vector;
    constant msg              : in  string;
    signal clk                : in  std_logic;
    signal ipbus_if           : inout t_ipbus_if;
    constant scope            : in  string                    := C_SCOPE;
    constant msg_id_panel     : in  t_msg_id_panel            := shared_msg_id_panel;
    constant config           : in  t_ipbus_bfm_config        := C_IPBUS_BFM_CONFIG_DEFAULT;
    constant ext_proc_call    : in  string                    := ""  -- External proc_call; overwrite if called from other BFM procedure like sbi_check
    );

  procedure ipbus_check (
    constant addr_value       : in  unsigned;
    constant data_exp         : in  std_logic_vector;
    constant msg              : in  string;
    signal clk                : in  std_logic;
    signal ipbus_if           : inout t_ipbus_if;
    constant alert_level      : in  t_alert_level             := error;
    constant scope            : in  string                    := C_SCOPE;
    constant msg_id_panel     : in  t_msg_id_panel            := shared_msg_id_panel;
    constant config           : in  t_ipbus_bfm_config        := C_IPBUS_BFM_CONFIG_DEFAULT
    );

end package ipbus_bfm_pkg;


--========================================================================================================================
--========================================================================================================================

package body ipbus_bfm_pkg is

  function init_ipbus_if_signals return t_ipbus_if is
    variable result : t_ipbus_if;
  begin
    -- BFM to DUT signals
    result.wdata    := (others => '0');
    result.addr     := (others => '0');
    result.wr       := '0';
    result.strobe   := '0';

    -- DUT to BFM signals
    result.rdata     := (others => 'Z');
    result.ack       := 'Z';
    result.err       := 'Z';

    return result;
  end function;

  procedure ipbus_write (
    constant addr_value       : in  unsigned;
    constant data_value       : in  std_logic_vector;
    constant msg              : in  string;
    signal clk                : in  std_logic;
    signal ipbus_if           : inout t_ipbus_if;
    constant scope            : in  string                    := C_SCOPE;
    constant msg_id_panel     : in  t_msg_id_panel            := shared_msg_id_panel;
    constant config           : in  t_ipbus_bfm_config        := C_IPBUS_BFM_CONFIG_DEFAULT
    ) is
    constant proc_name : string := "ipbus_write";
    constant proc_call : string := "ipbus_write(A:" & to_string(addr_value, HEX, AS_IS, INCL_RADIX) &
                                   ", " & to_string(data_value, HEX, AS_IS, INCL_RADIX) & ")";
    -- normalize_and_check to the DUT addr/data widths
    variable v_normalized_addr : std_logic_vector(ipbus_if.addr'length-1 downto 0) :=
      normalize_and_check(std_logic_vector(addr_value), ipbus_if.addr, ALLOW_NARROWER, "address", "ipbus_if.addr", msg);
    variable v_normalized_data : std_logic_vector(ipbus_if.wdata'length-1 downto 0) :=
      normalize_and_check(data_value, ipbus_if.wdata, ALLOW_NARROWER, "data", "ipbus_if.wdata", msg);

    variable timeout            : boolean := false;
    variable v_last_falling_edge : time    := -1 ns;  -- time stamp for clk period checking

  begin
    -- setup_time and hold_time checking
    check_value(config.setup_time < config.clock_period/2, TB_FAILURE, "Sanity check: Check that setup_time do not exceed clock_period/2.", scope, ID_NEVER, msg_id_panel, proc_name);
    check_value(config.hold_time < config.clock_period/2, TB_FAILURE, "Sanity check: Check that hold_time do not exceed clock_period/2.", scope, ID_NEVER, msg_id_panel, proc_name);
    check_value(config.setup_time > 0 ns, TB_FAILURE, "Sanity check: Check that setup_time is more than 0 ns.", scope, ID_NEVER, msg_id_panel, proc_name);
    check_value(config.hold_time > 0 ns, TB_FAILURE, "Sanity check: Check that hold_time is more than 0 ns.", scope, ID_NEVER, msg_id_panel, proc_name);

    -- check if enough room for setup_time in low period
    if (clk = '1') and (config.setup_time > (config.clock_period/2 - clk'last_event))then
      await_value(clk, '0', 0 ns, config.clock_period/2, TB_FAILURE, proc_name & ": timeout waiting for clk low period for setup_time.");
    end if;
    -- Wait setup_time specified in config record  --wait_until_given_time_after_rising_edge(clk, config.clock_period/4);
    wait_until_given_time_after_rising_edge(clk, config.setup_time);

    ipbus_if.wdata    <= v_normalized_data;
    ipbus_if.addr     <= v_normalized_addr;
    ipbus_if.strobe   <= '1'; -- Addr and data qualifier
    ipbus_if.wr       <= '1'; -- Write enable

    wait until falling_edge(clk);     -- wait for DUT update of signal
    -- check if clk period since last rising edge is within specifications and take a new time stamp
    if v_last_falling_edge > -1 ns then
      check_value_in_range(now - v_last_falling_edge, config.clock_period - config.clock_period_margin, config.clock_period + config.clock_period_margin, config.clock_margin_severity, "checking clk period is within requirement.");
    end if;
    v_last_falling_edge := now; -- time stamp for clk period checking

    for cycle in 1 to config.max_wait_cycles loop
      if ipbus_if.ack = '0' and ipbus_if.err = '0' then
        wait until falling_edge(clk);
        -- check if clk period since last rising edge is within specifications and take a new time stamp
        if v_last_falling_edge > -1 ns then
          check_value_in_range(now - v_last_falling_edge, config.clock_period - config.clock_period_margin, config.clock_period + config.clock_period_margin, config.clock_margin_severity, "checking clk period is within requirement.");
        end if;
        v_last_falling_edge := now; -- time stamp for clk period checking
      else
        -- check the response
        if config.expected_response = ACK then
          check_value(ipbus_if.ack, '1', config.expected_response_severity, ": ACK detected", scope, ID_NEVER, msg_id_panel, proc_call);
        elsif config.expected_response = ERR then
          check_value(ipbus_if.err, '1', config.expected_response_severity, ": ERR detected", scope, ID_NEVER, msg_id_panel, proc_call);
        end if;
        exit;
      end if;
      if cycle = config.max_wait_cycles then
        timeout := true;
      end if;
    end loop;

    -- did we timeout?
    if timeout then
      alert(config.max_wait_cycles_severity, proc_call & "=> Failed. Timeout waiting for ack_i" & add_msg_delimiter(msg), scope);
    else
      wait until rising_edge(clk);

      -- Wait hold time specified in config record  --wait_until_given_time_after_rising_edge(clk, config.clock_period/4);
      wait_until_given_time_after_rising_edge(clk, config.hold_time);
    end if;

    ipbus_if <= init_ipbus_if_signals;
    log(config.id_for_bfm, proc_call & " completed. " & add_msg_delimiter(msg), scope, msg_id_panel);
  end procedure ipbus_write;


  procedure ipbus_read (
    constant addr_value       : in  unsigned;
    variable data_value       : out std_logic_vector;
    constant msg              : in  string;
    signal clk                : in  std_logic;
    signal ipbus_if           : inout t_ipbus_if;
    constant scope            : in  string                    := C_SCOPE;
    constant msg_id_panel     : in  t_msg_id_panel            := shared_msg_id_panel;
    constant config           : in  t_ipbus_bfm_config        := C_IPBUS_BFM_CONFIG_DEFAULT;
    constant ext_proc_call    : in  string                    := ""  -- External proc_call; overwrite if called from other BFM procedure like sbi_check
    ) is
    -- local_proc_name/call used if called from sequencer or VVC
    constant local_proc_name : string := "ipbus_read";
    constant local_proc_call : string := local_proc_name & "(A:" & to_string(addr_value, HEX, AS_IS, INCL_RADIX) & ")";

    -- normalize_and_check to the DUT addr/data widths
    variable v_normalized_addr : std_logic_vector(ipbus_if.addr'length-1 downto 0) :=
      normalize_and_check(std_logic_vector(addr_value), ipbus_if.addr, ALLOW_NARROWER, "addr", "ipbus_if.addr", msg);
    variable v_normalized_data : std_logic_vector(ipbus_if.rdata'length-1 downto 0) :=
      normalize_and_check(data_value, ipbus_if.rdata, ALLOW_NARROWER, "data", "ipbus_if.rdata", msg);

    -- Helper variables
    variable timeout              : boolean := false;
    variable v_last_falling_edge  : time    := -1 ns;  -- time stamp for clk period checking
    variable v_last_rising_edge   : time    := -1 ns;  -- time stamp for clk period checking
    variable v_proc_call          : line;
  begin
    -- setup_time and hold_time checking
    check_value(config.setup_time < config.clock_period/2, TB_FAILURE, "Sanity check: Check that setup_time do not exceed clock_period/2.", scope, ID_NEVER, msg_id_panel, local_proc_name);
    check_value(config.hold_time < config.clock_period/2, TB_FAILURE, "Sanity check: Check that hold_time do not exceed clock_period/2.", scope, ID_NEVER, msg_id_panel, local_proc_name);
    check_value(config.setup_time > 0 ns, TB_FAILURE, "Sanity check: Check that setup_time is more than 0 ns.", scope, ID_NEVER, msg_id_panel, local_proc_name);
    check_value(config.hold_time > 0 ns, TB_FAILURE, "Sanity check: Check that hold_time is more than 0 ns.", scope, ID_NEVER, msg_id_panel, local_proc_name);

    -- If called directly from sequencer/VVC, show 'wichbone_read...' in log
    if ext_proc_call = "" then
      write(v_proc_call, local_proc_call);
    else
      -- If called from other BFM procedure like sbi_check, log 'sbi_check(..) while executing sbi_read..'
      write(v_proc_call, ext_proc_call & " while executing " & local_proc_name);
    end if;


    -- check if enough room for setup_time in low period
    if (clk = '1') and (config.setup_time > (config.clock_period/2 - clk'last_event))then
      await_value(clk, '0', 0 ns, config.clock_period/2, TB_FAILURE, local_proc_name & ": timeout waiting for clk low period for setup_time.");
    end if;
    -- Wait setup_time specified in config record -- wait_until_given_time_after_rising_edge(clk, config.clock_period/4);
    wait_until_given_time_after_rising_edge(clk, config.setup_time);

    ipbus_if.addr     <= v_normalized_addr;
    ipbus_if.strobe   <= '1'; -- Addr and data qualifier
    ipbus_if.wr       <= '0'; -- Read enable

    wait until falling_edge(clk);     -- wait for DUT update of signal
    -- check if clk period since last rising edge is within specifications and take a new time stamp
    if v_last_falling_edge > -1 ns then
      check_value_in_range(now - v_last_falling_edge, config.clock_period - config.clock_period_margin, config.clock_period + config.clock_period_margin, config.clock_margin_severity, "checking clk period is within requirement.");
    end if;
    v_last_falling_edge := now; -- time stamp for clk period checking

    for cycle in 1 to config.max_wait_cycles loop
      if ipbus_if.ack = '0' and ipbus_if.err = '0' then
        wait until falling_edge(clk);
        -- check if clk period since last rising edge is within specifications and take a new time stamp
        if v_last_falling_edge > -1 ns then
          check_value_in_range(now - v_last_falling_edge, config.clock_period - config.clock_period_margin, config.clock_period + config.clock_period_margin, config.clock_margin_severity, "checking clk period is within requirement.");
        end if;
        v_last_falling_edge := now; -- time stamp for clk period checking
      else
        -- check the response
        if config.expected_response = ACK then
          check_value(ipbus_if.ack, '1', config.expected_response_severity, ": ACK detected", scope, ID_NEVER, msg_id_panel, v_proc_call.all);
        elsif config.expected_response = ERR then
          check_value(ipbus_if.err, '1', config.expected_response_severity, ": ERR detected", scope, ID_NEVER, msg_id_panel, v_proc_call.all);
        end if;
        exit;
      end if;
      if cycle = config.max_wait_cycles then
        timeout := true;
      end if;
    end loop;

    -- did we timeout?
    if timeout then
      alert(config.max_wait_cycles_severity, v_proc_call.all & "=> Failed. Timeout waiting for ack_i " & add_msg_delimiter(msg), scope);
    else
      wait until rising_edge(clk);
        -- check if clk period since last rising edge is within specifications and take a new time stamp
        if v_last_rising_edge > -1 ns then
          check_value_in_range(now - v_last_rising_edge, config.clock_period - config.clock_period_margin, config.clock_period + config.clock_period_margin, config.clock_margin_severity, "checking clk period is within requirement.");
        end if;
        v_last_rising_edge := now; -- time stamp for clk period checking

      -- Wait hold time specified in config record --wait_until_given_time_after_rising_edge(clk, config.clock_period/4);
      wait_until_given_time_after_rising_edge(clk, config.hold_time);
    end if;

    v_normalized_data := ipbus_if.rdata;
    data_value        := v_normalized_data(data_value'length-1 downto 0);

    ipbus_if <= init_ipbus_if_signals;

    if ext_proc_call = "" then -- proc_name = "ipbus_read"
      log(config.id_for_bfm, v_proc_call.all & "=> " & to_string(data_value, HEX, SKIP_LEADING_0, INCL_RADIX) & ". " & add_msg_delimiter(msg), scope, msg_id_panel);
    end if;
  end procedure ipbus_read;

  procedure ipbus_check (
    constant addr_value       : in  unsigned;
    constant data_exp         : in  std_logic_vector;
    constant msg              : in  string;
    signal clk                : in  std_logic;
    signal ipbus_if           : inout t_ipbus_if;
    constant alert_level      : in  t_alert_level             := error;
    constant scope            : in  string                    := C_SCOPE;
    constant msg_id_panel     : in  t_msg_id_panel            := shared_msg_id_panel;
    constant config           : in  t_ipbus_bfm_config        := C_IPBUS_BFM_CONFIG_DEFAULT
    ) is
    constant proc_name : string := "ipbus_check";
    constant proc_call : string := "ipbus_check(A:" & to_string(addr_value, HEX, AS_IS, INCL_RADIX) & ", " & to_string(data_exp, HEX, AS_IS, INCL_RADIX) & ")";

    -- normalize_and_check to the DUT addr/data widths
    variable v_normalized_data : std_logic_vector(ipbus_if.rdata'length-1 downto 0) :=
      normalize_and_check(data_exp, ipbus_if.rdata, ALLOW_NARROWER, "data", "ipbus_if.rdata", msg);

    -- Helper variables
    variable v_data_value : std_logic_vector(ipbus_if.rdata'length-1 downto 0) := (others => '0');
    variable v_check_ok   : boolean;
  begin
    ipbus_read(addr_value, v_data_value, msg, clk, ipbus_if, scope, msg_id_panel, config, proc_call);

    v_check_ok := true;
    for i in 0 to (v_normalized_data'length)-1 loop
      if v_normalized_data(i) = '-' or v_normalized_data(i) = v_data_value(i) then
        v_check_ok := true;
      else
        v_check_ok := false;
        exit;
      end if;
    end loop;

    if not v_check_ok then
      alert(alert_level, proc_call & "=> Failed. slv Was " & to_string(v_data_value, HEX, AS_IS, INCL_RADIX) & ". Expected " & to_string(data_exp, HEX, AS_IS, INCL_RADIX) & "." & LF & add_msg_delimiter(msg), scope);
    else
      log(config.id_for_bfm, proc_call & "=> OK, received data = " & to_string(v_normalized_data, HEX, SKIP_LEADING_0, INCL_RADIX) & ". " & add_msg_delimiter(msg), scope, msg_id_panel);
    end if;
  end procedure ipbus_check;

end package body ipbus_bfm_pkg;

