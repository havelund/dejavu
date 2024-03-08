# MSL logs

The 96,795 sized log is an extension of the 50,000 sized log.

## nfer Property

```
cmdExec :- CMD_DISPATCH before CMD_COMPLETE
  where CMD_DISPATCH.cmd = CMD_COMPLETE.cmd
    map {cmd -> CMD_DISPATCH.cmd}

okRace :- TLM_TR_ERROR during cmdExec
  where cmdExec.cmd = "MOB_NAV_PRM_SET" | cmdExec.cmd = "ARM_PRM_SETDMP"
```

## DejaVu property

```
prop races : suc("MOB_PRM") -> (P[<=5] dis("MOB_PRM") &
  @(!(suc("MOB_PRM") | Exists msg . tr_err(msg)) S dis("MOB_PRM")))

prop noARMRaces :
  (
    CMD_COMPLETE("ARM_PRM_SETDMP") ->
    (
      P[<=5] CMD_DISPATCH("ARM_PRM_SETDMP")
      &
      @(
        ! ( CMD_COMPLETE("ARM_PRM_SETDMP") | Exists msg . TLM_TR_ERROR(msg) )
        S
        CMD_DISPATCH("ARM_PRM_SETDMP")
      )
    )
  )

```

## Evaluation 

### Trace has 50,000  events.)

- verification with time 5:  0.7 seconds, 8 errors detected     -> factor  2.3   (4 bits)
- verification with time 10: 0.7 seconds, 8 errors detected     -> factor  2.3   (5 bits)
- verification with time 20: 0.7 seconds, 8 errors detected     -> factor  2.3   (6 bits)
- verification with time 50: 1.0 seconds, 6 errors detected     -> factor  3.3   (7 bits)
- verification with time 60: 1.0 seconds, 6 errors detected     -> factor  3.3   (7 bits)
- verification with time 70: 1161.5s seconds, 6 errors detected -> factor  1659.3   (8 bits)

- verification without time: 0.3s seconds, 6 errors detected

### Trace has 96,795  events.

- verification with time 5:  1.0 seconds, 20 errors detected    -> factor  2.0   (4 bits)
- verification with time 10: 1.0 seconds, 20 errors detected    -> factor  2.0   (5 bits)
- verification with time 20: 1.1 seconds, 20 errors detected    -> factor  2.2   (6 bits)
- verification with time 50: 1.5 seconds, 13 errors detected    -> factor  3.0   (7 bits)
- verification with time 60: 1.6 seconds, 13 errors detected    -> factor  3.2   (7 bits)
- verification with time 70: 2185.9 seconds, 13 errors detected -> factor  4371.8   (8 bits)

- verification without time: 0.5 seconds, 13 errors detected
