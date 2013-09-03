swirl
=====


### Function Details: ###

#### swirl_flow:start/4 #####

    start(FlowMod :: atom(), FlowOpts :: [flow_opts()], MapperNodes :: [node()], ReducerNode :: node()) -> ok
    
#### swirl_stream:emit/1 #####

    emit(StreamName :: atom(), Event :: event()) -> ok
    
### Data Types: ###

    event() :: [{atom(), value()}].
    flow_opts() :: {stream_name, atom()} | {stream_filter, string()}.
