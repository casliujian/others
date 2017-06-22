type 'a worker_message = 
	  Worker_box of int * Netmcore.res_id 
	| Result of int * 'a
	| No_task

type 'b collector_message = 
	| Terminate of int
	| New_task of 'b

type tq_header = {mutable lock: Netmcore_mutex.mutex}
type 'a tq = ('a, tq_header) Netmcore_queue.squeue
type 'a tq_decr = ('a, tq_header) Netmcore_queue.squeue_descr
