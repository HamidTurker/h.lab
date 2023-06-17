# Source
message("h.test.phasic_response :: v0.1: 2023 May 10")

# Function
h.test.phasic_response <- function(spikes, events, pre_window = c(-3,-1), post_window = c(0,1), group = NULL, 
                                   trial_window = c(-10,10), bin = .001, example = FALSE, hlab_dir = NULL) {
  
  # Want an example?
  if (example) {
    
    if (is.null(hlab_dir)) { stop("Please provide the path to the h.lab directory and pass it to argument 'hlab_dir'.") }
    
    # Load example data from example_data directory
    xd_dir = paste0(hlab_dir,'/example_data/')
    load(paste0(xd_dir,'trials.RData'))
    load(paste0(xd_dir,'spikes.RData'))
    
    message("First, unlist your output. The returned list is as follows:
            
            example = h.test.phasic_response(example=T,hlab_dir=hlab_dir)
            
            trials = example[[1]]
            spikes = example[[2]]
            
            
            ")
    
    return(list(trials,spikes))
    
    
    # Don't print error, just stop
    #{
    #  opt <- options(show.error.messages = FALSE)
    #  on.exit(options(opt))
    #  stop()
    #}
    
  }

  # Initialize
  n_events=length(events)
  if (is.null(group)) {
    df <- data.frame(
      events = 1:n_events,
      pre = rep(0,n_events),
      post = rep(0,n_events)
    )
  } else {
    df <- data.frame(
      group = group,
      events = 1:n_events,
      pre = rep(0,n_events),
      post = rep(0,n_events)
    )
  }
  
  # Compute average firing rate in the pre-event (baseline) period and post-event (signal/no-signal) period
  for (i in 1:n_events) {
    
    # Pre
    df$pre[i]<-h.calc.firingrate_for_event(events[i], spikes, pre = pre_window[1], post = pre_window[2], group = NULL)$Hz
    
    # Post
    df$post[i]<-h.calc.firingrate_for_event(events[i], spikes, pre = post_window[1], post = post_window[2], group = NULL)$Hz
    
  }
  
  # Test for differences
  if (is.null(group)) {
    
    # Return t-test results
    return(t.test(df$pre,df$post))
    
  } else {
    
    # Order the frame
    df_ord=df[order(df$group),]
    
    # ANOVA frame
    df_aov<-data.frame(
      group = as.factor(rep(df_ord$group, 2)),
      time = as.factor(rep(c("pre","post"), each=length(group))),
      Hz = c(df_ord$pre,df_ord$post)
    )
    
    # Return ANOVA results
    return(summary(aov(Hz~group*time, df_aov)))
    
  }
  
  
}