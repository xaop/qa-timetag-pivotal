(ns timetag-pivotal.storysource)

(defprotocol StorySource
  (verify-entries [source pm] "verifies that the entry exists in source")
  (process-entries [source entries story-id total-time user-time] "processes all entries of a single id")
  (post-process-results [source results] )
  )
