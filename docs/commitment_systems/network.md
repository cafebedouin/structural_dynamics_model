Network Properties That Will Need Revisiting

  1. Edge Discovery — drl_purity_network.pl:constraint_neighbors/3

  The current implementation discovers edges from three sources: affects_constraint/2 (explicit),
  infer_structural_coupling/3 (inferred from gradient data), and shared_agent_link (shared beneficiary/victim). The new
  cs_reading_relation/3 typed edges — forecloses, coexists_with, influences — are not included and currently have no
  pathway into the neighbor graph.

  The semantics are different enough to require a deliberate decision, not just a wiring addition. Purity contamination
  flows from low-purity neighbors to high-purity targets. But forecloses is logical preemption (one reading makes
  another structurally impossible), coexists_with is licensed plurality (no domination), and influences is already
  consumed by drl_composition.pl:detect_necessity_inheritance for mountain entailment. None of these map cleanly onto
  "contaminates." The question to settle: are kernel readings connected nodes in the contamination network at all, or do
   they form a separate layer that only the CS engine reasons about?

  2. Accidental Intra-Kernel Connectivity via Shared Agent Links

  Multiple readings of the same kernel typically share the same victim and beneficiary agent classes (e.g., the
  abolition_reading, deterrence_reading, and retributive_reading all involve the same social agents around capital
  punishment). This means shared_agent_link will automatically generate shared_beneficiary/shared_victim edges between
  all readings of the same kernel — edges that already have explicit cs_reading_relation coverage.

  This creates double-counting: the same between-reading relationship is expressed once intentionally (via
  cs_reading_relation) and once accidentally (via shared victim/beneficiary). The shared_agent_edge_strength formula
  (0.3 × N, capped at 1.0) will add weak coupling between readings without any semantic justification for contamination
  flow. Whether this should be blocked — e.g., by excluding intra-kernel pairs from the shared agent calculation — is an
   open architectural question.

  3. CS Drift State is Invisible to the Purity Network

  network_drift_velocity/4 in network_dynamics.pl reads drift signals exclusively from metric_drift_events:drift_velocity(C,
  base_extractiveness, Rate) — that is, from the metric-level temporal drift system (extractiveness changing over time).
   The CS-layer drift (cs_drift_state/3) is an entirely separate mechanism: gap(axiom_overriding, substantial, false) in
   a constraint heading toward axiom_foreclosure (via cs_drift_engine.pl) generates no purity decline signal visible to
  the network layer.

  Consequence: a reading that the CS axiom engine has diagnosed as foreclosed can still show high purity and low network
   drift velocity, because its structural failure is expressed entirely in the CS layer. The two drift systems are
  currently disconnected.

  4. type_contamination_strength Table Does Not Differentiate Naturalized Mountains

  drl_purity_network.pl:type_contamination_strength(mountain, 0.0) treats all mountain-typed constraints as immune and
  non-emitting. But cs_pattern_detection.pl:cs_naturalized_mountain/1 identifies a specific subpopulation: low-ε
  mountains with extraction authority grounding and both victims and beneficiaries — the cover-story variant. These
  currently inherit strength 0.0 from the mountain clause. The CS layer has the diagnostic, but the contamination table
  can't act on it without either a type refinement (adding naturalized_mountain as a separate DR type) or a hook from
  the CS layer into contamination strength lookup.

  5. Giant Component Analysis Edge Inventory is Stale

  The comment in giant_component_analysis.pl Phase 4 (~line 1255) explicitly says: "edges come from affects_constraint,
  infer_structural_coupling, and shared_agent_link — none of which depend on observer context." That description is now
  incomplete. The cs_reading_relation/3 edges exist in testsets (forecloses and coexists_with are both authored), but
  since they aren't fed into constraint_neighbors/3, the giant component analysis silently omits a whole connectivity
  structure. If/when reading-relation edges are added to the network, this analysis would need a rerun and the Phase 4
  invariant ("context-independent topology") needs verification, since reading relations are also context-independent.

  6. FPN Precompute and Convergence Scope

  drl_fpn.pl:fpn_run/2 enumerates all constraint_claim facts, which now includes CS reading constraints. This is
  probably fine and desirable. However, the neighbor graph it caches comes from constraint_neighbors/3 — so the FPN runs
   over readings as nodes with only accidental (shared-agent) connections between them, not the authored
  reading-relation edges. If reading-relation edges were added with non-standard semantics (e.g., forecloses as a hard
  type-override rather than a purity flow), the convergence proof (monotone endofunctor on a compact lattice) would need
   re-examination: the proof assumes contamination only flows downward in purity and never forces a categorical type
  change, but a forecloses edge carries a different kind of influence.

  7. network_stability_assessment Scope

  network_dynamics.pl:network_stability_assessment/2 enumerates all loaded constraint_claim facts and counts how many
  are actively drifting via detect_network_contamination. CS reading constraints will be counted here, but their drift signals
  come only from the metric drift system. So a corpus heavy with kernel readings will have many "stable" readings (no
  metric drift) even when they're in axiom-foreclosure territory. The assessment could be misleading — a reading the CS
  engine identifies as foreclosed will register as stable in the network layer.

  ---
  The central architectural question this survey surfaces: the CS kernel/reading system is currently a parallel
  analytical layer that sits beside the purity network rather than inside it. Whether it should feed into the network
  (and if so, which edges and which signals), or whether the two layers are intentionally separate with only
  drl_composition.pl bridging them via the influences entailment path, is the decision that determines which of these
  items are actual bugs versus acceptable scope boundaries.
