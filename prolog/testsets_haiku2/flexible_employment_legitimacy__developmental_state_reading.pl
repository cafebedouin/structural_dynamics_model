% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__developmental_state_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__developmental_state_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: flexible_employment_legitimacy__developmental_state_reading
 *   human_readable: Flexible Employment as Managed Transition Toward Formalization
 *   domain: labor_economics/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the developmental-state reading of flexible
 *   employment: informal and gig-economy work is authorized as a transitional
 *   form, managed by state policy toward gradual formalization and
 *   labor-market integration by a 2027 target. The reading frames flexible
 *   employment not as a permanent feature of labor-market efficiency
 *   (market-efficiency sibling) nor as structural precarity for extraction
 *   (precarity-extraction sibling), but as a staged transition requiring
 *   active state coordination. The state apparatus sets standards (12-point
 *   plan), monitors compliance, expands benefits progressively to formalized
 *   worker segments, and uses labor-market data to adjust the transition
 *   timeline. Workers in the flexible regime occupy a bounded, temporary
 *   status: precarious now, but with a credible pathway to formal employment
 *   if the state executes the plan. The constraint's legitimacy depends
 *   entirely on the believability of the sunset clause and the state's
 *   capacity to deliver formalization. If the target is missed or the plan
 *   stalls, the reading collapses into Piton (theatrical commitment masking
 *   indefinite informality) or Snare (extraction justified by a defunct
 *   transition narrative).
 *
 * KEY AGENTS:
 *   - developmental_state_apparatus: institutional agenda-setter; designs and enforces the 12-point plan; claims authority to manage the transition; bears reputational cost if formalization target fails
 *   - gig_platform_operators: institutional powerful; currently benefit from flexible-employment regime; subject to compliance requirements; operate under sunset threat
 *   - transitioning_workforce (formal-track): organized moderate; workers moving into formalized status per the plan; primary beneficiaries of the state apparatus's coordination
 *   - informal_sector_workers_not_in_gig_economy: powerless; excluded from the gig-platform ecosystem; bear costs of extended informality; not explicitly named in the plan's beneficiaries
 *   - precariat_anchored_in_gig_economy: organized/powerless; workers who remain in the flexible regime beyond the transition window; victims if formalization target is missed
 *   - labor_standards_advocacy_organizations: observer/organized; monitor state performance on the plan; corroborate or challenge the reading's empirical claims
 *   - competing_jurisdictions_without_managed_transition: analytical; their labor-market outcomes provide comparative data on whether formalization requires active state management
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, 0.58).
domain_priors:suppression_score(flexible_employment_legitimacy__developmental_state_reading, 0.42).
domain_priors:theater_ratio(flexible_employment_legitimacy__developmental_state_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__developmental_state_reading, scaffold).
narrative_ontology:human_readable(flexible_employment_legitimacy__developmental_state_reading, "Flexible Employment as Managed Transition Toward Formalization").
narrative_ontology:topic_domain(flexible_employment_legitimacy__developmental_state_reading, "labor_economics/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__developmental_state_reading).
narrative_ontology:has_sunset_clause(flexible_employment_legitimacy__developmental_state_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__developmental_state_reading, '18e05c1a-02de-44ce-922b-8668f8dd0091').
narrative_ontology:cs_kernel_codification('18e05c1a-02de-44ce-922b-8668f8dd0091', formalized).
narrative_ontology:cs_authority_grounding('18e05c1a-02de-44ce-922b-8668f8dd0091', extraction).
narrative_ontology:cs_interpretation_layer_present('18e05c1a-02de-44ce-922b-8668f8dd0091').
narrative_ontology:cs_reading_relation('18e05c1a-02de-44ce-922b-8668f8dd0091', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('18e05c1a-02de-44ce-922b-8668f8dd0091', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('18e05c1a-02de-44ce-922b-8668f8dd0091', foundational, flexible_employment_is_transitional_not_permanent).
narrative_ontology:cs_axiom_status(flexible_employment_is_transitional_not_permanent, holdable).
narrative_ontology:cs_axiom_grounding('18e05c1a-02de-44ce-922b-8668f8dd0091', flexible_employment_is_transitional_not_permanent, instrumental).
narrative_ontology:cs_axiom('18e05c1a-02de-44ce-922b-8668f8dd0091', foundational, state_managed_formalization_is_achievable_by_2027).
narrative_ontology:cs_axiom_status(state_managed_formalization_is_achievable_by_2027, holdable).
narrative_ontology:cs_axiom_grounding('18e05c1a-02de-44ce-922b-8668f8dd0091', state_managed_formalization_is_achievable_by_2027, empirically_contingent).
narrative_ontology:cs_reference_frame('18e05c1a-02de-44ce-922b-8668f8dd0091', managed_labor_market_transition_framework).
narrative_ontology:cs_drift_state('18e05c1a-02de-44ce-922b-8668f8dd0091', id_2026_mid_implementation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('18e05c1a-02de-44ce-922b-8668f8dd0091', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, developmental_state_apparatus).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, transitioning_workforce).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, informal_sector_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, precariat_anchored_in_gig_economy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, gig_platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, transitioning_workforce_formal_track).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, gig_platform_operators).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, informal_sector_workers_outside_gig).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the 12-point formalization plan; sets compliance standards for platforms; monitors worker transitions; expands benefits progressively to workers entering formal status; bears political cost if the 2027 target is missed. Controls the narrative frame that flexible employment is transitional, not permanent. Retains authority to revise the plan and adjust the timeline.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, developmental_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate under the flexible-employment regime's cost structure through 2027 (minimal benefits, schedule flexibility, labor-cost optimization). Subject to compliance monitoring and progressive regulation (12-point plan requirements). After 2027, transition to formal-employment model increases labor costs. Exit available via regulatory capture or offshore relocation, but both carry high political/operational cost.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, gig_platform_operators, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, gig_platform_operators, payer).

% Workers moving into formal employment per the state's transition plan. Receive progressive benefit expansion, wage improvements, and formal-employment protections. Bear near-term precarity but are credibly promised exit via formalization. Their labor-market outcomes are the empirical test of whether the developmental-state reading's claims are valid.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, transitioning_workforce_formal_track, beneficiary,
    organized, biographical, constrained, national).

% Workers in informal economy not connected to gig platforms (street vending, informal manufacturing, agricultural day labor, domestic service). Not explicitly named in the formalization plan's beneficiaries. Bear the cost of extended informality and may face increased regulatory pressure as state capacity for formal-employment enforcement improves. Their exclusion from the transition pathway is a structural feature of the developmental-state reading.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, informal_sector_workers_outside_gig, payer,
    powerless, biographical, trapped, national).

% Gig-economy workers who remain in the flexible regime after 2027 (either because formalization target was missed, or because their credentials/demographics exclude them from formal pathways). Victims if the constraint persists beyond its designed sunset. Have internalized precarity framing ('gig work is all available to me') which locks exit options even if external barriers are removed. Bear the regime's extraction indefinitely.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, precariat_anchored_in_gig_economy, payer,
    organized, biographical, identity_locked, national).

% Monitor state performance on the 12-point plan; corroborate or challenge claims about worker transitions and wage growth; provide external accountability mechanism. Excluded from direct plan governance but hold significant voice in public discourse about whether the developmental-state reading is being executed honestly. Their testimony is critical for falsifying the reading if plan execution fails.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, labor_standards_advocacy_organizations, observer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, labor_standards_advocacy_organizations, excluded).

% Other countries managing flexible employment under different readings (pure market-efficiency approach with no formalization target, or regulatory capture by platform extraction logic). Provide comparative data on labor-market outcomes under different frameworks. Their outcomes are evidence for or against the developmental-state reading's claim that managed transition is necessary.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, competing_policy_jurisdictions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__developmental_state_reading, developmental_state_apparatus).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__developmental_state_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages a transitional labor-market configuration where flexible/gig employment serves as a holding mechanism for workers entering formal employment. The state coordinates benefit expansion, platform compliance, worker transition pathways, and labor-market data collection so that workers can move from informal to formal status within a bounded timeline (2027). Without this coordination, workers would remain trapped in informal status indefinitely and platforms would resist formalization absent regulatory pressure.
% TRANSFER_FUNCTION: Moves income and protections from workers in the flexible regime (suppressed wages, minimal benefits) to the state apparatus (labor-market control and policy authority) and to gig platforms (labor-cost minimization) during the transition period (0–24 months). As formalization progresses, the transfer reverses: platforms begin bearing formal-employment costs (benefits, protections, predictable scheduling), and the state apparatus receives credit for delivering worker transitions.
% ABSENT_VOICES: Workers excluded from the formalization plan's beneficiary set (informal-sector workers outside gig economy, workers in remote areas where formal employment is unavailable) would object if they had standing in the plan's design. Their absence is structural to the developmental-state reading, which assumes formal employment is available and desirable — an assumption false in many labor markets. Also absent: gig-economy workers skeptical that formalization is achievable or desirable (those who prefer schedule flexibility to formal-employment rigidity), who would advocate for permanent flexible status rather than managed transition.
% DISAPPEARANCE_RATIONALE: If the developmental-state reading and its formalization machinery disappeared overnight, workers would face unmanaged precarity (no formalization pathway), gig platforms would optimize labor costs without regulatory constraint, and the labor-market would fragment into informal sub-economies. State capacity for labor-market coordination would collapse, formal-employment protections would erode absent regulatory enforcement, and workers would have no credible exit from gig status.
% FOUNDING_PROBLEM: Rapid growth of flexible/gig employment outpaced state labor-market management capacity. Labor-market mismatch left workers vulnerable, state lacked real-time data to design policy, and informal work expanded because formal-employment growth did not keep pace. The developmental-state reading's response: treat flexible employment as a bounded, manageable transition state and deploy state coordination to move workers into formal status within a defined timeframe.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus attests the problem is live and the formalization plan will solve it. Labor standards advocacy organizations corroborate that labor-market mismatch was real (pre-plan empirical studies documented wage suppression and benefits fragmentation in gig sectors). However, competing-policy-jurisdictions' outcomes are mixed: some labor markets experienced informal-to-formal transitions without explicit state formalization plans, suggesting the founding problem may not require the developmental-state reading's specific solution. No external party fully corroborates the claim that the 12-point plan is necessary; independent researchers remain agnostic pending 2027 outcome data.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__developmental_state_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__developmental_state_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__developmental_state_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__developmental_state_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__developmental_state_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins at 0.48 (the flexible regime extracts value through wage suppression, benefits fragmentation, and work-schedule precarity) and rises slightly to 0.58 by time 12 as formalization is postponed and workers bear accumulating costs of delay. The projection then descends to 0.48 by time 24, representing the constraint's designed sunset: as formalization progresses and workers move to formal status, the extraction mechanism itself dissolves because the constraint only exists to govern the transitional regime. Theater ratio remains moderate (0.28–0.31 observed, then descends) because the state's coordination function is genuine (managing a real transition) but some enforcement energy goes to performing the transition's inevitability rather than defending it — the theater is lower than a Piton precisely because the underlying coordination function has not atrophied. Suppression begins at 0.45 and is projected to fall to 0.22 by the endpoint: workers remain in the flexible regime because the state's authority (the 12-point plan, the 2027 target, progressive benefit expansion) makes staying rational; suppression is lower than a Snare because exit is not blocked but rather postponed-with-promise. The measurement grid shares a single time axis: every metric is authored at every time point (0, 3, 6, 9, 12, 15, 18, 21, 24). First 12 points are observed (historical/contemporary data); points 15–24 are projected (based on the reading's expected trajectory under the formalization plan). This separation (observed/projected basis) allows downstream analysis to distinguish historical constraint behavior from the reading's forward claims about transition speed and worker trajectory.
 *
 * PERSPECTIVAL GAP:
 *   The developmental-state apparatus and the transitioning-workforce seats should compute Scaffold-compatible classifications from this reading's data (finite exit window, genuine coordination function, sunset clause). The precariat-anchored-in-gig-economy seat — those who will remain in the flexible regime past 2027 if formalization misses targets — should compute a divergent type: Snare if the plan fails (extraction without exit), or Tangled Rope if the plan partially succeeds but creates a two-tier labor market (formalized insiders coordinated by state policy, informal outsiders bearing the regime's residual extraction). Gig-platform-operators sit near the beneficiary end of directionality under this reading (they retain operational flexibility through 2027, then gradually face formal-employment requirements): d near 0.2–0.3. Workers in the transitioning pathway sit near coordination symmetry (they bear near-term cost, receive formalization benefit): d near 0.45–0.55. The precariat-anchored cohort sits near the full-target end (they bear the regime's extraction cost indefinitely if formalization fails): d near 0.75–0.85. The engine computes these divergences from the stakeholder structural data; the reading's authorization frame (developmental-state management) is what holds all seats within a single logical system despite their divergent experienced types.
 *
 * DIRECTIONALITY LOGIC:
 *   The developmental-state apparatus is the primary agenda-setter (power=institutional, exit=arbitrage via policy revision); it is the seat that benefits most from the constraint's existence (controls the transition narrative, retains labor-market authority, can defer formal-employment costs to employers). Directionality for the state apparatus: d ≈ 0.15 (beneficiary end — the apparatus designed the system and collects political capital from managing the transition). Gig-platform operators are powerful (power=institutional) with constrained exit (they operate under the regime's rules but can exit by refusing to comply, at high cost of regulatory consequence); they are net beneficiaries through 2027 (they retain cost-minimized labor), then payers after formalization. Directionality for platforms: d ≈ 0.35 (symmetric during transition, trending target-ward as formalization approaches). Transitioning workers are organized (power=organized, exit=constrained until formalization), net beneficiaries (they receive formal-employment pathway + gradual benefit expansion). Directionality: d ≈ 0.40 (symmetric-beneficiary, leaning beneficiary because the state apparatus's coordination is delivering real pathway). Precariat-anchored workers are powerless (power=powerless, exit=identity_locked — they have internalized precarity framing and believe informal work is all available to them) or trapped (geographic/credential barriers). Directionality: d ≈ 0.75 (target end — they bear extraction indefinitely if the formalization target is not met; they are the regime's victims). No override is needed because the derivation chain (beneficiary/victim declarations + exit structure) already yields accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem is 'rapid growth of informal and gig-economy work outpacing formal-employment capacity; labor-market mismatch leaves workers vulnerable and state apparatus lacks data for policy design.' This problem is LIVE only if formalization targets are achievable and the state apparatus maintains commitment to the 12-point plan. If the plan stalls or the 2027 target is missed, the founding problem shifts to DEAD (labor-market mismatch was never solved; the constraint persists as theatrical justification for indefinite informality). The disappearance verdict is WORLD_REARRANGES (if the developmental-state reading and its formalization machinery disappeared, workers would face unmanaged precarity, platforms would exploit labor more directly, and the labor-market would fragment into informal sub-economies without state coordination). The Mandatrophy question: Is this a genuine Scaffold (transitional coordination with sunset) or a degraded constraint masquerading as transition? The resolution hinges on three empirical gates: (1) State apparatus publishes and enforces the 12-point plan on-schedule. (2) Worker transitions to formal status track the 2027 target. (3) Theater ratio descends and extractiveness plateaus (as projected) — if both rise instead, the reading has collapsed into Piton or Snare and Mandatrophy is active (founding problem dead, constraint persists as performance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalization_target_achievability,
    'Can the state credibly execute the 2027 standardization target and 12-point plan, or is the sunset clause a theatrical commitment masking indefinite informality?',
    'Monitoring enforcement milestones: 2026 Q3 plan publication, 2026 Q4 first-tranche benefit expansion, 2027 Q1-Q2 compliance audits on signatory platforms. Measurable failure to hit three consecutive milestones triggers re-classification.',
    'If achievable, the constraint remains Scaffold (transitional coordination with real exit timeline). If unachievable, it reclassifies to Piton (theatrical maintenance of a defunct commitment) or Tangled Rope (extraction masquerading as transition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalization_target_achievability, empirical, 'Whether the sunset clause represents genuine staged transition or theatrical commitment masking indefinite informality.').

omega_variable(
    kernel_reading_stability,
    'Does the developmental-state reading remain the dominant authorization frame for flexible employment policy, or does the market-efficiency reading or precarity-extraction reading capture the actual institutional operating logic?',
    'Institutional audit: track actual enforcement activity (which constraints are enforced, which are performed but not defended, which are ignored), budget allocation to formalization machinery, and regulatory authority ratification of the 12-point plan. Dominant enforcement pattern reveals the true institutional frame.',
    'If market-efficiency reading dominates actual enforcement, the constraint reclassifies as Rope (genuine coordination). If precarity-extraction reading dominates, it reclassifies as Snare (extraction with formalization as cover). If developmental-state reading dominates enforcement while other readings remain in public discourse, the reading remains stable and Mandatrophy is resolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Whether the developmental-state reading remains institutionally authoritative or has been displaced by a sibling reading''s actual operating logic.').

omega_variable(
    wage_growth_mechanism_independence,
    'Is wage growth during the transition period driven by the state''s managed formalization pathway, or by independent labor-market tightening that would occur regardless of the constraint?',
    'Comparative analysis: wage growth trajectories in jurisdictions with explicit formalization targets vs. comparable jurisdictions without state coordination. If wage growth decouples from state machinery, the mechanism is independent.',
    'If independent, the developmental-state reading''s claim to managed transition is undermined — the state is riding external dynamics and claiming authorship. If coupled, the reading is supported. Either outcome affects whether workers are benefiting from state coordination or benefiting despite state marginalizing itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_growth_mechanism_independence, empirical, 'Whether wage growth during transition is driven by state formalization machinery or by independent labor-market dynamics.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.42) structural — external barriers to exit (legal restrictions, platform contract lock-in) — or internalized — workers have internalized precarity framing and believe formalization will not improve their situation?',
    'Post-exit survey: track workers who exited the flexible-employment regime (either to formal employment or to exit the labor market entirely). If suppression persists post-exit (workers report ongoing precarity framing despite changed circumstances), reclassify as partially internalized. If suppression drops sharply, it is structural.',
    'If structural, removing barriers (legal reform, contract renegotiation) suffices to raise exit options. If internalized, the constraint carries further institutional depth — belief-level coordination is required, not just external reform. Higher effective suppression if internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural (external barriers) or internalized (workers'' belief systems about their own precarity).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__developmental_state_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(flex_tr_t3, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 3, 0.29).
narrative_ontology:measurement(flex_tr_t6, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(flex_tr_t9, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 9, 0.31).
narrative_ontology:measurement(flex_tr_t12, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(flex_tr_t18, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(flex_tr_t21, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 21, 0.22).
narrative_ontology:measurement(flex_tr_t24, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(flex_be_t3, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(flex_be_t6, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(flex_be_t9, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 9, 0.56).
narrative_ontology:measurement(flex_be_t12, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(flex_be_t18, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 18, 0.57).
narrative_ontology:measurement(flex_be_t21, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 21, 0.53).
narrative_ontology:measurement(flex_be_t24, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 24, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(flex_su_t3, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 3, 0.44).
narrative_ontology:measurement(flex_su_t6, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 6, 0.43).
narrative_ontology:measurement(flex_su_t9, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 9, 0.42).
narrative_ontology:measurement(flex_su_t12, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(flex_su_t18, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 18, 0.36).
narrative_ontology:measurement(flex_su_t21, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 21, 0.3).
narrative_ontology:measurement(flex_su_t24, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 24, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__developmental_state_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__developmental_state_reading, 0.12).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, platform_labor_standard_enforcement).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, state_formalization_capacity_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the flexible-employment-legitimacy kernel; sibling readings market_efficiency_reading and precarity_extraction_reading instantiate the same kernel under different authority frames. All three readings share the referent (the standing institutional commitment to flexible/gig employment as permissible labor status) but author different ε values (developmental-state: 0.58, reflecting extraction-during-transition; market-efficiency: lower, reflecting coordination without extraction; precarity-extraction: higher, reflecting pure extraction with coordination cover). The three stories are linked by network.affects_constraints and by their shared cs_structure blocks. Do NOT attempt to force a single ε across readings — each reading's ε describes the standing arrangement (the kernel) as that reading sees it. Decomposition is structural, not observational: different readings are different constraints because their ε values are reading-indexed over a fixed referent (OQ-26 / OQ-258 compatibility).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
