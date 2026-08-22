% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market Default as Lapsed Historical Memory
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint story instantiates the lapsed_alternative_reading of the
 *   market_as_natural_default kernel. The claim: market dominance as the
 *   default economic coordination mechanism results from historical
 *   forgetting of alternatives (mutualism, guild economies, commons-based
 *   provision, state-led planning) rather than active closure by
 *   beneficiaries. The naturalization is a D3 artifact — the constraint
 *   appears as natural law because the historical record of its contingency
 *   has lapsed from collective memory, not because beneficiaries actively
 *   suppress alternatives. Alternatives could be recovered through historical
 *   research; extractiveness is low (ε ≤ 0.15) because no identifiable class
 *   extracts rents from the forgetting itself. The
 *   beneficiary_maintained_reading and hybrid_amnesia_reading are sibling
 *   constraints, not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - economic_historians: Primary observers (analytical/powerless) — recover the forgotten alternatives through archival work
 *   - policy_elites: Secondary observers (institutional/moderate) — operate within the default but could deploy recovered alternatives
 *   - general_population: Implicit participants (powerless/constrained) — experience the default as natural, exit options limited by imaginative horizon
 *   - heterodox_economists: Excluded voices (organized/constrained) — maintain alternative frameworks but are marginalized in mainstream discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.12).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.18).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market Default as Lapsed Historical Memory").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies/economic_history").

domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '475a9fc1-1b32-40d7-91be-2b0ec226b6bd').
narrative_ontology:cs_kernel_codification('475a9fc1-1b32-40d7-91be-2b0ec226b6bd', distributed).
narrative_ontology:cs_authority_grounding('475a9fc1-1b32-40d7-91be-2b0ec226b6bd', diffuse_epistemic).
narrative_ontology:cs_reading_relation('475a9fc1-1b32-40d7-91be-2b0ec226b6bd', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('475a9fc1-1b32-40d7-91be-2b0ec226b6bd', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('475a9fc1-1b32-40d7-91be-2b0ec226b6bd', foundational, naturalization_as_epistemic_drift).
narrative_ontology:cs_axiom_status(naturalization_as_epistemic_drift, holdable).
narrative_ontology:cs_axiom_grounding('475a9fc1-1b32-40d7-91be-2b0ec226b6bd', naturalization_as_epistemic_drift, empirically_contingent).
narrative_ontology:cs_axiom('475a9fc1-1b32-40d7-91be-2b0ec226b6bd', foundational, alternatives_recoverable_through_research).
narrative_ontology:cs_axiom_status(alternatives_recoverable_through_research, holdable).
narrative_ontology:cs_axiom_grounding('475a9fc1-1b32-40d7-91be-2b0ec226b6bd', alternatives_recoverable_through_research, empirically_contingent).
narrative_ontology:cs_reference_frame('475a9fc1-1b32-40d7-91be-2b0ec226b6bd', pre_naturalization_pluralism).
narrative_ontology:cs_drift_state('475a9fc1-1b32-40d7-91be-2b0ec226b6bd', contemporary_neoliberal_hegemony, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('475a9fc1-1b32-40d7-91be-2b0ec226b6bd', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, policy_elites).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, general_population).
narrative_ontology:constraint_vindicates(market_as_natural_default__lapsed_alternative_reading, historical_contingency_of_market_institutions).
narrative_ontology:constraint_vindicates(market_as_natural_default__lapsed_alternative_reading, recoverability_of_alternative_economic_imaginaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recover forgotten economic alternatives through archival research and comparative history. Their work makes the constraint visible as contingent rather than natural. They face no professional penalty for this work and can move freely between frameworks — arbitrage-grade exit from the default.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economic_historians, observer,
    analytical, generational, arbitrage, global).

% Operate within the market default for legitimacy and technical feasibility. Could deploy recovered alternatives (e.g., commons governance, platform cooperatives) but face institutional inertia and legitimacy costs. Constrained exit: they can imagine alternatives but implementing them risks political capital.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, policy_elites, observer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__lapsed_alternative_reading, policy_elites, payer).

% Experience market coordination as the only imaginable form. Bear costs of market failures (inequality, externalities) without access to alternative frameworks. Exit is constrained by educational and media environments that naturalize the default — not by active suppression but by imaginative horizon.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, general_population, payer,
    powerless, biographical, constrained, global).

% Maintain alternative economic frameworks (Marxian, institutional, feminist, ecological) but are systematically excluded from mainstream journals, funding, and policy advisory roles. Their exclusion is structural — the constraint's naturalization makes their frameworks appear 'unscientific' — not the result of active suppression by a beneficiary class.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, heterodox_economists, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_as_natural_default__lapsed_alternative_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a default coordination mechanism for complex exchange at scale without requiring continuous conscious design or central planning — the 'invisible hand' as cognitive default.
% TRANSFER_FUNCTION: Does not transfer resources between identifiable groups. The constraint operates as a cognitive filter: it makes certain forms of coordination (market exchange) cognitively cheap and others (commons, planning, mutualism) cognitively expensive by rendering them unimaginable.
% ABSENT_VOICES: Historical actors who practiced and theorized alternatives (Owenites, guild mutualists, commons stewards, Soviet planners, Latin American cooperativists) — their voices are absent because the constraint operates through historical forgetting, not active exclusion. They cannot object because they are dead; their archives are the excluded seat.
% DISAPPEARANCE_RATIONALE: If the cognitive default vanished overnight, economic historians' recovered alternatives would enter policy discourse immediately. Policy elites would face legitimacy pressure to experiment with non-market forms. General population's imaginative horizon would expand. Heterodox economists would gain institutional access. The world rearranges because the constraint is the *forgetting itself* — its removal restores the historical menu.
% FOUNDING_PROBLEM: Coordinating complex division of labor and resource allocation at societal scale without a central planner — the classic socialist calculation problem in reverse: how to achieve coordination without hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (coordination at scale) is attested as live by mainstream economists (Hayek, 1945; modern mechanism design), by heterodox economists who argue markets solve it poorly (Polanyi, 1944; Gibson-Graham, 2006), and by historians of non-market coordination (Bollier, 2014; Linebaugh, 2008). Corroboration comes from outside any beneficiary set because this reading declares no beneficiaries — the problem's persistence is acknowledged across the ideological spectrum.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint operates as a cognitive default, not an extractive mechanism — no rent collection, no enforcement machinery. Suppression is low (0.18) because alternatives are not actively blocked; they are merely unimagined. Theater ratio (0.25) reflects the performative invocation of 'market efficiency' as justification, which exceeds the functional coordination need. Accessibility collapse (0.35) is moderate: alternatives exist in history but require specialist knowledge to access. Resistance (0.15) is low because the constraint meets little active opposition — it is the water fish swim in. The Mountain claim rests on emerges_naturally: true — the forgetting is an emergent property of historical transmission, not a designed erasure.
 *
 * PERSPECTIVAL GAP:
 *   From the analytical seat (economic historians), the constraint is a Mountain — a cognitive default with no extractive intent. From the excluded seat (heterodox economists), it may appear as a Snare — their alternatives are systematically marginalized. The engine computes this divergence from structural data: the analytical seat has arbitrage-grade exit (can leave the default by changing framework), the excluded seat has constrained exit (institutional barriers to publishing/funding). The claimed_type (mountain) reflects the analytical seat's structural truth; the engine will compute per-seat types independently.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims declared because the constraint's operation does not structurally transfer resources from one identifiable group to another. The 'forgetting' is a diffuse historical process, not a targeted extraction. Economic historians (analytical) sit at d ≈ 0.0 (beneficiary of the constraint's transparency to research). Policy elites (institutional) sit at d ≈ 0.3 (mild target — constrained by default but can access alternatives). General population (powerless) sits at d ≈ 0.6 (target — bears the imaginative constraint but without identifiable extractor). Heterodox economists (organized) sit at d ≈ 0.4 (mixed — marginalized but retain framework). Directionality derives from exit_options and power, not beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Q5/R5) was coordinating complex exchange at scale without central planning. That problem is live (founding_problem_status: live) — markets still solve it. But the naturalization of *this specific market form* as the only solution is mandatrophic: the arrangement (market-as-only-default) persists after its justification (solving coordination) has been met by other forms in history. The constraint prevents recognition that the founding problem admits multiple solutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the lapsed_alternative_reading a distinct structural constraint from the beneficiary_maintained_reading and hybrid_amnesia_reading of the same kernel?',
    'Compare ε, beneficiary/victim structures, and enforcement requirements across readings. If ε differs by >0.3 or beneficiary sets are disjoint, they are distinct constraints per ε-invariance principle.',
    'If readings are distinct constraints, each gets its own classification. If they collapse to one, the kernel is a single constraint with observer-dependent classification — which the framework forbids.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings instantiate three constraints or one.').

omega_variable(
    naturalness_vs_constructedness,
    'Is the market''s apparent naturalness a genuine Mountain (emergent order, no extractive intent) or a constructed default maintained by forgotten history?',
    'Historical recovery of suppressed alternatives: if documented alternatives existed at formation and were displaced without coercion, naturalness claim holds. If displacement involved active suppression, the Mountain claim fails.',
    'If constructed, the constraint is a false summit mountain — FSM signature triggers reclassification to tangled_rope. If genuine, Mountain certification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_vs_constructedness, empirical, 'Whether the constraint''s natural-law presentation reflects structural reality or historical erasure.').

omega_variable(
    beneficiary_absence_verification,
    'Are there truly no identifiable beneficiaries of the market-as-default arrangement, or have they become invisible through successful naturalization?',
    'Trace material interests: who gains from the constraint that alternatives are unimaginable? Incumbent firms, financial intermediaries, state capacity — map their interests against the constraint''s operation.',
    'If beneficiaries exist but are undeclared, the Mountain claim is a false summit. If genuinely none, the constraint is a rare non-extractive coordination default.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_absence_verification, empirical, 'Whether the ''no beneficiaries'' claim survives interest-tracing.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the lapsed_alternative_reading logically foreclose the beneficiary_maintained_reading within a single commitment framework?',
    'Test whether a single analytical framework can hold both ''naturalization is accidental memory loss'' and ''naturalization is actively defended by beneficiaries'' without contradiction.',
    'If forecloses, the readings are mutually exclusive structural claims. If coexists_with, they are competing but compatible framings held by different parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Structural relationship between this reading and the beneficiary_maintained_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 1750, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1750, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1750, 0.1).
narrative_ontology:measurement(mark_tr_t1800, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(mark_tr_t1850, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1850, 0.2).
narrative_ontology:measurement(mark_tr_t1900, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1900, 0.22).
narrative_ontology:measurement(mark_tr_t1950, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1950, 0.24).
narrative_ontology:measurement(mark_tr_t2000, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 2000, 0.25).

% Extraction over time
narrative_ontology:measurement(mark_be_t1750, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1750, 0.05).
narrative_ontology:measurement(mark_be_t1800, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1800, 0.08).
narrative_ontology:measurement(mark_be_t1850, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1850, 0.1).
narrative_ontology:measurement(mark_be_t1900, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(mark_be_t1950, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1950, 0.11).
narrative_ontology:measurement(mark_be_t2000, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 2000, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(market_as_natural_default__lapsed_alternative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__lapsed_alternative_reading, 0.1).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This constraint family (market_as_natural_default) decomposes the single colloquial claim 'markets are natural' into three structurally distinct constraints with different ε values, beneficiary structures, and enforcement requirements. The lapsed_alternative_reading has ε≈0.12, no beneficiaries, no enforcement. The beneficiary_maintained_reading has ε≈0.6+, identifiable beneficiaries (incumbents), active enforcement. The hybrid_amnesia_reading has ε≈0.35, transitional beneficiary structure, partial enforcement. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
