% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Market as Natural Default (Lapsed Alternative Reading)
 *   domain: political_economy/ideology/economic_history
 *
 * SUMMARY:
 *   This constraint instantiates the lapsed_alternative_reading of the
 *   contested kernel market_as_natural_default. In this reading, the
 *   appearance that market allocation is the inevitable or natural default
 *   state of economic organization results not from active closure by
 *   beneficiaries, but from the historical forgetting of
 *   alternativesâcommons-based production, state planning, gift economies,
 *   feudal obligation, and mutual aid. The frame persists because the
 *   institutional memory of these alternatives has lapsed, not because an
 *   identifiable class invests in defending it. Analytical observers
 *   (heterodox historians, anthropologists) can recover these alternatives
 *   through archival and ethnographic research, but embedded market
 *   participants experience the constraint as an unchangeable background
 *   condition.
 *
 * KEY AGENTS:
 *   - heterodox_economic_historians: Analytical observers (analytical/arbitrage) who recover non-market alternatives through archival and comparative research; they sit outside the frame and can access exit via epistemic mobility.
 *   - embedded_market_agents: Targets from their own cognitive seat (moderate/identity_locked) who experience market allocation as common sense; they lack exit because the frame constitutes their economic imagination.
 *   - mainstream_economic_pedagogues: Diffuse epistemic reproducers (institutional/constrained) who transmit the frame through curricula; they are not beneficiaries of the constraint itself but unwitting carriers of the forgetting.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.12).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.08).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market as Natural Default (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology/economic_history").

domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, 'e6ec53d3-ad2d-47e1-bb71-42abe29df864').
narrative_ontology:cs_kernel_codification('e6ec53d3-ad2d-47e1-bb71-42abe29df864', implicit).
narrative_ontology:cs_authority_grounding('e6ec53d3-ad2d-47e1-bb71-42abe29df864', diffuse_epistemic).
narrative_ontology:cs_reading_relation('e6ec53d3-ad2d-47e1-bb71-42abe29df864', market_as_natural_default__beneficiary_maintained_reading, forecloses).
narrative_ontology:cs_reading_relation('e6ec53d3-ad2d-47e1-bb71-42abe29df864', market_as_natural_default__hybrid_amnesia_reading, forecloses).
narrative_ontology:cs_axiom('e6ec53d3-ad2d-47e1-bb71-42abe29df864', foundational, market_naturalization_from_forgetting).
narrative_ontology:cs_axiom_status(market_naturalization_from_forgetting, holdable).
narrative_ontology:cs_axiom_grounding('e6ec53d3-ad2d-47e1-bb71-42abe29df864', market_naturalization_from_forgetting, empirically_contingent).
narrative_ontology:cs_axiom('e6ec53d3-ad2d-47e1-bb71-42abe29df864', foundational, no_identifiable_beneficiary_class).
narrative_ontology:cs_axiom_status(no_identifiable_beneficiary_class, holdable).
narrative_ontology:cs_axiom_grounding('e6ec53d3-ad2d-47e1-bb71-42abe29df864', no_identifiable_beneficiary_class, empirically_contingent).
narrative_ontology:cs_reference_frame('e6ec53d3-ad2d-47e1-bb71-42abe29df864', lapsed_memory_equilibrium).
narrative_ontology:cs_drift_state('e6ec53d3-ad2d-47e1-bb71-42abe29df864', contemporary_political_economy, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('e6ec53d3-ad2d-47e1-bb71-42abe29df864', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No active coordination function; the constraint operates as a cognitive default rather than solving a collective-action problem. It is the absence of coordinated memory around non-market allocation systems.
% TRANSFER_FUNCTION: No active transfer of resources; the constraint operates by foreclosing imaginative and discursive access to non-market allocation mechanisms, not by moving value from one party to another.
% ABSENT_VOICES: Heterodox economic historians, economic anthropologists, and practitioners of surviving non-market systems (commons, gift economies, state-led coordination) are largely excluded from mainstream economic pedagogy and policy discourse. Their absence is structural, not incidental: the frame cannot acknowledge them without dissolving its own inevitability.
% DISAPPEARANCE_RATIONALE: If the naturalized frame vanished overnight, the imaginative and political space for non-market allocation would reopen, but existing market institutions possess material and network inertia independent of the frame. The reading claims the frame is constitutive of dominance, yet material structures might persist even if the ideological constraint dissolved.
% FOUNDING_PROBLEM: No deliberate founding problem; the constraint accumulated from the gradual erosion of counter-hegemonic institutional memory and the historiographic victory of market-centric narratives, not from a designed response to a coordination failure.
% FOUNDING_PROBLEM_CORROBORATION: Economic historian Karl Polanyi and contemporary economic anthropologists attest that pre-market and non-market allocation systems were historically substantive and effective; they corroborate from outside market-beneficiary circles that the 'markets as default' framing is a naturalization myth rather than a response to an unresolved founding problem.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, contested).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.12, 'kimi-k2.6', 'none', direct).

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
 *   The constraint is claimed as mountain because the reading asserts it persists without active enforcement or identifiable beneficiaries, operating as a cognitive default. However, accessibility_collapse is authored low (0.15) because understanding the constraintârecognizing market dominance as historical forgettingâactually restores access to alternatives rather than collapsing them further. Resistance is minimal (0.10) because the frame is invisible and undefended. Extractiveness is low (0.12) because no party captures rents from the naturalization itself. Theater ratio is near-zero (0.05) because there is no performative maintenance. The divergence between the mountain claim and the low accessibility_collapse is the measurement the corpus exists to take: it signals whether an apparently natural constraint is genuinely inevitability-shaped or merely amnesia-shaped.
 *
 * PERSPECTIVAL GAP:
 *   Analytical observers experience the constraint as a recoverable historical contingency with negligible extraction. Embedded market participants experience it as an unchangeable background conditionâeffectively a mountain from their seatâbecause their cognitive frame lacks access to the historical record. The engine computes this divergence from the structural data: embedded agents with identity-locked exit from the ideological frame experience higher effective extraction than analytical agents who can access counter-historical narratives, even though the base extractiveness of the constraint itself is uniformly low.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared, so directionality is structurally uniform across the constraint itself. All agents sit near symmetric (d â 0.5) with respect to this specific constraint because the frame extracts from no one and subsidizes no one; it merely forecloses imaginative alternatives. The effective extraction Ï is therefore damped to near-zero for all positions. Analytical observers with arbitrage-grade exit (access to heterodox archives) sit slightly toward the beneficiary end because they can leverage recovered alternatives for intellectual or institutional advantage, but this is a second-order effect not captured in the base constraint structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by isolating the naturalization frame from the market institution. The market institution, considered as a separate constraint, may be a tangled_rope or snare; this story isolates the ideological belief that markets are the default. That belief has no mandate to resolve and was never a scaffold. It is not a coordination mechanism that has atrophied into a piton, but a cognitive baseline that persists through absence of counter-memory. Mandatrophy in the conventional sense does not apply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_recovery_feasibility,
    'Can institutional alternatives to market allocation be recovered once they have been forgotten, or does the lapse create irreversible path dependence in organizational capabilities?',
    'Comparative historical analysis of revivals: cases where non-market allocation systems were successfully reconstructed after long periods of disuse (e.g., commons governance, cooperative coordination) versus cases where institutional memory proved irrecoverable.',
    'If recovery is generally feasible, the constraint remains a low-extraction mountain of ideology; if forgetting is functionally irreversible, the constraint operates more like a scaffold that has become a piton, and the classification should shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_recovery_feasibility, empirical, 'Whether lapsed alternatives are recoverable or path-dependent').

omega_variable(
    passive_vs_diffuse_maintenance,
    'Is the naturalization of market dominance truly passive, or does it rely on diffuse epistemic maintenance (textbooks, professional socialization, media discourse) that functions as distributed suppression of alternatives?',
    'Content analysis of economic pedagogy and policy discourse: measure the frequency and framing of non-market alternatives. If suppression is diffuse but active, the directionality for pedagogical agents shifts toward agenda_setter and the effective extraction for students rises.',
    'If diffuse maintenance is substantial, the constraint is not a pure mountain of forgetting but a tangled_rope of distributed coordination and extraction; this would require declaring beneficiaries (epistemic institutions) and victims (learners locked into the frame).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passive_vs_diffuse_maintenance, conceptual, 'Whether naturalization is passive forgetting or diffuse active suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(mark_tr_t25, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 25, 0.04).
narrative_ontology:measurement(mark_tr_t50, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(mark_be_t25, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 25, 0.11).
narrative_ontology:measurement(mark_be_t50, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 50, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(market_as_natural_default__lapsed_alternative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% The market_as_natural_default kernel decomposes into three structurally distinct constraints per the epsilon-invariance principle. The lapsed_alternative_reading carries low extractiveness (Îµ â 0.12) and no beneficiaries. The beneficiary_maintained_reading posits active incumbent defense and higher extraction. The hybrid_amnesia_reading posits amnesia enabling subsequent capture. Each reading requires its own constraint story with distinct metrics, stakeholders, and classification. They are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
