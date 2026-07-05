% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__hybrid_cascade_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__hybrid_cascade_reading
 *   human_readable: Hybrid Cascade Installation of State Commitments (Apex-to-Fringe with Required Local Ratification)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the hybrid cascade reading of the state
 *   commitment installation kernel: a new commitment (legal code, currency,
 *   calendar, or doctrine) is drafted and issued at the apex, but its
 *   stabilization is two-phase — the apex decree alone does not produce
 *   compliance; it must be picked up and locally re-legitimated by
 *   intermediary elites at the fringe before it functions as a lived
 *   commitment. This is distinct from a pure top-down imposition story (no
 *   fringe validation step is required for stabilization there) and distinct
 *   from an endogenous climb story (there the commitment originates at the
 *   fringe and ascends by demonstrated superiority, with no apex-first
 *   issuance). Only the hybrid cascade reading is modeled here; the siblings
 *   are separate constraints linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - central_state_apparatus: apex issuer, institutional/arbitrage — drafts and dispatches the commitment, collects long-run legibility gains
 *   - reform_aligned_elites: intermediary beneficiaries, powerful/mobile — translate and legitimate the commitment locally in exchange for preferment
 *   - peripheral_communities: primary payers, powerless/trapped — absorb disruption and enforcement with no direct forum to contest apex decisions
 *   - local_customary_authorities: displaced incumbents, moderate/constrained — pressured to co-legitimate or be sidelined
 *   - later_historians: analytical observers reconstructing whether cascade legitimacy was genuine or archivally manufactured
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.58).
domain_priors:suppression_score(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.52).
domain_priors:theater_ratio(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__hybrid_cascade_reading, "Hybrid Cascade Installation of State Commitments (Apex-to-Fringe with Required Local Ratification)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__hybrid_cascade_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__hybrid_cascade_reading, 'ae517423-97b6-4a8d-a671-82e225d7dee7').
narrative_ontology:cs_kernel_codification('ae517423-97b6-4a8d-a671-82e225d7dee7', distributed).
narrative_ontology:cs_authority_grounding('ae517423-97b6-4a8d-a671-82e225d7dee7', extraction).
narrative_ontology:cs_interpretation_layer_present('ae517423-97b6-4a8d-a671-82e225d7dee7').
narrative_ontology:cs_reading_relation('ae517423-97b6-4a8d-a671-82e225d7dee7', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae517423-97b6-4a8d-a671-82e225d7dee7', state_commitment_installation_mechanism__exogenous_imposition_reading, influences).
narrative_ontology:cs_axiom('ae517423-97b6-4a8d-a671-82e225d7dee7', foundational, stabilization_requires_fringe_ratification).
narrative_ontology:cs_axiom_status(stabilization_requires_fringe_ratification, holdable).
narrative_ontology:cs_axiom_grounding('ae517423-97b6-4a8d-a671-82e225d7dee7', stabilization_requires_fringe_ratification, empirically_contingent).
narrative_ontology:cs_axiom('ae517423-97b6-4a8d-a671-82e225d7dee7', foundational, apex_initiation_precedes_local_adaptation).
narrative_ontology:cs_axiom_status(apex_initiation_precedes_local_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('ae517423-97b6-4a8d-a671-82e225d7dee7', apex_initiation_precedes_local_adaptation, empirically_contingent).
narrative_ontology:cs_axiom('ae517423-97b6-4a8d-a671-82e225d7dee7', secondary, intermediary_legitimation_is_structurally_necessary_not_ceremonial).
narrative_ontology:cs_axiom_status(intermediary_legitimation_is_structurally_necessary_not_ceremonial, holdable).
narrative_ontology:cs_axiom_grounding('ae517423-97b6-4a8d-a671-82e225d7dee7', intermediary_legitimation_is_structurally_necessary_not_ceremonial, conventional).
narrative_ontology:cs_reference_frame('ae517423-97b6-4a8d-a671-82e225d7dee7', apex_initiated_two_phase_legitimation).
narrative_ontology:cs_drift_state('ae517423-97b6-4a8d-a671-82e225d7dee7', post_bureaucratic_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ae517423-97b6-4a8d-a671-82e225d7dee7', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, central_state_apparatus).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, reform_aligned_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, peripheral_communities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, local_customary_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and issues the new commitment (a law, calendar, currency, religious formula, or administrative code) at the capital and dispatches it outward through officials, garrisons, and courts. It cannot directly verify compliance at the periphery and depends on local actors reprocessing the commitment into locally intelligible form. It collects the long-run gains of standardization: tax legibility, conscription, unified jurisdiction.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, central_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Provincial notables, clergy, or literate intermediaries who adopt the apex commitment early and translate it into local idiom, gaining preferment, patronage, and enhanced local authority in exchange for legitimating the new order to their own communities.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, reform_aligned_elites, beneficiary,
    powerful, generational, mobile, national).

% Ordinary subjects at the fringe who must live under the new commitment once it is locally ratified. They bear the disruption cost of abandoning prior custom, absorb enforcement visits, and have no forum to contest the apex decision itself — only local intermediaries who filter it can be petitioned or resisted.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, peripheral_communities, payer,
    powerless, biographical, trapped, regional).

% Village elders, guild heads, or minor customary courts whose prior authority rested on the old commitment. They are pressured to either legitimate the new apex order (and be absorbed into the reform-aligned elite) or be sidelined; genuine holdouts lose standing and are gradually excluded from the arrangements they once ran.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, local_customary_authorities, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, local_customary_authorities, excluded).

% The judicial and archival apparatus that records which fringe validations succeeded and which failed, feeding back into future apex commitments. It treats successful local uptake as proof of the commitment's legitimacy, without distinguishing genuine consent from coerced or opportunistic ratification.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, royal_or_state_courts, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, royal_or_state_courts, observer).

% Study the surviving record of apex decrees and local adaptations to reconstruct whether legitimacy actually cascaded down or was manufactured through selective retention of compliant local voices.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, later_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__hybrid_cascade_reading, central_state_apparatus).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, legible commitment (law, currency, calendar, doctrine) across a jurisdiction too large and heterogeneous for organic bottom-up consensus, allowing large-scale administration, taxation, and defense to function under one standard.
% TRANSFER_FUNCTION: Moves interpretive authority and local legitimacy from customary fringe institutions to state-aligned intermediaries, and moves compliance costs and disruption from the apex (which decides) to peripheral communities (which must absorb the change).
% ABSENT_VOICES: Genuine local holdouts who reject the new commitment outright are rarely recorded except as suppressed revolts; their objections to the apex's right to impose in the first place are structurally excluded from the archival record that later validates the cascade as successful.
% DISAPPEARANCE_RATIONALE: If the installation mechanism vanished, apex commitments would have no pathway to local stabilization; either the state would revert to negotiated, slower endogenous adoption, or new commitments would fail to take root at all, fragmenting jurisdictional uniformity and returning greater autonomy to local customary authorities.
% FOUNDING_PROBLEM: A central authority needed to unify a fragmented, custom-governed territory quickly enough to tax, conscript, and adjudicate across it, faster than organic convergence from the fringes could produce.
% FOUNDING_PROBLEM_CORROBORATION: State chroniclers and reform-aligned elites attest the mechanism as necessary and successful integration. Independent evidence from suppressed local records, tax-revolt chronicles, and comparative historians outside the state's own archive suggests many cascades were narrower and more coerced than the surviving record shows — corroboration from outside the benefiting parties is partial and contested.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__hybrid_cascade_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises over the interval (0.35 to 0.58) as the apex's dependence on intermediary legitimation hardens into a routine mechanism that both administers and extracts — early on the commitment functions mostly as coordination (a genuine unification need), but as reform-aligned elites entrench themselves as gatekeepers, the mechanism increasingly transfers rents and authority to intermediaries at fringe communities' expense. Theater ratio climbs (0.18 to 0.40) as archival practice increasingly records successful ratification performatively (compliant testimonials, ceremonial adoption) rather than substantively verifying genuine local uptake. Suppression is moderate and rising modestly (0.40 to 0.52) reflecting the state's growing but still partial capacity to enforce compliance where local validation stalls.
 *
 * PERSPECTIVAL GAP:
 *   From the apex's seat, the mechanism looks like successful, largely voluntary integration — local validation is read as proof of legitimacy. From the peripheral seat, the same mechanism looks like coerced reclassification dressed as consent, since 'validation' by local elites was itself often a condition of those elites' continued standing rather than a free assessment by the community. The engine's per-seat computation should surface this divergence structurally rather than resolve it toward either seat's self-report.
 *
 * DIRECTIONALITY LOGIC:
 *   The central state apparatus sits near the beneficiary end: it authors the commitment and captures the long-run administrative gains, with arbitrage-grade exit (it can revise or reissue commitments at will). Reform-aligned elites are also beneficiaries, trading early adoption for enhanced local standing. Peripheral communities and local customary authorities are targets: they are trapped or constrained, bear the disruption and reclassification costs, and have no direct channel to contest the apex decision, only the intermediaries who filter it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unifying a fragmented jurisdiction fast enough for tax and defense needs) is genuinely contested as live or dead across cases: some cascades solve a real coordination failure that organic convergence could not solve in time; others persist as extraction machinery for reform-aligned elites long after the original unification problem was solved, with fringe validation becoming a ritual gatekeeping step rather than a genuine check. Classifying this reading as tangled_rope (not snare or rope outright) captures that both a real coordination function and a real, structurally required extraction channel coexist and cannot be disentangled without case-specific archival work — which the omega variables below route to.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_vs_manufactured_fringe_validation,
    'When the historical record shows local elites ''validating'' an apex commitment, is this evidence of genuine bottom-up legitimation, or is it a compliance performance produced by the same intermediaries whose standing depends on validating whatever the apex issues?',
    'Comparative archival analysis contrasting official validation records against independent local sources (dissenting chronicles, tax-revolt records, oral tradition) where they survive, to check whether validation correlates with intermediary self-interest or with independent local assessment.',
    'If validation is substantially manufactured, the coordination function claimed by this reading is weaker than authored and the constraint drifts toward snare; if validation reflects genuine local assessment in a meaningful share of cases, tangled_rope is the more defensible classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_manufactured_fringe_validation, empirical, 'Whether fringe validation reflects genuine consent or intermediary self-interest.').

omega_variable(
    kernel_reading_boundary_hybrid_vs_imposition,
    'In borderline historical cases, how do we distinguish a hybrid cascade (apex issues, fringe validation structurally required for stabilization) from exogenous imposition (apex issues and enforces directly, with local compliance being pure coercion rather than a distinct validation phase)?',
    'Examine whether the commitment persists and functions in cases where local validation demonstrably failed or was withheld for an extended period — if the commitment still stabilizes and functions without local legitimation, the case is better read as exogenous imposition, not hybrid cascade.',
    'Determines which sibling reading (this one or exogenous_imposition_reading) correctly classifies a given historical episode; misclassification would assign the wrong beneficiary/victim structure and χ trajectory to the case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_hybrid_vs_imposition, conceptual, 'Where the structural line falls between hybrid cascade and pure top-down imposition.').

omega_variable(
    endogenous_climb_counterfactual,
    'In cases claimed as hybrid cascade, could the same commitment have instead emerged via endogenous climb from the fringe, with the apex merely ratifying an already-ascendant local practice rather than initiating it?',
    'Trace the documentary chronology: does independent evidence of local practice predate the apex decree, suggesting the apex followed rather than led? A genuine hybrid cascade requires apex initiation to precede fringe adaptation, not merely coincide with it.',
    'If apex issuance in fact followed pre-existing fringe practice, the case belongs to the endogenous_climb_reading family instead, and this reading''s beneficiary structure (central_state_apparatus as primary agenda_setter) would be miscast.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endogenous_climb_counterfactual, conceptual, 'Whether apex initiation genuinely precedes fringe practice in this reading''s cases, versus retroactive apex ratification of prior local emergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__hybrid_cascade_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(stat_tr_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(stat_tr_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(stat_tr_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(stat_tr_t32, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stat_be_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(stat_be_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(stat_be_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(stat_be_t32, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(stat_su_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(stat_su_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(stat_su_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 24, 0.49).
narrative_ontology:measurement(stat_su_t32, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 32, 0.51).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__hybrid_cascade_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the state_commitment_installation_mechanism kernel, decomposed per the ε-invariance principle because the three readings assign structurally different beneficiary/victim sets, different required mechanisms (fringe validation as a necessary stabilization phase here; absent in exogenous_imposition_reading; apex issuance absent in endogenous_climb_reading), and different extraction trajectories. Endogenous_climb_reading is upstream in confidence terms for cases where local practice demonstrably predates apex ratification; exogenous_imposition_reading is the limiting case of this reading where fringe validation ceases to be structurally necessary for stabilization. All three are linked here and in each other's files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
