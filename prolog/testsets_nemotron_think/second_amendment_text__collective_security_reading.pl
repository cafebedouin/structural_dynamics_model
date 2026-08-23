% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment Collective Security Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   The collective security reading of the Second Amendment interprets the
 *   militia clause ('A well regulated Militia, being necessary to the
 *   security of a free State') as conditioning the right to keep and bear
 *   arms on organized civic defense. Under this reading, the state may
 *   regulate arms — including licensing, permitting, and bans — to serve
 *   collective security. The constraint is the regulatory regime built on
 *   this interpretation: it coordinates collective defense through state
 *   authority while extracting compliance costs from gun owners. The
 *   claim/metric gap is deliberate: the reading is CLAIMED as tangled_rope
 *   (coordination + extraction) while the authored metrics describe a regime
 *   that has become increasingly extractive and suppressive over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.65).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.72).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment Collective Security Reading").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, '3144b8be-e95c-4d65-99b3-54b450368af6').
narrative_ontology:cs_kernel_codification('3144b8be-e95c-4d65-99b3-54b450368af6', fixed_text).
narrative_ontology:cs_authority_grounding('3144b8be-e95c-4d65-99b3-54b450368af6', lineage).
narrative_ontology:cs_interpretation_layer_present('3144b8be-e95c-4d65-99b3-54b450368af6').
narrative_ontology:cs_reading_relation('3144b8be-e95c-4d65-99b3-54b450368af6', second_amendment_text__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('3144b8be-e95c-4d65-99b3-54b450368af6', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('3144b8be-e95c-4d65-99b3-54b450368af6', foundational, militia_clause_conditions_right).
narrative_ontology:cs_axiom_status(militia_clause_conditions_right, holdable).
narrative_ontology:cs_axiom_grounding('3144b8be-e95c-4d65-99b3-54b450368af6', militia_clause_conditions_right, conventional).
narrative_ontology:cs_axiom('3144b8be-e95c-4d65-99b3-54b450368af6', foundational, state_may_regulate_for_collective_security).
narrative_ontology:cs_axiom_status(state_may_regulate_for_collective_security, holdable).
narrative_ontology:cs_axiom_grounding('3144b8be-e95c-4d65-99b3-54b450368af6', state_may_regulate_for_collective_security, conventional).
narrative_ontology:cs_reference_frame('3144b8be-e95c-4d65-99b3-54b450368af6', founding_era_militia_conditionality).
narrative_ontology:cs_drift_state('3144b8be-e95c-4d65-99b3-54b450368af6', contemporary_regulatory_state, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('3144b8be-e95c-4d65-99b3-54b450368af6', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, law_enforcement).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, public_safety_advocates).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, collective_security_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, militia_clause_conditionality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces firearms regulations (licensing, permits, bans) under the collective security reading; derives regulatory authority and revenue from the permitting regime; its legitimacy rests on the militia clause's conditionality.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Gains enforcement tools and statutory authority to regulate arms possession; benefits from reduced illegal arms traffic and clearer regulatory standards; operates the permitting and background-check infrastructure.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, law_enforcement, beneficiary,
    institutional, biographical, analytical, national).

% Must comply with licensing, registration, waiting periods, and bans on certain arms; bear financial and time costs of compliance; face criminal penalties for non-compliance; exit options limited to moving jurisdictions or surrendering arms.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    organized, biographical, constrained, national).

% Advocate for regulations that they believe reduce gun violence; benefit politically and socially from the regulatory framework; their preferred policies are enabled by the collective security reading.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, public_safety_advocates, beneficiary,
    organized, biographical, mobile, national).

% Provide historical analysis of the founding-era militia understanding; their work informs judicial interpretation but they hold no enforcement power; they are situated outside the regulatory apparatus.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, originalist_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes collective defense through state-regulated militia and arms control, solving the coordination problem of distributing defensive capacity while preventing private arms races and ensuring public safety.
% TRANSFER_FUNCTION: Moves regulatory authority and compliance costs from individual gun owners to the state regulatory apparatus; the state gains licensing revenue, enforcement discretion, and control over arms distribution, while gun owners lose unrestricted access and bear the costs of compliance.
% ABSENT_VOICES: Unorganized militia proponents and individual-right originalists who argue the right is personal and independent of militia service are structurally excluded from the regulatory framework; they would challenge the conditionality but are not seated in the permitting process.
% DISAPPEARANCE_RATIONALE: If the collective security reading were abandoned, the constitutional footing for the modern regulatory regime (licensing, background checks, bans) would collapse, leading to either deregulation or a shift to an individual-right framework that would restructure the entire firearms policy landscape.
% FOUNDING_PROBLEM: The founding problem was balancing the need for a well-regulated militia for collective defense against the risk of a standing army; the solution was to condition the right to arms on militia service and state regulation.
% FOUNDING_PROBLEM_CORROBORATION: Founding-era sources (Federalist Nos. 29, 46; state constitutions of 1776–1790) corroborate a militia-centered purpose, but modern originalist scholarship (e.g., Heller majority) disputes its continued applicability; the corroboration is split across historical and contemporary legal seats.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (founding era, minimal regulation) to 0.65 (modern permitting regime) as the regulatory apparatus expands. Suppression requirement rises similarly as enforcement machinery (background checks, registration, criminal penalties) hardens. Theater ratio grows because a portion of the regulatory activity (e.g., certain licensing delays, cosmetic feature bans) performs the appearance of collective security without measurable safety impact. Accessibility collapse is high because once the regulatory framework is understood, legal alternatives for unregulated ownership vanish. Resistance is substantial and organized (NRA, state-level sanctuary movements) but has not reversed the trend.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, the constraint is genuine coordination for collective security; from the gun owner's seat, it is enforced extraction. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The state regulatory apparatus is the structural beneficiary (d near 0.0): it collects fees, wields discretion, and its institutional existence depends on the regulatory regime. Law enforcement and public safety advocates are secondary beneficiaries (d ~0.2–0.3). Individual gun owners are the primary targets (d near 1.0): they bear the costs, face penalties, and have constrained exit. Originalist scholars are analytical observers (d=0.5). The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (militia-based collective defense) is contested: some argue it is obsolete (no militia system exists), others argue it persists in the National Guard and unorganized militia. The constraint persists despite the contested status because the regulatory apparatus has become self-justifying (inertial capture). This is a classic mandatrophy signal: the arrangement outlives its founding problem but is maintained by the beneficiaries it created.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the collective security reading a distinct constraint with its own ε, or a measurement variant of the same kernel?',
    'Apply ε-invariance test: if changing the observable (e.g., looking at licensing laws vs. judicial opinions) changes ε for this reading, then it is not a single constraint. The reading''s ε is assessed against the standing arrangement (the regulatory regime) from the reading''s own lights.',
    'If ε varies by observable, the reading must be decomposed into multiple constraints (e.g., one for licensing, one for bans). This would affect classification and network links.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the collective security reading constitutes a single ε-invariant constraint.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem (militia-based collective defense) become obsolete, and if so, does the constraint''s persistence constitute mandatrophy?',
    'Historical analysis of the militia system''s evolution (Dick Act 1903, National Guard, total army policy) and whether the regulatory regime still serves the original coordination function.',
    'If the founding problem is dead and the constraint persists, mandatrophy_resolved should be true and the constraint may be reclassified as piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Obsolescence of the militia-conditioned collective defense rationale.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, enforcement) or internalized (gun owners self-censoring due to regulatory complexity)?',
    'Post-exit suppression trajectory: if gun owners in low-regulation states still exhibit compliance behaviors conditioned by the federal framework, internalized suppression is present.',
    'If internalized, effective suppression is higher than the structural measure suggests, and the constraint''s extraction is amplified for identity-locked owners.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in firearms regulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 0, 233).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__collective_security_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(seco_tr_t50, second_amendment_text__collective_security_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(seco_tr_t100, second_amendment_text__collective_security_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement(seco_tr_t150, second_amendment_text__collective_security_reading, theater_ratio, 150, 0.22).
narrative_ontology:measurement(seco_tr_t200, second_amendment_text__collective_security_reading, theater_ratio, 200, 0.3).
narrative_ontology:measurement(seco_tr_t233, second_amendment_text__collective_security_reading, theater_ratio, 233, 0.38).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__collective_security_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(seco_be_t50, second_amendment_text__collective_security_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(seco_be_t100, second_amendment_text__collective_security_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement(seco_be_t150, second_amendment_text__collective_security_reading, base_extractiveness, 150, 0.45).
narrative_ontology:measurement(seco_be_t200, second_amendment_text__collective_security_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(seco_be_t233, second_amendment_text__collective_security_reading, base_extractiveness, 233, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__collective_security_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(seco_su_t50, second_amendment_text__collective_security_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(seco_su_t100, second_amendment_text__collective_security_reading, suppression_requirement, 100, 0.35).
narrative_ontology:measurement(seco_su_t150, second_amendment_text__collective_security_reading, suppression_requirement, 150, 0.5).
narrative_ontology:measurement(seco_su_t200, second_amendment_text__collective_security_reading, suppression_requirement, 200, 0.62).
narrative_ontology:measurement(seco_su_t233, second_amendment_text__collective_security_reading, suppression_requirement, 233, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__collective_security_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, firearms_regulation_regime).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, national_instant_criminal_background_check_system).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, state_preemption_laws).

% DUAL FORMULATION NOTE:
% This reading is one of three in the second_amendment_text constraint family. The individual_right_reading and originalist_civic_virtue_reading are sibling constraints with distinct ε and stakeholder structures. The collective security reading structurally influences the firearms_regulation_regime by providing its constitutional footing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__collective_security_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
