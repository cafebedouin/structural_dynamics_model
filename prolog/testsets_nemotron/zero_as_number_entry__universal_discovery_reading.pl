% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__universal_discovery_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_as_number_entry__universal_discovery_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: zero_as_number_entry__universal_discovery_reading
 *   human_readable: Zero as Number — Universal Discovery Reading
 *   domain: philosophy_of_mathematics/conceptual_history
 *
 * SUMMARY:
 *   This constraint story represents the universal_discovery_reading of the
 *   zero_as_number_entry kernel. It asserts that zero-as-number is a timeless
 *   mathematical necessity — a logical consequence of positional notation and
 *   arithmetic operations — rather than a contingent cultural invention.
 *   Indian mathematicians (Brahmagupta, 7th c.) were the first to explicitly
 *   formalize this necessity; Islamic mathematicians transmitted and refined
 *   the system; European mathematicians adopted it centuries later. The
 *   ontological status of zero is independent of who discovered it or when.
 *   The constraint has no victims and no extractive beneficiaries; its
 *   'coordination function' is the structural completion of arithmetic
 *   itself.
 *
 * KEY AGENTS:
 *   - indian_mathematicians_early: Primary discoverer-formalizers (organized/civilizational/arbitrage) — first to make the necessity explicit
 *   - islamic_mathematicians_transmission: Transmitters and refiners (organized/civilizational/arbitrage) — propagated the system without gatekeeping
 *   - european_mathematicians_late: Late adopters (organized/civilizational/arbitrage) — gained same structural benefits without disadvantage
 *   - mathematics_as_discipline: Universal beneficiary (non-agent/analytical) — the abstract discipline gains structural completion
 *   - greek_arithmetical_tradition: Historically excluded by own framework (organized/civilizational/constrained) — conceptual barrier, not suppression
 *   - contemporary_philosopher_of_mathematics: Analytical observer (analytical/biographical/analytical) — classifies the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__universal_discovery_reading, 0.02).
domain_priors:suppression_score(zero_as_number_entry__universal_discovery_reading, 0.01).
domain_priors:theater_ratio(zero_as_number_entry__universal_discovery_reading, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__universal_discovery_reading, mountain).
narrative_ontology:human_readable(zero_as_number_entry__universal_discovery_reading, "Zero as Number — Universal Discovery Reading").
narrative_ontology:topic_domain(zero_as_number_entry__universal_discovery_reading, "philosophy_of_mathematics/conceptual_history").

domain_priors:emerges_naturally(zero_as_number_entry__universal_discovery_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__universal_discovery_reading, 'e0390ec2-e281-46f0-a877-1615aa26ffb9').
narrative_ontology:cs_kernel_codification('e0390ec2-e281-46f0-a877-1615aa26ffb9', formalized).
narrative_ontology:cs_authority_grounding('e0390ec2-e281-46f0-a877-1615aa26ffb9', expertise).
narrative_ontology:cs_reading_relation('e0390ec2-e281-46f0-a877-1615aa26ffb9', zero_as_number_entry__contingent_thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0390ec2-e281-46f0-a877-1615aa26ffb9', zero_as_number_entry__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('e0390ec2-e281-46f0-a877-1615aa26ffb9', foundational, mathematical_objects_are_discovered_not_invented).
narrative_ontology:cs_axiom_status(mathematical_objects_are_discovered_not_invented, holdable).
narrative_ontology:cs_axiom_grounding('e0390ec2-e281-46f0-a877-1615aa26ffb9', mathematical_objects_are_discovered_not_invented, deontological).
narrative_ontology:cs_axiom('e0390ec2-e281-46f0-a877-1615aa26ffb9', foundational, logical_consequence_is_ontologically_prior_to_human_recognition).
narrative_ontology:cs_axiom_status(logical_consequence_is_ontologically_prior_to_human_recognition, holdable).
narrative_ontology:cs_axiom_grounding('e0390ec2-e281-46f0-a877-1615aa26ffb9', logical_consequence_is_ontologically_prior_to_human_recognition, deontological).
narrative_ontology:cs_reference_frame('e0390ec2-e281-46f0-a877-1615aa26ffb9', timeless_mathematical_necessity).
narrative_ontology:cs_drift_state('e0390ec2-e281-46f0-a877-1615aa26ffb9', contemporary_formal_metamathematics, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e0390ec2-e281-46f0-a877-1615aa26ffb9', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__universal_discovery_reading, indian_mathematicians_early).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__universal_discovery_reading, islamic_mathematicians_transmission).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__universal_discovery_reading, european_mathematicians_late).
narrative_ontology:constraint_vindicates(zero_as_number_entry__universal_discovery_reading, positional_notation_entails_zero).
narrative_ontology:constraint_vindicates(zero_as_number_entry__universal_discovery_reading, arithmetic_closure_requires_additive_identity).
narrative_ontology:constraint_vindicates(zero_as_number_entry__universal_discovery_reading, mathematical_truth_is_ontologically_independent_of_discoverer).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developed the first fully explicit symbolic zero and decimal positional system (Brahmasphutasiddhanta, 7th c.). Gained a powerful computational tool; no extractive cost was imposed on others by their discovery. Their priority is historical fact, not a source of ongoing rent.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, indian_mathematicians_early, beneficiary,
    organized, civilizational, arbitrage, regional).

% Received, refined, and transmitted the Indian system (al-Khwarizmi, al-Kindi, 9th c.). Benefited from an enhanced computational framework; acted as a conduit, not a gatekeeper extracting tolls.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, islamic_mathematicians_transmission, beneficiary,
    organized, civilizational, arbitrage, continental).

% Adopted the Hindu-Arabic system via translations (Fibonacci, 13th c.; later widespread). Gained the same computational advantages; their later arrival conferred no disadvantage on the structure of mathematics itself.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, european_mathematicians_late, beneficiary,
    organized, civilizational, arbitrage, continental).

% The abstract discipline itself is the primary beneficiary: zero completes the additive group structure of integers, enables algebra, calculus, and modern computation. No agent loses from this completion; the benefit is universal and non-rival.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, mathematics_as_discipline, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(zero_as_number_entry__universal_discovery_reading, mathematics_as_discipline).

% Lacked a symbol for zero as number due to philosophical commitment to number-as-magnitude and geometric ontology. This was a conceptual barrier, not a suppression by others. They did not object to zero — the framework could not yet formulate it.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, greek_arithmetical_tradition, excluded,
    organized, civilizational, constrained, regional).

% Analyzes the ontological status of zero: whether it is a discovered necessity (this reading) or a contingent conceptual achievement (sibling readings). Has no stake in priority disputes; the classification follows from structural analysis of the constraint.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, contemporary_philosopher_of_mathematics, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Zero coordinates the entire edifice of modern mathematics by providing the additive identity that makes positional notation, algebra, and calculus possible. It solves the coordination problem of representing 'nothing' as a computable object, enabling a single universal arithmetic.
% TRANSFER_FUNCTION: No transfer occurs. The constraint is a logical necessity: once positional notation and arithmetic operations are adopted, zero is entailed. No value moves from any agent to another; all mathematical practitioners gain the same structural completion.
% ABSENT_VOICES: Pre-zero mathematical traditions (Greek, early Chinese rod numerals without zero-symbol) could not articulate the concept; they are absent not because they were silenced but because the conceptual vocabulary did not exist. No living agent is excluded from zero's benefits today.
% DISAPPEARANCE_RATIONALE: If zero-as-number vanished overnight, the entire structure of modern mathematics, science, engineering, and digital computation would collapse. Positional notation would lose its closure; algebra would lack additive inverses; calculus would lose its foundation. The world would rearrange catastrophically.
% FOUNDING_PROBLEM: The problem was not 'invented' — it is the logical requirement that any positional notation system with arithmetic operations must have an additive identity to be closed. Indian mathematicians were the first to explicitly formalize this necessity; the problem exists wherever arithmetic exists.
% FOUNDING_PROBLEM_CORROBORATION: The logical necessity of an additive identity in any group structure is corroborated by every mathematician working in abstract algebra, independent of cultural tradition. No beneficiary group disputes this; the corroboration comes from the internal logic of mathematics itself, not from any interested party.
narrative_ontology:disappearance_verdict(zero_as_number_entry__universal_discovery_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_as_number_entry__universal_discovery_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__universal_discovery_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-25',
    'no_scope_rebuild_nemotron+seed_rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(zero_as_number_entry__universal_discovery_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__universal_discovery_reading, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__universal_discovery_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, ExtMetricName, E),
    domain_priors:suppression_score(zero_as_number_entry__universal_discovery_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(zero_as_number_entry__universal_discovery_reading),
    narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(zero_as_number_entry__universal_discovery_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   All metrics are near-zero because the constraint is a logical necessity, not a human arrangement. extractiveness = 0.02 (only the trivial cost of learning a symbol); suppression = 0.01 (no enforcement, no alternatives to suppress); theater_ratio = 0.03 (no performative maintenance); accessibility_collapse = 0.95 (once the logical structure is seen, no alternative arithmetic without zero remains viable); resistance = 0.02 (only initial conceptual resistance from frameworks lacking the vocabulary, not active opposition). The claimed_type is mountain; the metrics are authored independently and support this classification.
 *
 * PERSPECTIVAL GAP:
 *   The contingent_thinkability_reading and hybrid_scaffolding_reading would compute higher extractiveness and suppression by treating the *transmission* of zero as the constraint (which involved gatekeeping, delay, and conceptual barriers). This reading treats the *mathematical object itself* as the constraint — which has no transmission contingency. The perspectival gap is exactly which object is held fixed: the abstract necessity (this reading) or the historical path to its adoption (sibling readings).
 *
 * DIRECTIONALITY LOGIC:
 *   Every mathematical practitioner is a beneficiary with directionality near 0.0: the constraint subsidizes all users equally by completing arithmetic. No agent is a target (d ≈ 1.0) because no one pays a cost to maintain zero-as-number beyond the universal cost of learning mathematics. The greek_arithmetical_tradition is excluded by its own conceptual framework, not by the constraint — their exit_options = constrained reflects historical path-dependence, not structural extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable — this constraint has no mandate that could atrophy. It is a logical necessity that persists because it is true, not because any institution maintains it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_vs_independent_discovery,
    'Did European adoption of zero-as-number occur via transmission from Islamic sources, independent rediscovery, or a hybrid path?',
    'Historical analysis of Fibonacci''s Liber Abaci, earlier European manuscript marginalia, and the timing of zero-symbol appearance in European commercial arithmetic.',
    'If purely independent, it strengthens the mountain claim (multiple discovery of a necessity). If transmitted, it does not weaken the mountain claim (transmission of a truth is not extraction), but it affects the historical narrative for sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_vs_independent_discovery, empirical, 'Whether the European path to zero was transmitted or independent.').

omega_variable(
    ontological_independence_of_mathematical_objects,
    'Is the ontological status of zero-as-number (as a logical consequence of arithmetic) independent of the historical path by which humans came to know it?',
    'Philosophical analysis of mathematical realism vs. constructivism; the constraint story itself embodies the realist position as a structural claim.',
    'If mathematical objects are mind-independent necessities, this reading''s mountain classification is structurally correct. If they are mind-dependent constructions, the contingent_thinkability_reading or hybrid_scaffolding_reading may better capture the constraint''s nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_independence_of_mathematical_objects, conceptual, 'Whether mathematical necessity is ontologically independent of human discovery history.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''zero_as_number_entry'' refer to the mathematical object (the additive identity in a positional system), the historical event of its first explicit formalization, or the cultural transmission of the concept?',
    'Disambiguate the kernel label into separate constraint stories (already done: this reading, contingent_thinkability_reading, hybrid_scaffolding_reading). The epsilon-invariance principle requires this decomposition.',
    'If the kernel is ambiguous, the three readings are not readings of one kernel but three distinct constraints. The current decomposition follows the BGS pattern: separate stories linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel label is a single stable referent or conflates multiple structural claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__universal_discovery_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_tr_t0, zero_as_number_entry__universal_discovery_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_tr_t300, zero_as_number_entry__universal_discovery_reading, theater_ratio, 300, 0.03).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_tr_t600, zero_as_number_entry__universal_discovery_reading, theater_ratio, 600, 0.03).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_tr_t900, zero_as_number_entry__universal_discovery_reading, theater_ratio, 900, 0.03).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_tr_t1200, zero_as_number_entry__universal_discovery_reading, theater_ratio, 1200, 0.03).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_tr_t1500, zero_as_number_entry__universal_discovery_reading, theater_ratio, 1500, 0.03).

% Extraction over time
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_be_t0, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_be_t300, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 300, 0.02).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_be_t600, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 600, 0.02).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_be_t900, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 900, 0.02).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_be_t1200, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 1200, 0.02).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_be_t1500, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 1500, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_su_t0, zero_as_number_entry__universal_discovery_reading, suppression_requirement, 0, 0.01).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_su_t300, zero_as_number_entry__universal_discovery_reading, suppression_requirement, 300, 0.01).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_su_t600, zero_as_number_entry__universal_discovery_reading, suppression_requirement, 600, 0.01).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_su_t900, zero_as_number_entry__universal_discovery_reading, suppression_requirement, 900, 0.01).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_su_t1200, zero_as_number_entry__universal_discovery_reading, suppression_requirement, 1200, 0.01).
narrative_ontology:measurement(zero_as_number_entry__universal_discovery_reading_su_t1500, zero_as_number_entry__universal_discovery_reading, suppression_requirement, 1500, 0.01).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__universal_discovery_reading, information_standard).
narrative_ontology:affects_constraint(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry__contingent_thinkability_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This reading (universal_discovery_reading) and its siblings form a constraint family decomposing the 'zero-as-number' label per the epsilon-invariance principle. The universal_discovery_reading treats the mathematical object as a mountain (ε ≈ 0). The contingent_thinkability_reading treats the *European epistemic access* as the constraint (substantial ε, likely snare or tangled_rope). The hybrid_scaffolding_reading treats the *conceptual scaffolding requirement* as the constraint (moderate ε, likely tangled_rope). Each has distinct beneficiaries, victims, and temporal dynamics. They are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
