% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__placeholder_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_mathematical_status__placeholder_reading, []).

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
 *   constraint_id: zero_mathematical_status__placeholder_reading
 *   human_readable: Zero as Notational Placeholder Without Arithmetic Status
 *   domain: history_of_mathematics/philosophy_of_mathematics/conceptual_history
 *
 * SUMMARY:
 *   This reading treats zero as a purely instrumental device in positional
 *   notation systems — a mark that enables place-value representation without
 *   itself participating in arithmetic operations. It emerged in Babylonian
 *   and Hellenistic astronomical calculation, was transmitted through Indian
 *   and Islamic mathematics as a computational convention, and persisted in
 *   European practical arithmetic well after Brahmagupta's arithmetic rules
 *   for zero were known. The constraint is the exclusion of zero from the
 *   domain of operands: zero appears in written numbers but does not enter
 *   addition, subtraction, multiplication, or division as a number. This
 *   yields intermediate extractiveness: practitioners gain massive
 *   computational efficiency from positional notation but lose the ability to
 *   treat zero uniformly in algebraic reasoning, creating a structural
 *   friction that persists until the number_reading fully displaces it.
 *
 * KEY AGENTS:
 *   - positional_notation_practitioners: Primary beneficiaries (organized/biographical) — gain computational throughput from place-value notation without needing arithmetic closure
 *   - computational_trade_networks: Beneficiaries (powerful/biographical) — merchants and tax administrators who rely on efficient calculation for commerce and state revenue
 *   - astronomical_calculators: Beneficiaries (organized/generational) — specialists who need compact representation of large numbers for planetary models
 *   - full_arithmetic_theorists: Victims (moderate/biographical) — mathematicians who seek unified algebraic structures but are blocked by zero's exclusion from operations
 *   - number_ontology_traditions: Victims (organized/civilizational) — philosophical schools that require coherent number ontology and find the placeholder approach incoherent
 *   - analytical_observer: Observer (analytical/civilizational/universal) — sees the full structural trade-off across the historical interval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__placeholder_reading, 0.42).
domain_priors:suppression_score(zero_mathematical_status__placeholder_reading, 0.58).
domain_priors:theater_ratio(zero_mathematical_status__placeholder_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__placeholder_reading, tangled_rope).
narrative_ontology:human_readable(zero_mathematical_status__placeholder_reading, "Zero as Notational Placeholder Without Arithmetic Status").
narrative_ontology:topic_domain(zero_mathematical_status__placeholder_reading, "history_of_mathematics/philosophy_of_mathematics/conceptual_history").

domain_priors:requires_active_enforcement(zero_mathematical_status__placeholder_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__placeholder_reading, '361532e5-e7f6-4084-804a-dab4faba658e').
narrative_ontology:cs_kernel_codification('361532e5-e7f6-4084-804a-dab4faba658e', distributed).
narrative_ontology:cs_authority_grounding('361532e5-e7f6-4084-804a-dab4faba658e', practice).
narrative_ontology:cs_reading_relation('361532e5-e7f6-4084-804a-dab4faba658e', zero_mathematical_status__number_reading, coexists_with).
narrative_ontology:cs_reading_relation('361532e5-e7f6-4084-804a-dab4faba658e', zero_mathematical_status__parmenidean_rejection, coexists_with).
narrative_ontology:cs_axiom('361532e5-e7f6-4084-804a-dab4faba658e', foundational, zero_is_notational_not_arithmetical).
narrative_ontology:cs_axiom_status(zero_is_notational_not_arithmetical, holdable).
narrative_ontology:cs_axiom_grounding('361532e5-e7f6-4084-804a-dab4faba658e', zero_is_notational_not_arithmetical, instrumental).
narrative_ontology:cs_axiom('361532e5-e7f6-4084-804a-dab4faba658e', secondary, positional_efficiency_suffices_for_practice).
narrative_ontology:cs_axiom_status(positional_efficiency_suffices_for_practice, holdable).
narrative_ontology:cs_axiom_grounding('361532e5-e7f6-4084-804a-dab4faba658e', positional_efficiency_suffices_for_practice, instrumental).
narrative_ontology:cs_reference_frame('361532e5-e7f6-4084-804a-dab4faba658e', positional_notation_with_placeholder_zero).
narrative_ontology:cs_drift_state('361532e5-e7f6-4084-804a-dab4faba658e', pre_algebraic_unification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('361532e5-e7f6-4084-804a-dab4faba658e', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__placeholder_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, positional_notation_practitioners).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, computational_trade_networks).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, astronomical_calculators).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, full_arithmetic_theorists).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, number_ontology_traditions).
narrative_ontology:constraint_vindicates(zero_mathematical_status__placeholder_reading, positional_efficiency_suffices_for_practice).
narrative_ontology:constraint_vindicates(zero_mathematical_status__placeholder_reading, arithmetic_closure_not_required_for_computation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scribes, calculators, and mathematicians who use positional notation daily for astronomy, surveying, and commerce. They gain enormous computational speed from the placeholder convention. Exit is mobile: they can adopt the number_reading when it becomes available in their textual tradition, but doing so requires retraining and textual revision.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, positional_notation_practitioners, beneficiary,
    organized, biographical, mobile, continental).

% Merchant networks, tax administrations, and state bureaus that depend on efficient large-number arithmetic for revenue and logistics. They benefit from the placeholder convention's speed. Exit is constrained: their computational infrastructure (abacus methods, written algorithms, training pipelines) is built around the placeholder convention; switching requires coordinated institutional change.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, computational_trade_networks, beneficiary,
    powerful, biographical, constrained, continental).

% Specialists who compute planetary positions, eclipses, and calendar cycles. They need compact representation of very large numbers. The placeholder convention is essential to their workflow. Exit is constrained: their entire corpus of tables and methods assumes the placeholder convention; the number_reading offers theoretical elegance but no immediate computational advantage for their specific tasks.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, astronomical_calculators, beneficiary,
    organized, generational, constrained, continental).

% Mathematicians working on algebraic structure, equation solving, and theoretical unification. They encounter the placeholder convention as a barrier: zero cannot be added, subtracted, multiplied, or divided like other numbers. They must maintain exceptional cases or develop parallel formalisms. Exit is constrained: their work is embedded in the same notational and pedagogical tradition; they cannot simply 'use a different system' without losing communication with the computational community.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, full_arithmetic_theorists, payer,
    moderate, biographical, constrained, continental).

% Philosophical and mathematical traditions (Pythagorean, Neoplatonic, certain Islamic and European scholastic lineages) that require a coherent ontology of number. The placeholder reading creates a bifurcated ontology: zero is a mark but not a number. This is experienced as a structural incoherence. Exit is identity_locked: the tradition's self-conception is bound to number ontology; abandoning the critique would dissolve the tradition's intellectual identity.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, number_ontology_traditions, payer,
    organized, civilizational, identity_locked, universal).

% Commentators, pedagogical lineages, and institutional curricula that reproduce the placeholder convention. They set the rules for what counts as legitimate arithmetic in their domain. They have arbitrage-grade exit: they can adopt the number_reading when it gains prestige (as in the 12th-13th century Latin translations) without losing authority — indeed, adopting it enhances their authority.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, textual_authorities, agenda_setter,
    institutional, generational, arbitrage, continental).

% The retrospective analytical seat that sees the full historical trajectory: the placeholder convention as a genuine coordination solution that became a constraint on theoretical development, and its eventual supersession by the number_reading. Neither collects nor pays; observes the structural trade-offs across all seats.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables compact representation and efficient manipulation of arbitrarily large numbers in positional notation systems, solving the computational bottleneck of additive notation (Roman, Egyptian, etc.) for trade, astronomy, and administration.
% TRANSFER_FUNCTION: Moves theoretical coherence (zero's exclusion from arithmetic operations) from theorists and ontologists to practitioners and administrators, who gain computational throughput. The transfer is not monetary but cognitive: theorists pay in exceptional cases and bifurcated formalisms; practitioners collect in speed and reliability of calculation.
% ABSENT_VOICES: The number_reading proponents (Brahmagupta's lineage and their successors) are present in the broader network but excluded from the specific institutional domains where the placeholder convention holds sway (European practical arithmetic, certain Islamic computational schools). The parmenidean_rejection voices are excluded from mathematical practice entirely — their objection is philosophical, not computational, so they have no seat in the calculational arrangement.
% DISAPPEARANCE_RATIONALE: If the placeholder constraint vanished overnight (i.e., zero were universally admitted to arithmetic operations), computational practice would initially disrupt — algorithms and pedagogical texts would need revision — but within a generation the number_reading would stabilize as the new standard, eliminating the theoretical friction. The world rearranges because the constraint actively structures the computational/pedagogical infrastructure.
% FOUNDING_PROBLEM: How to represent and calculate with large numbers efficiently without a fully developed theory of zero as a number. Additive notation systems (Roman, Egyptian, Greek alphabetic) made multiplication and division of large numbers extremely laborious; positional notation with a placeholder symbol solved this without requiring the ontological commitment to zero as a number.
% FOUNDING_PROBLEM_CORROBORATION: The placeholder reading's proponents (computational practitioners, textual authorities in the practical arithmetic tradition) attest the founding problem remains live: they argue that for calculation, the placeholder suffices and the number_reading adds unnecessary complexity. The number_reading proponents (algebraic theorists, later European mathematicians from Fibonacci onward) and external historians of mathematics attest the founding problem is substantially solved: positional notation's efficiency is fully compatible with zero's arithmetic status, and the placeholder constraint now only obstructs theoretical unification.
narrative_ontology:disappearance_verdict(zero_mathematical_status__placeholder_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__placeholder_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__placeholder_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(zero_mathematical_status__placeholder_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__placeholder_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_mathematical_status__placeholder_reading_tests).
:- end_tests(zero_mathematical_status__placeholder_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the asymmetric trade-off: positional notation delivers enormous coordination value (computational efficiency across trade, astronomy, administration) while actively suppressing the development of full arithmetic closure. The suppression (0.58) is structural — zero is permitted in writing but banned from operations, a rule maintained by pedagogical tradition and textual authority rather than by mathematical necessity. Theater ratio is low (0.15) because the coordination function is genuine and dominant for most of the interval; the performative element grows only late when the number_reading offers a visibly superior framework but institutional inertia maintains the placeholder convention. Accessibility collapse is moderate (0.35): alternatives (full arithmetic with zero) exist and are known in parts of the network but are not adopted because they require restructuring the entire computational pedagogy. Resistance (0.48) comes from theorists who encounter the limitation in algebraic work and from ontological traditions that reject the bifurcation.
 *
 * PERSPECTIVAL GAP:
 *   From the practitioner seats (positional_notation_practitioners, computational_trade_networks, astronomical_calculators) the constraint appears as a rope — genuine coordination with minor friction. From the theorist seats (full_arithmetic_theorists, number_ontology_traditions) it appears as a tangled_rope — coordination mixed with extraction, because the suppression of zero's arithmetic status blocks theoretical unification. The engine computes this seat divergence from the power/exit/scope data: practitioners have institutional power and mobile exit within their computational domains; theorists have moderate power but constrained exit because their work depends on the same notational infrastructure they critique.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the computational users who extract efficiency gains from positional notation without paying the theoretical cost of arithmetic incompleteness. Victims are the theorists and ontologists who bear the cost of a bifurcated system — they must either work within the limitation or maintain a parallel formalism. The agenda_setters are the textual authorities (commentators, pedagogical lineages) who reproduce the convention. Directionality derives from this structure: practitioners have low d (beneficiaries), theorists have high d (targets of suppression), authorities sit near symmetric but with institutional power to maintain the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The placeholder reading solved a genuine founding problem: how to represent large numbers compactly for calculation without a full theory of zero as number. That problem was live for over a millennium. By the later interval the founding problem is substantially solved by the number_reading, but the placeholder constraint persists through institutional inertia in pedagogical texts and practical arithmetic traditions — a classic mandatrophy pattern where the coordination function has been superseded but the constraint remains. The extraction does not disappear; it shifts from 'necessary friction' to 'inertial drag' on algebraic development.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    placeholder_vs_arithmetic_boundary,
    'Is the exclusion of zero from arithmetic operations a coherent theoretical choice or an artifact of notational conservatism?',
    'Trace the pedagogical transmission: if commentators explicitly articulate a rationale for the exclusion (e.g., ''zero has no magnitude''), it is a theoretical choice; if they simply omit zero from operation tables without comment, it is conservatism.',
    'If theoretical, the constraint has lower extraction (the boundary is justified); if conservative, the extraction is higher (the boundary is maintained by inertia). Affects classification toward tangled_rope vs. scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(placeholder_vs_arithmetic_boundary, conceptual, 'Whether the placeholder/arithmetic boundary is principled or inertial').

omega_variable(
    transmission_path_ambiguity,
    'Did the placeholder convention spread because it was computationally superior, or because it was transmitted by authoritative texts that happened to carry it?',
    'Compare adoption rates in regions with vs. without direct textual transmission from the Indian/Islamic sources; control for computational demand.',
    'If transmission-driven, the constraint''s persistence is more extractive (path dependence); if demand-driven, more coordinative. Affects the mandatrophy timeline.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transmission_path_ambiguity, empirical, 'Causal pathway of the placeholder convention''s spread').

omega_variable(
    kernel_reading_boundary,
    'Does the placeholder reading genuinely foreclose the number_reading, or do they coexist as complementary tools for different purposes?',
    'Examine whether any single mathematical tradition holds both readings simultaneously for different domains (e.g., placeholder in commerce, number_reading in algebra). If yes, they coexist; if no, the placeholder reading forecloses the number_reading within that tradition.',
    'Determines reading_relations: forecloses vs. coexists_with. If forecloses, the kernel has a structural fault line; if coexists_with, the kernel is a stable pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between placeholder and number readings within a single framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__placeholder_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_placeholder_tr_t0, zero_mathematical_status__placeholder_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(zero_placeholder_tr_t300, zero_mathematical_status__placeholder_reading, theater_ratio, 300, 0.08).
narrative_ontology:measurement(zero_placeholder_tr_t600, zero_mathematical_status__placeholder_reading, theater_ratio, 600, 0.12).
narrative_ontology:measurement(zero_placeholder_tr_t900, zero_mathematical_status__placeholder_reading, theater_ratio, 900, 0.14).
narrative_ontology:measurement(zero_placeholder_tr_t1200, zero_mathematical_status__placeholder_reading, theater_ratio, 1200, 0.15).
narrative_ontology:measurement(zero_placeholder_tr_t1500, zero_mathematical_status__placeholder_reading, theater_ratio, 1500, 0.15).

% Extraction over time
narrative_ontology:measurement(zero_placeholder_be_t0, zero_mathematical_status__placeholder_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(zero_placeholder_be_t300, zero_mathematical_status__placeholder_reading, base_extractiveness, 300, 0.32).
narrative_ontology:measurement(zero_placeholder_be_t600, zero_mathematical_status__placeholder_reading, base_extractiveness, 600, 0.38).
narrative_ontology:measurement(zero_placeholder_be_t900, zero_mathematical_status__placeholder_reading, base_extractiveness, 900, 0.42).
narrative_ontology:measurement(zero_placeholder_be_t1200, zero_mathematical_status__placeholder_reading, base_extractiveness, 1200, 0.45).
narrative_ontology:measurement(zero_placeholder_be_t1500, zero_mathematical_status__placeholder_reading, base_extractiveness, 1500, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(zero_placeholder_su_t0, zero_mathematical_status__placeholder_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(zero_placeholder_su_t300, zero_mathematical_status__placeholder_reading, suppression_requirement, 300, 0.45).
narrative_ontology:measurement(zero_placeholder_su_t600, zero_mathematical_status__placeholder_reading, suppression_requirement, 600, 0.52).
narrative_ontology:measurement(zero_placeholder_su_t900, zero_mathematical_status__placeholder_reading, suppression_requirement, 900, 0.58).
narrative_ontology:measurement(zero_placeholder_su_t1200, zero_mathematical_status__placeholder_reading, suppression_requirement, 1200, 0.55).
narrative_ontology:measurement(zero_placeholder_su_t1500, zero_mathematical_status__placeholder_reading, suppression_requirement, 1500, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__placeholder_reading, information_standard).
narrative_ontology:boltzmann_floor_override(zero_mathematical_status__placeholder_reading, 0.02).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__number_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__parmenidean_rejection).

% DUAL FORMULATION NOTE:
% This constraint is one member of the zero_mathematical_status kernel family. The three readings (placeholder, number, parmenidean) represent structurally distinct constraints on zero's status, linked by shared referent (the symbol '0' in mathematical practice) but different ε values and beneficiary/victim structures. The placeholder reading has intermediate extractiveness (0.42) — it coordinates positional notation efficiently but extracts theoretical coherence. The number_reading has lower extractiveness (full arithmetic closure) but higher initial suppression (requires ontological acceptance). The parmenidean_rejection has near-zero extractiveness but maximal suppression (excludes zero entirely).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zero_mathematical_status__placeholder_reading, organized, 0.2).
constraint_indexing:directionality_override(zero_mathematical_status__placeholder_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
