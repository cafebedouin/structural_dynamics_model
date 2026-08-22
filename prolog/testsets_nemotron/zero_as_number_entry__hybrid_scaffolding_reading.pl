% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_as_number_entry__hybrid_scaffolding_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: zero_as_number_entry__hybrid_scaffolding_reading
 *   human_readable: Zero-as-Number Entry Constraint (Hybrid Scaffolding Reading)
 *   domain: history_of_mathematics/philosophy_of_mathematics/conceptual_history
 *
 * SUMMARY:
 *   This constraint story captures the hybrid scaffolding reading of the
 *   zero-as-number entry kernel. The core claim: zero was mathematically
 *   latent in the structure of positional notation (any base-b system has a
 *   'no digit here' position) but required specific conceptual scaffolding —
 *   a grammar of emptiness, a logic of negation, a metaphysics of void — to
 *   become operationally thinkable as a number that can be added, subtracted,
 *   multiplied. Indian philosophical traditions (Pāṇinian grammar's
 *   lopa/zero-morpheme, Nyāya's abhāva/absence as a padārtha, Buddhist
 *   śūnyatā as structured emptiness) provided this scaffolding by the 5th–7th
 *   centuries CE. European traditions, locked into Greek geometric algebra
 *   where number = ratio of magnitudes and void = non-being, could not
 *   operationalize zero until the merchant-computational practice (driven by
 *   the massive labor-saving of positional arithmetic) forced a pragmatic
 *   adoption that bypassed the philosophical blockade. Contact did not
 *   transmit a 'concept' but triggered recognition of a latent structure that
 *   the European framework had the notation for but not the scaffolding to
 *   activate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__hybrid_scaffolding_reading, 0.38).
domain_priors:suppression_score(zero_as_number_entry__hybrid_scaffolding_reading, 0.22).
domain_priors:theater_ratio(zero_as_number_entry__hybrid_scaffolding_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__hybrid_scaffolding_reading, rope).
narrative_ontology:human_readable(zero_as_number_entry__hybrid_scaffolding_reading, "Zero-as-Number Entry Constraint (Hybrid Scaffolding Reading)").
narrative_ontology:topic_domain(zero_as_number_entry__hybrid_scaffolding_reading, "history_of_mathematics/philosophy_of_mathematics/conceptual_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__hybrid_scaffolding_reading, 'eda7f78e-3098-4ced-adb2-371a828c3f4b').
narrative_ontology:cs_kernel_codification('eda7f78e-3098-4ced-adb2-371a828c3f4b', distributed).
narrative_ontology:cs_authority_grounding('eda7f78e-3098-4ced-adb2-371a828c3f4b', practice).
narrative_ontology:cs_interpretation_layer_present('eda7f78e-3098-4ced-adb2-371a828c3f4b').
narrative_ontology:cs_reading_relation('eda7f78e-3098-4ced-adb2-371a828c3f4b', zero_as_number_entry__contingent_thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('eda7f78e-3098-4ced-adb2-371a828c3f4b', zero_as_number_entry__universal_discovery_reading, influences).
narrative_ontology:cs_axiom('eda7f78e-3098-4ced-adb2-371a828c3f4b', foundational, scaffolding_contingency_of_operational_thinkability).
narrative_ontology:cs_axiom_status(scaffolding_contingency_of_operational_thinkability, holdable).
narrative_ontology:cs_axiom_grounding('eda7f78e-3098-4ced-adb2-371a828c3f4b', scaffolding_contingency_of_operational_thinkability, empirically_contingent).
narrative_ontology:cs_axiom('eda7f78e-3098-4ced-adb2-371a828c3f4b', foundational, mathematical_latency_in_positional_notation).
narrative_ontology:cs_axiom_status(mathematical_latency_in_positional_notation, holdable).
narrative_ontology:cs_axiom_grounding('eda7f78e-3098-4ced-adb2-371a828c3f4b', mathematical_latency_in_positional_notation, empirically_contingent).
narrative_ontology:cs_reference_frame('eda7f78e-3098-4ced-adb2-371a828c3f4b', pre_scaffolding_positional_notation).
narrative_ontology:cs_drift_state('eda7f78e-3098-4ced-adb2-371a828c3f4b', post_algebraic_closure, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('eda7f78e-3098-4ced-adb2-371a828c3f4b', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, islamic_mathematical_tradition).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_algebra_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, european_scholastic_tradition).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, european_scholastic_tradition).
narrative_ontology:constraint_vindicates(zero_as_number_entry__hybrid_scaffolding_reading, positional_notation_latent_structure).
narrative_ontology:constraint_vindicates(zero_as_number_entry__hybrid_scaffolding_reading, scaffolding_contingency_of_operational_thinkability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developed śūnya as a full algebraic operand within a grammatical-philosophical framework (Pāṇinian syntax, Nyāya logic, Buddhist śūnyatā) that treated emptiness as a structured placeholder. This scaffolding made zero operationally thinkable as a number centuries before Europe. The tradition collects the epistemic benefit of earlier formalization and the historical priority claim.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition, beneficiary,
    institutional, civilizational, analytical, continental).

% Transmitted, systematized, and extended the Hindu algebraic framework (al-Khwārizmī, al-Kindī, al-Bīrūnī). Served as the primary vector through which the operational concept reached Europe. Benefits from the coordination function of a shared algebraic vocabulary across the Islamic world and into Europe.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, islamic_mathematical_tradition, beneficiary,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__hybrid_scaffolding_reading, islamic_mathematical_tradition, agenda_setter).

% Operated within a geometric ontology where magnitude is continuous and number is ratio of magnitudes; void/empty cannot be a magnitude. The Aristotelian prohibition on actual infinity and the geometric interpretation of algebra (Euclid Books II, V, X) made zero-as-number structurally unthinkable. Bears the cost of delayed algebraic generality and the historical narrative of 'missing' zero.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_algebra_tradition, payer,
    institutional, civilizational, identity_locked, continental).

% Inherited the Greek geometric-algebraic framework via Arabic-Latin translations (12th century). Initially resisted zero-as-number as a 'Saracen' abstraction violating Aristotelian categories. Eventually adopted it through the merchant-banker computational practices (fibonacci, treatises on algorism) that bypassed philosophical objections. Pays the cost of conceptual transition but benefits from the resulting algebraic power.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, european_scholastic_tradition, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__hybrid_scaffolding_reading, european_scholastic_tradition, beneficiary).

% Drove the practical adoption of Hindu-Arabic numerals and zero in European commerce (13th–15th centuries) because the positional system with zero reduced computational labor dramatically. This practice operated below the philosophical radar and created the coordination pressure that eventually forced the scholarly tradition to accommodate zero-as-number.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, merchant_computational_practice, agenda_setter,
    organized, biographical, mobile, regional).

% Analyzes the constraint from outside the historical contest. Sees the structural latency of zero in positional notation, the scaffolding role of Indian philosophical grammar, the transmission pathway through Islamic mathematics, and the European identity-locked resistance. Does not collect rents or bear costs from the historical arrangement.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, modern_historiography_of_mathematics, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared algebraic vocabulary (positional notation + zero as operand) that allows computations to be specified, communicated, and verified across linguistic, cultural, and institutional boundaries — replacing the fragmented, geometry-bound, and abacus-dependent practices that could not scale.
% TRANSFER_FUNCTION: Moves the epistemic and practical burden of 'making zero thinkable' from the receiving tradition (which must restructure its ontology) to the originating tradition (which has already done the scaffolding work). The Hindu algebraic tradition invests the conceptual labor; the Greek geometric tradition pays the cost of incompatibility; the Islamic tradition intermediates and standardizes; the European tradition pays the transition cost but captures the algebraic generality.
% ABSENT_VOICES: Chinese rod-numeral tradition (which had a blank-space zero in positional calculation but did not algebraicize it as a number) and pre-Columbian Mesoamerican traditions (which had a calendrical zero but no algebraic tradition) are excluded from the standard transmission narrative. They would object to the Eurocentric framing of 'discovery' and 'transmission' but are not in the conversation.
% DISAPPEARANCE_RATIONALE: If the conceptual scaffolding for zero-as-number had never been constructed, positional notation would remain a computational trick without algebraic closure; polynomial algebra, calculus, and modern mathematics would not have emerged in their historical form. The world rearranges because the coordination vocabulary for higher algebra is missing.
% FOUNDING_PROBLEM: How to represent 'no quantity' as a quantity that participates in arithmetic operations (addition, subtraction, multiplication) without collapsing the distinction between magnitude and number, and without violating the ontological categories of the receiving tradition.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is dead in the sense that zero-as-number is now universally operational in mathematics — no living tradition treats it as conceptually problematic. Corroboration from outside the beneficiary set: the universal adoption of Hindu-Arabic numerals in all modern mathematical practice, including traditions (Chinese, Japanese, Korean) that had independent computational histories but adopted the zero-bearing system wholesale in the 19th–20th centuries.
narrative_ontology:disappearance_verdict(zero_as_number_entry__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_as_number_entry__hybrid_scaffolding_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__hybrid_scaffolding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(zero_as_number_entry__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__hybrid_scaffolding_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__hybrid_scaffolding_reading_tests).
:- end_tests(zero_as_number_entry__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint operates as a coordination problem: traditions with compatible scaffolding (Hindu, Islamic) gain earlier access to algebraic generality; traditions with incompatible scaffolding (Greek) pay a delay cost. The cost is not extracted by a beneficiary — it is the structural price of ontological incompatibility. Suppression is low (0.22) because no active enforcement maintains the barrier; the Greek tradition's resistance is internal (identity-locked to geometric ontology). Theater is low (0.15) because the constraint's function (enabling algebraic coordination) is genuine and the historical narrative of 'transmission' is not a cover story. Accessibility collapse is moderate (0.45) because alternatives (geometric algebra, abacus computation) remained viable for centuries alongside the new system. Resistance is moderate-high (0.55) because the European scholarly tradition actively resisted the concept for centuries on philosophical grounds.
 *
 * PERSPECTIVAL GAP:
 *   From the Hindu/Islamic beneficiary seats, the constraint looks like a rope: a genuine coordination problem solved by shared vocabulary, with the originating tradition having done the scaffolding work. From the Greek victim seat, it looks like a snare: an alien conceptual framework that invalidates their ontology and forces a costly restructuring they cannot easily make. From the European payer-beneficiary seat, it looks like a tangled rope: the coordination function is real (algebraic generality) but the transition extracts a high conceptual cost. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Hindu, Islamic traditions) have analytical exit — they can see the full structure and are not trapped by it. The Greek geometric tradition is identity-locked: its self-conception as the bearer of rigorous mathematics is fused with the geometric ontology that makes zero unthinkable; exit would require abandoning its identity as 'the mathematical tradition.' European scholastics are constrained: they have mobile exit at the practice level (merchants adopt zero) but constrained exit at the institutional level (universities resist). Merchant practice is mobile — it adopts whatever reduces computational labor. The observer seat is analytical by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (making zero thinkable) is dead — zero is now universal mathematical infrastructure. The constraint has resolved its mandatrophy: the scaffolding that was once a live conceptual barrier is now a historical artifact. The current arrangement (universal Hindu-Arabic notation) is a mountain-like coordination standard with negligible extraction. The historical constraint (the scaffolding barrier) no longer operates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latent_structure_vs_scaffolding_boundary,
    'Where exactly does the mathematical latency of zero in positional notation end and the conceptual scaffolding begin? Is the ''empty place'' in a positional numeral already a proto-zero, or is it merely a notational gap?',
    'Formal analysis of pre-zero positional systems (Babylonian sexagesimal, Chinese rod numerals, Mayan calendrical) to determine whether they treat the empty position as a value-operand or a spacing convention.',
    'If the empty position is already a proto-zero operand, the scaffolding requirement is smaller (contingent_thinkability_reading gains ground). If it is purely a spacing convention, the scaffolding requirement is larger (hybrid_scaffolding_reading strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latent_structure_vs_scaffolding_boundary, conceptual, 'Boundary between mathematical latency and conceptual scaffolding in positional notation.').

omega_variable(
    transmission_vs_recognition_mechanism,
    'Did the Islamic-to-European contact transmit the concept of zero-as-number, or did it trigger recognition of a latent structure already present in European commercial computation (abacus boards, counting tables with empty columns)?',
    'Philological and codicological analysis of 12th–13th century Latin algorismus texts: do they present zero as a novel import or as a formalization of an existing computational practice?',
    'If transmission, contingent_thinkability_reading is strengthened. If recognition-trigger, hybrid_scaffolding_reading is strengthened. Universal_discovery_reading predicts independent emergence regardless.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transmission_vs_recognition_mechanism, empirical, 'Mechanism of zero''s entry into European mathematics: transmission vs. recognition-trigger.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the zero_as_number_entry kernel best framed as (a) a concept-acquisition event, (b) a notational standardization event, or (c) an algebraic closure event? Different framings yield different beneficiary/victim structures and different ε values.',
    'Meta-historiographical analysis: trace how each framing structures the evidence selection and narrative in major histories of mathematics (Datta & Singh, Ifrah, Katz, Netz, Plofker).',
    'If (a), contingent_thinkability_reading dominates. If (b), hybrid_scaffolding_reading dominates (scaffolding = notational grammar). If (c), universal_discovery_reading dominates (closure is logical necessity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Framing under-determination of the kernel itself: concept vs. notation vs. algebraic closure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__hybrid_scaffolding_reading, 500, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_tr_t500, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 500, 0.05).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_tr_t700, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 700, 0.08).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_tr_t900, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_tr_t1100, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1100, 0.12).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_tr_t1300, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1300, 0.14).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_tr_t1500, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1500, 0.15).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_tr_t1700, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1700, 0.15).

% Extraction over time
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_be_t500, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 500, 0.15).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_be_t700, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 700, 0.2).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_be_t900, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 900, 0.25).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_be_t1100, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1100, 0.3).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_be_t1300, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1300, 0.35).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_be_t1500, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1500, 0.38).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_be_t1700, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1700, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_su_t500, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 500, 0.1).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_su_t700, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 700, 0.15).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_su_t900, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 900, 0.2).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_su_t1100, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 1100, 0.22).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_su_t1300, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 1300, 0.22).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_su_t1500, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 1500, 0.22).
narrative_ontology:measurement(zero_as_number_entry__hybrid_scaffolding_reading_su_t1700, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 1700, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__hybrid_scaffolding_reading, information_standard).
narrative_ontology:boltzmann_floor_override(zero_as_number_entry__hybrid_scaffolding_reading, 0.02).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry__contingent_thinkability_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry__universal_discovery_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, hindu_arabic_numeral_standardization).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, algebraic_closure_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the zero_as_number_entry kernel. The three readings form a constraint family linked by mutual affects_constraints edges. This reading (hybrid_scaffolding) emphasizes scaffolding contingency + mathematical latency; contingent_thinkability emphasizes transmission-dependence + metaphysical barrier; universal_discovery emphasizes logical necessity + accidental priority. Their ε values differ: hybrid ~0.38, contingent ~0.55 (higher extraction from barrier), universal ~0.15 (lower extraction, coordination only).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zero_as_number_entry__hybrid_scaffolding_reading, institutional, 0.25).
constraint_indexing:directionality_override(zero_as_number_entry__hybrid_scaffolding_reading, organized, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
