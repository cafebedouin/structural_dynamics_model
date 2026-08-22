% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Equality Bounded by 18th-Century Social Taxonomy (Originalist Reading)
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the originalist reading of the
 *   Declaration's 'all men are created equal' — the reading that bounds
 *   equality's scope by the 18th-century social taxonomy its authors
 *   inhabited. The reading claims the constraint is a Mountain of
 *   interpretive method (original public meaning fixes meaning at
 *   ratification); the authored metrics describe a Tangled Rope that
 *   coordinates legal interpretation around a fixed historical referent while
 *   extracting from groups the 1787 taxonomy excluded. The constraint
 *   requires active enforcement (judicial appointment, doctrinal policing,
 *   Federalist Society infrastructure) to maintain the boundary against
 *   universalist pressure. Beneficiaries are the founding elite's
 *   institutional and ideological descendants who inherit the fixed meaning
 *   as a bulwark against redistributive claims. Victims are the historically
 *   excluded groups whose exclusion the reading treats as historically
 *   settled rather than morally contestable. The claim/metric divergence is
 *   deliberate: the reading CLAIMS Mountain; the metrics describe Tangled
 *   Rope. The engine measures this divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.78).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.82).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Equality Bounded by 18th-Century Social Taxonomy (Originalist Reading)").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, '0b3774bc-1241-4314-b909-5a82c7961097').
narrative_ontology:cs_kernel_codification('0b3774bc-1241-4314-b909-5a82c7961097', fixed_text).
narrative_ontology:cs_authority_grounding('0b3774bc-1241-4314-b909-5a82c7961097', lineage).
narrative_ontology:cs_interpretation_layer_present('0b3774bc-1241-4314-b909-5a82c7961097').
narrative_ontology:cs_reading_relation('0b3774bc-1241-4314-b909-5a82c7961097', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b3774bc-1241-4314-b909-5a82c7961097', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('0b3774bc-1241-4314-b909-5a82c7961097', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('0b3774bc-1241-4314-b909-5a82c7961097', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('0b3774bc-1241-4314-b909-5a82c7961097', foundational, equality_scope_bounded_by_founding_taxonomy).
narrative_ontology:cs_axiom_status(equality_scope_bounded_by_founding_taxonomy, holdable).
narrative_ontology:cs_axiom_grounding('0b3774bc-1241-4314-b909-5a82c7961097', equality_scope_bounded_by_founding_taxonomy, conventional).
narrative_ontology:cs_reference_frame('0b3774bc-1241-4314-b909-5a82c7961097', founding_original_public_meaning).
narrative_ontology:cs_drift_state('0b3774bc-1241-4314-b909-5a82c7961097', post_reconstruction_amendments, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0b3774bc-1241-4314-b909-5a82c7961097', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_elite_descendants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, institutional_originalists).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, property_holding_classes).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, enslaved_persons_and_descendants).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women_excluded_from_franchise).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_nations).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, propertyless_males).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, property_holding_classes).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, original_public_meaning_doctrine).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, constitutional_fixity_at_ratification).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, limited_government_by_enumerated_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit the fixed constitutional meaning as a bulwark against redistributive equality claims. Their wealth, status, and institutional position are protected by the reading's barrier to inclusion. They can shift interpretive frameworks if originalism becomes inconvenient (arbitrage-grade exit).
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_elite_descendants, beneficiary,
    organized, generational, arbitrage, national).

% Produce, police, and enforce the originalist interpretive method through judicial appointments, law school hiring, think tanks, and judicial networks. They administer the constraint and benefit from its institutionalization. Their exit is mobile — they could adopt other methodologies but have invested careers in this one.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, institutional_originalists, agenda_setter,
    institutional, generational, mobile, national).

% Benefit from the fixed property-rights regime the originalist reading protects (regulatory takings, contract clause, due process). They also bear some constraint when originalism limits state power they might otherwise use. Exit is constrained — they need the constitutional order but could lobby for living constitutionalism on specific issues.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, property_holding_classes, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__originalist_reading, property_holding_classes, payer).

% Bear the founding exclusion the reading treats as historically settled. The originalist boundary fixes their ancestors' exclusion as the constitutional baseline, making every expansion of equality a departure from 'original meaning' rather than its fulfillment. Exit is identity-locked: the constraint defines their standing in the polity; leaving the polity is the only exit.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, enslaved_persons_and_descendants, payer,
    powerless, generational, identity_locked, national).

% The reading treats the 1787 taxonomy's exclusion of women from 'men' as determinative. Every feminist constitutional claim must overcome the originalist barrier that the founders did not intend women's equality. Exit is identity-locked — the constraint constitutes their political standing.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women_excluded_from_franchise, payer,
    powerless, generational, identity_locked, national).

% The founding taxonomy treated indigenous nations as foreign sovereigns or obstacles, not as 'men' within the equality claim. Originalist reading freezes this exclusion. Exit is trapped: they exist in a domestic-dependent sovereignty the reading helped construct, with no constitutional exit to full equality.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_nations, payer,
    powerless, generational, trapped, national).

% Formally included in 'men' but historically excluded from full political participation by property qualifications the founding taxonomy accepted. The reading's fixed meaning protects the property regime that facilitated their historical exclusion. Exit is constrained — they have formal standing but the constraint shapes the terms of their inclusion.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, propertyless_males, payer,
    moderate, biographical, constrained, national).

% Analyze the reading's doctrinal coherence, historical accuracy, and distributive effects from outside the constraint. They neither collect nor pay but their analysis shapes the legitimacy contest.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, legal_academy_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed interpretive anchor that prevents judicial improvisation and democratic majorities from rewriting constitutional meaning — coordinates legal interpretation around a single historical referent.
% TRANSFER_FUNCTION: Moves interpretive authority and protection from equality claims from historically excluded groups to founding-elite-descendant classes and their institutional representatives. The fixed meaning transfers the burden of proof: excluded groups must overcome the founding taxonomy; included groups need not justify their inclusion.
% ABSENT_VOICES: The enslaved persons at the founding, women denied the franchise, indigenous nations — the very groups the 1787 taxonomy excluded — are structurally absent from the originalist conversation. They cannot participate in recovering 'original public meaning' because that meaning was constructed without them. Their descendants remain excluded from the interpretive community that polices the boundary.
% DISAPPEARANCE_RATIONALE: If the originalist reading vanished overnight, the fixed historical barrier to equality expansion would collapse. Courts would adopt living constitutionalist or universalist frameworks within a generation. The Federalist Society infrastructure would lose its organizing methodology. Excluded groups' equality claims would face a lower doctrinal barrier. The conservative legal movement would reorganize around a different constraint.
% FOUNDING_PROBLEM: Legitimating a slaveholding republic's declaration of equality without extending equality to the enslaved, to women, to indigenous nations, or to the propertyless — the founding problem was reconciling 'all men are created equal' with a social taxonomy that denied equality to most humans in the polity.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is dead: slavery abolished (13th Amendment), citizenship and equal protection extended (14th), franchise expanded (15th, 19th, 24th, 26th Amendments). Historians outside the originalist tradition (Foner, Wood, Bailyn, Gordon-Reed) corroborate that the founding taxonomy was contested even in 1776-1787 and that the Constitution's amendments were designed to overcome it. Originalist beneficiaries contest this status, claiming the amendments did not alter the original meaning's scope — but no non-originalist scholar accepts this reading of the Reconstruction Amendments.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the reading's operation systematically insulates founding-era power distributions from equality claims — the fixed meaning functions as a barrier to inclusion that benefits descendant classes. Suppression (0.82) is high because maintaining the boundary requires active exclusion of rival readings (judicial selection, law school curricula, originalist infrastructure). Theater (0.45) is moderate: the historical scholarship is genuine, but a substantial fraction of originalist activity is boundary defense rather than interpretive discovery. Accessibility collapse (0.65) reflects that alternatives (universalist expansion) exist but are treated as illegitimate by the reading's internal logic. Resistance (0.58) is substantial: universalist and textualist-paradox readings contest the boundary across courts, academy, and public discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist seat, the constraint is coordination: a shared interpretive method that prevents judicial improvisation. From the excluded-group seat, the same structure is extraction: a method that freezes their exclusion at the founding taxonomy. The engine computes this divergence from beneficiary/victim declarations + power + exit. The reading's claim that it is 'bound by history' is the coordination story; the victim structure reveals the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding elite descendants and institutional originalists are structural beneficiaries (d near 0.0-0.2): they collect interpretive authority, institutional position, and protection from equality claims that would redistribute status or resources. Their exit is arbitrage-grade — they can shift to textualist or pragmatic frames if originalism becomes costly. Historically excluded groups are structural targets (d near 0.8-1.0): they bear the exclusion the reading treats as settled, with identity-locked exit (the constraint defines their standing in the polity). Property-holding classes are secondary beneficiaries (d ~0.3): they benefit from the fixed property-rights regime the reading protects but face some constraint from originalist limits on state power. Observers (legal academy, foreign courts) sit near analytical (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimating a slaveholding republic's equality claim without extending equality to the enslaved) is dead — slavery is abolished, the 13th/14th/15th Amendments exist. But the arrangement persists because the originalist reading repurposes the founding fixity as a general barrier to equality expansion. The constraint has undergone mandatrophy: its founding problem is dead, but the reading persists as a coordination mechanism for a conservative legal movement that benefits from the barrier. The theater ratio captures this repurposing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is this constraint one reading of a contested kernel (all_men_created_equal) or an independent constraint?',
    'Compare the ε and victim/beneficiary structure of this story against sibling readings (universalist_reading, textualist_paradox_reading). If ε and structural positions differ substantially across readings, they are distinct constraints linked by kernel_id.',
    'If kernel-structured, the three readings form a constraint family with network.affects_constraints linking them. Classification divergence between readings measures the kernel''s interpretive extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether this story instantiates a kernel reading and its structural relationship to sibling readings').

omega_variable(
    originalist_naturalness_ambiguity,
    'Does the originalist reading describe a genuine interpretive constraint (Mountain of legal method) or a constructed constraint that benefits identifiable agents (Tangled Rope/Snare)?',
    'Track whether originalist method produces outcomes that systematically benefit founding-elite-descendant classes across doctrinal domains, or whether it constrains all interpreters symmetrically regardless of outcome.',
    'If outcomes systematically benefit identifiable classes, the reading operates as extraction cover. If it symmetrically binds all interpreters, it is a genuine methodological Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_naturalness_ambiguity, empirical, 'Whether originalist interpretive method is a natural constraint or an extraction mechanism').

omega_variable(
    founder_intent_recoverability,
    'Is the ''founders'' intent'' that governs scope recoverable as a determinate historical fact, or is it an underdetermined construct that the reading''s beneficiaries fill with preferred content?',
    'Historiographical consensus on whether the historical record yields a single coherent intent on equality''s scope, or a contested field that originalist selection navigates.',
    'If intent is underdetermined, the reading''s claim to be bound by history is a cover for beneficiary-driven selection. If determinate, the constraint is genuinely historical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founder_intent_recoverability, conceptual, 'Whether the historical referent the reading claims to follow is structurally fixed or selector-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all_men_created_equal__originalist_reading_tr_t1787, all_men_created_equal__originalist_reading, theater_ratio, 1787, 0.6).
narrative_ontology:measurement(all_men_created_equal__originalist_reading_tr_t1865, all_men_created_equal__originalist_reading, theater_ratio, 1865, 0.3).
narrative_ontology:measurement(all_men_created_equal__originalist_reading_tr_t1896, all_men_created_equal__originalist_reading, theater_ratio, 1896, 0.55).
narrative_ontology:measurement(all_men_created_equal__originalist_reading_tr_t1954, all_men_created_equal__originalist_reading, theater_ratio, 1954, 0.4).
narrative_ontology:measurement(all_men_created_equal__originalist_reading_tr_t2024, all_men_created_equal__originalist_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(all_men_created_equal__originalist_reading_be_t1787, all_men_created_equal__originalist_reading, base_extractiveness, 1787, 0.85).
narrative_ontology:measurement(all_men_created_equal__originalist_reading_be_t1865, all_men_created_equal__originalist_reading, base_extractiveness, 1865, 0.75).
narrative_ontology:measurement(all_men_created_equal__originalist_reading_be_t1896, all_men_created_equal__originalist_reading, base_extractiveness, 1896, 0.8).
narrative_ontology:measurement(all_men_created_equal__originalist_reading_be_t1954, all_men_created_equal__originalist_reading, base_extractiveness, 1954, 0.72).
narrative_ontology:measurement(all_men_created_equal__originalist_reading_be_t2024, all_men_created_equal__originalist_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(all_men_created_equal__originalist_reading_su_t1787, all_men_created_equal__originalist_reading, suppression_requirement, 1787, 0.9).
narrative_ontology:measurement(all_men_created_equal__originalist_reading_su_t1865, all_men_created_equal__originalist_reading, suppression_requirement, 1865, 0.75).
narrative_ontology:measurement(all_men_created_equal__originalist_reading_su_t1896, all_men_created_equal__originalist_reading, suppression_requirement, 1896, 0.85).
narrative_ontology:measurement(all_men_created_equal__originalist_reading_su_t1954, all_men_created_equal__originalist_reading, suppression_requirement, 1954, 0.78).
narrative_ontology:measurement(all_men_created_equal__originalist_reading_su_t2024, all_men_created_equal__originalist_reading, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__originalist_reading, 0.08).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the 'all men are created equal' kernel. originalist_reading (this story): high ε, narrow victim set, Tangled Rope. universalist_reading: low ε, expanding victim set, Rope/Scaffold. textualist_paradox_reading: moderate ε, focuses on the performative gap, Snare/Tangled Rope. The three stories form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, organized, 0.25).
constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
