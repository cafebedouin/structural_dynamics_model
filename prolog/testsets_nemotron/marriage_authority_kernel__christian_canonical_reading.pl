% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage Authority (Indian Christian Marriage Act 1872)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   The Indian Christian Marriage Act 1872 codified Christian canonical
 *   marriage law for Christians in British India, establishing church
 *   solemnization requirements and fault-based divorce grounds (adultery,
 *   cruelty, desertion) with church tribunals handling annulments.
 *   Post-independence, the Act continues to govern Christian marriages while
 *   other communities have seen reform (Hindu Marriage Act 1955, Special
 *   Marriage Act 1954). The constraint presents as coordination — providing a
 *   unified marriage framework for a minority community — but operates with
 *   significant extraction: fault-based divorce traps spouses (especially
 *   women) in abusive marriages; church tribunals controlled by male clergy
 *   gatekeep annulments; the Act's persistence blocks access to civil
 *   no-fault divorce. Beneficiaries include the clergy establishment
 *   (jurisdictional authority, gatekeeping revenue) and conservative laity
 *   (doctrinal coherence). Victims are primarily Christian women seeking
 *   divorce, abandoned spouses unable to prove fault, and men similarly
 *   trapped though with slightly better exit options via conversion or civil
 *   marriage.
 *
 * KEY AGENTS:
 *   - church_tribunals: agenda_setter (institutional/biographical/constrained/national) — adjudicates annulments, controls canonical interpretation
 *   - clergy_establishment: beneficiary (institutional/generational/arbitrage/national) — derives authority and revenue from canonical jurisdiction
 *   - conservative_laity: beneficiary (organized/biographical/mobile/national) — doctrinal coherence, community boundary maintenance
 *   - christian_women_seeking_divorce: payer (powerless/biographical/trapped/national) — bear fault burden, limited exit, gendered evidentiary standards
 *   - christian_men_seeking_divorce: payer (moderate/biographical/constrained/national) — fault burden but better exit via conversion
 *   - abandoned_spouses: payer (powerless/biographical/trapped/national) — desertion grounds require proof, often impossible
 *   - secular_courts: observer (institutional/generational/analytical/national) — supervisory jurisdiction, constitutional review
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.58).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.65).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Christian Canonical Marriage Authority (Indian Christian Marriage Act 1872)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, 'b515b51a-c485-4694-8f2c-9711790d9ed3').
narrative_ontology:cs_kernel_codification('b515b51a-c485-4694-8f2c-9711790d9ed3', formalized).
narrative_ontology:cs_authority_grounding('b515b51a-c485-4694-8f2c-9711790d9ed3', lineage).
narrative_ontology:cs_interpretation_layer_present('b515b51a-c485-4694-8f2c-9711790d9ed3').
narrative_ontology:cs_reading_relation('b515b51a-c485-4694-8f2c-9711790d9ed3', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('b515b51a-c485-4694-8f2c-9711790d9ed3', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('b515b51a-c485-4694-8f2c-9711790d9ed3', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('b515b51a-c485-4694-8f2c-9711790d9ed3', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('b515b51a-c485-4694-8f2c-9711790d9ed3', foundational, marriage_indissoluble_sacrament).
narrative_ontology:cs_axiom_status(marriage_indissoluble_sacrament, holdable).
narrative_ontology:cs_axiom_grounding('b515b51a-c485-4694-8f2c-9711790d9ed3', marriage_indissoluble_sacrament, deontological).
narrative_ontology:cs_axiom('b515b51a-c485-4694-8f2c-9711790d9ed3', foundational, ecclesiastical_court_exclusive_annulment).
narrative_ontology:cs_axiom_status(ecclesiastical_court_exclusive_annulment, holdable).
narrative_ontology:cs_axiom_grounding('b515b51a-c485-4694-8f2c-9711790d9ed3', ecclesiastical_court_exclusive_annulment, conventional).
narrative_ontology:cs_axiom('b515b51a-c485-4694-8f2c-9711790d9ed3', secondary, fault_based_dissolution_only).
narrative_ontology:cs_axiom_status(fault_based_dissolution_only, holdable).
narrative_ontology:cs_axiom_grounding('b515b51a-c485-4694-8f2c-9711790d9ed3', fault_based_dissolution_only, conventional).
narrative_ontology:cs_reference_frame('b515b51a-c485-4694-8f2c-9711790d9ed3', canonical_law_ecclesiastical_jurisdiction).
narrative_ontology:cs_drift_state('b515b51a-c485-4694-8f2c-9711790d9ed3', post_constitutional_equality_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b515b51a-c485-4694-8f2c-9711790d9ed3', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, church_tribunals).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, clergy_establishment).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, conservative_laity).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_men_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, abandoned_spouses).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, indissolubility_of_marriage).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, ecclesiastical_jurisdiction_over_sacraments).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, canonical_law_as_suprastate_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate annulment petitions under canonical grounds (non-consummation, lack of consent, impediments). Composed of clergy appointed by bishops. No independent appeal — decisions are final within the canonical system. Collect fees for petition processing. Their jurisdiction is the Act's core enforcement mechanism; without it, the fault-based divorce regime has no ecclesiastical gatekeeper.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, church_tribunals, agenda_setter,
    institutional, biographical, constrained, national).

% Derives institutional authority, jurisdictional relevance, and revenue from the Act's canonical framework. Bishops control tribunal appointments; parish priests gatekeep marriage solemnization. The Act's persistence validates the church's claim to govern Christian family life. Can exit by accepting civil marriage reform but would lose jurisdictional monopoly — arbitrage-grade exit exists but is institutionally costly.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, clergy_establishment, beneficiary,
    institutional, generational, arbitrage, national).

% Value the Act for doctrinal coherence and community boundary maintenance. See fault-based divorce as protecting marriage sanctity. Benefit from shared normative framework but bear no direct costs. Can exit by joining reform movements or using Special Marriage Act — mobile exit with moderate social cost.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, conservative_laity, beneficiary,
    organized, biographical, mobile, national).

% Must prove adultery, cruelty, or desertion to obtain divorce — evidentiary standards are gendered (cruelty requires 'grave and weighty' conduct, often excluding emotional abuse). Desertion requires proving intent to abandon, difficult when husband simply disappears. Annulment requires church tribunal approval, which is male-dominated and rarely grants relief. Conversion to access civil divorce carries social ostracism and loss of community. No meaningful exit — trapped by identity, law, and social enforcement simultaneously.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_divorce, payer,
    powerless, biographical, trapped, national).

% Face same fault requirements but have slightly better exit options: conversion to another personal law system is socially more acceptable for men; they can initiate desertion more credibly; they control more economic resources for legal battle. Still constrained — the Act's fault regime applies equally on paper, but enforcement asymmetry favors men. Exit is possible but costly.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_men_seeking_divorce, payer,
    moderate, biographical, constrained, national).

% Spouses deserted by partners who vanish without formal desertion proceedings. Cannot prove 'intent to abandon' required for desertion ground. Cannot access cruelty/adultery grounds without evidence. Church tribunals rarely grant annulment for abandonment alone. Remarriage is canonically forbidden. Civil divorce via Special Marriage Act requires conversion, which the abandoned spouse may not want or be able to pursue. Trapped in marital limbo — neither married nor free.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, abandoned_spouses, payer,
    powerless, biographical, trapped, national).

% Exercise supervisory jurisdiction over church tribunals via writs (certiorari, mandamus). Hear constitutional challenges to the Act's gender discrimination. Can refer cases to Law Commission for reform recommendations. Do not directly bear costs or collect benefits from the constraint — their role is constitutional review and rights protection.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, secular_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified marriage framework for Indian Christians: solemnization requirements, registration, and a canonical dispute resolution system that maintains community coherence without requiring state courts to adjudicate sacramental questions.
% TRANSFER_FUNCTION: Moves jurisdictional authority and gatekeeping revenue from the state to church tribunals; moves the cost of marital dissolution from a mutual no-fault process to individual fault-proving (disproportionately borne by women); moves the power to define marriage validity from civil law to canonical law.
% ABSENT_VOICES: Christian women's organizations advocating for no-fault divorce reform; abandoned spouses with no access to legal representation; progressive clergy who support canonical reform but are silenced by institutional hierarchy; interfaith couples blocked by the Act's prohibition on non-Christian spouses.
% DISAPPEARANCE_RATIONALE: If the Act vanished overnight, Christian marriages would default to the Special Marriage Act (secular, no-fault, gender-equitable). Church tribunals would lose annulment monopoly. Clergy establishment would lose jurisdictional authority. Women would gain immediate access to equitable divorce. The Christian community would reorganize around voluntary canonical adherence rather than statutory compulsion.
% FOUNDING_PROBLEM: 1872: British colonial administration needed a unified marriage law for Indian Christians to replace fragmented ecclesiastical courts and provide legal certainty for property, inheritance, and legitimacy in a plural legal system.
% FOUNDING_PROBLEM_CORROBORATION: Law Commission of India reports (multiple, 1980s-present) document that the colonial-era unification problem is solved by the Special Marriage Act 1954. The clergy establishment asserts the problem is live (canonical coherence), but no independent legal scholar corroborates this — the Act's persistence is widely attested as jurisdictional preservation, not coordination necessity.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the fault-based divorce regime's asymmetric cost burden: women disproportionately bear the evidentiary burden for adultery/cruelty, desertion requires proof of intent, and annulment depends on church tribunals with no independent appeal. Suppression (0.65) is structural: the Act's continued operation blocks access to the no-fault divorce available under the Special Marriage Act, and conversion to access civil divorce carries social ostracism. Theater ratio (0.35) captures the gap between the Act's stated purpose (orderly Christian marriage) and its actual operation (jurisdictional preservation for church tribunals). Accessibility collapse (0.72) is high because the canonical framework treats marriage as indissoluble sacrament — alternatives (civil divorce, remarriage) are doctrinally foreclosed, not merely legally difficult. Resistance (0.45) is moderate: reform movements exist but are fragmented; the clergy establishment successfully frames reform as anti-minority interference.
 *
 * PERSPECTIVAL GAP:
 *   From the clergy establishment's seat, the constraint is genuine coordination: canonical law provides stable marriage norms, protects sacramental integrity, and preserves community identity. From Christian women's seat, the same structure is extractive: fault requirements trap them in abuse, church tribunals are male-dominated and dismissive, and the Act blocks access to constitutional equality guarantees. The engine computes this divergence from the structural data — the authored claim (tangled_rope) acknowledges both coordination and extraction without adjudicating which seat is 'correct.'
 *
 * DIRECTIONALITY LOGIC:
 *   Church tribunals and clergy are structural beneficiaries (d ~ 0.15): they collect jurisdictional authority, gatekeeping fees, and doctrinal control. Conservative laity are moderate beneficiaries (d ~ 0.3): they gain community coherence but bear no direct costs. Christian women seeking divorce are full targets (d ~ 0.95): trapped by fault requirements, gendered evidence standards, and social identity lock. Christian men are high targets (d ~ 0.8): fault burden applies but conversion exit exists. Abandoned spouses are full targets (d ~ 0.95): desertion proof is near-impossible, no unilateral exit. Secular courts are analytical observers (d ~ 0.5): they review but do not directly bear costs or collect benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The Act's founding problem (1872) was providing a unified marriage law for Christians under British rule, replacing fragmented ecclesiastical courts. That problem is dead — India has a unified secular marriage option (Special Marriage Act) and the colonial framework is gone. Yet the Act persists because the clergy establishment extracts jurisdictional authority from it, and the state defers to 'personal law' pluralism. The mandatrophy is unresolved: the constraint's coordination function (unified Christian marriage law) could be served by the secular option, but the extraction function (church tribunal gatekeeping) would be lost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the christian_canonical_reading of the marriage_authority_kernel. What structural elements distinguish this reading from its sibling readings (hindu_codified_reading, muslim_shariat_reading, parsi_communal_reading, secular_civil_reading)?',
    'Comparative analysis of each reading''s beneficiary/victim structure, exit options for affected parties, and the specific canonical provisions each reading treats as binding.',
    'If the structural deltas are smaller than claimed, the kernel may be less contested and more of a coordination framework; if larger, each reading operates as a genuinely distinct constraint with different extraction profiles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committers structure: this reading''s location within the kernel family').

omega_variable(
    fault_divorce_extraction_boundary,
    'Is the fault-based divorce requirement a genuine coordination mechanism (preserving marital stability) or an extraction mechanism that traps spouses in abusive or broken marriages?',
    'Empirical study of divorce outcomes under fault-based vs. no-fault regimes; testimony from abandoned spouses and church tribunal records on denial rates.',
    'If primarily extraction, the constraint''s claimed_type should shift toward snare; if genuine coordination with unavoidable overhead, tangled_rope stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fault_divorce_extraction_boundary, empirical, 'Whether fault-based divorce serves coordination or extraction').

omega_variable(
    church_tribunal_independence,
    'Do church tribunals operate as independent adjudicators or as arms of the clergy establishment that benefits from restrictive annulment standards?',
    'Analysis of tribunal composition, funding, appeal structures, and correlation between tribunal rulings and institutional interests.',
    'If tribunals are captured, the coordination function is compromised and extraction is higher; if independent, the fault-based system has genuine adjudicative legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(church_tribunal_independence, empirical, 'Independence of ecclesiastical adjudication from institutional self-interest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_authority_christian_canonical_tr_t0, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marriage_authority_christian_canonical_tr_t30, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(marriage_authority_christian_canonical_tr_t60, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(marriage_authority_christian_canonical_tr_t90, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 90, 0.3).
narrative_ontology:measurement(marriage_authority_christian_canonical_tr_t120, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 120, 0.35).

% Extraction over time
narrative_ontology:measurement(marriage_authority_christian_canonical_be_t0, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(marriage_authority_christian_canonical_be_t30, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(marriage_authority_christian_canonical_be_t60, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(marriage_authority_christian_canonical_be_t90, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 90, 0.55).
narrative_ontology:measurement(marriage_authority_christian_canonical_be_t120, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 120, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marriage_authority_christian_canonical_su_t0, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(marriage_authority_christian_canonical_su_t30, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(marriage_authority_christian_canonical_su_t60, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(marriage_authority_christian_canonical_su_t90, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 90, 0.63).
narrative_ontology:measurement(marriage_authority_christian_canonical_su_t120, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 120, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__christian_canonical_reading, 0.08).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, special_marriage_act_access).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, constitutional_equality_jurisprudence).

% DUAL FORMULATION NOTE:
% Part of the marriage_authority_kernel constraint family. This reading instantiates Christian canonical law as codified in the 1872 Act. The kernel decomposes into five readings with different beneficiary/victim structures, exit options, and extraction profiles. The christian_canonical_reading is distinctive for its unreformed colonial statute, church tribunal monopoly on annulment, and fault-based divorce regime. The secular_civil_reading provides the exit option that makes this reading's extraction visible — without the Special Marriage Act alternative, the fault regime would look like universal coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__christian_canonical_reading, moderate, 0.8).
constraint_indexing:directionality_override(marriage_authority_kernel__christian_canonical_reading, powerless, 0.95).
constraint_indexing:directionality_override(marriage_authority_kernel__christian_canonical_reading, institutional, 0.15).
constraint_indexing:directionality_override(marriage_authority_kernel__christian_canonical_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
