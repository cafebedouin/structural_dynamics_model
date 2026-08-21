% ============================================================================
% CONSTRAINT STORY: notability_guidelines__inclusionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__inclusionist_reading, []).

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
 *   constraint_id: notability_guidelines__inclusionist_reading
 *   human_readable: Wikipedia Notability Guidelines (Inclusionist Reading)
 *   domain: digital_commons_governance/knowledge_infrastructure
 *
 * SUMMARY:
 *   This constraint story analyzes Wikipedia's Notability Guidelines (WP:N)
 *   from an inclusionist perspective, viewing it as a structural gatekeeping
 *   apparatus. While ostensibly a quality control mechanism, this reading
 *   argues that WP:N systematically excludes knowledge from marginalized
 *   communities due to its reliance on 'reliable sources' that privilege
 *   mainstream, institutionalized forms of knowledge production. The
 *   constraint operates as a Snare, extracting epistemic legitimacy and
 *   visibility from non-traditional knowledge forms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.85).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.9).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "Wikipedia Notability Guidelines (Inclusionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital_commons_governance/knowledge_infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, 'e8016fc6-f114-4d29-90fd-a10f6c45cd5f').
narrative_ontology:cs_kernel_codification('e8016fc6-f114-4d29-90fd-a10f6c45cd5f', fixed_text).
narrative_ontology:cs_authority_grounding('e8016fc6-f114-4d29-90fd-a10f6c45cd5f', practice).
narrative_ontology:cs_interpretation_layer_present('e8016fc6-f114-4d29-90fd-a10f6c45cd5f').
narrative_ontology:cs_reading_relation('e8016fc6-f114-4d29-90fd-a10f6c45cd5f', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8016fc6-f114-4d29-90fd-a10f6c45cd5f', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('e8016fc6-f114-4d29-90fd-a10f6c45cd5f', foundational, knowledge_is_power_neutral).
narrative_ontology:cs_axiom_status(knowledge_is_power_neutral, holdable).
narrative_ontology:cs_axiom_grounding('e8016fc6-f114-4d29-90fd-a10f6c45cd5f', knowledge_is_power_neutral, deontological).
narrative_ontology:cs_axiom('e8016fc6-f114-4d29-90fd-a10f6c45cd5f', secondary, epistemic_justice_mandate).
narrative_ontology:cs_axiom_status(epistemic_justice_mandate, holdable).
narrative_ontology:cs_axiom_grounding('e8016fc6-f114-4d29-90fd-a10f6c45cd5f', epistemic_justice_mandate, deontological).
narrative_ontology:cs_reference_frame('e8016fc6-f114-4d29-90fd-a10f6c45cd5f', universal_knowledge_representation).
narrative_ontology:cs_drift_state('e8016fc6-f114-4d29-90fd-a10f6c45cd5f', contemporary_wikipedia_bias_critiques, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e8016fc6-f114-4d29-90fd-a10f6c45cd5f', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, wikipedia_editors_maintaining_status_quo).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, wikipedia_foundation).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, deletionist_editors).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_knowledge_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A collective of editors who actively interpret and enforce WP:N, often prioritizing established academic and media sources. They benefit from the stability of the existing knowledge hierarchy and the perceived quality control it provides, which aligns with their identity as guardians of Wikipedia's standards.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_editors_maintaining_status_quo, agenda_setter,
    institutional, generational, identity_locked, global).

% Academics, journalists, and publishers whose work is already recognized by mainstream institutions. Their publications are readily accepted as 'reliable sources' by WP:N, ensuring their knowledge is privileged and easily included in Wikipedia, reinforcing their authority.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, beneficiary,
    powerful, biographical, mobile, global).

% Communities whose knowledge, histories, and perspectives are often documented in non-traditional, community-specific, or oral sources. These sources are frequently deemed 'unreliable' or 'not notable' by WP:N, leading to the systematic exclusion of their knowledge and perpetuating epistemic injustice.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_communities, payer,
    powerless, generational, trapped, global).

% Editors, scholars, and activists who champion the inclusion of knowledge from marginalized communities. They expend significant effort attempting to navigate or challenge WP:N, often facing resistance and burnout due to the structural barriers to inclusion.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_knowledge_advocates, payer,
    moderate, biographical, constrained, global).

% The non-profit organization that hosts Wikipedia. It benefits from the perceived authority and neutrality that WP:N is claimed to uphold, which helps secure funding and public trust, even as the policy's gatekeeping function is critiqued.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_foundation, agenda_setter,
    institutional, civilizational, arbitrage, global).

% A subset of editors who rigorously apply WP:N to remove articles deemed 'not notable', believing this is essential for maintaining Wikipedia's quality and preventing 'content bloat'. Their identity is often tied to this gatekeeping function.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, deletionist_editors, agenda_setter,
    organized, biographical, identity_locked, global).

% Editors who engage in the Articles for Deletion (AfD) process, seeking consensus on notability through discussion. While they aim for fair process, their decisions are often constrained by the prevailing interpretations of WP:N, which can inadvertently reinforce existing biases.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, deliberative_editors, observer,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate editorial effort around a shared standard for inclusion, preventing arbitrary content and maintaining a consistent scope for the encyclopedia, thereby ensuring a baseline of quality and verifiability.
% TRANSFER_FUNCTION: Transfers editorial power and legitimacy from diverse knowledge communities to those who control the interpretation and enforcement of WP:N, effectively transferring attention and validation to institutionally-sanctioned knowledge and away from marginalized epistemologies.
% ABSENT_VOICES: Representatives of marginalized communities, indigenous knowledge holders, and scholars working outside mainstream academic/publishing structures are systematically excluded from the discourse that defines 'notability' and 'reliable sources'. Their perspectives are often not heard in policy debates or AfD discussions.
% DISAPPEARANCE_RATIONALE: If WP:N and its enforcement vanished overnight, Wikipedia's content policies would need a radical overhaul. This would likely lead to a flood of new content, a re-evaluation of existing articles, and a fundamental shift in what constitutes 'encyclopedic knowledge', profoundly reorganizing the power dynamics of knowledge inclusion.
% FOUNDING_PROBLEM: To prevent Wikipedia from becoming a repository of trivial, unverifiable, or self-promotional content, ensuring a baseline of quality and encyclopedic relevance by requiring topics to have received significant coverage in reliable, independent sources.
% FOUNDING_PROBLEM_CORROBORATION: Deletionist editors and the Wikipedia Foundation claim the problem of content quality and verifiability is still live. Inclusionist editors and marginalized communities argue the founding problem has largely shifted from preventing spam to systematically excluding valid, but non-mainstream, knowledge; independent academic studies on knowledge bias in Wikipedia corroborate this shifted function.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__inclusionist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__inclusionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(notability_guidelines__inclusionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant cost borne by marginalized communities whose knowledge is denied inclusion, and the transfer of epistemic authority to established institutions. Suppression (0.90) is high due to the active enforcement of WP:N through deletion processes (AfD) and the systemic barriers to establishing 'notability' for non-mainstream topics. The moderate theater ratio (0.45) indicates that while some quality control function remains, a substantial portion of the enforcement activity serves to maintain the existing knowledge hierarchy under the guise of neutrality. The increasing trends in extractiveness, suppression, and theater ratio over time reflect the growing entrenchment of WP:N's gatekeeping function and the increasing effort required to maintain it against challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries (e.g., deletionist editors), WP:N is a necessary Rope for quality control. From the perspective of victims (e.g., marginalized communities), it is a Snare that systematically excludes their knowledge. The engine's classification will highlight this divergence, showing how the same structural rules are experienced as coordination by some and extraction by others.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional knowledge producers, Wikipedia editors maintaining the status quo, the Wikipedia Foundation, and deletionist editors are beneficiaries; they benefit from the stability, perceived authority, and reduced editorial burden that WP:N provides. Marginalized communities and their advocates are victims, bearing the cost of exclusion and the effort required to challenge the system. Deliberative editors act as observers, participating in the process but often constrained by its underlying structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reliable_sources_definition_ambiguity,
    'Is the definition of ''reliable sources'' in WP:N inherently neutral and objective, or does it implicitly privilege certain forms of knowledge production and exclude others?',
    'Content analysis of sources accepted vs. rejected in AfD, combined with ethnographic studies of knowledge production in marginalized communities. If a systematic bias towards institutional sources is found, the definition is not neutral.',
    'If the definition is biased, the constraint''s extractiveness and suppression are higher than acknowledged, reinforcing its Snare classification and highlighting epistemic injustice. If truly neutral, the coordination function is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reliable_sources_definition_ambiguity, empirical, 'Ambiguity in the neutrality of ''reliable sources'' definition.').

omega_variable(
    alternative_notability_models_impact,
    'How would Wikipedia''s quality and scope be affected by adopting alternative notability models that prioritize community-based sourcing or indigenous knowledge frameworks?',
    'Pilot projects or case studies in specific language Wikipedias or thematic wikis that implement alternative notability criteria. Evaluate changes in content diversity, quality, and editor participation.',
    'If quality is maintained or enhanced, it would demonstrate that the current WP:N is not structurally necessary for quality, strengthening the Snare classification. If quality degrades, it would suggest a genuine coordination function, potentially shifting the classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_notability_models_impact, empirical, 'Impact of alternative notability models on Wikipedia''s function.').

omega_variable(
    framing_underdetermination_notability,
    'Is WP:N fundamentally a quality control mechanism (deletionist framing), a dynamic consensus process (deliberative framing), or a structural gatekeeping apparatus (inclusionist framing)?',
    'Longitudinal analysis of AfD outcomes, editor demographics, and content bias trends. If bias persists despite deliberative efforts, the gatekeeping framing is strengthened. If quality consistently improves without bias, the quality control framing is strengthened.',
    'The choice of framing significantly alters the perceived extractiveness and suppression. If the gatekeeping framing is adopted, the constraint is a Snare; if the quality control framing, it''s closer to a Rope. This omega documents the conceptual contest over the constraint''s core function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_notability, conceptual, 'Conceptual contest over the core function of WP:N.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__inclusionist_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(nota_tr_t4, notability_guidelines__inclusionist_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__inclusionist_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__inclusionist_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__inclusionist_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__inclusionist_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__inclusionist_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__inclusionist_reading, base_extractiveness, 4, 0.75).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__inclusionist_reading, base_extractiveness, 8, 0.8).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__inclusionist_reading, base_extractiveness, 12, 0.82).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__inclusionist_reading, base_extractiveness, 16, 0.84).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__inclusionist_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__inclusionist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(nota_su_t4, notability_guidelines__inclusionist_reading, suppression_requirement, 4, 0.8).
narrative_ontology:measurement(nota_su_t8, notability_guidelines__inclusionist_reading, suppression_requirement, 8, 0.85).
narrative_ontology:measurement(nota_su_t12, notability_guidelines__inclusionist_reading, suppression_requirement, 12, 0.87).
narrative_ontology:measurement(nota_su_t16, notability_guidelines__inclusionist_reading, suppression_requirement, 16, 0.89).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__inclusionist_reading, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__inclusionist_reading, identity_coordination).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, verifiability_policy).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, reliable_sources_policy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'notability_guidelines' kernel, focusing on its function as a gatekeeping apparatus. It is linked to 'verifiability_policy' and 'reliable_sources_policy' as WP:N's operation is deeply intertwined with these foundational content policies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
