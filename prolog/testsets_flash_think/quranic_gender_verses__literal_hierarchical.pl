% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Quranic Gender Verses: Literal Hierarchical Reading
 *   domain: Islamic Jurisprudence/Legal Hermeneutics/Gender Studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'literal_hierarchical' reading of
 *   specific Quranic verses (4:11, 2:282, 4:34) concerning gender roles and
 *   rights. This reading asserts these verses are direct, timeless legal
 *   constraints establishing male guardianship (qawamah) and differentiated
 *   rights as divine ordinance. It is characterized by high extraction from
 *   women and high suppression, maintained through religious authority and
 *   social norms. The claimed type is 'snare' because the coordination story
 *   (divine order) serves as cover for substantial, enforced extraction from
 *   identifiable victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.85).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.9).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.85).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, snare).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Quranic Gender Verses: Literal Hierarchical Reading").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "Islamic Jurisprudence/Legal Hermeneutics/Gender Studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, '2ca96adc-9f94-43c6-8055-9e9e444e7397').
narrative_ontology:cs_kernel_codification('2ca96adc-9f94-43c6-8055-9e9e444e7397', fixed_text).
narrative_ontology:cs_authority_grounding('2ca96adc-9f94-43c6-8055-9e9e444e7397', lineage).
narrative_ontology:cs_interpretation_layer_present('2ca96adc-9f94-43c6-8055-9e9e444e7397').
narrative_ontology:cs_reading_relation('2ca96adc-9f94-43c6-8055-9e9e444e7397', quranic_gender_verses__contextual_egalitarian, forecloses).
narrative_ontology:cs_reading_relation('2ca96adc-9f94-43c6-8055-9e9e444e7397', quranic_gender_verses__progressive_abrogation, forecloses).
narrative_ontology:cs_axiom('2ca96adc-9f94-43c6-8055-9e9e444e7397', foundational, quranic_verses_are_direct_legal_commands).
narrative_ontology:cs_axiom_status(quranic_verses_are_direct_legal_commands, holdable).
narrative_ontology:cs_axiom_grounding('2ca96adc-9f94-43c6-8055-9e9e444e7397', quranic_verses_are_direct_legal_commands, theological).
narrative_ontology:cs_axiom('2ca96adc-9f94-43c6-8055-9e9e444e7397', foundational, male_guardianship_is_divine_ordinance).
narrative_ontology:cs_axiom_status(male_guardianship_is_divine_ordinance, holdable).
narrative_ontology:cs_axiom_grounding('2ca96adc-9f94-43c6-8055-9e9e444e7397', male_guardianship_is_divine_ordinance, theological).
narrative_ontology:cs_reference_frame('2ca96adc-9f94-43c6-8055-9e9e444e7397', classical_islamic_jurisprudence).
narrative_ontology:cs_drift_state('2ca96adc-9f94-43c6-8055-9e9e444e7397', contemporary_global_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2ca96adc-9f94-43c6-8055-9e9e444e7397', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_scholars_and_courts).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_in_literalist_jurisdictions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As per the literal hierarchical reading, these individuals are granted guardianship (qawamah) over women in their households, including authority in decision-making and control over resources. They benefit from a clear, divinely sanctioned social order.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, agenda_setter,
    powerful, generational, mobile, local).

% These institutions and individuals interpret, codify, and enforce the literal hierarchical reading of the verses, translating them into legal rulings (fatwas) and judicial decisions. They gain significant authority and legitimacy from upholding what they consider divine law.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_scholars_and_courts, agenda_setter,
    institutional, civilizational, constrained, national).

% These women bear the direct costs of the literal hierarchical interpretation, experiencing constrained inheritance rights, reduced weight of testimony in legal matters, and limited legal autonomy. Their religious identity often makes exit from the system unthinkable, leading to identity-locked status.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_in_literalist_jurisdictions, payer,
    powerless, biographical, identity_locked, local).

% These scholars offer alternative, egalitarian interpretations of the Quranic verses, challenging the literal hierarchical reading. They are often excluded from mainstream religious discourse and institutions that uphold the literalist view, but their work provides intellectual resistance.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, islamic_feminist_scholars, excluded,
    analytical, generational, analytical, global).

% These organizations and individuals observe and critique the application of the literal hierarchical reading from a human rights perspective, advocating for gender equality and legal reform in jurisdictions where these interpretations are dominant. They have no direct power within the religious legal system but exert external pressure.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, secular_human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, divinely sanctioned social and legal hierarchy within families and society, providing a framework for order, resource distribution, and dispute resolution based on gender roles.
% TRANSFER_FUNCTION: Transfers authority, decision-making power, and often economic control from women to men, particularly male household heads and religious authorities, in matters of family, inheritance, and legal standing.
% ABSENT_VOICES: Islamic feminist scholars and secular human rights advocates are largely excluded from the interpretive and enforcement mechanisms of this reading; they would argue for egalitarian interpretations and legal reforms that challenge the gender hierarchy.
% DISAPPEARANCE_RATIONALE: If this literal hierarchical interpretation and its enforcement vanished overnight, the legal and social fabric of societies governed by it would be fundamentally altered. Family law, inheritance systems, women's public roles, and the authority of religious institutions would undergo massive reorganization, leading to significant shifts in power dynamics.
% FOUNDING_PROBLEM: To establish social order, family structure, and legal principles in early Islamic society, addressing issues of inheritance, marriage, and dispute resolution within a nascent community.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the literal hierarchical reading (religious scholars and courts) assert the founding problem of maintaining divine order and social stability is still live. Critics (Islamic feminist scholars, human rights advocates) argue that while social order is still a concern, the specific gendered solutions are historically contingent and now serve to maintain existing power structures rather than solve contemporary problems; independent sociological and legal analyses support this shifted-function reading.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.85) due to the significant legal and social disadvantages imposed on women, including unequal inheritance, reduced legal testimony weight, and male guardianship. Suppression is very high (0.90) because the interpretation is presented as divine law, making resistance difficult and exit (e.g., apostasy, challenging family structures) extremely costly, often leading to social ostracization or legal penalties. Theater ratio is low (0.10) as the constraint is actively and genuinely enforced by religious institutions and social structures, with little performative maintenance masking atrophy. Accessibility collapse is high (0.75) as alternatives are severely limited by religious doctrine and social pressure. Resistance is moderate (0.60) reflecting ongoing, though often suppressed, challenges from within and outside Islamic communities.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of male household heads and religious authorities, this constraint is a divinely ordained, just, and necessary framework for social order (claimed as a 'rope' or even 'mountain'). From the perspective of women experiencing its effects, and critical observers, it functions as a 'snare' that extracts resources and autonomy through religious coercion. The engine's classification will highlight this divergence between the claimed divine order and the observed extractive reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads and religious scholars/courts are the primary beneficiaries and agenda-setters, gaining structural authority and control (low directionality). Women in literalist jurisdictions are the primary targets, bearing the costs of constrained rights and autonomy (high directionality, often identity_locked). Islamic feminist scholars and secular human rights advocates operate as excluded or analytical observers, challenging the constraint from outside its direct enforcement mechanisms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_ordinance_vs_human_interpretation,
    'Is the literal hierarchical reading of these Quranic verses a direct, timeless divine ordinance, or a human interpretation shaped by historical and cultural contexts?',
    'Comparative theological and hermeneutical analysis across diverse Islamic traditions, examining the historical evolution of interpretations and the role of human agency in legal codification.',
    'If primarily a human interpretation, the constraint''s ''divine'' justification weakens, potentially reducing its perceived legitimacy and opening pathways for reform. If genuinely divine, its resistance to change is structurally higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_ordinance_vs_human_interpretation, conceptual, 'Ambiguity between divine command and human interpretive construct.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal penalties, social ostracization) or internalized (religious belief, identity fusion)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., self-censorship, guilt) after structural barriers are removed (e.g., emigration to secular state), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as women carry the suppression with them even after leaving literalist jurisdictions, making true exit more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in religious contexts.').

omega_variable(
    mandatrophy_of_founding_problem,
    'Is the founding problem (establishing social order in early Islamic society) still genuinely live, or has the interpretation become a means of maintaining existing power structures under the guise of divine order?',
    'Independent sociological and legal analysis comparing the original context and problems with contemporary societal needs and challenges, particularly regarding gender equality and human rights.',
    'If the founding problem is largely ''dead'' but the constraint persists, it strengthens the ''snare'' classification by highlighting the disconnect between original intent and current function, potentially triggering mandatrophy detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_founding_problem, empirical, 'Whether the constraint''s mandate has outlived its original function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t20, quranic_gender_verses__literal_hierarchical, theater_ratio, 20, 0.11).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__literal_hierarchical, theater_ratio, 40, 0.1).
narrative_ontology:measurement(qura_tr_t60, quranic_gender_verses__literal_hierarchical, theater_ratio, 60, 0.1).
narrative_ontology:measurement(qura_tr_t80, quranic_gender_verses__literal_hierarchical, theater_ratio, 80, 0.1).
narrative_ontology:measurement(qura_tr_t100, quranic_gender_verses__literal_hierarchical, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(qura_be_t20, quranic_gender_verses__literal_hierarchical, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__literal_hierarchical, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(qura_be_t60, quranic_gender_verses__literal_hierarchical, base_extractiveness, 60, 0.84).
narrative_ontology:measurement(qura_be_t80, quranic_gender_verses__literal_hierarchical, base_extractiveness, 80, 0.85).
narrative_ontology:measurement(qura_be_t100, quranic_gender_verses__literal_hierarchical, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(qura_su_t20, quranic_gender_verses__literal_hierarchical, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__literal_hierarchical, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(qura_su_t60, quranic_gender_verses__literal_hierarchical, suppression_requirement, 60, 0.89).
narrative_ontology:measurement(qura_su_t80, quranic_gender_verses__literal_hierarchical, suppression_requirement, 80, 0.9).
narrative_ontology:measurement(qura_su_t100, quranic_gender_verses__literal_hierarchical, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, islamic_family_law_codes).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, religious_education_curricula).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quranic_gender_verses' kernel. The other readings are 'contextual_egalitarian' and 'progressive_abrogation', each representing a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
