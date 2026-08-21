% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Divine Marriage Command (Continuationist Reading)
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the 'continuationist_reading' of the
 *   divine marriage command kernel. From this perspective, the divine command
 *   for plural marriage remains doctrinally valid as an eternal principle.
 *   The 1890 Manifesto is interpreted not as a doctrinal rescission, but as a
 *   prudential suspension of practice under duress from federal anti-polygamy
 *   laws. The mainstream church benefits from maintaining its historical and
 *   theological continuity, while adherents who believe in plural marriage
 *   face significant extraction due to legal and social suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.85).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Divine Marriage Command (Continuationist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, 'b6a3be8e-023c-4582-a250-12ee5993ebca').
narrative_ontology:cs_kernel_codification('b6a3be8e-023c-4582-a250-12ee5993ebca', fixed_text).
narrative_ontology:cs_authority_grounding('b6a3be8e-023c-4582-a250-12ee5993ebca', lineage).
narrative_ontology:cs_interpretation_layer_present('b6a3be8e-023c-4582-a250-12ee5993ebca').
narrative_ontology:cs_reading_relation('b6a3be8e-023c-4582-a250-12ee5993ebca', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('b6a3be8e-023c-4582-a250-12ee5993ebca', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('b6a3be8e-023c-4582-a250-12ee5993ebca', foundational, plural_marriage_is_eternal_principle).
narrative_ontology:cs_axiom_status(plural_marriage_is_eternal_principle, holdable).
narrative_ontology:cs_axiom_grounding('b6a3be8e-023c-4582-a250-12ee5993ebca', plural_marriage_is_eternal_principle, theological).
narrative_ontology:cs_axiom('b6a3be8e-023c-4582-a250-12ee5993ebca', foundational, manifesto_is_prudential_suspension).
narrative_ontology:cs_axiom_status(manifesto_is_prudential_suspension, holdable).
narrative_ontology:cs_axiom_grounding('b6a3be8e-023c-4582-a250-12ee5993ebca', manifesto_is_prudential_suspension, conventional).
narrative_ontology:cs_reference_frame('b6a3be8e-023c-4582-a250-12ee5993ebca', original_divine_command_for_plural_marriage).
narrative_ontology:cs_drift_state('b6a3be8e-023c-4582-a250-12ee5993ebca', contemporary_legal_context, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b6a3be8e-023c-4582-a250-12ee5993ebca', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, mainstream_church_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, historical_theological_tradition).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, polygamous_adherents).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, fundamentalist_splinter_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the official doctrine that plural marriage remains a valid, eternal principle, but its practice is currently suspended due to external legal duress (the Manifesto). Benefits from preserving the authority of original revelation and historical continuity, while avoiding legal conflict with the federal government.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Believe in the divine command for plural marriage as an eternal principle. They bear the cost of either not practicing it (contrary to their belief) or facing legal prosecution and excommunication from the mainstream church if they do. Their identity is deeply tied to this doctrine.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, polygamous_adherents, payer,
    powerless, biographical, identity_locked, local).

% Have separated from the mainstream church to continue practicing plural marriage, viewing the Manifesto as a betrayal of divine command. They face legal persecution from the federal government and social ostracization, but maintain what they see as true doctrinal continuity.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_splinter_groups, payer,
    organized, generational, constrained, regional).

% Enforces anti-polygamy laws, which are the primary external constraint on the practice of plural marriage. Its actions directly create the 'duress' that led to the Manifesto and continues to prevent open practice.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% The abstract body of doctrine and historical precedent that benefits from the mainstream church's efforts to maintain the validity of plural marriage as an eternal principle, even if suspended. Its integrity is preserved by this reading.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, historical_theological_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(divine_marriage_command__continuationist_reading, historical_theological_tradition).

% Study the historical and theological development of the divine marriage command and its interpretations, including the continuationist reading. They analyze the structural tensions and doctrinal implications without direct participation.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, analytical_theologians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the theological continuity and authority of the divine command for plural marriage within the church's doctrine, reconciling it with external legal and social pressures through a prudential suspension of practice.
% TRANSFER_FUNCTION: Transfers the burden of non-practice or legal risk to adherents who believe in plural marriage, while transferring doctrinal authority and historical legitimacy to the mainstream church leadership.
% ABSENT_VOICES: Early church leaders who established plural marriage as a divine command would likely object to its suspension, even if prudential. Contemporary voices within the church who might advocate for its full restoration are marginalized.
% DISAPPEARANCE_RATIONALE: If the continuationist reading vanished, the mainstream church's theological foundation would be severely challenged, potentially leading to a doctrinal schism or a full embrace of either substitutionism or open practice. Fundamentalist splinter groups would lose their primary theological justification for separation, and the lives of polygamous adherents would be profoundly altered.
% FOUNDING_PROBLEM: How to reconcile a foundational divine command for plural marriage with overwhelming external legal and social pressure against its practice, without disavowing the original revelation or losing institutional standing.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the Manifesto's issuance under duress (e.g., federal confiscation of church property, imprisonment of leaders); continued existence and theological arguments of fundamentalist splinter groups; academic theological analyses of the church's doctrinal evolution.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because adherents who believe in plural marriage are prevented from practicing it, incurring costs of non-compliance or legal risk. Suppression is very high due to active federal law enforcement and internal church discipline against open practice. Theater ratio is low because the doctrinal validity of plural marriage is genuinely held by the mainstream church, not merely performed; the tension is real. Accessibility collapse is high as legal and social barriers make open practice extremely difficult. Resistance is moderate, primarily from fundamentalist splinter groups who actively defy the suspension.
 *
 * PERSPECTIVAL GAP:
 *   The mainstream church leadership experiences this as a necessary, albeit difficult, act of institutional preservation and doctrinal fidelity. For polygamous adherents, it is a source of profound personal and spiritual conflict, forcing them to choose between deeply held beliefs and legal/social acceptance. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The mainstream church leadership and the historical theological tradition are beneficiaries, as this reading preserves their authority and continuity. Polygamous adherents and fundamentalist splinter groups are victims, bearing the costs of legal and social suppression for their beliefs. The federal government acts as an external agenda-setter, enforcing the laws that create the duress.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_vs_prudential_status,
    'Is the Manifesto truly a prudential suspension of practice, or has it implicitly become a new, superseding doctrine for the mainstream church, despite official claims?',
    'Analysis of internal church discourse, policy changes, and disciplinary actions over time. If the church actively suppresses any theological discussion of plural marriage''s future practice, it suggests a de facto doctrinal shift.',
    'If a de facto doctrinal shift has occurred, the constraint''s extractiveness from polygamous adherents might be reclassified as internal doctrinal enforcement rather than external legal duress, potentially shifting the claimed type towards a Snare for those within the mainstream church.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_vs_prudential_status, conceptual, 'Ambiguity between declared prudential suspension and de facto doctrinal rescission.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of plural marriage practice primarily structural (federal law enforcement) or internalized (social and theological pressure within the mainstream church community)?',
    'Post-exit suppression trajectory: if adherents who leave the mainstream church and move to jurisdictions with less federal enforcement still experience significant internalized barriers to practicing plural marriage, it suggests a strong internalized component.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than the structural measure suggests, as adherents carry the suppression with them even after external barriers are reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for plural marriage practice.').

omega_variable(
    legitimacy_of_original_revelation,
    'To what extent does the mainstream church''s continued claim of doctrinal validity for plural marriage genuinely reflect belief in the original revelation, versus a strategic move to maintain historical authority?',
    'Historical-critical analysis of church archives, internal theological debates, and the evolution of official statements regarding the nature of revelation and prophetic authority.',
    'If the claim of doctrinal validity is primarily strategic, the theater_ratio would be higher, and the constraint''s classification might lean more towards a Snare, as the coordination story (doctrinal continuity) would be more of a cover for institutional power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_original_revelation, conceptual, 'Authenticity of doctrinal validity claim versus strategic institutional interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__continuationist_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__continuationist_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(divi_tr_t1950, divine_marriage_command__continuationist_reading, theater_ratio, 1950, 0.13).
narrative_ontology:measurement(divi_tr_t1980, divine_marriage_command__continuationist_reading, theater_ratio, 1980, 0.14).
narrative_ontology:measurement(divi_tr_t2024, divine_marriage_command__continuationist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__continuationist_reading, base_extractiveness, 1890, 0.6).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__continuationist_reading, base_extractiveness, 1920, 0.62).
narrative_ontology:measurement(divi_be_t1950, divine_marriage_command__continuationist_reading, base_extractiveness, 1950, 0.65).
narrative_ontology:measurement(divi_be_t1980, divine_marriage_command__continuationist_reading, base_extractiveness, 1980, 0.67).
narrative_ontology:measurement(divi_be_t2024, divine_marriage_command__continuationist_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__continuationist_reading, suppression_requirement, 1890, 0.8).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__continuationist_reading, suppression_requirement, 1920, 0.82).
narrative_ontology:measurement(divi_su_t1950, divine_marriage_command__continuationist_reading, suppression_requirement, 1950, 0.83).
narrative_ontology:measurement(divi_su_t1980, divine_marriage_command__continuationist_reading, suppression_requirement, 1980, 0.84).
narrative_ontology:measurement(divi_su_t2024, divine_marriage_command__continuationist_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
