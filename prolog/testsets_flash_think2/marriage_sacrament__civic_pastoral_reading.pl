% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__civic_pastoral_reading, []).

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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage as Pastoral Discernment (Civic-Pastoral Reading)
 *   domain: religious/canon_law/political_sociology
 *
 * SUMMARY:
 *   This constraint story describes the 'civic-pastoral' reading of the
 *   marriage sacrament kernel, which emphasizes marriage as a pastoral
 *   relationship subject to human failure, where indissolubility is an ideal
 *   requiring compassionate discernment in individual cases. This reading
 *   aims to provide mercy and inclusion for individuals in complex marital
 *   situations, but it generates moderate extraction from traditional
 *   Catholics who experience doctrinal relativization and a loss of normative
 *   clarity. Institutional authority is also eroded by inconsistent
 *   enforcement, leading to internal tensions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.6).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.55).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Discernment (Civic-Pastoral Reading)").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '9578799d-3ec5-4efa-814c-d82abc7fb17f').
narrative_ontology:cs_kernel_codification('9578799d-3ec5-4efa-814c-d82abc7fb17f', fixed_text).
narrative_ontology:cs_authority_grounding('9578799d-3ec5-4efa-814c-d82abc7fb17f', lineage).
narrative_ontology:cs_interpretation_layer_present('9578799d-3ec5-4efa-814c-d82abc7fb17f').
narrative_ontology:cs_reading_relation('9578799d-3ec5-4efa-814c-d82abc7fb17f', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('9578799d-3ec5-4efa-814c-d82abc7fb17f', foundational, pastoral_mercy_priority).
narrative_ontology:cs_axiom_status(pastoral_mercy_priority, holdable).
narrative_ontology:cs_axiom_grounding('9578799d-3ec5-4efa-814c-d82abc7fb17f', pastoral_mercy_priority, deontological).
narrative_ontology:cs_axiom('9578799d-3ec5-4efa-814c-d82abc7fb17f', secondary, discernment_of_individual_conscience).
narrative_ontology:cs_axiom_status(discernment_of_individual_conscience, holdable).
narrative_ontology:cs_axiom_grounding('9578799d-3ec5-4efa-814c-d82abc7fb17f', discernment_of_individual_conscience, conventional).
narrative_ontology:cs_reference_frame('9578799d-3ec5-4efa-814c-d82abc7fb17f', pastoral_accompaniment_model).
narrative_ontology:cs_drift_state('9578799d-3ec5-4efa-814c-d82abc7fb17f', post_synodal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9578799d-3ec5-4efa-814c-d82abc7fb17f', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, laity_seeking_discernment).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_clergy).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, doctrinal_conservatives).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, pastoral_care_priority).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, mercy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tasked with guiding individuals through complex marital situations, balancing traditional doctrine with a compassionate, discerning approach. They implement the pastoral reading, often facing pressure from both progressive and conservative factions within the Church.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_clergy, agenda_setter,
    institutional, generational, constrained, national).

% Individuals in complex marital situations (e.g., divorced and remarried) who seek reconciliation and participation in the Church. They benefit from the compassionate and flexible approach offered by this reading, which provides a path for their spiritual lives.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, laity_seeking_discernment, beneficiary,
    moderate, biographical, constrained, local).

% Lay faithful whose identity and spiritual life are deeply tied to a stable, unchanging understanding of marriage doctrine. They experience a loss of normative clarity and feel their faith is undermined by perceived doctrinal relativization, leading to spiritual distress and confusion.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_catholics, payer,
    powerless, generational, identity_locked, global).

% Organized groups and theologians who actively advocate for strict adherence to traditional, immutable marriage doctrine. They view this pastoral reading as an erosion of institutional authority and a source of confusion, often engaging in public critique and resistance.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, doctrinal_conservatives, payer,
    organized, generational, constrained, global).

% The central teaching authority of the Catholic Church, which promulgates doctrine and oversees pastoral practice. It attempts to balance the need for pastoral care with doctrinal continuity, observing the tensions and impacts created by this reading on the faithful and the institution.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, institutional_magisterium, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__civic_pastoral_reading, pastoral_clergy).
narrative_ontology:fixing_cost_class(marriage_sacrament__civic_pastoral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide pastoral care and guidance for individuals in complex marital situations, integrating traditional doctrine with lived experience and the Church's mission of mercy, thereby maintaining their connection to the faith community.
% TRANSFER_FUNCTION: Transfers interpretive authority from a strictly codified, immutable doctrine to a process of pastoral discernment in individual cases. This shifts the burden of clarity and consistency onto local clergy and individuals, while transferring perceived doctrinal stability away from traditionalists.
% ABSENT_VOICES: Early Church Fathers, historical canonists, and those who believe in a purely ontological and immutable understanding of marriage. They would argue against any relativization of indissolubility, asserting its absolute and constitutive nature, and would see this reading as a departure from tradition.
% DISAPPEARANCE_RATIONALE: If this pastoral reading vanished, the Church's approach to complex marital situations would revert to a more rigid, less compassionate stance. Many individuals currently seeking discernment would find themselves without a path for reconciliation or full participation in the faith, leading to significant spiritual and social disruption for a large segment of the laity.
% FOUNDING_PROBLEM: The perceived rigidity of traditional marriage doctrine in addressing complex, real-world marital failures and the need for a more merciful, compassionate, and inclusive pastoral approach that acknowledges human frailty and seeks to accompany individuals.
% FOUNDING_PROBLEM_CORROBORATION: Pastoral theologians, ethicists, and many lay Catholics attest to the ongoing need for compassionate discernment in complex marital cases, citing lived experience, contemporary social realities, and the Church's mission of mercy. Doctrinal conservatives dispute this, arguing the problem is one of fidelity to doctrine, not a deficiency in the doctrine itself.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates pastoral care and inclusion for many (beneficiaries) while simultaneously extracting from others (victims) through the perceived erosion of doctrinal stability. Extractiveness is moderate (0.6) due to the spiritual and identity costs borne by traditionalists. Suppression (0.55) arises from the institutional pressure to conform to the pastoral approach, even if not through explicit coercion. The theater ratio (0.4) reflects the balancing act required by clergy to reconcile traditional doctrine with individual pastoral needs, sometimes leading to ambiguous or performative applications of rules. Accessibility collapse (0.7) for traditionalists refers to the perceived loss of clear, unchanging doctrinal alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The pastoral clergy and laity seeking discernment experience this constraint as a beneficial, merciful coordination mechanism, offering a path for spiritual life. In contrast, traditional Catholics and doctrinal conservatives experience the same constraint as an extractive force, undermining their faith identity and the stability of core doctrines. The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Pastoral clergy and laity seeking discernment are beneficiaries (low d) as they gain flexibility and inclusion. Traditional Catholics and doctrinal conservatives are targets (high d) as they bear the costs of doctrinal ambiguity and perceived erosion of tradition. Traditional Catholics are identity-locked due to their deep spiritual commitment, making their exit options severely constrained.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_ambiguity_indissolubility,
    'Is indissolubility an absolute ontological reality of marriage, or a normative ideal requiring compassionate discernment in individual cases?',
    'Further theological development and magisterial clarification, or a shift in the Church''s understanding of sacramental ontology.',
    'If absolute, the extraction from traditionalists is a direct consequence of doctrinal deviation; if an ideal, the extraction is a cost of necessary pastoral adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_ambiguity_indissolubility, conceptual, 'Ambiguity regarding the nature of marriage''s indissolubility.').

omega_variable(
    authority_of_conscience_vs_magisterium,
    'To what extent does individual conscience legitimately interpret doctrine in complex marital situations, and how does this relate to the Magisterium''s teaching authority?',
    'Formal theological pronouncements clarifying the role and limits of conscience in relation to objective moral norms, or a shift in the Church''s governance model.',
    'If conscience holds significant interpretive authority, the pastoral reading is more aligned with individual agency; if Magisterial authority is paramount, the reading''s flexibility is a source of tension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_of_conscience_vs_magisterium, conceptual, 'Tension between individual conscience and institutional teaching authority.').

omega_variable(
    impact_on_doctrinal_stability,
    'Does this pastoral reading genuinely provide compassionate care and inclusion, or does its primary effect for traditionalists remain the erosion of doctrinal stability and clarity?',
    'Longitudinal sociological studies of Catholic communities, surveys of lay faithful across different theological orientations, and analysis of pastoral outcomes.',
    'Empirical evidence of widespread spiritual distress among traditionalists would strengthen the extraction claim; evidence of successful integration and spiritual growth for those seeking discernment would support the coordination claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_on_doctrinal_stability, empirical, 'Empirical impact of the reading on doctrinal stability and spiritual well-being.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__civic_pastoral_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(marr_tr_t6, marriage_sacrament__civic_pastoral_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(marr_tr_t12, marriage_sacrament__civic_pastoral_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(marr_tr_t18, marriage_sacrament__civic_pastoral_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(marr_tr_t24, marriage_sacrament__civic_pastoral_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(marr_tr_t30, marriage_sacrament__civic_pastoral_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_be_t6, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(marr_be_t12, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(marr_be_t18, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(marr_be_t24, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(marr_be_t30, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t6, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(marr_su_t12, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(marr_su_t18, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 18, 0.53).
narrative_ontology:measurement(marr_su_t24, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(marr_su_t30, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_sacrament' kernel, focusing on pastoral discernment. It is linked to the 'hierarchical_indissolubility_reading' which represents a more traditional, ontological interpretation of the same kernel. Their ε values differ significantly due to their distinct structural impacts on various stakeholders.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
