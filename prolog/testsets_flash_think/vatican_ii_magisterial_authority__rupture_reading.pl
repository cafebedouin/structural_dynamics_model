% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Magisterial Authority (Rupture Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'rupture reading' of Vatican II,
 *   which posits that the Council represents a fundamental break with
 *   pre-conciliar Catholic teaching and ecclesiology. The texts are
 *   interpreted as authorizing radical implementation, superseding prior
 *   positions (e.g., 'error has no rights'), legitimizing liturgical
 *   experimentation, and affirming religious freedom (Dignitatis Humanae) as
 *   a doctrinal progress that contradicts prior teaching. This reading is
 *   actively enforced by segments of the institutional Church, leading to
 *   high extraction from those who adhere to traditional interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.8).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.9).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Magisterial Authority (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, '4b903aca-6c56-40eb-be2e-751b6c403a98').
narrative_ontology:cs_kernel_codification('4b903aca-6c56-40eb-be2e-751b6c403a98', fixed_text).
narrative_ontology:cs_authority_grounding('4b903aca-6c56-40eb-be2e-751b6c403a98', lineage).
narrative_ontology:cs_interpretation_layer_present('4b903aca-6c56-40eb-be2e-751b6c403a98').
narrative_ontology:cs_reading_relation('4b903aca-6c56-40eb-be2e-751b6c403a98', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('4b903aca-6c56-40eb-be2e-751b6c403a98', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('4b903aca-6c56-40eb-be2e-751b6c403a98', foundational, doctrinal_development_as_rupture).
narrative_ontology:cs_axiom_status(doctrinal_development_as_rupture, holdable).
narrative_ontology:cs_axiom_grounding('4b903aca-6c56-40eb-be2e-751b6c403a98', doctrinal_development_as_rupture, conventional).
narrative_ontology:cs_axiom('4b903aca-6c56-40eb-be2e-751b6c403a98', foundational, religious_freedom_as_new_doctrine).
narrative_ontology:cs_axiom_status(religious_freedom_as_new_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('4b903aca-6c56-40eb-be2e-751b6c403a98', religious_freedom_as_new_doctrine, deontological).
narrative_ontology:cs_reference_frame('4b903aca-6c56-40eb-be2e-751b6c403a98', post_conciliar_aggiornamento).
narrative_ontology:cs_drift_state('4b903aca-6c56-40eb-be2e-751b6c403a98', contemporary_church_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4b903aca-6c56-40eb-be2e-751b6c403a98', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, vatican_curia_progressive_wing).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, reform_minded_clergy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, conservative_laity).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, doctrinal_progress_narrative).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, aggiornamento_principle).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, religious_freedom_as_new_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promotes and enforces the interpretation of Vatican II as a fundamental break, superseding prior teachings and authorizing radical implementation in liturgy and theology. Benefits from the shift in institutional power and theological direction.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, vatican_curia_progressive_wing, agenda_setter,
    institutional, generational, constrained, global).

% Their academic and pastoral work is validated and promoted by the rupture reading. They gain influence and career opportunities within the Church's institutions by articulating and defending this interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians, beneficiary,
    organized, biographical, mobile, global).

% Embrace the rupture reading as a path to modernize the Church and make it more relevant. They benefit from the perceived legitimacy of their reforms but face career constraints if they deviate too far from the official interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, reform_minded_clergy, beneficiary,
    moderate, biographical, constrained, national).

% Experience the rupture reading as a betrayal of tradition, leading to marginalization, suppression of their liturgical preferences, and accusations of disloyalty. Their deep commitment to pre-conciliar teaching makes leaving the Church an existential crisis.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy, payer,
    moderate, biographical, identity_locked, national).

% Feel alienated by liturgical changes and theological shifts, often struggling to reconcile the new teachings with their faith formation. Their options are to conform, seek out traditionalist communities (often marginalized), or leave the Church.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, conservative_laity, payer,
    powerless, biographical, constrained, local).

% The abstract concept of the teaching authority of the Church, which is the object of interpretation and contestation. It does not act as an agent but is the ground upon which the readings are built.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, magisterial_authority_itself, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_magisterial_authority__rupture_reading, magisterial_authority_itself).

% The body of teachings and pronouncements from before Vatican II, which this reading asserts has been superseded or contradicted. Its 'voice' is actively suppressed in favor of the new interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_magisterium, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_magisterium).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__rupture_reading, vatican_curia_progressive_wing).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to unify the Catholic Church around a new, more modern theological and pastoral vision, adapting its mission and self-understanding to the contemporary world.
% TRANSFER_FUNCTION: Transfers theological authority and interpretive power from traditionalist interpretations to progressive ones, demanding conformity to new liturgical and pastoral practices and marginalizing dissenters.
% ABSENT_VOICES: Pre-conciliar theologians and traditionalist groups whose views are deemed incompatible with the new ecclesiology are structurally excluded from mainstream discourse and institutional influence. They would argue for the immutability of prior teaching.
% DISAPPEARANCE_RATIONALE: If the rupture reading vanished overnight, the entire post-Vatican II institutional structure, theological discourse, and liturgical practices would face a profound crisis of legitimacy and identity, leading to a fundamental reorganization of the Catholic Church.
% FOUNDING_PROBLEM: The Church's perceived irrelevance and isolation in the modern world, necessitating an 'aggiornamento' (updating) to engage with contemporary society and thought.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians, some historians, and secular observers attest that the need for adaptation and engagement with modernity remains a live problem. Traditionalist groups and some conservative historians dispute this, arguing the problem was misdiagnosed or the solution was flawed.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.8) because this reading demands a fundamental shift in theological understanding and pastoral practice, imposing significant costs on those who resist or are deeply formed by pre-conciliar traditions. Suppression is very high (0.9) due to active marginalization, disciplinary actions, and the systematic exclusion of traditionalist viewpoints from official discourse and institutional roles. Theater ratio is moderate (0.4) as there is still a performance of 'organic development' or 'pastoral necessity' to justify the changes, but the core claim is a real, acknowledged break. Accessibility collapse is high (0.85) because pre-conciliar alternatives are officially deemed superseded. Resistance is moderate-high (0.7) due to ongoing, albeit often marginalized, opposition from traditionalist clergy and laity. The measurement series reflects an initial period of increasing enforcement and extraction as the rupture reading gained institutional dominance, followed by stabilization.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, this constraint is a necessary and beneficial adaptation, a 'tangled rope' that coordinates the Church's mission with modernity while requiring some to adapt. From the perspective of victims, it operates as a 'snare,' coercively imposing a new vision that contradicts their understanding of faith and tradition, with little genuine coordination benefit for them.
 *
 * DIRECTIONALITY LOGIC:
 *   The progressive wing of the Vatican Curia, progressive theologians, and reform-minded clergy are beneficiaries, gaining institutional power, academic validation, and freedom for their preferred practices. Traditionalist clergy and conservative laity are victims, bearing the costs of marginalization, loss of familiar practices, and intellectual suppression. The abstract 'magisterial authority' and 'pre-conciliar magisterium' are included as non-agent stakeholders to represent the contested objects of interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately identified as the ''rupture_reading'' of the ''vatican_ii_magisterial_authority'' kernel?',
    'Analysis of primary source documents from proponents of this reading, comparing their explicit claims to the structural delta described.',
    'If misidentified, the entire classification of this constraint would be invalid, requiring re-authoring under the correct kernel/reading identity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the specific kernel and reading being instantiated.').

omega_variable(
    doctrinal_contradiction_acknowledgment,
    'To what extent is the contradiction between conciliar texts (e.g., Dignitatis Humanae) and prior teaching explicitly acknowledged as doctrinal progress by the institutional proponents of this reading?',
    'Content analysis of official magisterial documents, theological commentaries, and statements from key institutional figures over time.',
    'If acknowledgment is less explicit or more nuanced than assumed, the ''rupture'' aspect of the reading is weaker, potentially reducing measured extractiveness and suppression, and shifting the classification towards a ''tangled_rope'' with less clear victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_contradiction_acknowledgment, empirical, 'Assesses the degree of explicit acknowledgment of doctrinal contradiction as progress.').

omega_variable(
    impact_on_lay_belief_vs_clerical_enforcement,
    'Does the rupture reading''s enforcement primarily affect clerical practice and theological discourse, or has it fundamentally reshaped the lived faith and belief of the majority of the Catholic laity?',
    'Sociological studies of Catholic belief and practice, surveys of lay adherence to pre-conciliar vs. post-conciliar theological tenets, and ethnographic research on liturgical preferences.',
    'If the impact on lay belief is less profound than on clerical enforcement, the ''powerless'' and ''constrained'' exit options for conservative laity might be overstated, potentially reducing their effective extraction and shifting the overall classification towards a more contained, clerical-level ''tangled_rope'' rather than a broad ''snare''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_on_lay_belief_vs_clerical_enforcement, empirical, 'Distinguishes the scope of impact between clerical and lay populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1985, 0.4).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2005, 0.39).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1965, 0.65).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1975, 0.75).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1985, 0.8).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2005, 0.79).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2015, 0.8).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2025, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1975, 0.85).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1985, 0.9).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1995, 0.88).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2005, 0.89).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2015, 0.9).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vatican_ii_magisterial_authority' kernel, each representing a distinct interpretation of the Council's relationship to prior Church teaching. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
