% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Text (Popular Sovereignty Reading)
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the 'popular sovereignty' reading of a
 *   constitutional text, where the ultimate authority for constitutional
 *   interpretation and amendment rests with the people (the demos), not with
 *   specific institutional actors like courts or legislatures. It posits that
 *   the constitution's legitimacy flows from the constituent power of the
 *   people, who retain the right to alter or abolish government through
 *   formal (amendment, convention) or informal (revolutionary) means. This
 *   reading emphasizes democratic participation as the primary beneficiary,
 *   while acknowledging potential costs to institutional stability and the
 *   perceived expertise of judicial or legislative bodies.
 *
 * KEY AGENTS:
 *   - the_demos: Primary beneficiary (organized/generational) — retains ultimate authority
 *   - constitutional_scholars: Agenda setter (analytical/generational) — articulate and defend this reading
 *   - judicial_institutions: Payer (institutional/generational) — their claims to supremacy are challenged
 *   - legislative_institutions: Payer (institutional/generational) — their claims to supremacy are challenged
 *   - popular_movements: Beneficiary (organized/biographical) — empowered by this reading
 *   - institutional_stability: Victim (analytical/civilizational) — potentially undermined by constant popular revision
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.4).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.3).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Text (Popular Sovereignty Reading)").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, '280f4379-0d54-4bd2-9adc-224ae2e70f66').
narrative_ontology:cs_kernel_codification('280f4379-0d54-4bd2-9adc-224ae2e70f66', fixed_text).
narrative_ontology:cs_authority_grounding('280f4379-0d54-4bd2-9adc-224ae2e70f66', lineage).
narrative_ontology:cs_interpretation_layer_present('280f4379-0d54-4bd2-9adc-224ae2e70f66').
narrative_ontology:cs_reading_relation('280f4379-0d54-4bd2-9adc-224ae2e70f66', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('280f4379-0d54-4bd2-9adc-224ae2e70f66', constitutional_text__legislative_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('280f4379-0d54-4bd2-9adc-224ae2e70f66', foundational, demos_as_constituent_power).
narrative_ontology:cs_axiom_status(demos_as_constituent_power, holdable).
narrative_ontology:cs_axiom_grounding('280f4379-0d54-4bd2-9adc-224ae2e70f66', demos_as_constituent_power, deontological).
narrative_ontology:cs_axiom('280f4379-0d54-4bd2-9adc-224ae2e70f66', foundational, institutional_subordination_to_popular_will).
narrative_ontology:cs_axiom_status(institutional_subordination_to_popular_will, holdable).
narrative_ontology:cs_axiom_grounding('280f4379-0d54-4bd2-9adc-224ae2e70f66', institutional_subordination_to_popular_will, conventional).
narrative_ontology:cs_reference_frame('280f4379-0d54-4bd2-9adc-224ae2e70f66', founding_moment_of_popular_ratification).
narrative_ontology:cs_drift_state('280f4379-0d54-4bd2-9adc-224ae2e70f66', contemporary_institutional_entrenchment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('280f4379-0d54-4bd2-9adc-224ae2e70f66', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, the_demos).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, democratic_participation).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_stability).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, judicial_expertise).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislative_efficiency).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).
:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.4) because while it empowers the demos, it also imposes costs on institutional actors by subordinating their interpretive authority. Suppression is low (0.3) as this reading actively resists institutional suppression of popular will. Theater ratio is low (0.1) because the claim of popular sovereignty, while sometimes rhetorical, is a foundational principle that drives genuine political action and resistance. Resistance is high (0.7) because this reading is often invoked in opposition to established institutional interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'the_demos' and 'popular_movements', this constraint is a 'rope' that coordinates collective action and legitimizes their ultimate authority. From the perspective of 'judicial_institutions' and 'legislative_institutions', it can be seen as a 'tangled_rope' or even a 'snare' that undermines their established roles and introduces instability, forcing them to contend with extra-institutional pressures.
 *
 * DIRECTIONALITY LOGIC:
 *   'The_demos' and 'democratic_participation' are clear beneficiaries (d=0.0-0.1) as the constraint directly empowers them. 'Judicial_institutions' and 'legislative_institutions' are targets (d=0.8-0.9) as their claims to final authority are challenged. 'Constitutional_scholars' are agenda-setters (d=0.2-0.3) as they articulate and defend this reading, benefiting from its intellectual currency. 'Institutional_stability' is a victim (d=1.0) as it bears the costs of potential disruption.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine popular movements as mere 'resistance' to a legitimate 'mountain' of institutional authority. By asserting the demos as the ultimate source of constitutional meaning, it frames institutional resistance to popular will as a form of extraction, rather than a defense of natural law. It highlights that the 'mandate' of institutions is derived from, and subordinate to, the constituent power of the people.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine expression of popular sovereignty, or a rhetorical cover for institutional power struggles?',
    'Analysis of historical instances of constitutional change: whether popular movements genuinely drove change or were co-opted by institutional actors.',
    'If genuine, it reinforces the ''rope'' classification by highlighting the coordination of popular will. If rhetorical, it suggests a ''tangled_rope'' or ''snare'' where institutional actors benefit from the illusion of popular control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''popular_sovereignty_reading'' of the ''constitutional_text'' kernel. Sibling readings include ''judicial_supremacy_reading'' and ''legislative_sovereignty_reading''. The disagreement is located in the ultimate locus of interpretive authority.').

omega_variable(
    popular_will_vs_institutional_form,
    'How is ''the people''s will'' genuinely expressed and translated into constitutional meaning without being filtered or distorted by existing institutional forms?',
    'Empirical study of constitutional conventions, referenda, and revolutionary moments: identifying mechanisms that genuinely bypass or subordinate institutional interpretation.',
    'If such mechanisms are robust, the constraint functions as a ''rope'' for democratic participation. If they are consistently weak or co-opted, the constraint''s ''popular sovereignty'' aspect becomes performative, pushing it towards ''piton'' or ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_will_vs_institutional_form, empirical, 'The challenge of translating abstract popular will into concrete constitutional action.').

omega_variable(
    stability_vs_flexibility_tradeoff,
    'What is the optimal balance between constitutional stability (favored by institutional readings) and flexibility (favored by popular sovereignty)?',
    'Comparative analysis of constitutional systems with different amendment processes and judicial review powers, assessing long-term democratic health and adaptability.',
    'A preference for stability would push this reading towards a ''tangled_rope'' (acknowledging institutional costs), while a strong preference for flexibility would reinforce ''rope'' (viewing institutional costs as necessary friction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stability_vs_flexibility_tradeoff, preference, 'The inherent tension between constitutional stability and democratic responsiveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__popular_sovereignty_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__popular_sovereignty_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cons_be_t10, constitutional_text__popular_sovereignty_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(cons_be_t20, constitutional_text__popular_sovereignty_reading, base_extractiveness, 20, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cons_su_t10, constitutional_text__popular_sovereignty_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(cons_su_t20, constitutional_text__popular_sovereignty_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, amendment_process_constraint).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, judicial_review_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'constitutional_text' kernel, each with different structural properties and beneficiaries/victims. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
