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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Authority from Popular Sovereignty
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint represents the 'popular sovereignty' reading of
 *   constitutional authority, where the constitutional text derives its
 *   ultimate legitimacy from the constituent power of the people (the demos).
 *   In this view, neither the judiciary nor the legislature holds supreme
 *   interpretive authority; rather, the people retain ultimate control
 *   through mechanisms like amendment, constitutional conventions, or even
 *   revolution. This reading emphasizes democratic participation as the
 *   primary beneficiary, while institutional stability and the exclusive
 *   claims of institutional expertise are seen as 'victims' or costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.45).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.55).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Authority from Popular Sovereignty").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, '34b1c812-021b-49ce-89f9-06de814ea38c').
narrative_ontology:cs_kernel_codification('34b1c812-021b-49ce-89f9-06de814ea38c', fixed_text).
narrative_ontology:cs_authority_grounding('34b1c812-021b-49ce-89f9-06de814ea38c', lineage).
narrative_ontology:cs_interpretation_layer_present('34b1c812-021b-49ce-89f9-06de814ea38c').
narrative_ontology:cs_reading_relation('34b1c812-021b-49ce-89f9-06de814ea38c', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('34b1c812-021b-49ce-89f9-06de814ea38c', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('34b1c812-021b-49ce-89f9-06de814ea38c', foundational, ultimate_sovereignty_resides_in_the_people).
narrative_ontology:cs_axiom_status(ultimate_sovereignty_resides_in_the_people, holdable).
narrative_ontology:cs_axiom_grounding('34b1c812-021b-49ce-89f9-06de814ea38c', ultimate_sovereignty_resides_in_the_people, deontological).
narrative_ontology:cs_axiom('34b1c812-021b-49ce-89f9-06de814ea38c', secondary, institutional_power_is_delegated_not_original).
narrative_ontology:cs_axiom_status(institutional_power_is_delegated_not_original, holdable).
narrative_ontology:cs_axiom_grounding('34b1c812-021b-49ce-89f9-06de814ea38c', institutional_power_is_delegated_not_original, conventional).
narrative_ontology:cs_reference_frame('34b1c812-021b-49ce-89f9-06de814ea38c', constituent_power_as_origin).
narrative_ontology:cs_drift_state('34b1c812-021b-49ce-89f9-06de814ea38c', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('34b1c812-021b-49ce-89f9-06de814ea38c', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, the_demos).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, democratic_participation).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, courts).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislature).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_stability).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_expertise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective body of citizens who, in this reading, are the ultimate source of constitutional authority. They benefit from the recognition of their constituent power and the right to amend, convene, or revolutionize the constitutional order. Their identity is fused with the concept of popular sovereignty.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, the_demos, beneficiary,
    organized, generational, identity_locked, national).

% Judicial bodies whose interpretive authority is, under this reading, subordinate to the ultimate will of the people. They bear the cost of not having final say on constitutional meaning, facing potential override by popular action. Their exit is constrained by their institutional role within the constitutional framework.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, courts, payer,
    institutional, generational, constrained, national).

% Legislative bodies whose law-making and constitutional amendment powers are derived from, and ultimately subordinate to, the constituent power of the people. They bear the cost of not being the supreme authority, facing potential popular challenges to their enactments. Their exit is constrained by their institutional role.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislature, payer,
    institutional, generational, constrained, national).

% Academics and theorists who analyze and debate the nature of constitutional authority. They observe the interplay between popular will and institutional power, often advocating for or against this reading based on historical, philosophical, or practical arguments.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% Those who benefit from a more stable, institutionally-driven constitutional order and often resist direct popular intervention in constitutional matters. They are excluded from the direct exercise of ultimate interpretive authority by this reading, and their power is challenged by it.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, political_elites, excluded,
    powerful, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__popular_sovereignty_reading, the_demos).
narrative_ontology:fixing_cost_class(constitutional_text__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective will of the people as the ultimate source of constitutional authority, providing mechanisms (amendment, convention, revolution) for popular expression to shape or reshape the fundamental law.
% TRANSFER_FUNCTION: Transfers ultimate interpretive and constituent authority from established governmental institutions (courts, legislature) to the collective body of the people (the demos).
% ABSENT_VOICES: Those who advocate for judicial or legislative supremacy, or for a more rigid, less mutable constitutional order, are often absent from the direct exercise of popular constituent power. They would argue for institutional stability and expertise over direct popular will.
% DISAPPEARANCE_RATIONALE: If the principle of popular sovereignty as ultimate constitutional authority vanished, the entire constitutional order would fundamentally shift. Institutions would likely assert greater, potentially unchecked, power, and the mechanisms for popular constitutional change would lose their legitimacy, leading to a reordering of political power.
% FOUNDING_PROBLEM: To prevent tyranny, ensure self-governance, and establish a constitutional order that genuinely reflects the will of the governed, rather than being imposed by an elite or a past generation.
% FOUNDING_PROBLEM_CORROBORATION: Political philosophers from Locke to Rousseau, historical revolutionary movements, and contemporary democratic theorists attest to the ongoing relevance of this problem. While institutions often assert their own authority, the underlying tension between popular will and institutional power remains a live issue in constitutional debates globally.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.45) is moderate: it extracts from institutional claims of supremacy but empowers the people. Suppression (0.55) is also moderate; while the reading itself is anti-suppressive for the demos, institutional resistance to popular will can manifest as suppression. The theater ratio (0.20) is low, as this reading is often a genuine, active political claim rather than a mere performance. Accessibility collapse (0.40) is moderate, as alternative readings (judicial/legislative supremacy) are still live. Resistance (0.70) is high because this reading inherently challenges established institutional power structures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'the demos', this constraint is a fundamental right and a source of empowerment, leading to a low effective extraction. From the perspective of 'courts' and 'legislature', it represents a constant challenge to their authority and a potential extraction of their claimed supremacy, leading to a higher effective extraction. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   'The demos' and 'democratic_participation' are the primary beneficiaries, as the constraint asserts their ultimate authority (low directionality). 'Courts' and 'legislature' are payers/victims, as their authority is explicitly subordinated (higher directionality). 'Political_elites' are excluded, as their power is challenged by direct popular sovereignty.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently resists mandatrophy by asserting the people's continuous right to revise or replace the constitutional order. The 'founding_problem_status' being 'live' and the 'disappearance_verdict' being 'world_rearranges' further indicate that the constraint's function (ensuring popular self-governance) is considered ongoing and vital, preventing it from degrading into a mere Piton or Snare without a clear mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_the_demos,
    'Who constitutes ''the people'' (the demos) in practice, and how is their ''will'' genuinely ascertained, beyond institutional representation?',
    'Empirical study of constitutional conventions, referenda, and revolutionary movements; philosophical analysis of collective agency and representation.',
    'If ''the people'' is found to be consistently co-opted or manipulated by elites, the effective extractiveness from the actual populace would be higher, and the constraint might reclassify towards a Snare or Tangled Rope for the broader population, despite its claimed intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_the_demos, conceptual, 'Ambiguity in the practical definition and expression of ''the people''s will''.').

omega_variable(
    stability_vs_legitimacy_tradeoff,
    'What is the optimal balance between institutional stability (which this reading ''victims'') and the ongoing democratic legitimacy derived from popular sovereignty?',
    'Comparative constitutional studies examining polities with varying degrees of popular constitutional intervention; political philosophy debates on the value of stability versus responsiveness.',
    'If high institutional instability is shown to consistently undermine fundamental rights or effective governance, the ''victim'' status of institutional stability would be amplified, potentially shifting the constraint''s classification towards a more problematic type (e.g., Snare for the broader society due to chaos).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stability_vs_legitimacy_tradeoff, preference, 'The inherent tension between constitutional stability and continuous popular legitimation.').

omega_variable(
    revolutionary_legitimacy_threshold,
    'What constitutes a legitimate ''revolution'' as an exercise of popular constituent power, and how is it distinguished from mere insurrection or civil unrest?',
    'Historical analysis of successful and failed revolutions; legal and political theory on the conditions for legitimate extra-constitutional change.',
    'If the threshold for legitimate revolution is too low or ill-defined, the constraint could be seen as promoting instability rather than legitimate change, increasing its perceived extractiveness from institutional order. If too high, it might effectively suppress popular will, increasing extractiveness from the demos.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revolutionary_legitimacy_threshold, conceptual, 'Defining the boundary between legitimate popular revolution and illegitimate unrest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__popular_sovereignty_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__popular_sovereignty_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__popular_sovereignty_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__popular_sovereignty_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__popular_sovereignty_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cons_be_t10, constitutional_text__popular_sovereignty_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(cons_be_t20, constitutional_text__popular_sovereignty_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(cons_be_t30, constitutional_text__popular_sovereignty_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(cons_be_t40, constitutional_text__popular_sovereignty_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(cons_be_t50, constitutional_text__popular_sovereignty_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cons_su_t10, constitutional_text__popular_sovereignty_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(cons_su_t20, constitutional_text__popular_sovereignty_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(cons_su_t30, constitutional_text__popular_sovereignty_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(cons_su_t40, constitutional_text__popular_sovereignty_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(cons_su_t50, constitutional_text__popular_sovereignty_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'constitutional_text' kernel. This 'popular_sovereignty_reading' asserts the ultimate authority of the people, directly challenging the claims of judicial and legislative supremacy found in the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
