% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__narrow_armed_attack_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: UN Charter Article 51: Narrow Armed Attack Reading
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint represents the narrow interpretation of Article 51 of the
 *   UN Charter, which limits the right of self-defense to responses to an
 *   actual or imminent armed attack by a state, attributable under
 *   international law. It is a reading of the 'article_51_self_defense'
 *   kernel, emphasizing state attribution and a high threshold for the use of
 *   force. This reading aims to preserve the UN Security Council's primary
 *   role in maintaining international peace and security by restricting
 *   unilateral state action.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.6).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.7).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "UN Charter Article 51: Narrow Armed Attack Reading").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, '0b3edfc9-bcc8-4349-a7ee-896231f87128').
narrative_ontology:cs_kernel_codification('0b3edfc9-bcc8-4349-a7ee-896231f87128', fixed_text).
narrative_ontology:cs_authority_grounding('0b3edfc9-bcc8-4349-a7ee-896231f87128', lineage).
narrative_ontology:cs_interpretation_layer_present('0b3edfc9-bcc8-4349-a7ee-896231f87128').
narrative_ontology:cs_reading_relation('0b3edfc9-bcc8-4349-a7ee-896231f87128', article_51_self_defense__expansive_preventive_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b3edfc9-bcc8-4349-a7ee-896231f87128', article_51_self_defense__unable_unwilling_doctrine_reading, coexists_with).
narrative_ontology:cs_axiom('0b3edfc9-bcc8-4349-a7ee-896231f87128', foundational, force_only_against_state_armed_attack).
narrative_ontology:cs_axiom_status(force_only_against_state_armed_attack, holdable).
narrative_ontology:cs_axiom_grounding('0b3edfc9-bcc8-4349-a7ee-896231f87128', force_only_against_state_armed_attack, deontological).
narrative_ontology:cs_axiom('0b3edfc9-bcc8-4349-a7ee-896231f87128', foundational, unsc_primary_authority_on_force).
narrative_ontology:cs_axiom_status(unsc_primary_authority_on_force, holdable).
narrative_ontology:cs_axiom_grounding('0b3edfc9-bcc8-4349-a7ee-896231f87128', unsc_primary_authority_on_force, conventional).
narrative_ontology:cs_reference_frame('0b3edfc9-bcc8-4349-a7ee-896231f87128', post_un_charter_prohibition_on_force).
narrative_ontology:cs_drift_state('0b3edfc9-bcc8-4349-a7ee-896231f87128', post_9_11_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('0b3edfc9-bcc8-4349-a7ee-896231f87128', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, international_legal_order).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, powerful_states_seeking_unilateral_force).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, states_facing_non_state_actor_threats).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary body for authorizing the use of force under international law, whose authority is preserved by a narrow interpretation of Article 51. It adjudicates claims of self-defense and can authorize collective action.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, united_nations_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the constraint on powerful states' unilateral use of force, as it provides a measure of protection against intervention and aggression, channeling disputes through multilateral mechanisms.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_states, beneficiary,
    powerless, generational, trapped, global).

% Bear the cost of constrained strategic freedom, as they cannot easily use force preemptively or against non-state actors without clear state attribution, requiring Security Council authorization or a direct armed attack.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, powerful_states_seeking_unilateral_force, payer,
    powerful, biographical, constrained, global).

% Are constrained in their ability to respond to threats from non-state actors operating from other territories, unless those actions are directly attributable to the host state, which can be difficult to prove.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, states_facing_non_state_actor_threats, payer,
    moderate, biographical, constrained, national).

% Analyze and interpret Article 51, advocating for a strict construction to uphold the UN Charter's prohibition on the use of force, contributing to the normative pressure that sustains this reading.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the legitimate use of force in international relations, preventing unilateral military action and channeling disputes through the UN Security Council, thereby maintaining international peace and security.
% TRANSFER_FUNCTION: Transfers authority over the use of force from individual states to the collective security mechanism of the UN, particularly the Security Council, limiting states' strategic autonomy in exchange for collective stability.
% ABSENT_VOICES: States that believe they face existential threats from non-state actors, or those who advocate for a more robust right of self-preservation, are often marginalized in the discourse that upholds this narrow reading. They would argue for a more flexible interpretation of 'armed attack' and 'imminence'.
% DISAPPEARANCE_RATIONALE: If this narrow reading vanished, powerful states would likely assert broader rights to use force preemptively or against non-state actors, leading to increased unilateral interventions, a weakening of the UN Security Council's authority, and a more unstable international system with greater risk of interstate conflict.
% FOUNDING_PROBLEM: The UN Charter was established to prevent a recurrence of world wars by prohibiting the unilateral use of force and establishing a collective security system, with self-defense as a narrow exception.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing need to prevent interstate aggression and maintain international peace, attested by the UN's continued operations and the persistent efforts of international legal bodies and most states, corroborates the founding problem's live status. While powerful states may contest its application, the underlying problem remains.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is moderate, reflecting the significant strategic freedom powerful states forgo by adhering to this reading. Suppression (0.7) is high due to the normative and institutional pressure exerted by the international legal system and multilateral bodies to prevent broader interpretations. Theater ratio (0.2) is relatively low, as the core function of limiting unilateral force is still actively pursued, though some states may rhetorically adhere while seeking loopholes. The post-9/11 period saw a rise in extractiveness and suppression as states grappled with non-state actor threats, leading to pressure for broader interpretations, but the narrow reading has largely reasserted itself.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states often experience this constraint as a 'snare' on their strategic freedom, while weaker states and international legalists perceive it as a 'rope' or 'mountain' that protects the international order. The engine's per-seat classification will reflect this divergence based on the declared power, exit options, and beneficiary/victim status.
 *
 * DIRECTIONALITY LOGIC:
 *   Weaker states and multilateral institutions are beneficiaries (low d) as this reading protects them from unilateral intervention and upholds collective security. Powerful states seeking unilateral force and states facing non-state actor threats are victims (high d) as their strategic options are constrained. The UN Security Council acts as the agenda-setter, enforcing this reading to maintain its authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_attribution_ambiguity,
    'What level of state involvement or control is required for non-state actor actions to be ''attributable'' to a state under international law, thereby triggering Article 51 self-defense?',
    'Further ICJ advisory opinions or state practice solidifying a clear threshold for attribution, or a new UN General Assembly resolution clarifying the standard.',
    'A clearer, higher threshold for attribution would strengthen this narrow reading, increasing its suppressive effect on states. A lower, more flexible threshold would weaken it, allowing more unilateral responses to non-state actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_attribution_ambiguity, empirical, 'Ambiguity in attributing non-state actor actions to states.').

omega_variable(
    imminence_standard_divergence,
    'Is the ''imminence'' standard for an armed attack a strict temporal proximity requirement, or does it encompass a broader ''accumulation of events'' or ''continuing threat'' perspective?',
    'ICJ rulings or widespread state practice explicitly defining the temporal and qualitative scope of ''imminence'' in contemporary security contexts.',
    'A strict temporal reading reinforces the narrow constraint, limiting preemptive action. A broader reading would allow more flexibility, moving towards the ''expansive_preventive_reading'' and reducing the constraint''s extractiveness on powerful states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminence_standard_divergence, conceptual, 'Divergence in the interpretation of ''imminence'' for armed attacks.').

omega_variable(
    narrow_vs_expansive_framing,
    'Is this constraint a genuine ''tangled_rope'' coordinating state behavior while extracting strategic freedom, or is it a ''snare'' for powerful states whose legitimate security concerns are suppressed by an outdated framework?',
    'Long-term observation of state compliance patterns and the frequency of unilateral force outside UN authorization. If unilateral force becomes the norm, the ''tangled_rope'' framing weakens.',
    'If reclassified as a ''snare'', it would imply the coordination function is largely cover for extraction, and the international legal order is failing to adapt to contemporary threats, justifying greater resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrow_vs_expansive_framing, preference, 'Whether the constraint is a legitimate coordination mechanism or an outdated snare.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(arti_tr_t1965, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(arti_tr_t1985, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2001, 0.3).
narrative_ontology:measurement(arti_tr_t2010, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(arti_be_t1965, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(arti_be_t1985, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2001, 0.65).
narrative_ontology:measurement(arti_be_t2010, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(arti_su_t1965, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1965, 0.65).
narrative_ontology:measurement(arti_su_t1985, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2001, 0.75).
narrative_ontology:measurement(arti_su_t2010, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, unable_unwilling_doctrine_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, un_security_council_veto_power).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'article_51_self_defense' kernel. Its narrow interpretation of 'armed attack' and state attribution directly influences the scope and legitimacy of other interpretations, such as the 'expansive_preventive_reading' and the 'unable_unwilling_doctrine_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
