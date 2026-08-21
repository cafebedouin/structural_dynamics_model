% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Total War Winnability: Strategic Culture Drift (Post-1945)
 *   domain: international_relations_theory/strategic_studies/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint describes the ideational shift in strategic culture
 *   post-1945, where the concept of 'total war' as a winnable or viable
 *   strategic option largely dropped from elite discourse, despite its
 *   physical possibility. It is a reading of the
 *   'total_war_winnability_post1945' kernel, focusing on the role of
 *   strategic culture rather than structural factors (nuclear weapons) or
 *   normative shifts (international law). The constraint operates as a Piton
 *   because its primary function (preventing total war by making it
 *   unthinkable) has atrophied into an inertial, performative maintenance of
 *   a limited war paradigm, extracting strategic flexibility without clear,
 *   active beneficiaries beyond the intellectual class invested in the status
 *   quo.
 *
 * KEY AGENTS:
 *   - defense_intellectuals_limited_war: Agenda setter/Beneficiary (institutional/identity_locked) — shapes and benefits from the prevailing strategic culture.
 *   - military_planners: Payer (organized/constrained) — constrained in their strategic options by the ideational shift.
 *   - political_leaders: Payer (powerful/constrained) — operate within the narrowed strategic discourse.
 *   - strategic_culture_scholars: Observer (analytical/analytical) — analyze the ideational shift.
 *   - advocates_for_total_war_preparedness: Excluded (powerless/constrained) — voices outside the mainstream discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.65).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.7).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.65).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Total War Winnability: Strategic Culture Drift (Post-1945)").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations_theory/strategic_studies/commitment_system_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, 'c79187b8-860e-4346-994b-46656ed7217d').
narrative_ontology:cs_kernel_codification('c79187b8-860e-4346-994b-46656ed7217d', implicit).
narrative_ontology:cs_authority_grounding('c79187b8-860e-4346-994b-46656ed7217d', practice).
narrative_ontology:cs_interpretation_layer_present('c79187b8-860e-4346-994b-46656ed7217d').
narrative_ontology:cs_reading_relation('c79187b8-860e-4346-994b-46656ed7217d', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('c79187b8-860e-4346-994b-46656ed7217d', total_war_winnability_post1945__structural_contraction_reading, forecloses).
narrative_ontology:cs_axiom('c79187b8-860e-4346-994b-46656ed7217d', foundational, strategic_culture_determines_strategic_feasibility).
narrative_ontology:cs_axiom_status(strategic_culture_determines_strategic_feasibility, holdable).
narrative_ontology:cs_axiom_grounding('c79187b8-860e-4346-994b-46656ed7217d', strategic_culture_determines_strategic_feasibility, empirically_contingent).
narrative_ontology:cs_axiom('c79187b8-860e-4346-994b-46656ed7217d', secondary, elite_discourse_shapes_strategic_options).
narrative_ontology:cs_axiom_status(elite_discourse_shapes_strategic_options, holdable).
narrative_ontology:cs_axiom_grounding('c79187b8-860e-4346-994b-46656ed7217d', elite_discourse_shapes_strategic_options, empirically_contingent).
narrative_ontology:cs_reference_frame('c79187b8-860e-4346-994b-46656ed7217d', post_nuclear_limited_war_paradigm).
narrative_ontology:cs_drift_state('c79187b8-860e-4346-994b-46656ed7217d', contemporary_strategic_discourse, gap(stable, severe, true)).
narrative_ontology:cs_created_at('c79187b8-860e-4346-994b-46656ed7217d', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, military_planners).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, political_leaders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These intellectuals and think tanks benefit from the prevailing strategic culture that frames total war as unwinnable or unthinkable. Their careers, publications, and influence are built upon frameworks of limited war, deterrence, and crisis management, which this ideational constraint reinforces. Exiting this framework would mean challenging foundational assumptions of their professional identity.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war, agenda_setter,
    institutional, generational, identity_locked, global).

% Military planners are constrained by the dominant strategic culture, which limits the scope of their planning and doctrine development. While total war capabilities may exist, the ideational constraint means they cannot realistically plan for its 'winnability,' potentially leading to a mismatch between material capacity and strategic flexibility. Their exit options are limited by professional norms and institutional directives.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, military_planners, payer,
    organized, biographical, constrained, national).

% Political leaders operate within the strategic culture shaped by defense intellectuals. This ideational constraint narrows their perceived range of strategic options in times of conflict, making it difficult to articulate or pursue strategies that deviate from the limited war paradigm, even if circumstances might warrant a broader consideration. Their political capital is tied to adhering to established strategic norms.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, political_leaders, payer,
    powerful, immediate, constrained, national).

% Academics who study strategic culture analyze the evolution and impact of these ideational shifts. They observe the constraint's operation and its effects on policy and planning, but are not directly subject to its extractive or coordinative functions in the same way as practitioners.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_culture_scholars, observer,
    analytical, generational, analytical, global).

% These are voices, often outside mainstream defense establishments, who argue for the continued relevance or necessity of planning for total war scenarios, or who challenge the 'unwinnable' narrative. They are largely excluded from elite discourse, their arguments often dismissed as anachronistic or dangerous, reinforcing the ideational constraint.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, advocates_for_total_war_preparedness, excluded,
    powerless, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__strategic_culture_drift, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates strategic thinking among elites to focus on limited war and deterrence, thereby reducing the perceived likelihood and planning for global, existential conflicts.
% TRANSFER_FUNCTION: Transfers intellectual legitimacy, academic attention, and policy resources away from total war planning and towards limited war frameworks, benefiting those invested in the latter.
% ABSENT_VOICES: Advocates for total war preparedness or those who challenge the 'unwinnable' narrative are largely absent from elite strategic discourse, dismissed as outmoded or dangerous. They would argue for a more comprehensive and less ideologically constrained approach to strategic planning.
% DISAPPEARANCE_RATIONALE: If the ideational constraint vanished overnight, strategic discourse would immediately broaden, leading to a re-evaluation of military doctrines, resource allocation for different types of conflict, and potentially a shift in how political leaders perceive and articulate options in crises. The intellectual landscape of strategic studies would be fundamentally reorganized.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons post-1945 created a perceived necessity to constrain strategic thought, moving away from the pre-nuclear concept of total war as a viable, winnable option, to prevent global annihilation.
% FOUNDING_PROBLEM_CORROBORATION: While the initial problem (nuclear threat) is widely acknowledged by historians and strategic scholars, the current status of the ideational constraint is contested. Mainstream defense establishments implicitly maintain the framework, while some critical security scholars and historians argue the constraint has become an inertial force, overshooting its original utility. No single external corroboration exists for its 'live' status beyond the benefiting parties' implicit maintenance.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the cost of lost strategic flexibility and the narrowing of intellectual space. Suppression (0.70) is high because the ideational constraint actively marginalizes alternative strategic thinking through professional norms and discourse exclusion, even without formal enforcement. The theater ratio (0.75) is very high because the 'unwinnable' narrative is largely maintained through intellectual inertia and performance, rather than a continuous, active re-evaluation of its premises. Resistance is low (0.20) because the ideational shift is deeply embedded and few actively challenge it from within the mainstream. Accessibility collapse (0.60) is moderate; while thinking about total war is not impossible, it is professionally discouraged and difficult to publish or gain traction for such ideas.
 *
 * PERSPECTIVAL GAP:
 *   Defense intellectuals, as beneficiaries, perceive this constraint as a necessary and beneficial evolution of strategic thought, a 'rope' that prevents catastrophic conflict. Military planners and political leaders, as payers, experience it as a 'snare' or 'tangled rope' that limits their options and forces them into suboptimal strategic postures, even if they acknowledge the initial rationale. The engine's classification as Piton reflects the atrophied function and inertial persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Defense intellectuals are beneficiaries (low d) as their professional identity and influence are tied to the limited war paradigm. Military planners and political leaders are payers (high d) as their strategic flexibility is extracted. Strategic culture scholars are observers (d=0.5). Advocates for total war preparedness are excluded, experiencing high d due to their marginalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Piton prevents mislabeling this as a Rope (genuine coordination) or Snare (active, concentrated extraction). While it once served a vital coordination function (preventing nuclear war), its current persistence is more due to institutional inertia and the self-reinforcing nature of strategic culture, with diffuse benefits to the intellectual class and diffuse costs to strategic flexibility. The 'unwinnable' narrative has become largely theatrical, maintained without a clear, active mandate beyond its own perpetuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ideational_vs_structural_causation,
    'To what extent is the decline in total war discourse due to ideational shifts in strategic culture (this reading) versus the structural reality of nuclear weapons (structural_contraction_reading)?',
    'Counterfactual historical analysis: examining periods where nuclear parity existed but strategic culture varied, or where non-nuclear powers considered total war. Also, analysis of contemporary military capabilities and doctrines for non-nuclear total war scenarios.',
    'If structural factors are dominant, this constraint''s extractiveness might be lower (as the ''unwinnable'' claim is more objectively true), and its classification might shift towards Mountain or Rope. If ideational factors are dominant, the Piton classification is reinforced, highlighting the constructed nature of the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ideational_vs_structural_causation, empirical, 'Distinguishing ideational from structural causes for total war''s decline.').

omega_variable(
    ideational_vs_normative_causation,
    'To what extent is the decline in total war discourse due to ideational shifts (this reading) versus the development of international normative frameworks (normative_reading_drop)?',
    'Comparative legal and strategic analysis: examining how strategic cultures in different states respond to international legal norms, and whether adherence to norms precedes or follows ideational shifts.',
    'If normative factors are dominant, the constraint''s suppression might be higher (due to legal enforcement), and its classification might lean towards Tangled Rope. If ideational factors are dominant, the Piton classification is reinforced, emphasizing the internal, cultural nature of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideational_vs_normative_causation, empirical, 'Distinguishing ideational from normative causes for total war''s decline.').

omega_variable(
    mandate_obsolescence_degree,
    'Has the original mandate (preventing nuclear total war) become entirely obsolete, or does the ideational constraint still serve a residual, albeit atrophied, function?',
    'Expert consensus survey among strategic scholars and military planners regarding the current utility of the ''unwinnable'' narrative in preventing escalation, versus its role in merely maintaining a status quo.',
    'If the mandate is entirely obsolete, the Piton classification is strongly confirmed. If a residual function is identified, the constraint might lean towards a degraded Rope or a very low-extraction Tangled Rope, indicating some lingering coordination utility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence_degree, conceptual, 'Degree of obsolescence of the constraint''s original mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1965, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(tota_tr_t1985, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1985, 0.5).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2005, 0.65).
narrative_ontology:measurement(tota_tr_t2024, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2024, 0.75).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1945, 0.3).
narrative_ontology:measurement(tota_be_t1965, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(tota_be_t1985, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(tota_be_t2024, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(tota_su_t1965, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement(tota_su_t1985, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(tota_su_t2024, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, identity_coordination).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, nuclear_deterrence_doctrine).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, limited_war_doctrine).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, arms_control_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
