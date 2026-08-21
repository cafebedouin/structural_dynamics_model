% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Border Legitimacy: Humanitarian Obligation Reading
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint represents the 'humanitarian obligation' reading of
 *   border legitimacy, which posits that states have a moral and legal duty
 *   to admit those fleeing persecution or disaster (refugees), but not
 *   general economic migrants. This distinction, codified in international
 *   law, aims to balance state sovereignty with humanitarian concerns.
 *   However, its application creates a bifurcated victim set and involves
 *   significant extraction from those deemed 'economic migrants,' requiring
 *   active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.65).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.78).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Border Legitimacy: Humanitarian Obligation Reading").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, 'a945d2d2-8cc9-4c56-9e07-ac91c7e94d7b').
narrative_ontology:cs_kernel_codification('a945d2d2-8cc9-4c56-9e07-ac91c7e94d7b', formalized).
narrative_ontology:cs_authority_grounding('a945d2d2-8cc9-4c56-9e07-ac91c7e94d7b', lineage).
narrative_ontology:cs_interpretation_layer_present('a945d2d2-8cc9-4c56-9e07-ac91c7e94d7b').
narrative_ontology:cs_reading_relation('a945d2d2-8cc9-4c56-9e07-ac91c7e94d7b', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('a945d2d2-8cc9-4c56-9e07-ac91c7e94d7b', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_axiom('a945d2d2-8cc9-4c56-9e07-ac91c7e94d7b', foundational, moral_duty_to_protect_persecuted).
narrative_ontology:cs_axiom_status(moral_duty_to_protect_persecuted, holdable).
narrative_ontology:cs_axiom_grounding('a945d2d2-8cc9-4c56-9e07-ac91c7e94d7b', moral_duty_to_protect_persecuted, deontological).
narrative_ontology:cs_axiom('a945d2d2-8cc9-4c56-9e07-ac91c7e94d7b', foundational, state_right_to_control_economic_migration).
narrative_ontology:cs_axiom_status(state_right_to_control_economic_migration, holdable).
narrative_ontology:cs_axiom_grounding('a945d2d2-8cc9-4c56-9e07-ac91c7e94d7b', state_right_to_control_economic_migration, conventional).
narrative_ontology:cs_reference_frame('a945d2d2-8cc9-4c56-9e07-ac91c7e94d7b', post_wwii_refugee_convention_framework).
narrative_ontology:cs_drift_state('a945d2d2-8cc9-4c56-9e07-ac91c7e94d7b', contemporary_global_crises_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a945d2d2-8cc9-4c56-9e07-ac91c7e94d7b', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, states_admitting_refugees).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, refugees_fleeing_persecution).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, states_bordering_crises).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states define and enforce the distinction between refugees and economic migrants, admitting the former under international law. They benefit from maintaining a managed border and a degree of moral legitimacy, but bear the costs of processing and integrating refugees.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, states_admitting_refugees, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, states_admitting_refugees, beneficiary).

% These individuals are granted protection and asylum under this framework, escaping immediate danger. However, they often face arduous journeys, legal uncertainty, and the trauma of displacement, making their 'benefit' conditional and costly.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, refugees_fleeing_persecution, beneficiary,
    powerless, immediate, trapped, global).

% These individuals are categorically excluded from legal entry under this framework, despite often facing severe economic hardship or indirect consequences of conflict/disaster. They bear the full cost of exclusion, often resorting to dangerous irregular migration routes.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, immediate, trapped, global).

% These states are often the first point of arrival for both refugees and economic migrants. They bear a disproportionate burden of initial processing, housing, and managing large populations, often with limited international support, making them victims of the system's uneven distribution of costs.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, states_bordering_crises, payer,
    institutional, biographical, constrained, regional).

% These organizations monitor state compliance with humanitarian obligations, advocate for refugee rights, and challenge the restrictive interpretations of the distinction. They provide an external analytical and advocacy seat.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, international_human_rights_advocates, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__humanitarian_obligation_reading, diffuse).
narrative_ontology:fixing_cost_class(border_legitimacy__humanitarian_obligation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate an international response to humanitarian crises by defining who has a right to asylum and channeling protection and aid to those fleeing persecution or disaster, thereby preventing mass statelessness and unmanaged displacement.
% TRANSFER_FUNCTION: Transfers the burden of care and protection for refugees from crisis zones to admitting states, while simultaneously denying legal entry and opportunity to economic migrants, effectively transferring the cost of their exclusion onto them.
% ABSENT_VOICES: Economic migrants are largely absent from the international legal and political discourse that defines their exclusion. Their perspectives on the legitimacy of the distinction and the alternatives to their exclusion are not systematically heard or integrated.
% DISAPPEARANCE_RATIONALE: If the distinction between refugees and economic migrants, and the associated obligations, vanished overnight, states would either face overwhelming, undifferentiated migration flows with no legal basis for control, or would have no legal basis to admit anyone, leading to a collapse of the international refugee system and a fundamental re-evaluation of all border controls and national sovereignty claims.
% FOUNDING_PROBLEM: The post-World War II displacement crisis and the need for an international legal framework to protect individuals fleeing persecution, while allowing sovereign states to manage their populations and economic migration.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing existence of armed conflicts, political persecution, and natural disasters, alongside the continued operation of international conventions (e.g., 1951 Refugee Convention) and UN agencies (UNHCR), corroborates the live status of the founding problem. However, the application of the distinction is increasingly contested by human rights advocates and states bordering crises.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate-high because while it provides a vital lifeline for refugees, it systematically excludes and disadvantages economic migrants, often with severe consequences. Suppression (0.78) is high due to the active and often militarized enforcement of borders to maintain this distinction. The theater ratio (0.15) is low, as border enforcement is largely functional in its stated aim of controlling entry, even if the underlying distinction is contested. Accessibility collapse (0.70) is high for economic migrants, whose alternatives are severely limited, but lower for refugees who have a legal pathway, albeit often difficult. Resistance (0.60) is substantial, coming from both migrants themselves and human rights advocates challenging the distinction's application.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of admitting states, this constraint is a necessary and legitimate balance between sovereignty and humanitarianism. From the perspective of economic migrants, it is an arbitrary and unjust exclusion. States bordering crises often view it as an inequitable distribution of international responsibility. The engine's per-seat classification will reflect these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   States admitting refugees are beneficiaries as they maintain control over their borders while fulfilling a moral duty. Refugees fleeing persecution are also beneficiaries, as they gain protection. However, economic migrants are clear targets/victims, facing categorical exclusion. States bordering crises are also victims, bearing the disproportionate burden of managing initial flows. International human rights advocates act as observers, challenging the system's fairness.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    refugee_economic_migrant_distinction_ambiguity,
    'Is the distinction between refugees and economic migrants genuinely clear and stable, or is it increasingly blurred by factors like climate change, generalized violence, and economic precarity?',
    'Empirical analysis of migration drivers and legal outcomes: if a significant portion of ''economic migrants'' are found to be fleeing conditions that are functionally indistinguishable from persecution or disaster, the distinction''s clarity is undermined.',
    'If the distinction is substantially blurred, the constraint''s effective extractiveness from ''economic migrants'' is higher than measured, as it denies protection to those who functionally need it. This would push the classification closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_economic_migrant_distinction_ambiguity, empirical, 'Clarity and stability of the refugee/economic migrant distinction.').

omega_variable(
    burden_sharing_equity,
    'Is the burden of humanitarian obligation equitably shared among states, or does it fall disproportionately on frontline states and those with less capacity?',
    'Quantitative analysis of refugee intake, processing costs, and international aid distribution per state, relative to GDP and population.',
    'If the burden is highly inequitable, the ''beneficiary'' status of states admitting refugees is partially undermined, as the system''s coordination function is revealed to be highly asymmetric in its cost distribution, pushing the classification towards a more extractive Tangled Rope or Snare for the ''states_bordering_crises'' seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_sharing_equity, preference, 'Equity of burden sharing in humanitarian migration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1951, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(bord_tr_t1970, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(bord_tr_t1990, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(bord_tr_t2010, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(bord_tr_t2024, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(bord_be_t1951, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1951, 0.5).
narrative_ontology:measurement(bord_be_t1970, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(bord_be_t1990, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(bord_be_t2010, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1951, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1951, 0.6).
narrative_ontology:measurement(bord_su_t1970, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(bord_su_t1990, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(bord_su_t2010, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
