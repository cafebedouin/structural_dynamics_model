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
 *   human_readable: Humanitarian Border Obligation and Economic Migration Exclusion
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint represents the 'humanitarian obligation' reading of
 *   border legitimacy, which posits that states have a moral and legal
 *   obligation to admit those fleeing persecution or disaster
 *   (refugees/asylum seekers) but retain the right to exclude general
 *   economic migrants. This reading attempts to balance state sovereignty
 *   with international human rights law, creating a bifurcated system of
 *   entry. It is one reading of the broader 'border_legitimacy' kernel, which
 *   is contested by 'sovereignty_reading' and 'freedom_of_movement_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.45).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.7).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Border Obligation and Economic Migration Exclusion").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, '7af92922-c10f-4023-9796-3f706057e23f').
narrative_ontology:cs_kernel_codification('7af92922-c10f-4023-9796-3f706057e23f', formalized).
narrative_ontology:cs_authority_grounding('7af92922-c10f-4023-9796-3f706057e23f', lineage).
narrative_ontology:cs_interpretation_layer_present('7af92922-c10f-4023-9796-3f706057e23f').
narrative_ontology:cs_reading_relation('7af92922-c10f-4023-9796-3f706057e23f', border_legitimacy__sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('7af92922-c10f-4023-9796-3f706057e23f', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_axiom('7af92922-c10f-4023-9796-3f706057e23f', foundational, state_has_right_to_control_borders).
narrative_ontology:cs_axiom_status(state_has_right_to_control_borders, holdable).
narrative_ontology:cs_axiom_grounding('7af92922-c10f-4023-9796-3f706057e23f', state_has_right_to_control_borders, conventional).
narrative_ontology:cs_axiom('7af92922-c10f-4023-9796-3f706057e23f', foundational, non_refoulement_is_absolute).
narrative_ontology:cs_axiom_status(non_refoulement_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('7af92922-c10f-4023-9796-3f706057e23f', non_refoulement_is_absolute, deontological).
narrative_ontology:cs_reference_frame('7af92922-c10f-4023-9796-3f706057e23f', post_geneva_convention_framework).
narrative_ontology:cs_drift_state('7af92922-c10f-4023-9796-3f706057e23f', contemporary_migration_crises_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7af92922-c10f-4023-9796-3f706057e23f', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, host_states).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, refugees).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, states_with_high_refugee_burden).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that define and enforce border policies, admitting those fleeing persecution/disaster while excluding general economic migrants. They benefit from managing their populations and resources, but bear the administrative and social costs of processing asylum claims and integrating refugees.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, host_states, agenda_setter,
    institutional, generational, constrained, national).

% Individuals fleeing persecution, war, or disaster who are granted protection and admission under this framework. They benefit from safety and a legal path to residency, but often face significant challenges in integration and rebuilding their lives.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, refugees, beneficiary,
    powerless, immediate, trapped, global).

% Individuals seeking better economic opportunities who are categorically excluded by this framework. They bear the costs of border enforcement, often facing dangerous journeys, detention, and deportation, with no legal path to entry.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, immediate, trapped, global).

% Organizations and individuals who monitor state compliance with international human rights and refugee law, advocating for the rights of all migrants and challenging restrictive interpretations of humanitarian obligations.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% States, often neighboring conflict zones, that receive a disproportionate number of refugees, straining their resources and infrastructure. They bear significant costs despite the humanitarian obligation, often seeking international assistance.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, states_with_high_refugee_burden, payer,
    institutional, biographical, constrained, regional).

% Groups and political movements who prioritize absolute state sovereignty over humanitarian obligations, arguing for an unfettered right to exclude any non-citizen. They are excluded from the core premise of this reading, which posits limits on sovereignty.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, sovereignty_advocates, excluded,
    organized, generational, analytical, global).

% Groups who argue for a universal human right to freedom of movement, challenging the legitimacy of state borders as restrictions on this right. Their core premise is foreclosed by this reading's distinction between legitimate and illegitimate reasons for migration.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, freedom_of_movement_advocates, excluded,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__humanitarian_obligation_reading, host_states).
narrative_ontology:fixing_cost_class(border_legitimacy__humanitarian_obligation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages state borders by distinguishing between those with a right to seek asylum/protection (refugees) and those without (economic migrants), providing a framework for orderly entry and exclusion in line with international law.
% TRANSFER_FUNCTION: Transfers security and stability to host states by controlling entry, and transfers protection and a legal pathway to refugees. It imposes costs of exclusion (e.g., dangerous journeys, lack of legal status) on economic migrants and administrative/social burdens on host states.
% ABSENT_VOICES: Economic migrants would object to their categorical exclusion and the lack of legal pathways for economic betterment. Freedom of movement advocates would challenge the legitimacy of the distinction itself, arguing for open borders.
% DISAPPEARANCE_RATIONALE: If the distinction between refugees and economic migrants, and the associated obligations/exclusions, vanished overnight, states would either face uncontrolled migration flows (if all were admitted) or would have no basis to admit anyone (if all were excluded), leading to a collapse of international protection regimes and border management, and widespread humanitarian crises.
% FOUNDING_PROBLEM: The problem of managing mass movements of people, particularly those displaced by conflict or disaster, while balancing state sovereignty with humanitarian principles and preventing uncontrolled migration.
% FOUNDING_PROBLEM_CORROBORATION: International conventions (e.g., 1951 Refugee Convention), humanitarian organizations, and many states' domestic laws corroborate the ongoing need for a framework to manage forced displacement and distinguish between different categories of migrants. The problem is exacerbated by ongoing global conflicts and climate change.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).
:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while it provides a vital pathway for refugees, it categorically excludes economic migrants, imposing significant costs on them. Suppression is high (0.7) due to the active and often militarized enforcement of borders against those deemed 'economic migrants'. Theater ratio is low (0.1) as border enforcement is largely functional in maintaining the distinction, though the humanitarian aspect can sometimes be performative in practice. Accessibility collapse is moderate (0.6) as a legal path exists for some, but not for others. Resistance is moderate (0.5) from both excluded migrants and human rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   Host states and refugees experience this constraint as a legitimate and necessary framework for protection and orderly migration. Economic migrants, however, experience it as an arbitrary and unjust exclusion, enforced with coercive power. Sovereignty advocates would see the humanitarian obligation as an infringement on state rights, while freedom of movement advocates would see the entire distinction as illegitimate.
 *
 * DIRECTIONALITY LOGIC:
 *   Host states are beneficiaries as they gain control over their borders and manage population flows, while also fulfilling international obligations. Refugees are beneficiaries as they gain protection and a legal pathway. Economic migrants are targets/payers, bearing the full cost of exclusion. States with high refugee burdens are also payers, as they disproportionately bear the costs of the humanitarian obligation. International human rights advocates are observers, analyzing the system's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    refugee_economic_migrant_distinction_clarity,
    'Is the distinction between ''refugee'' (fleeing persecution/disaster) and ''economic migrant'' sufficiently clear and consistently applied in practice, or does it serve as a flexible pretext for broader exclusion?',
    'Empirical analysis of asylum claim rejection rates, criteria applied by border authorities, and the proportion of individuals with mixed migration motives who are denied entry. Legal review of case law on ''economic migrant'' definitions.',
    'If the distinction is found to be inconsistently applied or used as a pretext, the effective extractiveness and suppression of the constraint would be higher than currently measured, as more legitimate claims for protection are denied. This would push the classification closer to a Snare for those denied entry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_economic_migrant_distinction_clarity, empirical, 'Ambiguity in the practical application of the refugee/economic migrant distinction.').

omega_variable(
    state_capacity_and_burden_sharing,
    'What is the actual capacity of states to absorb refugees, and at what point does the ''humanitarian obligation'' become unsustainable without robust international burden-sharing mechanisms?',
    'Economic and social impact assessments in high-burden states, analysis of international aid flows, and studies on the long-term integration outcomes of refugees. This would involve empirical data combined with policy analysis.',
    'If state capacity is demonstrably overwhelmed without adequate burden-sharing, the ''obligation'' aspect of the constraint could degrade, leading to increased unilateral exclusions and a higher effective extractiveness for all migrants, potentially shifting the constraint towards a Snare or Piton if the obligation becomes purely theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_and_burden_sharing, empirical, 'Sustainability of humanitarian obligation given state capacity and burden-sharing.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is one reading of the ''border_legitimacy'' kernel. How would the classification change if a sibling reading (e.g., ''sovereignty_reading'' or ''freedom_of_movement_reading'') were adopted as the primary frame?',
    'Generate separate constraint stories for the ''sovereignty_reading'' and ''freedom_of_movement_reading'' and compare their metric profiles and classifications. The structural differences in beneficiary/victim sets and extractiveness would highlight the impact of framing.',
    'Adopting the ''sovereignty_reading'' would likely increase extractiveness and suppression for all non-citizens, potentially classifying it as a Snare. Adopting the ''freedom_of_movement_reading'' would drastically reduce extractiveness and suppression, potentially classifying it as a Rope or even a Mountain (if movement were seen as a natural right).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1951, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1951, 0.05).
narrative_ontology:measurement(bord_tr_t1970, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(bord_tr_t1990, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(bord_tr_t2010, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(bord_tr_t2024, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1951, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1951, 0.35).
narrative_ontology:measurement(bord_be_t1970, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1970, 0.38).
narrative_ontology:measurement(bord_be_t1990, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(bord_be_t2010, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1951, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1951, 0.5).
narrative_ontology:measurement(bord_su_t1970, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1970, 0.58).
narrative_ontology:measurement(bord_su_t1990, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(bord_su_t2010, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2024, 0.7).


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
