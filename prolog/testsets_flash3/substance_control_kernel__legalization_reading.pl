% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__legalization_reading, []).

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
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Substance Control: Legalization Reading (Individual Liberty)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'legalization' reading of substance
 *   control, where individual liberty is paramount, and state intervention is
 *   limited to preventing third-party harm and capturing externality costs.
 *   It contrasts with prohibition (moral transgression, state punishment) and
 *   harm reduction (health condition, pragmatic intervention). This reading
 *   shifts users from victims to beneficiaries and introduces a legal
 *   industry and state revenue collection, while creating a new victim class
 *   in third parties affected by externalities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.25).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.15).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Substance Control: Legalization Reading (Individual Liberty)").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '8ff4d1fd-f477-4a1b-866e-f69619e3b7af').
narrative_ontology:cs_kernel_codification('8ff4d1fd-f477-4a1b-866e-f69619e3b7af', formalized).
narrative_ontology:cs_authority_grounding('8ff4d1fd-f477-4a1b-866e-f69619e3b7af', lineage).
narrative_ontology:cs_interpretation_layer_present('8ff4d1fd-f477-4a1b-866e-f69619e3b7af').
narrative_ontology:cs_reading_relation('8ff4d1fd-f477-4a1b-866e-f69619e3b7af', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ff4d1fd-f477-4a1b-866e-f69619e3b7af', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('8ff4d1fd-f477-4a1b-866e-f69619e3b7af', foundational, individual_autonomy_over_personal_choices).
narrative_ontology:cs_axiom_status(individual_autonomy_over_personal_choices, holdable).
narrative_ontology:cs_axiom_grounding('8ff4d1fd-f477-4a1b-866e-f69619e3b7af', individual_autonomy_over_personal_choices, deontological).
narrative_ontology:cs_axiom('8ff4d1fd-f477-4a1b-866e-f69619e3b7af', foundational, state_intervention_limited_to_third_party_harm).
narrative_ontology:cs_axiom_status(state_intervention_limited_to_third_party_harm, holdable).
narrative_ontology:cs_axiom_grounding('8ff4d1fd-f477-4a1b-866e-f69619e3b7af', state_intervention_limited_to_third_party_harm, conventional).
narrative_ontology:cs_reference_frame('8ff4d1fd-f477-4a1b-866e-f69619e3b7af', liberal_democratic_principles).
narrative_ontology:cs_drift_state('8ff4d1fd-f477-4a1b-866e-f69619e3b7af', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8ff4d1fd-f477-4a1b-866e-f69619e3b7af', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, legal_substance_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_revenue_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, individual_users).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, third_party_victims_of_externalities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can legally acquire and use substances, free from criminal penalties, subject to regulations preventing harm to others. They benefit from reduced black market risks and increased product safety.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, individual_users, beneficiary,
    moderate, biographical, mobile, national).

% Operates legally, producing and distributing substances under state regulation. Benefits from access to a legitimate market and protection from black market competition, while paying taxes and complying with safety standards.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, legal_substance_industry, beneficiary,
    organized, generational, mobile, national).

% Collects tax revenue from the legal substance industry, which is then used to offset externality costs or fund public services. Sets and enforces regulations to prevent third-party harm.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_revenue_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Monitors public health outcomes related to substance use, implements public awareness campaigns, and provides treatment services. Operates within the framework of legalization, focusing on harm prevention rather than prohibition.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Suffer harm (e.g., traffic accidents, secondhand exposure, public nuisance) due to others' substance use, despite state intervention to mitigate these effects. Their costs are the uncaptured externalities.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, third_party_victims_of_externalities, payer,
    powerless, immediate, trapped, local).

% Are pushed out of the legal market but may persist in gray areas or by supplying substances not covered by legalization. They face reduced demand and increased enforcement pressure compared to a prohibition regime.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, black_market_operators, excluded,
    organized, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual liberty with public safety by allowing legal substance use while regulating to prevent third-party harm and capture externality costs through taxation.
% TRANSFER_FUNCTION: Transfers tax revenue from the legal substance industry to the state, and transfers the burden of uncaptured externality costs to third-party victims. It also transfers freedom from criminalization to individual users.
% ABSENT_VOICES: Advocates for total prohibition would argue that any substance use is a moral harm and that the state should intervene to prevent all use. They are excluded from a framework that prioritizes individual liberty.
% DISAPPEARANCE_RATIONALE: If this framework vanished, either a prohibition or a completely unregulated free-for-all would emerge. The legal industry would collapse or go underground, state revenue would disappear, and the balance between liberty and public safety would be fundamentally altered.
% FOUNDING_PROBLEM: The problem of balancing individual autonomy over personal choices with the state's responsibility to protect its citizens from harm and manage social costs.
% FOUNDING_PROBLEM_CORROBORATION: Philosophers and legal scholars attest to the enduring nature of the individual liberty vs. public good dilemma. Public health data and economic analyses from independent research bodies corroborate the ongoing challenge of managing externality costs.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).
:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the primary goal is not extraction but managing externalities; any 'extraction' is through taxation to cover social costs. Suppression is also low (0.15) as the state's role is regulatory, not punitive, for individual use. Theater ratio is minimal (0.05) as the system is designed to be functional and transparent in its goals. Accessibility collapse is moderate (0.3) as legal alternatives exist, but some substances or unregulated markets may persist. Resistance is low (0.1) as the framework generally aligns with public demand for liberty, though some groups may resist the remaining regulations or the concept of legalization itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individual users and the legal industry, this is a highly beneficial 'rope' that enables freedom and commerce. From the perspective of third-party victims, it may still feel like a 'snare' due to unmitigated externalities. The state aims for a 'rope' classification, balancing interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual users and the legal substance industry are primary beneficiaries (low d) as they gain freedom and market access. State revenue and public health agencies are agenda-setters (symmetric d) as they manage the system, collecting revenue and mitigating harm. Third-party victims of externalities are the primary targets (high d) as they bear uncaptured costs. Black market operators are excluded (high d) as the system actively works to displace them.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_cost_capture_completeness,
    'To what extent does state taxation and regulation truly capture and mitigate all externality costs associated with legal substance use?',
    'Comprehensive longitudinal economic and public health studies comparing pre- and post-legalization externality costs, including healthcare, public safety, and environmental impacts.',
    'If externality costs are largely uncaptured, the effective extractiveness on third parties is higher, pushing the constraint closer to a ''tangled_rope'' or ''snare'' for that seat. If fully captured, the ''rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_cost_capture_completeness, empirical, 'Assesses the effectiveness of the state''s mechanism for internalizing externality costs.').

omega_variable(
    black_market_persistence_threshold,
    'At what level of taxation and regulation does a significant black market for substances persist, undermining the goals of legalization?',
    'Comparative analysis of jurisdictions with varying tax rates and regulatory burdens on legal substances, measuring the size and activity of illicit markets.',
    'If the black market persists significantly, the ''suppression'' metric for illicit activity is lower than intended, and the ''beneficiary'' status of the legal industry and users is diminished by ongoing risks, potentially shifting the overall classification towards a ''tangled_rope'' due to unaddressed coordination failures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_persistence_threshold, empirical, 'Examines the trade-off between state revenue/control and the persistence of illicit markets.').

omega_variable(
    framing_of_individual_liberty_vs_collective_good,
    'Is the prioritization of individual liberty over collective good in substance use a universally accepted principle, or a contested conceptual framing?',
    'Analysis of philosophical arguments, legal traditions, and public opinion across diverse cultures and political systems regarding the scope of individual autonomy versus state paternalism or collective welfare.',
    'If the individual liberty framing is widely contested, the ''rope'' classification is conceptually unstable, as the foundational justification for the constraint is not universally shared, potentially leading to reclassification as a ''tangled_rope'' or ''snare'' from alternative conceptual seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_individual_liberty_vs_collective_good, conceptual, 'Examines the conceptual grounding of the individual liberty principle in public health policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(subs_tr_t5, substance_control_kernel__legalization_reading, theater_ratio, 5, 0.04).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__legalization_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(subs_tr_t15, substance_control_kernel__legalization_reading, theater_ratio, 15, 0.06).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(subs_be_t5, substance_control_kernel__legalization_reading, base_extractiveness, 5, 0.23).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__legalization_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(subs_be_t15, substance_control_kernel__legalization_reading, base_extractiveness, 15, 0.26).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(subs_su_t5, substance_control_kernel__legalization_reading, suppression_requirement, 5, 0.14).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__legalization_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(subs_su_t15, substance_control_kernel__legalization_reading, suppression_requirement, 15, 0.16).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'substance_control_kernel'. It represents the 'legalization_reading', focusing on individual liberty and externality management. It is linked to the 'prohibition_reading' and 'harm_reduction_reading' as alternative framings of the same underlying kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
