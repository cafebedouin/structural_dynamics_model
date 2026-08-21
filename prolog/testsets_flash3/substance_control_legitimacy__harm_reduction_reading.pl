% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Harm Reduction Approach to Substance Control
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'harm reduction' reading of substance
 *   control legitimacy, where state authority is justified by a duty to
 *   minimize harm without resorting to criminalization. It medicalizes
 *   substance use, shifting resources to public health and treatment. While
 *   reducing direct criminal penalties, it maintains a degree of state
 *   control and enforcement, leading to moderate extractiveness through
 *   treatment mandates and the persistence of a black market for unregulated
 *   substances. This reading is a distinct alternative to both full
 *   prohibition and full legalization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.45).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.6).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Harm Reduction Approach to Substance Control").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, 'bd348811-1cde-4601-9905-87641421e0ed').
narrative_ontology:cs_kernel_codification('bd348811-1cde-4601-9905-87641421e0ed', formalized).
narrative_ontology:cs_authority_grounding('bd348811-1cde-4601-9905-87641421e0ed', lineage).
narrative_ontology:cs_interpretation_layer_present('bd348811-1cde-4601-9905-87641421e0ed').
narrative_ontology:cs_reading_relation('bd348811-1cde-4601-9905-87641421e0ed', substance_control_legitimacy__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('bd348811-1cde-4601-9905-87641421e0ed', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('bd348811-1cde-4601-9905-87641421e0ed', foundational, substance_use_is_public_health_issue).
narrative_ontology:cs_axiom_status(substance_use_is_public_health_issue, holdable).
narrative_ontology:cs_axiom_grounding('bd348811-1cde-4601-9905-87641421e0ed', substance_use_is_public_health_issue, empirically_contingent).
narrative_ontology:cs_axiom('bd348811-1cde-4601-9905-87641421e0ed', foundational, state_duty_to_minimize_harm_without_criminalization).
narrative_ontology:cs_axiom_status(state_duty_to_minimize_harm_without_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('bd348811-1cde-4601-9905-87641421e0ed', state_duty_to_minimize_harm_without_criminalization, deontological).
narrative_ontology:cs_reference_frame('bd348811-1cde-4601-9905-87641421e0ed', public_health_paradigm_shift).
narrative_ontology:cs_drift_state('bd348811-1cde-4601-9905-87641421e0ed', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('bd348811-1cde-4601-9905-87641421e0ed', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_providers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, black_market_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer harm reduction programs, allocate resources for treatment, and advocate for decriminalization. They benefit from increased legitimacy and funding for public health interventions, but are constrained by political will and existing legal frameworks.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Receive funding and referrals from public health agencies to provide medical and psychological support to substance users. They benefit from the medicalization of substance use, expanding their client base and professional scope.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_providers, beneficiary,
    organized, biographical, mobile, local).

% Benefit from access to medical treatment, clean supplies, and reduced criminal penalties. However, they still face social stigma, mandatory treatment, and the risks associated with a persistent black market for substances not fully decriminalized or regulated.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, substance_users, beneficiary,
    powerless, immediate, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, substance_users, payer).

% Face continued enforcement efforts for substances not covered by harm reduction policies or for illicit distribution. They bear the costs of interdiction and competition from legal or quasi-legal alternatives, but persist due to unmet demand and regulatory gaps.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, black_market_actors, payer,
    organized, biographical, constrained, regional).

% Their role shifts from criminalization to supporting public health efforts, but they retain authority for interdiction of certain substances or large-scale trafficking. They are constrained by policy changes but adapt to new enforcement priorities.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, law_enforcement, agenda_setter,
    institutional, biographical, constrained, national).

% Advocate for full legalization and individual autonomy, arguing that harm reduction, while an improvement over prohibition, still imposes state control and creates a persistent black market. They are excluded from the core policy-making process of this reading, which prioritizes public health over individual liberty.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, advocacy_groups_for_legalization, excluded,
    moderate, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health interventions, law enforcement efforts, and social services to minimize the negative health and social consequences of substance use, shifting focus from punishment to treatment and support.
% TRANSFER_FUNCTION: Transfers resources from criminal justice enforcement to public health and treatment programs. It also transfers some autonomy from individuals to medical and state authorities through mandatory treatment or supervised consumption.
% ABSENT_VOICES: Advocacy groups for full legalization are largely absent from the core policy-making, as their emphasis on individual autonomy over state-managed harm minimization is not fully integrated into this framework. They would argue for a more radical shift away from any state control over adult substance use.
% DISAPPEARANCE_RATIONALE: If this harm reduction framework vanished, the system would likely revert to a more punitive, prohibition-based approach, leading to increased criminalization of substance users, reduced access to treatment, and a resurgence of unchecked public health harms. The entire social and legal infrastructure around substance use would shift dramatically.
% FOUNDING_PROBLEM: The criminalization of substance use created significant public health crises (e.g., HIV/AIDS, overdose deaths), overwhelmed the justice system, and failed to reduce substance use effectively.
% FOUNDING_PROBLEM_CORROBORATION: Public health data, medical professionals, and international organizations consistently corroborate that criminalization exacerbates public health harms and that a public health approach is necessary to mitigate these issues. This is attested by independent research and global health reports, not just by the benefiting agencies.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).
:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while direct criminal penalties are reduced, substance users still face mandatory treatment, social stigma, and the costs associated with a partially regulated or illicit market. Suppression (0.60) is present as the state actively enforces regulations, controls access to substances, and targets black market activities. Theater ratio (0.20) is relatively low, as the public health interventions are genuinely functional, though some enforcement efforts may be performative to appease prohibitionist sentiments. The metrics reflect a system that is genuinely trying to coordinate harm reduction but still extracts from users and maintains suppressive elements.
 *
 * PERSPECTIVAL GAP:
 *   Public health agencies view this as a progressive, effective coordination mechanism. Substance users experience a mixed bag of benefits and continued control/extraction. Law enforcement navigates a shift in mandate, balancing public health goals with residual enforcement duties. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and treatment providers are beneficiaries, gaining legitimacy and resources. Substance users are both beneficiaries (reduced criminalization, access to treatment) and payers (mandatory treatment, continued stigma, black market costs). Black market actors are payers, facing enforcement. Law enforcement's role is complex, shifting from primary enforcers to supporting public health, making them a constrained agenda-setter. Advocacy groups for legalization are excluded, as their vision of full autonomy is outside this reading's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    black_market_persistence,
    'To what extent does the persistence of a black market under harm reduction policies undermine the public health goals and contribute to extraction?',
    'Empirical studies comparing black market activity and associated harms in jurisdictions with varying degrees of harm reduction and legalization.',
    'If the black market remains significant and harmful, it suggests the harm reduction reading''s extractiveness and suppression are higher than measured, pushing it closer to a Snare. If it diminishes, the reading''s coordination function is more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_persistence, empirical, 'Ambiguity regarding the impact of a persistent black market on harm reduction outcomes.').

omega_variable(
    state_control_vs_autonomy,
    'Is the state''s ''duty to minimize harm'' an appropriate justification for mandatory treatment or supervised consumption, or does it infringe on individual autonomy in ways that constitute extraction?',
    'Conceptual analysis of liberty principles and ethical frameworks, alongside qualitative studies of substance users'' experiences with mandatory interventions.',
    'If mandatory interventions are deemed an unacceptable infringement on autonomy, the extractiveness for substance users is higher, pushing the constraint towards a Snare from their seat. If justified, the coordination function is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_control_vs_autonomy, conceptual, 'Debate over the ethical limits of state intervention in individual substance use decisions.').

omega_variable(
    reading_classification_ambiguity,
    'Is this constraint a genuine ''tangled_rope'' (coordination with extraction) or does the persistence of state control and black markets make it closer to a ''snare'' (pure extraction) from the perspective of substance users?',
    'Re-evaluation of extractiveness and suppression metrics based on the resolution of ''black_market_persistence'' and ''state_control_vs_autonomy'' omegas, particularly from the ''substance_users'' seat.',
    'If the underlying ambiguities resolve towards higher extraction and less genuine coordination, the constraint would reclassify as a Snare for substance users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_classification_ambiguity, conceptual, 'Ambiguity in the overall classification of the harm reduction approach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(subs_tr_t5, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(subs_be_t5, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(subs_su_t5, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, public_health_funding_allocation).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, criminal_justice_reform).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
