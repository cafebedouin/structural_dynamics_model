% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__legalization_reading, []).

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
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Adult Autonomy in Substance Use (Legalization Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'legalization' reading of substance
 *   control legitimacy, where competent adults retain autonomy over substance
 *   use, and state authority is strictly limited to preventing third-party
 *   harm. It shifts the focus from criminalizing users to regulating markets
 *   and mitigating externalized costs. The constraint is claimed as a Rope,
 *   reflecting its intent to coordinate individual liberty with collective
 *   safety, with minimal extraction from users themselves, though some
 *   extraction occurs from third-party harm victims and through corporate
 *   profits in legal markets.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.25).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.15).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Adult Autonomy in Substance Use (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, 'd537cfe6-23b3-427e-bbf4-3d94e13a5f04').
narrative_ontology:cs_kernel_codification('d537cfe6-23b3-427e-bbf4-3d94e13a5f04', formalized).
narrative_ontology:cs_authority_grounding('d537cfe6-23b3-427e-bbf4-3d94e13a5f04', lineage).
narrative_ontology:cs_interpretation_layer_present('d537cfe6-23b3-427e-bbf4-3d94e13a5f04').
narrative_ontology:cs_reading_relation('d537cfe6-23b3-427e-bbf4-3d94e13a5f04', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('d537cfe6-23b3-427e-bbf4-3d94e13a5f04', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('d537cfe6-23b3-427e-bbf4-3d94e13a5f04', foundational, individual_bodily_autonomy).
narrative_ontology:cs_axiom_status(individual_bodily_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('d537cfe6-23b3-427e-bbf4-3d94e13a5f04', individual_bodily_autonomy, deontological).
narrative_ontology:cs_axiom('d537cfe6-23b3-427e-bbf4-3d94e13a5f04', foundational, state_limited_to_third_party_harm).
narrative_ontology:cs_axiom_status(state_limited_to_third_party_harm, holdable).
narrative_ontology:cs_axiom_grounding('d537cfe6-23b3-427e-bbf4-3d94e13a5f04', state_limited_to_third_party_harm, conventional).
narrative_ontology:cs_reference_frame('d537cfe6-23b3-427e-bbf4-3d94e13a5f04', liberal_autonomy_framework).
narrative_ontology:cs_drift_state('d537cfe6-23b3-427e-bbf4-3d94e13a5f04', contemporary_policy_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d537cfe6-23b3-427e-bbf4-3d94e13a5f04', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, competent_adults).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, legal_substance_industry).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, tax_authorities).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, third_party_harm_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal access to substances, removing criminal penalties and associated social stigma. Their autonomy is respected, but they are still subject to regulations preventing harm to others.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, competent_adults, beneficiary,
    moderate, biographical, mobile, national).

% Shift from criminal enforcement to regulatory oversight, focusing on public health and safety, taxation, and preventing third-party harms (e.g., impaired driving, secondhand exposure). They administer licensing and taxation for legal markets.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Operates legally, generating profits and tax revenue. Subject to strict regulations regarding production, marketing, and sales to prevent public health issues and third-party harms.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, legal_substance_industry, beneficiary,
    organized, biographical, mobile, national).

% Bear the costs of harms caused by others' substance use (e.g., traffic accidents, public nuisance, secondhand smoke). The constraint aims to minimize these harms through regulation, but cannot eliminate them entirely.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, third_party_harm_victims, payer,
    powerless, immediate, trapped, local).

% Collect significant tax revenue from the legal substance market, which can be used to fund public services, including harm reduction programs or enforcement of third-party harm prevention.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, tax_authorities, beneficiary,
    institutional, generational, analytical, national).

% Monitor the public health outcomes of legalization, advocating for effective regulation to minimize harms and ensure equitable access to treatment. They assess whether the balance between autonomy and public safety is met.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, public_health_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual autonomy with public safety by establishing a legal framework for substance access while regulating to prevent harms to non-users and the broader community.
% TRANSFER_FUNCTION: Transfers the right to consume substances from state control to individual adults, while transferring regulatory oversight and tax revenue to the state, and residual harm costs to third parties.
% ABSENT_VOICES: Those who believe all substance use is inherently immoral or destructive, regardless of third-party harm, are largely excluded from the policy-making process under this framework; they would advocate for prohibition.
% DISAPPEARANCE_RATIONALE: If this framework vanished, the legal substance industry would collapse, tax revenues would disappear, and either a black market would re-emerge (if prohibition returned) or public health harms would escalate dramatically (if all regulation vanished). The social and economic landscape would fundamentally shift.
% FOUNDING_PROBLEM: The problem of balancing individual liberty with public safety and health, particularly in the context of substances that carry risks, and the failure of prohibition to eliminate use while creating criminal markets.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for individual liberty and economic efficiency attest that the problem of balancing autonomy and public safety is ongoing, and that prohibition failed. Public health advocates corroborate the need for a framework that addresses both individual rights and community well-being, even if they differ on the optimal approach.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).
:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the primary aim is not to extract from users, but to manage a legal market and mitigate external harms. Suppression is low (0.15) as it primarily involves regulatory enforcement against industry and individuals causing harm, not broad criminalization. Theater ratio is very low (0.05) as the state's actions are directly aligned with its stated goals of public safety and revenue generation, rather than maintaining a facade. Accessibility collapse is moderate (0.7) as legal access is provided, but still regulated, and resistance is low (0.1) as this framework generally aligns with public demand for autonomy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of competent adults, this is a clear Rope, restoring autonomy. From the perspective of third-party harm victims, it may feel more extractive, as they bear costs that were previously (theoretically) prevented by prohibition. The state's role shifts from moral enforcer to pragmatic regulator, which is a significant perspectival change from a prohibitionist view.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent adults and the legal substance industry are beneficiaries, gaining autonomy and market access respectively. Tax authorities also benefit from new revenue streams. Third-party harm victims are payers, bearing residual costs that the regulatory framework aims to minimize but cannot eliminate. State authorities act as agenda-setters, balancing these interests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    third_party_harm_quantification,
    'How accurately can third-party harms (e.g., impaired driving, secondhand exposure, public nuisance) be quantified and attributed to legal substance use, and are current regulations sufficient to mitigate them?',
    'Longitudinal epidemiological studies, economic impact assessments, and comparative policy analysis across jurisdictions with varying regulatory regimes.',
    'If harms are higher than anticipated or poorly mitigated, the extractiveness on third-party victims would be higher, potentially shifting the constraint towards a Tangled Rope or Snare for that seat. If harms are minimal, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_harm_quantification, empirical, 'Uncertainty regarding the true extent and mitigation of third-party harms under a legalization framework.').

omega_variable(
    corporate_capture_risk,
    'Does the legal substance industry, as a beneficiary, exert undue influence on regulatory bodies, leading to ''regulatory capture'' that prioritizes industry profits over public health and third-party harm prevention?',
    'Analysis of lobbying expenditures, campaign contributions, revolving door appointments, and the content of regulatory changes over time, compared to public health recommendations.',
    'If significant capture is detected, the constraint''s extractiveness would be higher, concentrated on the public and third-party victims, and the state authority''s directionality would shift towards a full beneficiary, indicating a Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_capture_risk, empirical, 'Risk of regulatory capture by the legal substance industry, shifting the balance from public good to private profit.').

omega_variable(
    autonomy_vs_paternalism_boundary,
    'Where is the legitimate boundary between individual autonomy over substance use and state paternalism aimed at protecting individuals from self-harm, even if no third-party harm is involved?',
    'Philosophical and ethical debate, public discourse, and judicial interpretation of individual rights versus state powers. This is a conceptual and preference-based question.',
    'A shift towards greater paternalism would increase suppression on individuals and potentially reintroduce elements of criminalization, moving the constraint towards a Tangled Rope or Snare for users. A stronger emphasis on autonomy would reduce regulatory burdens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_vs_paternalism_boundary, conceptual, 'The conceptual boundary between individual liberty and state intervention for self-protection, which defines the scope of this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(subs_tr_t5, substance_control_legitimacy__legalization_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__legalization_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__legalization_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(subs_be_t5, substance_control_legitimacy__legalization_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__legalization_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__legalization_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(subs_su_t5, substance_control_legitimacy__legalization_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__legalization_reading, suppression_requirement, 10, 0.13).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__legalization_reading, suppression_requirement, 15, 0.14).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'substance_control_legitimacy' kernel. It focuses on individual autonomy and third-party harm prevention, contrasting with prohibition and harm reduction approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
