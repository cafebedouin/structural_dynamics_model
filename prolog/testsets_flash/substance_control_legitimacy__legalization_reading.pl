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
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Legalization Reading of Substance Control Legitimacy
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'legalization reading' of substance
 *   control legitimacy, where adult autonomy over substance use is
 *   prioritized, and state intervention is limited to preventing third-party
 *   harm. It shifts the focus from criminalization to regulation and public
 *   health. This reading is one of three competing interpretations of the
 *   'substance_control_legitimacy' kernel, alongside 'prohibition_reading'
 *   and 'harm_reduction_reading'. The core structural delta for this reading
 *   is that individual users exit the victim set, third-party harms become
 *   the primary target of state constraint, and a legal corporate industry
 *   emerges, potentially introducing new forms of extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.3).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.2).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Legalization Reading of Substance Control Legitimacy").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, 'd25571cc-7dd6-459d-a5f0-ac7dcf8aa509').
narrative_ontology:cs_kernel_codification('d25571cc-7dd6-459d-a5f0-ac7dcf8aa509', formalized).
narrative_ontology:cs_authority_grounding('d25571cc-7dd6-459d-a5f0-ac7dcf8aa509', lineage).
narrative_ontology:cs_interpretation_layer_present('d25571cc-7dd6-459d-a5f0-ac7dcf8aa509').
narrative_ontology:cs_reading_relation('d25571cc-7dd6-459d-a5f0-ac7dcf8aa509', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('d25571cc-7dd6-459d-a5f0-ac7dcf8aa509', substance_control_legitimacy__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('d25571cc-7dd6-459d-a5f0-ac7dcf8aa509', foundational, adult_autonomy_over_body).
narrative_ontology:cs_axiom_status(adult_autonomy_over_body, holdable).
narrative_ontology:cs_axiom_grounding('d25571cc-7dd6-459d-a5f0-ac7dcf8aa509', adult_autonomy_over_body, deontological).
narrative_ontology:cs_axiom('d25571cc-7dd6-459d-a5f0-ac7dcf8aa509', foundational, state_limited_to_third_party_harm).
narrative_ontology:cs_axiom_status(state_limited_to_third_party_harm, holdable).
narrative_ontology:cs_axiom_grounding('d25571cc-7dd6-459d-a5f0-ac7dcf8aa509', state_limited_to_third_party_harm, conventional).
narrative_ontology:cs_reference_frame('d25571cc-7dd6-459d-a5f0-ac7dcf8aa509', liberal_autonomy_framework).
narrative_ontology:cs_drift_state('d25571cc-7dd6-459d-a5f0-ac7dcf8aa509', contemporary_public_health_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d25571cc-7dd6-459d-a5f0-ac7dcf8aa509', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, adult_consumers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, legal_substance_industry).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, tax_authorities).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, third_party_harm_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal access to substances, avoiding criminal penalties and black markets. They benefit from product quality control and reduced personal risk, but bear the costs of taxation and potential health impacts.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, adult_consumers, beneficiary,
    moderate, biographical, mobile, national).

% Operates legally, generating profits from the sale of regulated substances. Benefits from market access and legitimacy, but faces regulatory compliance costs and taxation. Actively lobbies for favorable regulations.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, legal_substance_industry, beneficiary,
    powerful, generational, arbitrage, national).

% Regulates the legal substance market, collects tax revenue, and enforces laws against third-party harm (e.g., impaired driving, public intoxication). Shifts resources from criminal enforcement to public health and safety.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Suffer harms from others' substance use (e.g., traffic accidents, secondhand smoke, public disorder). While the state aims to prevent these, some residual harm is an unavoidable cost of legalization, making them indirect victims.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, third_party_harm_victims, payer,
    powerless, immediate, trapped, local).

% Collects significant tax revenue from the legal sale of substances, which can be used to fund public services, including harm reduction programs or general government budgets.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, tax_authorities, beneficiary,
    institutional, generational, analytical, national).

% Argue against legalization on moral or public health grounds, believing it normalizes harmful behavior. Their views are marginalized in a legalization framework, as the policy explicitly rejects their core premise.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, prohibition_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the supply and demand of substances within a legal framework, ensuring product safety, collecting tax revenue, and establishing clear boundaries for acceptable public behavior related to substance use.
% TRANSFER_FUNCTION: Transfers tax revenue from consumers and industry to the state, and transfers the burden of managing individual substance use from the criminal justice system to public health and regulatory bodies. Shifts the cost of enforcement from criminalization to regulation and harm prevention.
% ABSENT_VOICES: Advocates for complete prohibition are excluded, as their moral and public health arguments against any legal access are rejected by the core premise of this reading. They would argue for a return to criminalization.
% DISAPPEARANCE_RATIONALE: If this legalization framework vanished, the legal substance industry would collapse, tax revenues would disappear, and consumers would revert to black markets, leading to increased criminal activity, unregulated products, and a resurgence of the 'founding problem' of unsafe, illicit supply. The state would have to re-establish a new framework, likely prohibition or a different form of harm reduction.
% FOUNDING_PROBLEM: The criminalization of substance use created a vast black market, fueled organized crime, led to unsafe products, and imposed disproportionate criminal justice burdens on individuals, without effectively curbing demand.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for legalization, public health experts, and economists corroborate that the problems of criminalization (black markets, unsafe products, criminal justice costs) were severe and persist where prohibition remains. The legal substance industry also corroborates the market failures of prohibition, as do many adult consumers who experienced the illicit market.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).

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
 *   Extractiveness is low (0.3) because the primary goal is not to extract from users, but to regulate a market and collect taxes. Suppression is also low (0.2) as the state's coercive power is significantly reduced compared to prohibition, focusing only on preventing external harms. Theater ratio is low (0.1) because the regulatory functions are genuinely aimed at public safety and revenue generation, not maintaining a facade. The metrics reflect a system designed for coordination and limited extraction, consistent with a Rope.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adult consumers and the legal industry, this is a beneficial coordination mechanism. From the perspective of prohibition advocates, it is a dangerous abdication of moral responsibility. The engine's classification will reflect the structural reality of reduced coercion and market-based extraction, rather than the moral arguments.
 *
 * DIRECTIONALITY LOGIC:
 *   Adult consumers and the legal substance industry are primary beneficiaries (d near 0.0), gaining freedom and market access. State authorities and tax authorities are also beneficiaries, gaining revenue and regulatory control. Victims of third-party harm are the primary payers (d near 1.0), bearing the residual costs of others' legal substance use. Prohibition advocates are excluded, as their core premise is rejected by this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    third_party_harm_quantification,
    'What is the actual magnitude and distribution of third-party harms under a legalization regime, and how effectively are they mitigated by regulation?',
    'Longitudinal epidemiological studies and public safety data from jurisdictions with legalized substance markets, comparing pre- and post-legalization harm indicators.',
    'If third-party harms are found to be substantial and poorly mitigated, the ''payer'' role of victims would be amplified, potentially shifting the constraint towards a Tangled Rope or even Snare if the benefits to industry outweigh the unmitigated social costs. If harms are minimal, the Rope classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(third_party_harm_quantification, empirical, 'Empirical assessment of third-party harms under legalization.').

omega_variable(
    corporate_extraction_potential,
    'Does the legal substance industry, over time, develop monopolistic or oligopolistic structures that lead to excessive pricing and marketing, effectively re-introducing extraction from consumers?',
    'Market concentration analysis, price elasticity studies, and regulatory oversight effectiveness reviews in mature legalized markets.',
    'If significant corporate extraction emerges, the ''beneficiary'' role of the industry would be re-evaluated, and the constraint could shift towards a Tangled Rope, as the coordination function (safe supply) becomes intertwined with asymmetric market power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_extraction_potential, empirical, 'Potential for corporate rent-seeking in a legalized market.').

omega_variable(
    autonomy_vs_paternalism_boundary,
    'At what point does state intervention to prevent ''third-party harm'' cross into paternalistic control over individual autonomy, and is this boundary consistently applied?',
    'Legal and philosophical analysis of specific regulations (e.g., public consumption bans, advertising restrictions) and their justification, alongside public discourse analysis on perceived overreach.',
    'If the boundary is frequently crossed, the ''suppression'' metric might be understated, and the constraint could be re-read as a more coercive form of Tangled Rope, where the state''s ''harm prevention'' is a cover for broader control.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_vs_paternalism_boundary, conceptual, 'The conceptual boundary between harm prevention and paternalism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t5, substance_control_legitimacy__legalization_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__legalization_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__legalization_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(subs_be_t5, substance_control_legitimacy__legalization_reading, base_extractiveness, 5, 0.27).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__legalization_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__legalization_reading, base_extractiveness, 15, 0.29).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(subs_su_t5, substance_control_legitimacy__legalization_reading, suppression_requirement, 5, 0.22).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__legalization_reading, suppression_requirement, 10, 0.21).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__legalization_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
