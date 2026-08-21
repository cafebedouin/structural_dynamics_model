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
 *   It shifts users from a victim set to a beneficiary set, introduces a
 *   legal industry as a beneficiary, and positions the state as a revenue
 *   collector and regulator, rather than a prohibitor. The black market is
 *   expected to diminish but may persist in niche areas. This reading
 *   contrasts sharply with prohibitionist and pure harm-reduction approaches.
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
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, 'b23707ca-3ee0-4ced-8e1b-10901f22c699').
narrative_ontology:cs_kernel_codification('b23707ca-3ee0-4ced-8e1b-10901f22c699', formalized).
narrative_ontology:cs_authority_grounding('b23707ca-3ee0-4ced-8e1b-10901f22c699', lineage).
narrative_ontology:cs_interpretation_layer_present('b23707ca-3ee0-4ced-8e1b-10901f22c699').
narrative_ontology:cs_reading_relation('b23707ca-3ee0-4ced-8e1b-10901f22c699', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('b23707ca-3ee0-4ced-8e1b-10901f22c699', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('b23707ca-3ee0-4ced-8e1b-10901f22c699', foundational, individual_autonomy_over_body).
narrative_ontology:cs_axiom_status(individual_autonomy_over_body, holdable).
narrative_ontology:cs_axiom_grounding('b23707ca-3ee0-4ced-8e1b-10901f22c699', individual_autonomy_over_body, deontological).
narrative_ontology:cs_axiom('b23707ca-3ee0-4ced-8e1b-10901f22c699', foundational, state_limited_to_third_party_harm).
narrative_ontology:cs_axiom_status(state_limited_to_third_party_harm, holdable).
narrative_ontology:cs_axiom_grounding('b23707ca-3ee0-4ced-8e1b-10901f22c699', state_limited_to_third_party_harm, conventional).
narrative_ontology:cs_reference_frame('b23707ca-3ee0-4ced-8e1b-10901f22c699', classical_liberal_state).
narrative_ontology:cs_drift_state('b23707ca-3ee0-4ced-8e1b-10901f22c699', contemporary_public_health_challenges, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('b23707ca-3ee0-4ced-8e1b-10901f22c699', '').
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

% Can legally acquire and use substances, free from criminal penalties, subject to regulations preventing third-party harm. Bears costs of taxation and potential health impacts, but gains autonomy.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, individual_users, beneficiary,
    moderate, biographical, mobile, national).

% Operates legally, producing and distributing substances, generating profits. Subject to taxation and regulation, but gains market access and legitimacy.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, legal_substance_industry, beneficiary,
    organized, generational, mobile, national).

% Collects tax revenue from legal substance sales, which can be used to offset externality costs or fund public services. Sets and enforces regulations to prevent third-party harm.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_revenue_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Monitors public health outcomes related to substance use, advises on regulatory frameworks, and manages public health campaigns. Their role shifts from prohibition enforcement to harm mitigation and education.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, public_health_authorities, observer,
    institutional, generational, analytical, national).

% Suffer harm (e.g., traffic accidents, secondhand exposure, public nuisance) from substance use by others, despite state intervention to prevent such harms. Their costs are the residual externalities not fully captured or prevented.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, third_party_victims_of_externalities, payer,
    powerless, immediate, trapped, local).

% Lose market share to legal industry but may persist in gray areas or by offering unregulated products. Their exclusion is a direct consequence of legalization and state regulation.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, black_market_operators, excluded,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual liberty with public safety by allowing legal substance use while regulating to prevent and mitigate third-party harms and capture externality costs through taxation.
% TRANSFER_FUNCTION: Transfers tax revenue from legal substance sales to the state, and transfers the right to use substances from the state to individuals, while attempting to transfer externality costs from third parties to users/industry via regulation and taxation.
% ABSENT_VOICES: Prohibition advocates would argue for stricter controls based on moral or public health grounds, while some harm reduction advocates might argue for even less state intervention in individual use, focusing purely on health services.
% DISAPPEARANCE_RATIONALE: If this framework vanished, substance use would either revert to a prohibitionist model (if enforcement capacity remained) or descend into unregulated chaos, leading to increased third-party harms and a resurgence of black markets. The legal industry would collapse, and state revenue streams would disappear.
% FOUNDING_PROBLEM: The prohibition of substances created a vast black market, fueled crime, failed to eliminate use, and infringed on individual liberties, while also failing to address public health issues effectively.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for legalization, economists, and civil liberties organizations attest that the problems of prohibition (crime, liberty infringement, ineffective public health) remain live under prohibitionist regimes. Public health data also corroborates the ongoing challenges of managing substance use under any regime.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.25) because the primary goal is not extraction but regulated liberty, with costs primarily from taxation and externality mitigation. Suppression is also low (0.15) as the state's role shifts from criminal enforcement against users to regulatory oversight of industry and public safety. Theater ratio is minimal (0.05) as the system is designed to be transparent and functional in its stated goals. The metrics reflect a system that, by its own lights, is a coordination mechanism for individual liberty and public safety, rather than a coercive or performative one.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individual users and the legal industry, this is a clear Rope, enabling freedom and commerce. From the perspective of third-party victims, it may still feel extractive if externalities are not fully addressed. Prohibition advocates would see this as a Snare, enabling societal decay, while pure harm reductionists might see it as a Tangled Rope, still imposing unnecessary state control.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual users and the legal substance industry are primary beneficiaries, gaining legal access and market opportunities, respectively. State revenue agencies benefit from new tax streams. Third-party victims of externalities bear residual costs not fully mitigated by regulation. Black market operators are excluded, losing their illicit market. Public health authorities act as observers and advisors, shifting their focus from enforcement to health outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by clearly distinguishing a system designed for regulated liberty (Rope) from one designed for pure extraction (Snare) or coercive coordination (Tangled Rope). The low extractiveness and suppression, coupled with identified beneficiaries and a clear coordination function, align with a Rope classification, even with the presence of victims from externalities. The system's mandate is to balance liberty and harm prevention, which it actively pursues, preventing mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_capture_efficacy,
    'How effectively does state intervention (taxation, regulation) capture and mitigate the full range of third-party externality costs?',
    'Longitudinal epidemiological and economic studies comparing pre- and post-legalization externality costs, including healthcare, public safety, and environmental impacts.',
    'If externality capture is low, the effective extractiveness on third parties is higher than measured, potentially shifting the classification for the ''third_party_victims'' seat towards Snare. If high, the Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_capture_efficacy, empirical, 'Uncertainty regarding the state''s ability to fully internalize externality costs.').

omega_variable(
    black_market_persistence,
    'To what extent does a black market for substances persist under a legalization regime, and what are its structural drivers?',
    'Market analysis comparing prices, product availability, and consumer preferences in legal vs. illicit markets, alongside law enforcement data on illegal trade.',
    'Significant black market persistence would indicate a failure of the legalization reading''s coordination function, potentially increasing suppression and extractiveness for users who remain in the illicit market, and for the state in its ongoing enforcement efforts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_persistence, empirical, 'Uncertainty about the complete displacement of the black market by legalization.').

omega_variable(
    reading_framing_legitimacy,
    'Is the ''individual liberty'' framing of substance use genuinely foundational, or is it a cover for economic interests of the legal substance industry?',
    'Analysis of legislative lobbying, campaign finance, and public discourse to identify the dominant drivers of legalization policy, alongside philosophical analysis of the liberty claim''s coherence.',
    'If the liberty framing is primarily a cover for economic interests, the constraint''s true extractiveness (ε) might be higher, and the ''legal_substance_industry'' seat''s directionality would be more strongly beneficiary, potentially shifting the overall classification towards a Tangled Rope or Snare from an analytical perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_legitimacy, conceptual, 'Ambiguity in the true grounding of the legalization reading''s core premise.').


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


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
