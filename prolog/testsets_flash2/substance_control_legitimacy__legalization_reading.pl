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
 *   This constraint represents the 'legalization reading' of substance
 *   control legitimacy, where competent adults retain autonomy over their
 *   substance use, and state intervention is strictly limited to preventing
 *   demonstrable harm to third parties. It shifts the focus from
 *   criminalization of users to regulation of markets and mitigation of
 *   public harms. The metrics reflect a relatively low-extraction,
 *   low-suppression regime compared to prohibition, but with inherent costs
 *   for managing externalities.
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
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Adult Autonomy in Substance Use (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, '3e548cd1-2d05-4f18-89ef-27460401d471').
narrative_ontology:cs_kernel_codification('3e548cd1-2d05-4f18-89ef-27460401d471', formalized).
narrative_ontology:cs_authority_grounding('3e548cd1-2d05-4f18-89ef-27460401d471', lineage).
narrative_ontology:cs_interpretation_layer_present('3e548cd1-2d05-4f18-89ef-27460401d471').
narrative_ontology:cs_reading_relation('3e548cd1-2d05-4f18-89ef-27460401d471', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('3e548cd1-2d05-4f18-89ef-27460401d471', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('3e548cd1-2d05-4f18-89ef-27460401d471', foundational, adult_bodily_autonomy).
narrative_ontology:cs_axiom_status(adult_bodily_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('3e548cd1-2d05-4f18-89ef-27460401d471', adult_bodily_autonomy, deontological).
narrative_ontology:cs_axiom('3e548cd1-2d05-4f18-89ef-27460401d471', foundational, state_limited_to_preventing_external_harm).
narrative_ontology:cs_axiom_status(state_limited_to_preventing_external_harm, holdable).
narrative_ontology:cs_axiom_grounding('3e548cd1-2d05-4f18-89ef-27460401d471', state_limited_to_preventing_external_harm, conventional).
narrative_ontology:cs_reference_frame('3e548cd1-2d05-4f18-89ef-27460401d471', liberal_autonomy_framework).
narrative_ontology:cs_drift_state('3e548cd1-2d05-4f18-89ef-27460401d471', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3e548cd1-2d05-4f18-89ef-27460401d471', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, competent_adults).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, legal_substance_industries).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, tax_authorities).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, third_party_victims_of_impaired_use).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, minors_exposed_to_substance_marketing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal access to substances for personal use, free from criminal penalties. They benefit from reduced stigma and a regulated market, but bear responsibility for preventing harm to others.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, competent_adults, beneficiary,
    organized, biographical, mobile, national).

% Operate legally in a regulated market, generating profits and tax revenue. They are subject to regulations on production, marketing, and sales, but benefit from market access and legitimacy.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, legal_substance_industries, beneficiary,
    powerful, generational, arbitrage, national).

% Collect significant tax revenue from legal substance sales, which can be used to fund public services or harm prevention programs. Their role is to administer and enforce tax laws.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, tax_authorities, beneficiary,
    institutional, generational, analytical, national).

% Establish and enforce regulations to prevent third-party harm (e.g., impaired driving laws, public consumption restrictions, marketing to minors). They balance individual autonomy with public safety.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Suffer direct harm (e.g., injury, property damage) from individuals who use substances irresponsibly. While the constraint aims to prevent this, residual harm remains a cost they bear.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, third_party_victims_of_impaired_use, payer,
    powerless, immediate, trapped, local).

% Are vulnerable to marketing and normalization of substance use, potentially leading to earlier initiation or increased risk. Regulations aim to protect them, but exposure is a persistent risk.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, minors_exposed_to_substance_marketing, payer,
    powerless, biographical, trapped, local).

% Believe all substance use is inherently harmful and should be criminalized. They are excluded from the policy-making process under this reading, which prioritizes autonomy and harm reduction over moral prohibition.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, prohibition_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual autonomy with public safety by establishing clear boundaries for legal substance use and state intervention, allowing for regulated markets while mitigating third-party harms.
% TRANSFER_FUNCTION: Transfers the burden of moral judgment from the state to the individual, while transferring regulatory oversight and tax revenue to the state, and potential residual harm to third parties.
% ABSENT_VOICES: Advocates for complete prohibition are excluded, as their moral framing of substance use is not the basis for this policy. They would argue for a more restrictive approach, emphasizing inherent harm and state paternalism.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the legal substance market would collapse into an unregulated free-for-all, leading to increased public health and safety risks, and a loss of tax revenue. The criminal justice system would also be profoundly altered.
% FOUNDING_PROBLEM: The problem of individual liberty versus state control over personal choices, particularly concerning activities with potential societal consequences, and the failure of prohibition to eliminate substance use while creating black markets and criminal justice burdens.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and civil liberties organizations corroborate the ongoing tension between individual autonomy and state power. Public health economists corroborate the failures of prohibition and the potential for regulated markets to generate revenue and reduce some harms, from outside the benefiting industries.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.25) because the primary goal is not to extract from users, but to manage a legal market and prevent harm. Suppression is also low (0.15) as it primarily targets behaviors causing third-party harm (e.g., impaired driving) rather than the act of use itself. Theater ratio is minimal (0.05) as the regulatory functions are largely genuine. The slight decrease in extractiveness and suppression over time reflects a maturing regulatory framework becoming more efficient.
 *
 * PERSPECTIVAL GAP:
 *   While this reading aims for a balanced approach, prohibition advocates would perceive it as highly extractive from society's moral fabric, while some harm reduction advocates might see it as not going far enough to address underlying social determinants of problematic use. The engine's classification will reflect the structural flows, not these external moral or social critiques.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent adults and legal industries are beneficiaries, gaining autonomy and market access. Tax authorities also benefit from revenue. State regulatory bodies act as agenda-setters, balancing autonomy and public safety. Third-party victims and minors are payers, bearing residual harms that the constraint aims to minimize but cannot eliminate entirely.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    third_party_harm_definition,
    'How broadly is ''third-party harm'' defined, and does this definition inadvertently expand state authority beyond the stated intent?',
    'Judicial review of regulatory scope and legislative intent analysis. Empirical studies on the actual incidence and nature of harms attributed to substance use.',
    'A broad definition could lead to ''mission creep'' where state authority expands into areas resembling paternalistic control, increasing suppression and extractiveness. A narrow definition would reinforce individual autonomy but might leave some harms unaddressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_harm_definition, conceptual, 'Ambiguity in defining the scope of legitimate state intervention.').

omega_variable(
    corporate_influence_on_regulation,
    'To what extent do legal substance industries influence regulatory bodies to shape rules that benefit their market position, potentially at the expense of public health goals?',
    'Lobbying disclosure analysis, campaign finance tracking, and independent investigations into regulatory capture.',
    'Significant corporate influence could shift the constraint towards a ''tangled_rope'' or ''snare'' for public health, where coordination (legal market) is used as cover for extraction (industry profits at public expense), increasing extractiveness and theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_influence_on_regulation, empirical, 'Risk of regulatory capture by legal substance industries.').

omega_variable(
    autonomy_competence_boundary,
    'How is ''competent adult'' defined, and does this definition exclude vulnerable populations who could benefit from autonomy but are deemed ''incompetent''?',
    'Ethical review of competence criteria, legal challenges to definitions, and sociological studies of excluded populations.',
    'A narrow definition of competence could create a ''snare'' for excluded populations, denying them autonomy while subjecting them to paternalistic or coercive interventions, increasing suppression for those groups.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_competence_boundary, preference, 'The boundary of ''competent adult'' and its impact on excluded groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(subs_tr_t5, substance_control_legitimacy__legalization_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__legalization_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__legalization_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t5, substance_control_legitimacy__legalization_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__legalization_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__legalization_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(subs_su_t5, substance_control_legitimacy__legalization_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__legalization_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__legalization_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is the 'legalization_reading' of the 'substance_control_legitimacy' kernel, distinct from the 'prohibition_reading' and 'harm_reduction_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
