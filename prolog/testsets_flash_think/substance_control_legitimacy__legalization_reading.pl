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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Adult Autonomy and Third-Party Harm Limitation in Substance Policy (Legalization Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'legalization reading' of the
 *   'substance_control_legitimacy' kernel. It posits that competent adults
 *   possess autonomy over their substance use, and state authority is
 *   legitimately limited to preventing third-party harm. This reading shifts
 *   the focus from criminalizing individual use to regulating legal markets
 *   and mitigating public health and safety risks. The metrics reflect a
 *   historical trend away from high extraction and suppression associated
 *   with prohibition towards a more moderate, market-driven extractive regime
 *   with targeted enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.45).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.35).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Adult Autonomy and Third-Party Harm Limitation in Substance Policy (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, '42d5f1e9-7369-4513-8eef-ebbc1b61b3b6').
narrative_ontology:cs_kernel_codification('42d5f1e9-7369-4513-8eef-ebbc1b61b3b6', formalized).
narrative_ontology:cs_authority_grounding('42d5f1e9-7369-4513-8eef-ebbc1b61b3b6', lineage).
narrative_ontology:cs_interpretation_layer_present('42d5f1e9-7369-4513-8eef-ebbc1b61b3b6').
narrative_ontology:cs_reading_relation('42d5f1e9-7369-4513-8eef-ebbc1b61b3b6', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('42d5f1e9-7369-4513-8eef-ebbc1b61b3b6', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('42d5f1e9-7369-4513-8eef-ebbc1b61b3b6', foundational, individual_bodily_autonomy).
narrative_ontology:cs_axiom_status(individual_bodily_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('42d5f1e9-7369-4513-8eef-ebbc1b61b3b6', individual_bodily_autonomy, deontological).
narrative_ontology:cs_axiom('42d5f1e9-7369-4513-8eef-ebbc1b61b3b6', foundational, state_limited_to_harm_prevention).
narrative_ontology:cs_axiom_status(state_limited_to_harm_prevention, holdable).
narrative_ontology:cs_axiom_grounding('42d5f1e9-7369-4513-8eef-ebbc1b61b3b6', state_limited_to_harm_prevention, conventional).
narrative_ontology:cs_created_at('42d5f1e9-7369-4513-8eef-ebbc1b61b3b6', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, competent_adults).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, state_treasuries).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, legal_substance_businesses).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, consumers).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, communities_affected_by_harm).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, individual_liberty_principle).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, harm_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise autonomy over their substance use, accessing legal substances in regulated markets. They benefit from reduced criminalization and stigma, but still bear market prices and taxes.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, competent_adults, beneficiary,
    moderate, biographical, mobile, global).

% Regulate legal substance markets, enforce public safety laws (e.g., impaired driving, public consumption), and collect tax revenue from sales. Their authority is limited to preventing third-party harm.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Operate legally in a regulated market, profiting from the production and sale of substances. They contribute tax revenue to the state and adhere to regulatory standards.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, legal_substance_businesses, beneficiary,
    powerful, biographical, mobile, national).

% Purchase legal substances, bearing market prices, taxes, and potential health risks. While benefiting from legal access, they are subject to market dynamics and regulatory costs.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, consumers, payer,
    moderate, immediate, constrained, local).

% Bear the social and economic costs of unmitigated third-party harms (e.g., impaired driving incidents, public health burdens from substance misuse) that may persist even in a legal framework.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, communities_affected_by_harm, payer,
    organized, biographical, constrained, local).

% Are excluded from the legal market and face continued enforcement for illegal production, distribution, and sale of substances that fall outside the regulated framework. Their activities are suppressed.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, illicit_market_actors, excluded,
    powerless, immediate, trapped, local).

% Monitor public health outcomes, advocate for effective harm mitigation strategies, and evaluate the impact of legalization policies on population health and safety within the new framework.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, public_health_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__legalization_reading, legal_substance_businesses).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the legal production, distribution, and consumption of substances for competent adults, while simultaneously coordinating state efforts to prevent and mitigate associated third-party harms.
% TRANSFER_FUNCTION: Transfers tax revenue from legal substance sales to state treasuries, transfers profits to legal substance businesses, and transfers the costs of market regulation and harm mitigation to consumers and taxpayers.
% ABSENT_VOICES: Prohibition advocates would argue that any substance use is inherently harmful and should be criminalized. Advocates for completely unregulated markets would object to state intervention and taxation. Both are structurally marginalized by this framework.
% DISAPPEARANCE_RATIONALE: If this framework vanished, the vacuum would likely be filled by either a return to prohibition (with its associated illicit markets and criminal justice costs) or a move towards a completely unregulated market (with potentially increased public health and safety harms). The current balance of autonomy and limited state intervention would be lost, fundamentally reorganizing society's relationship with substances.
% FOUNDING_PROBLEM: To address the failures of substance prohibition (e.g., illicit markets, mass incarceration, lack of quality control) while upholding individual liberty and mitigating genuine public health and safety risks associated with substance use.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations, public health researchers, and economists widely corroborate the ongoing challenge of balancing individual autonomy with public safety in substance policy, acknowledging the historical failures of prohibition and the complexities of legalization. This corroboration comes from outside the direct beneficiaries of the legal market.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   Base extractiveness (0.45) reflects the costs borne by consumers through market prices and taxes, as well as corporate profits within the legal market. While lower than a prohibition regime, it's not negligible. Suppression (0.35) is significantly reduced from prohibition, focusing on illicit markets and third-party harm prevention (e.g., impaired driving) rather than individual use. Theater ratio (0.15) is low, as enforcement efforts are generally functional in preventing specified harms. Accessibility collapse (0.45) is moderate; while legal access increases, illicit alternatives are suppressed, and the 'alternative' of not using substances remains. Resistance (0.25) is low, as the framework generally aligns with individual liberty, with resistance primarily from those advocating for stricter or looser controls. The claimed type 'tangled_rope' acknowledges the genuine coordination function of a regulated market alongside the asymmetric extraction from consumers and the profits accruing to legal businesses.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of competent adults, this framework is a significant liberation, shifting from victim to beneficiary. For legal substance businesses, it's an opportunity for profit within a regulated environment. However, consumers still bear market costs and taxes, and communities may still experience unmitigated harms. Prohibition advocates would see this as a dangerous erosion of state moral authority, while unregulated market advocates would see it as excessive state intervention.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent adults are beneficiaries due to increased autonomy and reduced criminalization. State treasuries benefit from tax revenue, and legal substance businesses from profits. Consumers are payers through market prices and taxes. Communities affected by harm are also payers, bearing the social costs of any unmitigated harms. Illicit market actors are excluded and targeted by enforcement. This structure leads to a 'tangled_rope' classification, balancing coordination with asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids mislabeling by acknowledging both the coordination function (safe, regulated access) and the extractive elements (corporate profits, state taxes, unmitigated harms). It prevents the 'legalization' narrative from being solely framed as a 'rope' (pure coordination) by highlighting the persistent costs and beneficiaries, and from being a 'snare' by recognizing the genuine benefits of autonomy and reduced criminalization. The 'live' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, indicates that the constraint is still functionally relevant, albeit with evolving dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    corporate_extraction_magnitude,
    'To what extent does legalization merely shift extraction from state criminalization to corporate rent-seeking within a legal market?',
    'Economic analysis comparing profit margins and market concentration in legal substance industries to other regulated consumer goods, alongside tax revenue distribution studies.',
    'If corporate extraction is found to be high and concentrated, the effective extractiveness of the constraint for consumers would be higher, potentially pushing it closer to a Snare for that seat. If low, it reinforces the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_extraction_magnitude, empirical, 'Assesses whether legalization primarily benefits corporations at consumer expense.').

omega_variable(
    third_party_harm_mitigation_efficacy,
    'How effectively does the state''s limited authority prevent and mitigate third-party harms (e.g., impaired driving, secondhand exposure, public health costs) in a legalized framework?',
    'Longitudinal public health and safety data from jurisdictions with legalized substance markets, comparing pre- and post-legalization trends in relevant harm indicators.',
    'If mitigation is ineffective, the ''communities_affected_by_harm'' seat experiences higher extraction, potentially increasing the overall extractiveness of the constraint and weakening its coordination claim. If highly effective, it strengthens the ''rope'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_harm_mitigation_efficacy, empirical, 'Evaluates the actual impact of limited state intervention on public safety and health.').

omega_variable(
    autonomy_vs_paternalism_boundary,
    'Where is the legitimate boundary between individual autonomy over substance use and state paternalistic intervention for individual well-being?',
    'Philosophical and legal discourse, public deliberation, and evolving social norms regarding individual rights versus collective welfare. This is a conceptual and preference-based question.',
    'A shift towards greater paternalism would reintroduce elements of prohibition, increasing suppression and extractiveness. A stronger emphasis on autonomy would further limit state intervention, potentially reducing extraction but also potentially increasing unmitigated harms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_vs_paternalism_boundary, conceptual, 'Examines the fundamental philosophical tension underlying substance policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_legitimacy__legalization_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(subs_tr_t1985, substance_control_legitimacy__legalization_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(subs_tr_t2000, substance_control_legitimacy__legalization_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(subs_tr_t2010, substance_control_legitimacy__legalization_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(subs_tr_t2020, substance_control_legitimacy__legalization_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(subs_tr_t2025, substance_control_legitimacy__legalization_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_legitimacy__legalization_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(subs_be_t1985, substance_control_legitimacy__legalization_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(subs_be_t2000, substance_control_legitimacy__legalization_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(subs_be_t2010, substance_control_legitimacy__legalization_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(subs_be_t2020, substance_control_legitimacy__legalization_reading, base_extractiveness, 2020, 0.46).
narrative_ontology:measurement(subs_be_t2025, substance_control_legitimacy__legalization_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_legitimacy__legalization_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(subs_su_t1985, substance_control_legitimacy__legalization_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(subs_su_t2000, substance_control_legitimacy__legalization_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(subs_su_t2010, substance_control_legitimacy__legalization_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(subs_su_t2020, substance_control_legitimacy__legalization_reading, suppression_requirement, 2020, 0.36).
narrative_ontology:measurement(subs_su_t2025, substance_control_legitimacy__legalization_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('legalization_reading') of the 'substance_control_legitimacy' kernel, which also includes 'prohibition_reading' and 'harm_reduction_reading'. Each reading represents a distinct structural claim about state authority and individual autonomy regarding substance use, with different ε values and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
