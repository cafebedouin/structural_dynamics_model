% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__expansive_secondary_objectives, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Mandate: Expansive Secondary Objectives
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   This constraint represents the 'expansive secondary objectives' reading
 *   of the ECB's mandate under Article 127 TFEU, which permits giving
 *   operational weight to employment and growth objectives when price
 *   stability is not threatened, leveraging the 'without prejudice' clause
 *   for discretionary balancing. This interpretation broadens the ECB's
 *   policy toolkit beyond a singular focus on inflation, allowing for
 *   consideration of broader economic welfare. It is a contested reading,
 *   with 'orthodox price stability' advocates arguing for strict
 *   subordination of secondary objectives.
 *
 * KEY AGENTS:
 *   - ecb_governing_council: Agenda setter (institutional/generational) — interprets and implements the mandate.
 *   - eurozone_governments: Beneficiary (institutional/generational) — benefit from policies supporting employment and growth.
 *   - workers: Beneficiary (powerless/biographical) — benefit from policies that prioritize employment.
 *   - debtors: Beneficiary (powerless/biographical) — benefit from policies that may lead to lower interest rates or higher inflation.
 *   - inflation_hawks: Payer (organized/biographical) — bear the cost of potentially higher inflation or less strict price stability.
 *   - savers: Payer (powerless/biographical) — bear the cost of lower real returns on savings due to inflation or low interest rates.
 *   - orthodox_economists: Excluded (analytical/biographical) — advocate for a stricter interpretation of the mandate, but their views are often marginalized in this reading's policy discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.45).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.55).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.45).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Mandate: Expansive Secondary Objectives").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, 'fafe396f-8f48-47bc-b804-1f662042a6b8').
narrative_ontology:cs_kernel_codification('fafe396f-8f48-47bc-b804-1f662042a6b8', fixed_text).
narrative_ontology:cs_authority_grounding('fafe396f-8f48-47bc-b804-1f662042a6b8', lineage).
narrative_ontology:cs_interpretation_layer_present('fafe396f-8f48-47bc-b804-1f662042a6b8').
narrative_ontology:cs_reading_relation('fafe396f-8f48-47bc-b804-1f662042a6b8', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('fafe396f-8f48-47bc-b804-1f662042a6b8', ecb_mandate_article_127__climate_incorporation, coexists_with).
narrative_ontology:cs_axiom('fafe396f-8f48-47bc-b804-1f662042a6b8', foundational, discretionary_balancing_of_objectives).
narrative_ontology:cs_axiom_status(discretionary_balancing_of_objectives, holdable).
narrative_ontology:cs_axiom_grounding('fafe396f-8f48-47bc-b804-1f662042a6b8', discretionary_balancing_of_objectives, conventional).
narrative_ontology:cs_axiom('fafe396f-8f48-47bc-b804-1f662042a6b8', foundational, secondary_objectives_operational_when_price_stability_not_threatened).
narrative_ontology:cs_axiom_status(secondary_objectives_operational_when_price_stability_not_threatened, holdable).
narrative_ontology:cs_axiom_grounding('fafe396f-8f48-47bc-b804-1f662042a6b8', secondary_objectives_operational_when_price_stability_not_threatened, conventional).
narrative_ontology:cs_reference_frame('fafe396f-8f48-47bc-b804-1f662042a6b8', holistic_economic_welfare_framework).
narrative_ontology:cs_drift_state('fafe396f-8f48-47bc-b804-1f662042a6b8', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fafe396f-8f48-47bc-b804-1f662042a6b8', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_governments).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, workers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, debtors).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, inflation_hawks).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, savers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary decision-making body of the ECB, responsible for interpreting and implementing the mandate. This reading grants them significant discretion in balancing objectives, which they view as essential for effective monetary policy in a complex economic environment.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from an ECB mandate that allows for policies supporting employment and economic growth, which can ease fiscal pressures and improve social outcomes in their respective countries. They advocate for this expansive interpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_governments, beneficiary,
    institutional, generational, constrained, continental).

% Benefit from monetary policies that prioritize employment, potentially leading to lower unemployment rates and better wage growth. Their economic well-being is directly influenced by the ECB's policy choices under this interpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, workers, beneficiary,
    powerless, biographical, trapped, national).

% Benefit from policies that may lead to lower interest rates or moderate inflation, which can reduce the real burden of their debts. This reading of the mandate implicitly supports their financial stability.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, debtors, beneficiary,
    powerless, biographical, trapped, national).

% Advocate for strict price stability and view any deviation towards employment or growth as a betrayal of the ECB's primary mandate. They bear the cost of policies that might lead to higher inflation or perceived monetary laxity, often through public criticism and political pressure.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, inflation_hawks, payer,
    organized, biographical, constrained, continental).

% Bear the cost of lower real returns on their savings due to policies that prioritize growth over strict price stability, potentially leading to lower interest rates or higher inflation. Their financial security is indirectly impacted.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, savers, payer,
    powerless, biographical, constrained, national).

% Academics and policy advisors who adhere to a strict interpretation of central bank mandates, prioritizing price stability above all else. Their arguments for a narrower ECB focus are often sidelined in policy debates shaped by this expansive reading.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_economists, excluded,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_governments).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__expansive_secondary_objectives, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate monetary policy across the Eurozone to achieve both price stability and support for general economic policies like employment and growth, especially during periods where price stability is not threatened, allowing for a more holistic response to economic challenges.
% TRANSFER_FUNCTION: Transfers policy flexibility and potential economic benefits (e.g., lower unemployment, easier debt burdens) to eurozone governments, workers, and debtors, while transferring the costs of potentially higher inflation or lower real returns to savers and those who prioritize strict price stability.
% ABSENT_VOICES: Strict monetarists and those who believe central banks should have a singular, narrow mandate are often excluded from the policy-setting discourse under this expansive reading. They would argue that broadening the mandate introduces moral hazard and compromises central bank independence.
% DISAPPEARANCE_RATIONALE: If this expansive interpretation vanished, the ECB would likely revert to a stricter price stability focus, leading to different policy choices (e.g., higher interest rates, less quantitative easing). This would significantly alter economic conditions across the Eurozone, impacting employment, growth, and debt dynamics, forcing governments and markets to adapt to a more constrained monetary policy environment.
% FOUNDING_PROBLEM: The original problem was how to design a central bank mandate for a diverse monetary union that could ensure price stability while also being responsive to the broader economic welfare of its member states, particularly in times of crisis or low inflation.
% FOUNDING_PROBLEM_CORROBORATION: The problem of balancing price stability with other economic objectives remains live, particularly in the context of persistent low inflation, high unemployment in some member states, and the need for coordinated responses to economic shocks. This is corroborated by ongoing debates among EU institutions, national governments, and a segment of academic economists who argue for central bank flexibility beyond a narrow inflation target.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).
:- end_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates economic policy towards broader welfare goals (beneficiaries: eurozone governments, workers, debtors) while simultaneously extracting from others (victims: inflation hawks, savers) through the same structure. The extractiveness (0.45) reflects the trade-offs inherent in balancing multiple objectives, where some groups benefit at the expense of others. Suppression (0.55) is moderate, as this reading requires active defense against more orthodox interpretations and the marginalization of alternative policy frameworks. Theater ratio (0.20) is low, indicating that the stated coordination function for employment and growth is largely genuine, though its operationalization is subject to interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The ECB Governing Council, as the agenda setter, experiences this as a legitimate and necessary flexibility in its mandate, allowing it to adapt to diverse economic conditions. Beneficiaries like eurozone governments, workers, and debtors perceive it as a beneficial coordination mechanism. Conversely, inflation hawks and savers experience it as an extractive mechanism, where their interests (price stability, real returns) are subordinated or diluted. The engine's per-seat classification will reflect these divergent experiences based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council, eurozone governments, workers, and debtors are beneficiaries (lower d) as the mandate's flexibility allows for policies that directly or indirectly support their interests. Inflation hawks and savers are targets (higher d) as their preference for strict price stability or higher real returns is compromised by the broader policy scope. The 'without prejudice' clause, in this reading, acts as the mechanism that allows the ECB to direct benefits towards some while imposing costs on others, requiring active enforcement against dissenting interpretations.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as pure extraction by acknowledging the genuine coordination function for employment and growth. However, the contestation around the 'without prejudice' clause highlights a potential for mandatrophy if the secondary objectives become a permanent justification for policies that primarily benefit certain groups without clear economic justification, or if the original problem of economic instability is no longer 'live' but the expansive interpretation persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_interpretation_ambiguity,
    'Is the ''without prejudice'' clause a genuine authorization for discretionary balancing, or merely a rhetorical cover for policy drift?',
    'Analysis of ECB policy decisions and communications over time, particularly during periods of low inflation, to identify explicit trade-offs or prioritization of secondary objectives over strict price stability.',
    'If genuine, it reinforces the ''tangled_rope'' classification by demonstrating a legitimate coordination function for employment/growth. If rhetorical, it suggests a ''snare'' where the coordination story is cover for extraction from savers/inflation hawks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_interpretation_ambiguity, conceptual, 'Ambiguity in the interpretation of the ECB''s ''without prejudice'' clause regarding secondary objectives.').

omega_variable(
    kernel_reading_expansive_secondary_objectives,
    'This constraint is the ''expansive_secondary_objectives'' reading of the ''ecb_mandate_article_127'' kernel. How would the classification change under the ''orthodox_price_stability'' or ''climate_incorporation'' readings?',
    'Analyzing the structural properties (beneficiaries, victims, extractiveness, suppression) of the sibling readings as separate constraints.',
    'The ''orthodox_price_stability'' reading would likely show lower extractiveness from savers and higher suppression of employment/growth considerations, potentially classifying as a ''rope'' or ''mountain'' (if naturalized). The ''climate_incorporation'' reading would introduce new beneficiaries (environmental initiatives) and potentially new victims (carbon-intensive industries), shifting the balance of extraction and coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_expansive_secondary_objectives, conceptual, 'This constraint is one reading of the ECB mandate kernel, with different implications for classification than sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 5, 0.17).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 10, 0.18).
narrative_ontology:measurement(ecb__tr_t15, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 15, 0.19).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(ecb__be_t15, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(ecb__su_t15, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 15, 0.53).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ECB Mandate Article 127 kernel. This 'expansive secondary objectives' reading emphasizes discretionary balancing of employment/growth, distinct from the 'orthodox price stability' reading (exclusive focus on inflation) and the 'climate incorporation' reading (integrating climate risk).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
