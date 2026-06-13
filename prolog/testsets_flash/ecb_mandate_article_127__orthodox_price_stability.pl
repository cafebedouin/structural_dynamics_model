% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__orthodox_price_stability, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Mandate (Orthodox Price Stability Reading)
 *   domain: monetary_policy/institutional_governance/constitutional_law
 *
 * SUMMARY:
 *   This constraint describes the 'orthodox price stability' reading of the
 *   ECB's mandate under Article 127 TFEU, which asserts an exclusive focus on
 *   achieving a 2% inflation target, with all other objectives (like
 *   employment or climate action) strictly subordinate and non-operational.
 *   This reading is actively enforced by the ECB's Governing Council and
 *   supported by a segment of economic and legal opinion. It is one of
 *   several contested interpretations of the ECB's foundational legal text.
 *
 * KEY AGENTS:
 *   - ecb_governing_council: Agenda setter (institutional/arbitrage) — enforces the orthodox reading.
 *   - savers_and_creditors: Beneficiaries (moderate/constrained) — benefit from low inflation and stable asset values.
 *   - high_debt_member_states: Victims (institutional/constrained) — bear the costs of tight monetary policy, hindering growth and debt sustainability.
 *   - unemployed_citizens: Victims (powerless/trapped) — suffer from policies prioritizing inflation over employment.
 *   - climate_risk_exposed_sectors: Victims (organized/constrained) — externalized climate risks are not addressed by monetary policy.
 *   - orthodox_economists: Beneficiaries (analytical/analytical) — their intellectual framework is vindicated and influential.
 *   - expansive_mandate_advocates: Excluded (organized/constrained) — argue for broader interpretation but are institutionally sidelined.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.65).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.78).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.65).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Mandate (Orthodox Price Stability Reading)").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/institutional_governance/constitutional_law").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, '2b86b360-7626-4c29-aeda-6060863585c3').
narrative_ontology:cs_kernel_codification('2b86b360-7626-4c29-aeda-6060863585c3', fixed_text).
narrative_ontology:cs_authority_grounding('2b86b360-7626-4c29-aeda-6060863585c3', lineage).
narrative_ontology:cs_interpretation_layer_present('2b86b360-7626-4c29-aeda-6060863585c3').
narrative_ontology:cs_reading_relation('2b86b360-7626-4c29-aeda-6060863585c3', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_reading_relation('2b86b360-7626-4c29-aeda-6060863585c3', ecb_mandate_article_127__climate_incorporation, coexists_with).
narrative_ontology:cs_axiom('2b86b360-7626-4c29-aeda-6060863585c3', foundational, price_stability_is_primary_and_exclusive).
narrative_ontology:cs_axiom_status(price_stability_is_primary_and_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('2b86b360-7626-4c29-aeda-6060863585c3', price_stability_is_primary_and_exclusive, deontological).
narrative_ontology:cs_axiom('2b86b360-7626-4c29-aeda-6060863585c3', foundational, secondary_objectives_are_non_operational).
narrative_ontology:cs_axiom_status(secondary_objectives_are_non_operational, holdable).
narrative_ontology:cs_axiom_grounding('2b86b360-7626-4c29-aeda-6060863585c3', secondary_objectives_are_non_operational, conventional).
narrative_ontology:cs_reference_frame('2b86b360-7626-4c29-aeda-6060863585c3', maastricht_treaty_original_intent).
narrative_ontology:cs_drift_state('2b86b360-7626-4c29-aeda-6060863585c3', contemporary_challenges_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('2b86b360-7626-4c29-aeda-6060863585c3', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, savers_and_creditors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, financial_stability_advocates).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, orthodox_economists).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, high_debt_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, unemployed_citizens).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_risk_exposed_sectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary decision-making body of the ECB, responsible for setting monetary policy and interpreting its mandate. They actively enforce the orthodox reading of price stability, resisting calls for broader objectives.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, arbitrage, continental).

% Individuals and institutions whose wealth is held in cash or fixed-income assets. They benefit from low and stable inflation, which preserves the real value of their savings and returns on loans.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, savers_and_creditors, beneficiary,
    moderate, biographical, constrained, national).

% Eurozone countries with high public debt levels. They bear the costs of tight monetary policy, which can make debt servicing more expensive and hinder economic growth, making fiscal consolidation more challenging.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, high_debt_member_states, payer,
    institutional, generational, constrained, national).

% Individuals seeking employment. They are negatively impacted by monetary policies that prioritize inflation control over employment growth, potentially leading to higher unemployment rates or slower job creation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, unemployed_citizens, payer,
    powerless, biographical, trapped, national).

% Industries and regions highly vulnerable to climate change impacts or transition risks. They bear the costs of monetary policy that does not actively integrate climate risks into its framework, potentially exacerbating financial instability in these sectors.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_risk_exposed_sectors, payer,
    organized, generational, constrained, continental).

% Institutions and experts who prioritize the stability of the financial system. They benefit from the orthodox reading's focus on price stability as a prerequisite for financial stability, aligning with their policy goals.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, financial_stability_advocates, beneficiary,
    institutional, generational, analytical, continental).

% Academics and policymakers whose economic theories emphasize the primacy of price stability. Their intellectual framework is validated and influential within the ECB's decision-making processes.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, orthodox_economists, beneficiary,
    analytical, generational, analytical, global).

% Groups (e.g., trade unions, some political parties, heterodox economists) who argue for a broader interpretation of the ECB's mandate to include employment, growth, or social objectives. Their arguments are institutionally sidelined by the orthodox reading.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, expansive_mandate_advocates, excluded,
    organized, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__orthodox_price_stability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, singular objective for monetary policy across the diverse Eurozone, aiming to coordinate economic expectations around stable prices and thus facilitate investment and trade.
% TRANSFER_FUNCTION: Transfers economic stability benefits (e.g., predictable inflation, preserved purchasing power) to savers and creditors, while transferring costs (e.g., higher unemployment, slower growth, unaddressed climate risks) to high-debt member states, the unemployed, and climate-vulnerable sectors.
% ABSENT_VOICES: Advocates for a broader mandate (e.g., employment, growth, climate action) are institutionally marginalized. Their arguments for a more balanced approach are not given operational weight within the ECB's decision-making, despite being present in broader EU discourse.
% DISAPPEARANCE_RATIONALE: If the orthodox price stability mandate vanished, the ECB's policy framework would immediately become ambiguous, leading to divergent national monetary policies, increased inflation volatility, and potentially a breakdown of the Eurozone's single monetary policy. Economic expectations would destabilize, and financial markets would react severely.
% FOUNDING_PROBLEM: The Eurozone was established with a strong emphasis on price stability to avoid the historical inflation problems of some member states and to build credibility for the new currency.
% FOUNDING_PROBLEM_CORROBORATION: The problem of maintaining price stability in a diverse monetary union is still live, as attested by central bank governors globally and independent economic analyses. However, the 'orthodox' interpretation's exclusivity is contested by many economists and policymakers who argue that the problem has evolved to include other systemic risks (e.g., climate, financial stability) that require a broader mandate, as evidenced by academic literature and parliamentary debates.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it provides a genuine coordination function (price stability for the Eurozone) but also involves significant asymmetric extraction. Savers and creditors benefit from the focus on low inflation, while high-debt member states and the unemployed bear the costs of tight monetary policy. Suppression is high (0.78) because alternative interpretations of the mandate are actively resisted and institutionally sidelined. Extractiveness (0.65) reflects the costs imposed on those whose objectives are subordinated. Theater ratio is low (0.15) as the ECB genuinely pursues its stated primary objective, but the 'orthodoxy' itself is a performance of institutional identity.
 *
 * PERSPECTIVAL GAP:
 *   The ECB Governing Council and orthodox economists perceive this as a necessary, legitimate constraint for economic stability (Rope-like). However, high-debt member states, the unemployed, and climate advocates experience it as an extractive Snare, where their legitimate concerns are suppressed in favor of a narrow interpretation that benefits others. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council, as the agenda setter, is a clear beneficiary (d=0.0-0.1). Savers and creditors also benefit from the policy's outcomes (d=0.1-0.2). High-debt member states, unemployed citizens, and climate-risk-exposed sectors are targets, bearing the costs of the narrow focus (d=0.8-1.0). Orthodox economists are beneficiaries as their intellectual framework is validated. Expansive mandate advocates are excluded, their directionality is effectively 1.0 due to their inability to influence the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (price stability) is still live, but its 'orthodox' interpretation is contested. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring the coordination function). The rising extractiveness and suppression over time suggest an enforcement ratchet, where the 'orthodox' reading is increasingly defended against challenges, accumulating costs on those whose objectives are subordinated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_interpretation_ambiguity,
    'Is the ECB''s mandate truly exclusive to price stability, or does Article 127 permit operational weight on secondary objectives?',
    'ECJ ruling on the interpretation of ''without prejudice'' clause, or a formal amendment to the Treaty on the Functioning of the European Union (TFEU).',
    'If the mandate is found to permit broader operational scope, the constraint''s suppression of secondary objectives would be reclassified as illegitimate, potentially shifting it from Tangled Rope towards a more balanced Rope or even a Scaffold if temporary. If confirmed as exclusive, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_interpretation_ambiguity, conceptual, 'Ambiguity in the legal interpretation of the ECB''s primary mandate versus secondary objectives.').

omega_variable(
    climate_risk_externalization,
    'Does the orthodox reading of the mandate, by externalizing climate risks from monetary policy, create a larger, unaddressed systemic risk for the Eurozone economy?',
    'Empirical evidence of climate-related financial instability directly impacting price stability, or a re-evaluation of Article 11 TFEU''s environmental integration clause by the ECJ.',
    'If climate risks are found to be a direct threat to price stability, the current reading''s ''suppression'' of climate considerations would be seen as counterproductive to its own primary objective, potentially leading to a reclassification towards a Snare for the Eurozone economy as a whole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_risk_externalization, empirical, 'Whether the narrow mandate creates unaddressed systemic risks.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''orthodox_price_stability'' reading of the ''ecb_mandate_article_127'' kernel. What would change if the ''expansive_secondary_objectives'' or ''climate_incorporation'' readings were adopted?',
    'Adoption of a different reading by the ECB Governing Council or a legal reinterpretation by the ECJ.',
    'The ''expansive_secondary_objectives'' reading would broaden the beneficiary set to include employment/growth advocates and reduce suppression of those objectives. The ''climate_incorporation'' reading would introduce climate-exposed sectors as beneficiaries and reduce suppression of climate risk integration, potentially shifting the constraint''s extractiveness and suppression profiles significantly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ECB mandate kernel; other readings would alter its structural properties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 5, 0.12).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 10, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'orthodox_price_stability' reading of the ECB mandate kernel (ecb_mandate_article_127). Sibling readings include 'expansive_secondary_objectives' and 'climate_incorporation', which would represent different constraints with distinct structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
