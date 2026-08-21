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
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Mandate Article 127: Orthodox Price Stability Reading
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   This constraint represents the orthodox reading of the European Central
 *   Bank's (ECB) mandate under Article 127 of the Treaty on the Functioning
 *   of the European Union (TFEU), which prioritizes price stability (defined
 *   as 2% inflation) as the primary objective, with other objectives (like
 *   full employment or sustainable growth) being strictly subordinate and
 *   non-operational. This reading emphasizes a narrow interpretation of the
 *   'without prejudice' clause, asserting that secondary objectives can only
 *   be pursued if price stability is not threatened. The constraint is
 *   claimed as a 'tangled_rope' because it provides a genuine coordination
 *   function (inflation targeting) but also involves significant asymmetric
 *   extraction from those who bear the costs of this narrow focus.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.65).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.78).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.65).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Mandate Article 127: Orthodox Price Stability Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, '41e74082-f31b-48c1-8a0d-b4b812d40221').
narrative_ontology:cs_kernel_codification('41e74082-f31b-48c1-8a0d-b4b812d40221', fixed_text).
narrative_ontology:cs_authority_grounding('41e74082-f31b-48c1-8a0d-b4b812d40221', lineage).
narrative_ontology:cs_interpretation_layer_present('41e74082-f31b-48c1-8a0d-b4b812d40221').
narrative_ontology:cs_reading_relation('41e74082-f31b-48c1-8a0d-b4b812d40221', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_reading_relation('41e74082-f31b-48c1-8a0d-b4b812d40221', ecb_mandate_article_127__climate_incorporation, coexists_with).
narrative_ontology:cs_axiom('41e74082-f31b-48c1-8a0d-b4b812d40221', foundational, price_stability_is_primary_and_exclusive).
narrative_ontology:cs_axiom_status(price_stability_is_primary_and_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('41e74082-f31b-48c1-8a0d-b4b812d40221', price_stability_is_primary_and_exclusive, deontological).
narrative_ontology:cs_axiom('41e74082-f31b-48c1-8a0d-b4b812d40221', foundational, secondary_objectives_are_strictly_subordinate_and_non_operational).
narrative_ontology:cs_axiom_status(secondary_objectives_are_strictly_subordinate_and_non_operational, holdable).
narrative_ontology:cs_axiom_grounding('41e74082-f31b-48c1-8a0d-b4b812d40221', secondary_objectives_are_strictly_subordinate_and_non_operational, conventional).
narrative_ontology:cs_reference_frame('41e74082-f31b-48c1-8a0d-b4b812d40221', maastricht_treaty_original_intent).
narrative_ontology:cs_drift_state('41e74082-f31b-48c1-8a0d-b4b812d40221', contemporary_multi_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('41e74082-f31b-48c1-8a0d-b4b812d40221', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, savers_and_creditors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council_orthodox_faction).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, unemployed_citizens).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, heavily_indebted_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_risk_exposed_sectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the ECB mandate as requiring an exclusive focus on price stability, subordinating all other objectives. Actively enforces this interpretation through policy decisions, communication, and internal dissent suppression. Their professional identity is deeply tied to this orthodox view.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council_orthodox_faction, agenda_setter,
    institutional, generational, identity_locked, continental).

% Benefit from policies that prioritize low inflation, preserving the real value of their assets and returns on lending. They advocate for strict adherence to the price stability mandate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, savers_and_creditors, beneficiary,
    organized, biographical, mobile, continental).

% Bear the costs of policies that prioritize price stability over employment, potentially leading to higher unemployment or slower job creation. They have limited direct influence on ECB policy.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, unemployed_citizens, payer,
    powerless, immediate, trapped, national).

% Face higher borrowing costs and fiscal constraints when monetary policy is exclusively focused on inflation, limiting their ability to stimulate growth or manage debt. They advocate for a more flexible interpretation of the mandate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, heavily_indebted_member_states, payer,
    moderate, biographical, constrained, national).

% Are indirectly impacted by the exclusion of climate considerations from monetary policy, potentially facing higher transition risks or lack of supportive financial conditions for green investments. They advocate for climate integration into the mandate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_risk_exposed_sectors, payer,
    organized, generational, constrained, continental).

% Hold alternative interpretations of the mandate, advocating for greater flexibility or integration of secondary objectives. Their views are often marginalized or outvoted within the council, but they continue to press their arguments.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council_dissenting_members, excluded,
    institutional, biographical, identity_locked, continental).

% Exercises democratic oversight over the ECB, questioning its policies and interpretations of the mandate. It can exert political pressure but has limited direct power to alter the mandate's operationalization.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, eu_parliament, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, singular objective for monetary policy across the diverse Eurozone, aiming to anchor inflation expectations and provide a stable economic environment for investment and planning.
% TRANSFER_FUNCTION: Transfers economic stability benefits (low inflation, predictable currency value) to savers and creditors, while transferring costs (potentially higher unemployment, slower growth, externalized climate risks) to other segments of society.
% ABSENT_VOICES: Advocates for a more expansive interpretation of the mandate, including those prioritizing employment, growth, or climate action, are structurally excluded from the dominant policy-setting discourse within the ECB. Their arguments are heard externally but not operationally integrated.
% DISAPPEARANCE_RATIONALE: If the orthodox price stability reading of the ECB mandate vanished overnight, the ECB would likely adopt a more flexible approach, potentially leading to higher inflation tolerance, more active employment support, and integration of climate objectives. This would fundamentally alter financial markets, national fiscal policies, and the EU's economic governance structure.
% FOUNDING_PROBLEM: The Eurozone was established with a strong emphasis on price stability to avoid the historical inflationary pressures experienced by some member states and to build credibility for the new currency.
% FOUNDING_PROBLEM_CORROBORATION: The ECB Governing Council (orthodox faction) attests the problem is live, citing the need for continued vigilance against inflation. Academic economists and some member states corroborate the historical importance of price stability but contest its current exclusive operationalization, arguing that the economic context has changed.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__orthodox_price_stability, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the costs imposed on certain groups (e.g., unemployed, indebted states) due to the singular focus on price stability, which may lead to tighter monetary conditions than optimal for other objectives. Suppression (0.78) is high because the orthodox interpretation actively suppresses alternative policy approaches or mandate expansions, both internally within the ECB and externally through legal and political discourse. The theater ratio is low (0.10) as the ECB genuinely pursues its stated primary objective, but the 'subordinate' nature of other objectives can be performative, as they are rarely operationalized. The increasing extractiveness and suppression over time reflect the hardening of this orthodox interpretation in response to calls for mandate expansion.
 *
 * PERSPECTIVAL GAP:
 *   The orthodox faction of the ECB Governing Council perceives this as a necessary and legitimate coordination mechanism for economic stability. In contrast, victims and excluded stakeholders experience it as an extractive constraint that prioritizes certain interests over others, with significant social and economic costs. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council (orthodox faction) and savers/creditors are the primary beneficiaries, as their interests align with strict price stability. Unemployed citizens, heavily indebted member states, and climate-risk-exposed sectors are victims, bearing the costs of this narrow policy focus. Dissenting ECB members and the EU Parliament are observers or excluded, with limited power to alter the operational mandate, leading to high directionality for victims and low for beneficiaries.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_interpretation_ambiguity,
    'Is the ''without prejudice'' clause in Article 127 TFEU a strict subordination clause or a discretionary balancing clause?',
    'A ruling by the European Court of Justice (ECJ) on the operational scope of secondary objectives, or a formal amendment to the TFEU clarifying the mandate.',
    'If ruled a discretionary balancing clause, the constraint would become less extractive for victims of the narrow focus, potentially shifting towards a ''rope'' or ''scaffold'' if the balancing is genuinely coordinated. If confirmed as strict subordination, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_interpretation_ambiguity, conceptual, 'Ambiguity in the legal interpretation of the ECB''s mandate regarding secondary objectives.').

omega_variable(
    climate_risk_integration_necessity,
    'Is the integration of climate risk into monetary policy (e.g., asset purchases, collateral frameworks) a necessary component of price stability in the long run, or a separate, subordinate objective?',
    'Empirical evidence demonstrating a direct and significant causal link between climate change and long-term price stability, or a consensus among central bank economists on the operational necessity of climate integration for the primary mandate.',
    'If climate integration is deemed necessary for price stability, the current reading''s suppression of climate objectives would be reclassified as a failure to coordinate, increasing its extractiveness and potentially shifting it towards a ''snare'' for climate-exposed sectors. If it remains a separate objective, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_risk_integration_necessity, empirical, 'Whether climate considerations are integral to price stability or merely secondary.').

omega_variable(
    identity_lock_of_orthodox_faction,
    'To what extent is the orthodox faction''s adherence to strict price stability an ''identity_locked'' position rooted in professional norms and historical narratives, rather than a purely rational policy choice?',
    'Qualitative sociological studies of central bank culture, analysis of internal ECB debates, and observation of how individual policymakers respond to new evidence or external pressure for mandate expansion.',
    'If identity-lock is high, the suppression of alternative readings is more deeply entrenched and resistant to empirical challenge, making the constraint more ''snare-like'' due to the difficulty of exit for dissenting views. If it''s primarily rational, policy shifts are more likely with new evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_of_orthodox_faction, empirical, 'The degree to which the orthodox interpretation is sustained by professional identity rather than pure policy logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 5, 0.07).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 10, 0.08).
narrative_ontology:measurement(ecb__tr_t15, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 15, 0.09).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 20, 0.1).
narrative_ontology:measurement(ecb__tr_t25, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 25, 0.1).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(ecb__be_t15, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(ecb__be_t25, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 25, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(ecb__su_t15, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(ecb__su_t25, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 25, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, eu_fiscal_rules_stability_and_growth_pact).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_quantitative_easing_program).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, eu_green_deal_financing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
