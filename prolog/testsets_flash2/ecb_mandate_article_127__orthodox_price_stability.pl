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
 *   human_readable: ECB Mandate: Orthodox Price Stability Reading
 *   domain: monetary_policy/constitutional_law/eu_governance
 *
 * SUMMARY:
 *   This constraint represents the 'orthodox price stability' reading of the
 *   ECB's mandate under Article 127 TFEU, which holds that the ECB's primary
 *   objective of price stability is exclusive and that secondary objectives
 *   (like supporting general economic policies of the Union) are strictly
 *   subordinate and non-operational. This reading is a foundational element
 *   of the ECB's institutional identity and operational framework, but it is
 *   increasingly contested by those advocating for a broader interpretation
 *   that includes employment, growth, or climate action. The constraint is
 *   classified as a Tangled Rope because it provides a coordination function
 *   (stable inflation expectations) but also involves significant asymmetric
 *   extraction (costs borne by those disadvantaged by the narrow focus) and
 *   requires active enforcement against alternative interpretations.
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
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Mandate: Orthodox Price Stability Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/constitutional_law/eu_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, 'bbbd6fb2-f62c-4248-87b1-39332235a9f1').
narrative_ontology:cs_kernel_codification('bbbd6fb2-f62c-4248-87b1-39332235a9f1', fixed_text).
narrative_ontology:cs_authority_grounding('bbbd6fb2-f62c-4248-87b1-39332235a9f1', lineage).
narrative_ontology:cs_interpretation_layer_present('bbbd6fb2-f62c-4248-87b1-39332235a9f1').
narrative_ontology:cs_reading_relation('bbbd6fb2-f62c-4248-87b1-39332235a9f1', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_reading_relation('bbbd6fb2-f62c-4248-87b1-39332235a9f1', ecb_mandate_article_127__climate_incorporation, coexists_with).
narrative_ontology:cs_axiom('bbbd6fb2-f62c-4248-87b1-39332235a9f1', foundational, price_stability_is_primary_and_exclusive).
narrative_ontology:cs_axiom_status(price_stability_is_primary_and_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('bbbd6fb2-f62c-4248-87b1-39332235a9f1', price_stability_is_primary_and_exclusive, deontological).
narrative_ontology:cs_axiom('bbbd6fb2-f62c-4248-87b1-39332235a9f1', foundational, secondary_objectives_are_non_operational).
narrative_ontology:cs_axiom_status(secondary_objectives_are_non_operational, holdable).
narrative_ontology:cs_axiom_grounding('bbbd6fb2-f62c-4248-87b1-39332235a9f1', secondary_objectives_are_non_operational, conventional).
narrative_ontology:cs_reference_frame('bbbd6fb2-f62c-4248-87b1-39332235a9f1', maastricht_treaty_original_intent).
narrative_ontology:cs_drift_state('bbbd6fb2-f62c-4248-87b1-39332235a9f1', contemporary_poly_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('bbbd6fb2-f62c-4248-87b1-39332235a9f1', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, savers_and_creditors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, financial_stability_advocates).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, unemployed_citizens).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, indebted_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_risk_exposed_sectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the ECB's primary mandate as exclusive price stability, subordinating all other objectives. Benefits from a clear, singular focus that simplifies decision-making and provides a strong institutional identity. Exit options are constrained by treaty obligations and political pressure.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from policies that prioritize low inflation, preserving the real value of their assets and fixed income. They exert political pressure for strict adherence to price stability, viewing it as a core promise of the monetary union.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, savers_and_creditors, beneficiary,
    organized, biographical, mobile, continental).

% Bear the costs of policies that prioritize inflation control over employment, potentially leading to higher unemployment or slower recovery from economic shocks. Their influence on ECB policy is indirect and diffuse.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, unemployed_citizens, payer,
    powerless, immediate, trapped, national).

% Face higher borrowing costs and fiscal austerity measures when the ECB's singular focus on price stability limits its willingness to support growth or provide liquidity during crises. Their exit options are limited by membership in the Eurozone.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, indebted_member_states, payer,
    powerful, generational, constrained, continental).

% Bear the costs of a mandate that externalizes climate risks, as the ECB's asset purchases and collateral frameworks do not actively discriminate against carbon-intensive assets. This perpetuates mispricing of climate risk in financial markets.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_risk_exposed_sectors, payer,
    moderate, generational, constrained, continental).

% Support the orthodox reading as it provides a clear anchor for monetary policy, which they believe is essential for long-term financial stability. They benefit from the perceived credibility and predictability of a single-minded central bank.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, financial_stability_advocates, beneficiary,
    institutional, generational, analytical, global).

% Exercises democratic oversight over the ECB but has limited direct influence on its mandate interpretation. It serves as a forum for debate on the ECB's role and can exert political pressure for mandate reform or reinterpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, eu_parliament, observer,
    institutional, biographical, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, singular objective for monetary policy across the Eurozone, coordinating expectations around low and stable inflation, which is argued to be a prerequisite for economic stability and investment.
% TRANSFER_FUNCTION: Transfers economic benefits (stable asset values, predictable returns) to savers and creditors by prioritizing low inflation, while transferring costs (potential underemployment, slower growth, unaddressed systemic risks) to other sectors and member states.
% ABSENT_VOICES: Advocates for a more expansive interpretation of secondary objectives (e.g., full employment, climate action) are present in public discourse but are structurally excluded from the ECB's internal mandate interpretation process, which prioritizes legalistic readings of Article 127 TFEU. Their arguments are heard but not operationally integrated.
% DISAPPEARANCE_RATIONALE: If the orthodox reading of the ECB's mandate vanished overnight, the ECB would immediately face immense pressure to operationalize secondary objectives, potentially leading to a more activist monetary policy, greening of asset purchases, and a re-evaluation of its independence. Financial markets would react to the uncertainty, and the political economy of the Eurozone would fundamentally shift.
% FOUNDING_PROBLEM: The Eurozone was founded on the principle of a stable currency, with the ECB designed to prevent a return to the high inflation and monetary instability that plagued some member states in the past.
% FOUNDING_PROBLEM_CORROBORATION: The ECB and its supporters consistently attest that maintaining price stability remains a live and critical problem, citing historical inflation risks and the need for a credible anchor. Critics acknowledge the historical problem but argue that its current manifestation is different, requiring a broader policy toolkit; independent economic historians corroborate the original intent.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) because the singular focus on price stability imposes significant costs on other societal objectives, such as employment and climate action, which are externalized or deprioritized. Suppression is also high (0.78) due to the institutional and legal mechanisms that actively resist attempts to broaden the mandate's operational scope. The theater ratio is low (0.10) because the ECB genuinely pursues price stability; the 'theater' is minimal, primarily involving rhetorical defense against mandate expansion rather than performative maintenance of an atrophied function. Accessibility collapse is high (0.70) because the legal and institutional interpretation makes alternative operationalizations of the mandate very difficult to access. Resistance is moderate (0.45) as there is ongoing, but not overwhelming, political and academic pressure for mandate reform.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ECB Governing Council and its beneficiaries, the orthodox reading is a necessary and effective coordination mechanism for monetary stability. From the perspective of victims, it is an extractive mechanism that prioritizes certain economic interests over broader societal welfare, maintained by institutional inertia and legalistic interpretation. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council, savers, creditors, and financial stability advocates are beneficiaries (low directionality) as they gain from the stable inflation environment and clear policy focus. Unemployed citizens, indebted member states, and climate-risk-exposed sectors are victims (high directionality) as they bear the costs of the narrow mandate. The EU Parliament acts as an observer, capable of analytical exit but not direct policy intervention.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_interpretation_ambiguity,
    'Is the orthodox reading of Article 127 TFEU the only legally defensible interpretation, or does the ''without prejudice'' clause allow for a more expansive operationalization of secondary objectives?',
    'A ruling by the European Court of Justice on the scope of the ECB''s secondary objectives, or a formal amendment to the TFEU clarifying the mandate.',
    'If a broader interpretation is legally affirmed, the constraint''s suppression and extractiveness would decrease, potentially reclassifying it towards a Rope or even a Scaffold if the expansion is temporary. If the orthodox reading is reaffirmed as exclusive, its Mountain-like qualities (in terms of perceived inevitability) would strengthen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_interpretation_ambiguity, conceptual, 'Ambiguity in the legal interpretation of the ECB''s mandate.').

omega_variable(
    climate_risk_externalization,
    'To what extent does the orthodox reading of the mandate externalize systemic climate risks, and what are the long-term economic costs of this externalization?',
    'Comprehensive, independent macroeconomic modeling of climate-related financial risks under different ECB mandate interpretations, including the impact on long-term price stability.',
    'If the externalized costs are shown to be substantial and to undermine long-term price stability, the orthodox reading''s claimed coordination function would be weakened, increasing its effective extractiveness and potentially shifting its classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_risk_externalization, empirical, 'The unacknowledged costs of externalizing climate risks due to a narrow mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 5, 0.09).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 10, 0.09).
narrative_ontology:measurement(ecb__tr_t15, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 15, 0.1).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 20, 0.1).
narrative_ontology:measurement(ecb__tr_t25, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 25, 0.1).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(ecb__be_t15, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(ecb__be_t25, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 25, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(ecb__su_t15, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(ecb__su_t25, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 25, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, eu_fiscal_rules_stability_pact).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, eu_green_deal_financing).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ECB's mandate under Article 127 TFEU. The 'orthodox price stability' reading emphasizes a singular focus on inflation, while 'expansive_secondary_objectives' and 'climate_incorporation' advocate for broader operational scope. Each reading constitutes a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
