% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Article 127 Mandate — Orthodox Price-Stability-Exclusive Reading
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   This story instantiates the orthodox_price_stability reading of the
 *   Article 127 TFEU kernel: the ECB's mandate is interpreted as requiring
 *   exclusive operational focus on the 2% inflation target, with the treaty's
 *   'without prejudice' language toward general EU economic objectives
 *   (growth, employment, and by extension Article 11 TFEU environmental
 *   integration) treated as non-binding and non-operational unless price
 *   stability is already secured. This is a distinct constraint from the
 *   expansive_secondary_objectives reading (which treats the same clause as
 *   authorizing discretionary balancing) and the climate_incorporation
 *   reading (which treats Article 11 as imposing an affirmative climate
 *   integration duty). The three readings have different beneficiary sets,
 *   different victim sets, and different ε profiles because they are
 *   different constraints sharing one contested textual kernel — they are not
 *   the same constraint viewed from different angles.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.58).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.72).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Article 127 Mandate — Orthodox Price-Stability-Exclusive Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, '9279f882-68ae-4c6a-94e3-ea4f00ed3f7b').
narrative_ontology:cs_kernel_codification('9279f882-68ae-4c6a-94e3-ea4f00ed3f7b', fixed_text).
narrative_ontology:cs_authority_grounding('9279f882-68ae-4c6a-94e3-ea4f00ed3f7b', extraction).
narrative_ontology:cs_interpretation_layer_present('9279f882-68ae-4c6a-94e3-ea4f00ed3f7b').
narrative_ontology:cs_reading_relation('9279f882-68ae-4c6a-94e3-ea4f00ed3f7b', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_reading_relation('9279f882-68ae-4c6a-94e3-ea4f00ed3f7b', ecb_mandate_article_127__climate_incorporation, coexists_with).
narrative_ontology:cs_axiom('9279f882-68ae-4c6a-94e3-ea4f00ed3f7b', foundational, price_stability_as_exclusive_operational_target).
narrative_ontology:cs_axiom_status(price_stability_as_exclusive_operational_target, holdable).
narrative_ontology:cs_axiom_grounding('9279f882-68ae-4c6a-94e3-ea4f00ed3f7b', price_stability_as_exclusive_operational_target, conventional).
narrative_ontology:cs_axiom('9279f882-68ae-4c6a-94e3-ea4f00ed3f7b', foundational, monetary_credibility_requires_insulation_from_distributive_objectives).
narrative_ontology:cs_axiom_status(monetary_credibility_requires_insulation_from_distributive_objectives, holdable).
narrative_ontology:cs_axiom_grounding('9279f882-68ae-4c6a-94e3-ea4f00ed3f7b', monetary_credibility_requires_insulation_from_distributive_objectives, instrumental).
narrative_ontology:cs_created_at('9279f882-68ae-4c6a-94e3-ea4f00ed3f7b', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, net_savers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, fixed_income_creditors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, german_ordoliberal_policy_establishment).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, price_stability_credibility_holders).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, unemployed_workers_in_periphery_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_transition_dependent_sectors).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, highly_indebted_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, future_generations_bearing_climate_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 127 TFEU as establishing price stability (operationalized as ~2% HICP inflation) as the singular primary objective, treating 'without prejudice' language on supporting general EU economic policies as strictly subordinate and non-binding. Sets interest rates, asset purchase programs, and collateral frameworks accordingly. Enjoys treaty-level independence from political override and controls its own interpretive doctrine through internal legal opinions and precedent.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, arbitrage, continental).

% Hold financial assets whose real value is protected by low, stable inflation. Benefit directly from the mandate's narrow reading because it prioritizes their asset preservation over employment or growth stabilization. Can diversify across jurisdictions and asset classes; not trapped by the constraint.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, net_savers, beneficiary,
    organized, biographical, mobile, continental).

% Banks, pension funds, and institutional bondholders whose returns depend on inflation staying low and predictable. The exclusive-mandate reading forecloses the kind of employment-weighted policy discretion that could erode real returns through higher inflation tolerance.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, fixed_income_creditors, beneficiary,
    organized, biographical, mobile, continental).

% Shaped the treaty language and continues to police its interpretation through the Bundesbank's seat on the Governing Council and through domestic constitutional litigation (e.g. the German Federal Constitutional Court's proportionality challenges to ECB asset purchases). Benefits from a doctrine that constrains fiscal transfer pressure and validates a monetary-stability-first ordering inherited from its own postwar institutional history.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, german_ordoliberal_policy_establishment, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__orthodox_price_stability, german_ordoliberal_policy_establishment, beneficiary).

% Bear the costs when the ECB holds rates tight or withdraws stimulus despite high unemployment, because the orthodox reading treats employment effects as non-operational unless they threaten price stability itself. Have no institutional voice in Governing Council deliberations and cannot exit the currency union without extraordinary political cost.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, unemployed_workers_in_periphery_states, payer,
    powerless, biographical, trapped, national).

% Renewable energy firms, green infrastructure developers, and climate-exposed industries seeking favorable collateral treatment or asset-purchase tilting find the orthodox reading treats climate risk as outside the mandate's operational scope entirely, since Article 11 TFEU environmental integration is read as non-binding on monetary operations. This externalizes transition risk pricing onto their sector specifically.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_transition_dependent_sectors, payer,
    moderate, generational, constrained, continental).

% Sovereign borrowers whose debt sustainability depends on ECB accommodation. The orthodox reading subordinates any implicit debt-sustainability support to the primary inflation target, leaving these states exposed to sudden tightening cycles they cannot offset with independent monetary policy, having ceded that instrument at currency union entry.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, highly_indebted_member_states, payer,
    moderate, generational, trapped, national).

% Inherit accumulated climate transition risk that a climate-blind monetary and collateral framework failed to price or discourage during the accumulation window. Have no representation in the present-day interpretive contest over the mandate's scope.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, future_generations_bearing_climate_risk, payer,
    powerless, civilizational, trapped, global).

% Repeatedly argues for a broader reading of the 'without prejudice' clause to authorize operational weight on employment and climate objectives, but lacks any binding mechanism to compel the ECB's interpretive doctrine — the ECB's independence provisions insulate it from Parliament's ordinary legislative or budgetary leverage.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, eu_parliament_progressive_bloc, excluded,
    organized, generational, constrained, continental).

% Adjudicates challenges to ECB action (e.g. Gauweiler, Weiss) and has generally upheld a wide margin of discretion for the ECB's own interpretation of its mandate, effectively ratifying whichever reading the Governing Council adopts as long as proportionality is nominally addressed.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, european_court_of_justice, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__orthodox_price_stability, diffuse).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__orthodox_price_stability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, credible nominal anchor (2% inflation) that coordinates wage-setting, price-setting, and long-term contracting expectations across nineteen-plus heterogeneous economies without requiring a shared fiscal authority — solving a genuine coordination problem that a currency union without fiscal union otherwise cannot solve institutionally.
% TRANSFER_FUNCTION: Moves real-value protection toward asset holders and creditors (via low, stable inflation) and moves adjustment burden toward labor markets and heavily indebted sovereigns (who cannot devalue or independently loosen policy), while externalizing unpriced climate transition risk onto future periods and climate-exposed sectors in the present.
% ABSENT_VOICES: Unemployed workers in periphery states, climate-exposed firms, and future generations have no seat in Governing Council deliberations or in the interpretive doctrine that decides whether 'without prejudice' language is operational or merely aspirational; the EU Parliament's progressive bloc raises these concerns but has no binding lever over the ECB's own reading of its mandate.
% DISAPPEARANCE_RATIONALE: The ECB and ordoliberal establishment would say price stability itself, and the currency union's credibility, unravel without the exclusive-focus doctrine — arrangements built on anchored inflation expectations (wage contracts, sovereign bond pricing) would need to reprice. Critics would say only the doctrinal exclusivity disappears — the ECB could pursue price stability as primary while treating employment and climate as operational secondary objectives without abandoning the anchor, meaning the world that actually rearranges is the ECB's internal legal interpretation, not the underlying coordination function.
% FOUNDING_PROBLEM: The Maastricht-era design problem: how to create a credible common currency across states with divergent fiscal discipline and inflation histories (especially the legacy of 1920s German hyperinflation and 1970s-80s stagflation across the bloc) without a unified fiscal authority to back it, given deep distrust that a politically-directed central bank would inflate away debts.
% FOUNDING_PROBLEM_CORROBORATION: The ECB and the Bundesbank tradition attest the founding problem (credibility risk from political interference in monetary policy) remains fully live, citing the 1970s inflation experience and ongoing fiscal indiscipline in several member states as evidence. Independent macroeconomic historians and IMF working papers attest that post-2008 and post-2020 experience (near-zero inflation for a decade, then supply-driven inflation largely unresponsive to ECB tools) has substantially undercut the empirical premise that exclusive focus on inflation targeting is the necessary or sufficient response to the credibility problem, suggesting the doctrine has outlived the specific historical conditions that justified its strict form.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, contested).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__orthodox_price_stability, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.58) reflects that the orthodox doctrine does more than solve the coordination problem of anchoring inflation expectations — it structurally redistributes adjustment costs toward labor markets, indebted sovereigns, and climate-exposed sectors who have no institutional voice in the interpretive question. Suppression (0.72) is high because the doctrine's persistence depends on the ECB's treaty-level independence, the ECJ's consistent deference to ECB self-interpretation (Gauweiler, Weiss), and the absence of any binding mechanism by which the Parliament's progressive bloc could compel a broader reading. Theater ratio is moderate (0.28) — the doctrine performs neutral technocratic necessity while functioning partly as ordoliberal ideological commitment, but genuine anchoring function is real, not fabricated. Measurements run on one shared time grid; the extraction spike around 2010 tracks the eurozone sovereign debt crisis period when the exclusive-mandate reading was used to resist debt-mutualization pressure, and the 2023 spike tracks the post-pandemic inflation surge and the doctrine's role in justifying rapid tightening despite fragile peripheral recoveries.
 *
 * DIRECTIONALITY LOGIC:
 *   Net savers, fixed-income creditors, and the ordoliberal policy establishment sit near the beneficiary end of directionality — the doctrine's narrow reading protects their real returns and validates their institutional preferences, and they have mobile or analytical exit options that let them adjust portfolios or doctrine without bearing the doctrine's costs. Unemployed workers in periphery states and future generations sit near the full-target end — trapped by currency union membership or by the passage of time itself, with no institutional lever to contest the interpretation that harms them. Highly indebted member states and climate-transition sectors occupy an intermediate position: real costs, some constrained exit through fiscal adjustment or sectoral diversification, but no ability to alter the mandate's interpretation directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — credible commitment against politically-directed inflation in a currency union lacking fiscal union — was genuinely live at Maastricht. Its status is contested rather than dead because low-inflation credibility retains real value, but three decades of divergent macroeconomic experience (a lost decade of below-target inflation 2009-2019, followed by supply-driven inflation the ECB's tools addressed only partially) undermines the specific claim that EXCLUSIVE focus, as opposed to primary-with-operational-secondary-objectives, is necessary to solve it. Classifying this as tangled_rope rather than snare or mountain preserves the genuine coordination function (nominal anchor) while registering the asymmetric extraction (savers/creditors protected, periphery labor and climate-exposed future generations pay) that the orthodox interpretive choice specifically produces — a choice the sibling readings show was not compelled by the treaty text itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_exclusivity_vs_interpretive_choice,
    'Does Article 127 TFEU''s text actually compel exclusive, non-operational treatment of secondary objectives, or is the orthodox reading an interpretive choice made possible by the ECB''s institutional independence and ratified by ECJ deference rather than required by the treaty language itself?',
    'Comparative treaty-drafting history analysis (examining Maastricht/Bundesbank negotiating record for whether exclusivity was explicitly intended vs. general primacy-with-discretion), and comparison with how other independent central banks with dual or multiple mandates (e.g. the US Federal Reserve''s employment/price-stability dual mandate) operationalize similarly worded ''without prejudice to'' language.',
    'If the treaty text does not compel exclusivity, this reading is a constructed doctrinal choice sustained by institutional path-dependence and ordoliberal preference rather than a textually mandated interpretation — strengthening the tangled_rope classification by showing the extraction is a choice, not a logical necessity of the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_exclusivity_vs_interpretive_choice, conceptual, 'Whether treaty text or interpretive doctrine drives the exclusivity claim.').

omega_variable(
    kernel_disagreement_location,
    'Where exactly does the disagreement among the three kernel readings locate — is it a disagreement about what the ''without prejudice'' clause MEANS (semantic), about WHO has authority to decide operational weight (institutional/procedural), or about WHETHER climate risk falls within ''general economic policies'' at all (scope)?',
    'Formal legal argument mapping across ECB legal opinions, ECJ rulings (Gauweiler, Weiss, and any future climate-mandate litigation), and European Parliament resolutions to identify whether disputants share premises about textual meaning but disagree about institutional authority, or genuinely disagree about the clause''s semantic content.',
    'If the disagreement is purely institutional/procedural (who decides) rather than semantic, the orthodox reading''s stability depends entirely on the ECB''s continued institutional independence being upheld by the ECJ — a single adverse ruling or treaty amendment could collapse the exclusivity doctrine without requiring any change to the underlying text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Locating whether the three readings diverge on semantics, institutional authority, or scope.').

omega_variable(
    climate_risk_externalization_reversibility,
    'Is the climate risk externalized by the orthodox reading''s exclusion of Article 11 TFEU integration a one-time stock cost (mispriced assets accumulated during the exclusion period) or an ongoing flow cost that compounds as long as the orthodox reading persists?',
    'Climate stress-testing of ECB collateral pools and asset purchase holdings under the orthodox framework versus a counterfactual climate-integrated framework, tracked over multiple monetary policy cycles.',
    'If flow rather than stock, the victim harm to future generations grows monotonically with the doctrine''s persistence, which would argue for treating the constraint''s extraction trajectory as still-rising rather than plateaued at the current 0.58 reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_risk_externalization_reversibility, empirical, 'Whether climate externalization under this reading is a fixed cost or a compounding one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 1999, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t1999, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 1999, 0.12).
narrative_ontology:measurement(ecb__tr_t2004, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2004, 0.15).
narrative_ontology:measurement(ecb__tr_t2010, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(ecb__tr_t2015, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(ecb__tr_t2020, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(ecb__tr_t2023, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2023, 0.26).
narrative_ontology:measurement(ecb__tr_t2026, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(ecb__be_t1999, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 1999, 0.38).
narrative_ontology:measurement(ecb__be_t2004, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2004, 0.42).
narrative_ontology:measurement(ecb__be_t2010, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(ecb__be_t2015, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(ecb__be_t2020, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2020, 0.48).
narrative_ontology:measurement(ecb__be_t2023, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2023, 0.62).
narrative_ontology:measurement(ecb__be_t2026, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t1999, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 1999, 0.55).
narrative_ontology:measurement(ecb__su_t2004, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2004, 0.58).
narrative_ontology:measurement(ecb__su_t2010, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(ecb__su_t2015, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(ecb__su_t2020, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(ecb__su_t2023, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2023, 0.74).
narrative_ontology:measurement(ecb__su_t2026, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'the ECB's Article 127 mandate' into structurally distinct readings of the same treaty kernel. orthodox_price_stability narrows the beneficiary set to savers/creditors and externalizes climate risk with high suppression of mandate-expansion efforts. expansive_secondary_objectives treats the same clause as authorizing discretionary balancing toward employment/growth, producing a different beneficiary set (labor-exposed regions, growth-dependent sovereigns) and a lower suppression profile. climate_incorporation treats Article 11 TFEU as imposing an affirmative integration duty, producing yet another beneficiary set (climate-transition sectors, future generations) and reframing the current orthodox reading's externalization as the extractive element. Each carries its own ε, its own stakeholder set, and its own claimed_type; they are linked here rather than merged because measuring 'the mandate' under different interpretive assumptions yields incompatible ε values — the signature of needing separate constraints, not one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
