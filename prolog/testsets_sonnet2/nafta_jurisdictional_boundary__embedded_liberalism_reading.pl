% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__embedded_liberalism_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA/USMCA Boundary: Market Access Balanced Against Legitimate Domestic Policy Space
 *   domain: International Trade Law / Political Economy / Regulatory Federalism
 *
 * SUMMARY:
 *   This story authors the embedded-liberalism reading of the NAFTA/USMCA
 *   jurisdictional boundary: the treaty text functions as a framework
 *   balancing market access against legitimate domestic policy space, and
 *   environmental/labor standards are read as compatible with trade
 *   obligations when non-discriminatory. Under this reading regulators keep
 *   real defensive authority — a measure survives challenge if it is
 *   genuinely non-discriminatory and pursues a legitimate objective — but
 *   that authority is not costless: litigation exposure and dispute risk
 *   operate as a structural tax on regulatory ambition, falling hardest on
 *   smaller agencies and never-enacted protections. This is a
 *   moderate-extraction tangled rope, not a capital-supremacy snare and not a
 *   sovereignty-primacy near-rope: the coordination function (predictable
 *   market access) is real, the extraction (litigation-cost deterrence,
 *   chilled regulation) is real, and both ride the same textual mechanism.
 *
 * KEY AGENTS:
 *   - domestic_regulatory_agencies_retaining_defensive_authority: institutional agenda-setter and partial beneficiary; retains rule-making power but budgets for litigation risk
 *   - exporting_firms_seeking_predictable_market_access: powerful beneficiary; benefits from the boundary holding and can exert exit leverage via supply-chain mobility
 *   - small_regulatory_agencies_facing_litigation_cost_deterrence: moderate-power payer; trapped within the framework, self-censors rule-making rather than risk dispute cost
 *   - communities_whose_protective_regulation_is_chilled_by_dispute_threat: powerless payer; bears diffuse, invisible cost of foregone protection
 *   - foreign_investors_denied_recovery_when_measures_are_upheld_as_legitimate: powerful payer under this specific reading; loses recovery precisely because the boundary favors regulatory space
 *   - trade_dispute_tribunals: institutional observer/agenda-setter; defines the boundary's practical location through interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.42).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.38).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA/USMCA Boundary: Market Access Balanced Against Legitimate Domestic Policy Space").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "International Trade Law / Political Economy / Regulatory Federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'f19c2004-338f-4738-a24e-1effd56ead02').
narrative_ontology:cs_kernel_codification('f19c2004-338f-4738-a24e-1effd56ead02', fixed_text).
narrative_ontology:cs_authority_grounding('f19c2004-338f-4738-a24e-1effd56ead02', practice).
narrative_ontology:cs_interpretation_layer_present('f19c2004-338f-4738-a24e-1effd56ead02').
narrative_ontology:cs_reading_relation('f19c2004-338f-4738-a24e-1effd56ead02', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('f19c2004-338f-4738-a24e-1effd56ead02', nafta_jurisdictional_boundary__sovereignty_primacy_reading, influences).
narrative_ontology:cs_axiom('f19c2004-338f-4738-a24e-1effd56ead02', foundational, non_discrimination_compatible_with_regulatory_autonomy).
narrative_ontology:cs_axiom_status(non_discrimination_compatible_with_regulatory_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('f19c2004-338f-4738-a24e-1effd56ead02', non_discrimination_compatible_with_regulatory_autonomy, conventional).
narrative_ontology:cs_axiom('f19c2004-338f-4738-a24e-1effd56ead02', foundational, legitimate_objectives_carveout_bounds_market_access_claims).
narrative_ontology:cs_axiom_status(legitimate_objectives_carveout_bounds_market_access_claims, holdable).
narrative_ontology:cs_axiom_grounding('f19c2004-338f-4738-a24e-1effd56ead02', legitimate_objectives_carveout_bounds_market_access_claims, instrumental).
narrative_ontology:cs_reference_frame('f19c2004-338f-4738-a24e-1effd56ead02', gatt_embedded_liberalism_compromise).
narrative_ontology:cs_drift_state('f19c2004-338f-4738-a24e-1effd56ead02', post_investor_state_dispute_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f19c2004-338f-4738-a24e-1effd56ead02', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, exporting_firms_seeking_predictable_market_access).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies_retaining_defensive_authority).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, consumer_and_environmental_advocates_within_legitimate_objectives_carveout).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, small_regulatory_agencies_facing_litigation_cost_deterrence).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, communities_whose_protective_regulation_is_chilled_by_dispute_threat).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, foreign_investors_denied_recovery_when_measures_are_upheld_as_legitimate).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, non_discrimination_as_organizing_trade_principle).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, regulatory_autonomy_compatible_with_liberalized_trade).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets environmental, labor, and health standards under domestic statutory authority, then must defend those standards if a foreign investor or trading partner challenges them as disguised protectionism. Retains rule-making power but must budget for litigation risk and structure regulations to survive a non-discrimination and legitimate-objectives test. Cannot exit the framework without leaving the trade agreement itself.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies_retaining_defensive_authority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies_retaining_defensive_authority, beneficiary).

% Relies on the treaty framework to keep tariffs low and market access predictable across three jurisdictions. Benefits when the boundary holds — trade flows without needing case-by-case political renegotiation. Can shift supply chains or lobby for interpretation favorable to market access, giving it real exit leverage relative to regulators.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, exporting_firms_seeking_predictable_market_access, beneficiary,
    powerful, biographical, mobile, continental).

% Subnational or resource-constrained regulatory bodies that must weigh a legitimate public-interest rule against the real cost of defending it in an investor-state or state-to-state proceeding. Even when confident the measure would be upheld as a legitimate objective, the litigation cost itself deters action — a chilling effect that operates whether or not any case is actually filed. Cannot leave the treaty framework; can only self-censor rule-making.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, small_regulatory_agencies_facing_litigation_cost_deterrence, payer,
    moderate, biographical, trapped, regional).

% Residents seeking stronger pesticide limits, labor protections, or environmental rules that never get enacted because the regulatory agency anticipates a trade challenge. They bear the cost of foregone protection without ever appearing as a party to any dispute — the effect is diffuse and largely invisible in the treaty's own textual record.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, communities_whose_protective_regulation_is_chilled_by_dispute_threat, payer,
    powerless, generational, trapped, local).

% Invests capital expecting treaty-level protection against arbitrary regulation, but under this reading a non-discriminatory measure pursuing a legitimate objective survives challenge even if it reduces the investment's value. Bears the cost of the boundary's tilt toward regulatory space when a tribunal applies the legitimate-objectives test against it.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, foreign_investors_denied_recovery_when_measures_are_upheld_as_legitimate, payer,
    powerful, biographical, constrained, continental).

% Interprets whether a challenged domestic measure is genuinely non-discriminatory and pursues a legitimate objective, or is protectionism in regulatory disguise. Its rulings define where the jurisdictional boundary actually sits in practice, which means it exercises real agenda-setting power even though it does not draft the underlying treaty text.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_dispute_tribunals, observer,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_dispute_tribunals, agenda_setter).

% Argues the treaty text should function as supreme, harmonizing law overriding domestic standards (the capital_supremacy_reading). This reading treats their view as a live but rejected interpretive claim — they continue to press it in litigation and treaty renegotiation but do not control this reading's boundary.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_mobility_advocates, excluded,
    organized, generational, mobile, continental).

% Argues domestic law should retain full, untrammeled authority over labor/environmental/health standards with no treaty-level review at all (the sovereignty_primacy_reading). This reading treats their position as understating the coordination function the framework performs — their voice is present in domestic political debate but not decisive in tribunal interpretation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, sovereignty_maximalist_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__embedded_liberalism_reading, diffuse).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__embedded_liberalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, predictable framework so firms across three jurisdictions can plan investment and trade without each government's regulatory choices unilaterally closing markets, while preserving each government's authority to pursue genuine public-interest regulation as long as it is not a disguised trade barrier.
% TRANSFER_FUNCTION: Moves predictability and market access to exporting firms and to regulators who successfully defend legitimate measures; moves litigation risk and chilling-effect costs to smaller agencies, affected communities, and to investors in the cases where a genuinely non-discriminatory measure is upheld against them.
% ABSENT_VOICES: Communities whose regulation never gets written because of anticipated litigation cost have no seat in any dispute proceeding — their absence is definitional, since a chilled rule leaves no textual trace to contest. Capital-mobility advocates and sovereignty-maximalist advocates are present in domestic and trade-policy debate but do not control tribunal interpretation under this reading.
% DISAPPEARANCE_RATIONALE: Exporters and firms with cross-border supply chains would say the world rearranges sharply if the framework vanished — tariff and non-tariff barriers would re-emerge unpredictably. Sovereignty-primacy advocates would say little rearranges beyond removing a layer of external review, since domestic regulatory capacity itself would remain. The dispute is genuine and unresolved between the readings, hence contested rather than either pole.
% FOUNDING_PROBLEM: Cross-border trade among three economies needed a rules-based framework to prevent arbitrary tariff escalation and unpredictable market closure, while critics of early drafts worried that market-access rules would be used to strike down legitimate environmental, labor, and health regulation as disguised barriers.
% FOUNDING_PROBLEM_CORROBORATION: Trade ministries and exporting industry associations attest the market-access problem remains live and the framework functions as intended. Independent legal scholars and several domestic regulatory agencies (outside the direct beneficiary set) attest that the legitimate-objectives boundary is unevenly enforced by tribunals and that litigation-cost deterrence operates as a real, under-documented chilling effect even where no dispute is filed.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, contested).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the boundary genuinely does protect legitimate regulation in many cases, but litigation-cost deterrence extracts value even from measures that would ultimately survive challenge — the tax is levied on the anticipation of dispute, not only on adverse outcomes. Suppression is lower than a capital-supremacy reading would show (0.38) because regulatory agencies retain real defensive authority rather than being structurally foreclosed; what suppression exists is the chilling effect on unwritten regulation, which is real but indirect. Theater ratio is moderate-low (0.28): the legitimate-objectives test is a functioning legal doctrine, not pure performance, though its application by tribunals is uneven enough that some invocations of it function more as cover than as genuine constraint on interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (domestic regulators), the structure looks like a workable compromise: real authority retained, occasional litigation cost accepted as the price of predictable trade. From the payer seats (small agencies, chilled communities), the same structure looks like extraction operating below the threshold of visibility — a tax collected in the form of rules never written. The engine computing these as different seat-level types from the same structural data is exactly the point: neither seat's perception is wrong, and neither should be read as the single truth about the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Exporting firms sit near the beneficiary end: they gain predictable access and can exit via supply-chain reallocation if the framework fails them, giving them real bargaining leverage. Domestic regulatory agencies are a genuine dual seat — beneficiaries of retained authority but payers of litigation-cost overhead, which is why the derivation treats them as structurally mixed rather than purely benefiting. Small agencies and affected communities sit near the target end: trapped inside the framework with no exit and bearing costs (self-censorship, foregone protection) that never surface as formal disputes. Investors are payers under THIS reading specifically because the legitimate-objectives test can defeat their claims — this is the reading-specific fact that most sharply distinguishes embedded liberalism from capital supremacy, where investors would be the clear beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing arbitrary market closure while preserving legitimate regulatory space) remains structurally live — cross-border trade coordination is not obsolete, and the embedded-liberalism reading's central claim is precisely that the balance still functions. This is contested rather than resolved: critics argue the balance has drifted toward de facto deterrence of regulation (a piton-adjacent concern) even though the doctrine formally preserves regulatory authority. The reading resists classifying this drift as full mandatrophy because tribunals do sometimes uphold legitimate measures — the coordination function has not been hollowed out, only taxed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_objectives_test_application_consistency,
    'Do trade tribunals apply the ''legitimate objectives'' and non-discrimination tests consistently enough that the embedded-liberalism reading describes actual practice, or do outcomes track investor/state power asymmetry more than doctrinal consistency?',
    'Systematic coding of tribunal decisions across environmental, labor, and health disputes for outcome correlation with respondent state capacity/resources versus doctrinal factors (discrimination finding, necessity analysis).',
    'If outcomes track power rather than doctrine, this reading''s claimed extraction (0.42, moderate) understates actual extraction and the constraint drifts toward the capital_supremacy_reading''s structural profile in practice even while embedded-liberalism remains the formally operative doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_objectives_test_application_consistency, empirical, 'Whether the legitimate-objectives boundary is applied consistently or tracks power asymmetry between disputing parties.').

omega_variable(
    chilling_effect_measurement_problem,
    'How much regulation is actually foregone due to anticipated litigation cost, given that chilled regulation by definition leaves no textual record to measure against?',
    'Comparative case studies of jurisdictions with and without investor-state dispute exposure attempting similar regulation, tracking regulatory ambition and pace as a proxy for chilling effect.',
    'A large chilling effect would mean the true extraction borne by excluded communities is substantially higher than this story''s authored 0.42, since the measured metric can only capture disputed cases, not foregone ones.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_measurement_problem, empirical, 'The chilling effect on unwritten regulation is structurally hard to measure and may be undercounted in this story''s extractiveness score.').

omega_variable(
    reading_which_is_descriptively_dominant,
    'Which of the three kernel readings (capital_supremacy, embedded_liberalism, sovereignty_primacy) best describes the modal outcome of the treaty''s operation across its full dispute history, versus which is merely the officially stated doctrine?',
    'Cross-reading meta-analysis comparing this story''s metrics against the sibling stories'' authored metrics and against aggregate dispute-outcome data.',
    'If capital_supremacy_reading''s metrics better fit aggregate outcomes, the embedded_liberalism_reading (this story) would be better understood as the treaty''s legitimating self-description rather than its operative logic — a conceptual, not merely empirical, reclassification risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_which_is_descriptively_dominant, conceptual, 'Whether embedded liberalism is the treaty''s operative logic or its legitimating narrative relative to the sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(naft_tr_t5, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(naft_tr_t10, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(naft_tr_t15, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(naft_tr_t20, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(naft_tr_t25, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(naft_tr_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(naft_be_t5, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(naft_be_t10, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(naft_be_t15, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(naft_be_t20, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(naft_be_t25, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 25, 0.41).
narrative_ontology:measurement(naft_be_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(naft_su_t5, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(naft_su_t10, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(naft_su_t15, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 15, 0.34).
narrative_ontology:measurement(naft_su_t20, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(naft_su_t25, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 25, 0.37).
narrative_ontology:measurement(naft_su_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.12).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the nafta_jurisdictional_boundary kernel. capital_supremacy_reading treats the same treaty text as supreme law with mandatory harmonization (higher ε, investor-favoring victim set); sovereignty_primacy_reading treats it as fully subordinate coordination with no external review (near-rope, minimal extraction). All three share ε-referent (the standing treaty arrangement as each reading's lights construe it) but diverge in claimed_type, beneficiary/victim structure, and measured extraction because they disagree about where interpretive authority over the market-access/policy-space boundary sits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
