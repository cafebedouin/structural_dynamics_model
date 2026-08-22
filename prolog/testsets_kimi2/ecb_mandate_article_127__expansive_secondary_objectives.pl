% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Article 127 Expansive Secondary Objectives Reading
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   This constraint instantiates the expansive_secondary_objectives reading
 *   of the ECB mandate under Article 127 TFEU. It treats the phrase 'without
 *   prejudice to the objective of price stability' as authorizing the ECB to
 *   assign genuine operational weight to employment and growth when price
 *   stability is not threatened. The reading is contested within the EU legal
 *   and political order: it is endorsed by the ECB Executive Board and debtor
 *   member states, resisted by creditor member states and the German Federal
 *   Constitutional Court, and validated by the ECJ in key bond-purchase
 *   rulings. As a kernel reading, it is structurally distinct from the
 *   orthodox_price_stability reading (which treats secondary objectives as
 *   rhetorical surplusage) and the climate_incorporation reading (which
 *   extends secondary-objective logic to environmental integration).
 *
 * KEY AGENTS:
 *   - ECB Governing Council: agenda setter interpreting and enforcing the mandate (institutional/constrained)
 *   - Indebted member states: primary beneficiaries of lower spreads and fiscal space (organized/constrained)
 *   - Debtor households and wage earners: diffuse beneficiaries of accommodative policy (powerless/trapped)
 *   - Net savers and fixed-income retirees: bear negative real returns (moderate-powerless/constrained-trapped)
 *   - Creditor member states: bear inflation risk and loss of monetary culture (powerful/constrained)
 *   - European Court of Justice: adjudicator validating the expansive reading (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.62).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.58).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.62).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Article 127 Expansive Secondary Objectives Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, '95f7e452-bd4f-42af-90a8-50106d955288').
narrative_ontology:cs_kernel_codification('95f7e452-bd4f-42af-90a8-50106d955288', formalized).
narrative_ontology:cs_authority_grounding('95f7e452-bd4f-42af-90a8-50106d955288', lineage).
narrative_ontology:cs_interpretation_layer_present('95f7e452-bd4f-42af-90a8-50106d955288').
narrative_ontology:cs_reading_relation('95f7e452-bd4f-42af-90a8-50106d955288', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('95f7e452-bd4f-42af-90a8-50106d955288', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('95f7e452-bd4f-42af-90a8-50106d955288', foundational, secondary_objectives_operationally_substantive).
narrative_ontology:cs_axiom_status(secondary_objectives_operationally_substantive, holdable).
narrative_ontology:cs_axiom_grounding('95f7e452-bd4f-42af-90a8-50106d955288', secondary_objectives_operationally_substantive, conventional).
narrative_ontology:cs_axiom('95f7e452-bd4f-42af-90a8-50106d955288', foundational, discretionary_balancing_permitted).
narrative_ontology:cs_axiom_status(discretionary_balancing_permitted, holdable).
narrative_ontology:cs_axiom_grounding('95f7e452-bd4f-42af-90a8-50106d955288', discretionary_balancing_permitted, conventional).
narrative_ontology:cs_reference_frame('95f7e452-bd4f-42af-90a8-50106d955288', balanced_monetary_policy_mandate).
narrative_ontology:cs_drift_state('95f7e452-bd4f-42af-90a8-50106d955288', post_pspp_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('95f7e452-bd4f-42af-90a8-50106d955288', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, debtor_households).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, wage_earners).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, indebted_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, net_savers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, fixed_income_retirees).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, creditor_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 127 TFEU and calibrates euro area monetary policy. Under this reading, it weighs employment and growth data alongside price stability, defending the balance through press conferences, legal argumentation, and bond-purchase programs. It cannot exit the Treaty framework but shapes its operational meaning.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from compressed sovereign spreads and lower refinancing costs when the ECB maintains accommodative policy to support growth and employment. Their finance ministries advocate for the expansive reading in Eurogroup and ECB forums.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, indebted_member_states, beneficiary,
    organized, biographical, constrained, continental).

% Experience lower debt-service burdens on mortgages and consumer credit due to sustained low policy rates. They are embedded in the euro area banking system and cannot opt out of the monetary regime.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, debtor_households, beneficiary,
    powerless, biographical, trapped, continental).

% Benefit from output-gap closure and reduced unemployment when the ECB delays tightening to support growth. They lack direct voice in Governing Council deliberations and depend on aggregate labor demand.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, wage_earners, beneficiary,
    powerless, biographical, trapped, continental).

% Bear negative or below-inflation real returns on deposits and safe assets due to prolonged low and negative policy rates. They can shift to riskier assets but cannot escape the euro-denominated monetary base and its yield structure.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, net_savers, payer,
    moderate, biographical, constrained, continental).

% Rely on interest income from savings and annuities that is eroded by negative real rates and QE-driven compression of risk-free yields. They are politically diffuse and lack institutional representation at the ECB.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, fixed_income_retirees, payer,
    powerless, biographical, trapped, continental).

% Member states with large net creditor positions and domestic political cultures favoring price stability bear inflation risk and loss of monetary policy influence. They are formally represented on the Governing Council but increasingly outvoted on unconventional measures.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, creditor_member_states, payer,
    powerful, generational, constrained, continental).

% Adjudicates challenges to ECB programs, validating the expansive mandate reading against orthodox challenges. Its authority as final interpreter is contested by national constitutional courts, creating legal uncertainty.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, european_court_of_justice, observer,
    institutional, generational, analytical, continental).

% Analyze ECB reaction functions and mandate compliance, splitting between endorsing the multi-objective approach and warning of fiscal dominance or mandate creep.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, monetary_policy_economists, observer,
    organized, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__expansive_secondary_objectives, diffuse).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__expansive_secondary_objectives, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates multi-objective monetary policy across a heterogeneous currency union by permitting the central bank to weigh employment and growth alongside price stability, preventing procyclical over-tightening when inflation is subdued and fiscal union is absent.
% TRANSFER_FUNCTION: Transfers purchasing power and fiscal space from net savers, fixed-income retirees, and creditor-biased member states to debtor households, wage earners, and indebted governments through lower real interest rates, expanded balance sheets, and tolerance of moderately higher inflation.
% ABSENT_VOICES: Net savers and fixed-income retirees in creditor member states lack formal representation in ECB Governing Council deliberations; orthodox central bankers from hawkish traditions are present but consistently outvoted or structurally marginalized in the consensus-driven council.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished and the ECB were constrained to exclusive price stability without operational weight for employment, policy rates would likely have been higher for longer in the 2010s, sovereign spreads wider for periphery states, and unemployment persistently elevated; the euro area would have moved closer to breakup scenarios during the debt crisis.
% FOUNDING_PROBLEM: The absence of fiscal union in a heterogeneous currency union creates asymmetric shocks and divergent competitiveness that cannot be corrected by exchange-rate adjustment, risking deflationary spirals, secular stagnation, and political disintegration.
% FOUNDING_PROBLEM_CORROBORATION: Independent macroeconomic historians and non-ECB academic economists corroborate the structural problem of a suboptimal currency area; however, the expansive reading as the necessary solution is contested by the Bundesbank, the German Federal Constitutional Court, and creditor-state finance ministries. The ECB and debtor-state governments attest the live nature of the problem from inside the benefiting parties.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the constraint authorizes a standing distributional transfer from savers to debtors through monetary policy, maintained by suppressing the orthodox inflation-only alternative. Suppression (0.58) is moderate: the ECJ actively validates the reading and the ECB enforces it through program design, but the orthodox alternative remains intellectually and politically live, especially in creditor states. Theater ratio (0.35) reflects that ECB rhetoric about employment often exceeds its actual reaction-function weight, though the coordination is not merely performative. Accessibility collapse (0.50) captures that euro exit is politically prohibitive and internal mandate reversal requires Treaty change, though the orthodox reading remains conceptually available. Resistance (0.55) is significant due to Bundesbank critiques, German Constitutional Court challenges, and hawkish dissent on the Governing Council.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (savers, creditor states) experience the constraint as extraction because they bear uncompensated costs with limited exit. The beneficiary seats (debtors, workers) experience it as coordination because it prevents worse macroeconomic outcomes. The ECB seat experiences it as legitimate legal discretion. The engine computes these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (debtor households, wage earners, indebted member states) sit at low directionality: the constraint subsidizes their debt burdens and labor-market outcomes. Payers (net savers, fixed-income retirees, creditor member states) sit at high directionality: they bear the costs of negative real rates and inflation tolerance. The ECB Governing Council sits near the beneficiary end in institutional terms (it expands its own authority through this reading) but is constrained by Treaty obligations. The divergence between saver and debtor seats is the core structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents pure-extraction mislabeling because it solves a genuine coordination problem (suboptimal currency area lacking fiscal union). Without the secondary-objective safety valve, procyclical tightening in 2011-2014 would likely have deepened the depression. However, the solution is not Pareto-improving: it asymmetrically benefits debtors at saver expense and requires active legal suppression of the orthodox reading, satisfying the Tangled Rope gate rather than Rope or Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secondary_clause_justiciability,
    'Is the ''without prejudice to'' clause in Article 127(1) TFEU a genuine legal authorization for discretionary balancing between price stability and employment, or merely a non-justiciable rhetorical commitment?',
    'ECJ ruling explicitly calibrating the justiciability standard for secondary-objective weighting, or treaty revision clarifying the hierarchy.',
    'If non-justiciable, the constraint''s extraction is lower (mere coordination); if justiciable, the constraint represents enforceable distributional authority and tangled_rope extraction is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_clause_justiciability, conceptual, 'Legal status of the secondary objective clause').

omega_variable(
    distributional_net_effect,
    'Does the secondary-objective framework generate net welfare gains for the currency union as a whole, or primarily redistribute real resources between savers and debtors?',
    'Comparative counterfactual analysis of euro area macroeconomic outcomes under the orthodox price-stability-only reading versus the expansive reading.',
    'If net welfare gains are demonstrated, the coordination component dominates and extraction is overstated; if pure redistribution, asymmetric extraction dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_net_effect, empirical, 'Whether the constraint coordinates or primarily redistributes').

omega_variable(
    ecj_authority_erosion,
    'Does the German Federal Constitutional Court''s challenge to the PSPP and ECB mandate interpretation constitute temporary resistance or structural authority erosion for the ECJ''s interpretive monopoly?',
    'Tracking subsequent compliance of German institutions with ECJ rulings and the evolution of Bundesverfassungsgericht jurisprudence on EU law primacy.',
    'If structural erosion, the expansive reading loses its enforcement backbone and may collapse toward the orthodox reading or institutional deadlock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecj_authority_erosion, empirical, 'Stability of the legal authority sustaining the expansive reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_exp_sec_tr_t0, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ecb_exp_sec_tr_t3, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 3, 0.22).
narrative_ontology:measurement(ecb_exp_sec_tr_t6, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 6, 0.25).
narrative_ontology:measurement(ecb_exp_sec_tr_t9, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 9, 0.28).
narrative_ontology:measurement(ecb_exp_sec_tr_t12, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 12, 0.32).
narrative_ontology:measurement(ecb_exp_sec_tr_t14, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 14, 0.35).

% Extraction over time
narrative_ontology:measurement(ecb_exp_sec_be_t0, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ecb_exp_sec_be_t3, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(ecb_exp_sec_be_t6, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(ecb_exp_sec_be_t9, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 9, 0.55).
narrative_ontology:measurement(ecb_exp_sec_be_t12, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(ecb_exp_sec_be_t14, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 14, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ecb_exp_sec_su_t0, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ecb_exp_sec_su_t3, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 3, 0.38).
narrative_ontology:measurement(ecb_exp_sec_su_t6, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(ecb_exp_sec_su_t9, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 9, 0.5).
narrative_ontology:measurement(ecb_exp_sec_su_t12, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(ecb_exp_sec_su_t14, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, climate_incorporation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 127 TFEU kernel, which decomposes into at least three structurally distinct constraints: orthodox_price_stability (mountain/rope candidate), expansive_secondary_objectives (tangled_rope), and climate_incorporation (tangled_rope/scaffold candidate). The expansive reading treats the 'without prejudice' clause as authorizing discretionary balancing; the orthodox reading treats it as rhetorical surplusage; the climate reading extends the secondary-objective logic to environmental integration. Each has distinct beneficiary/victim structures and epsilon profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
