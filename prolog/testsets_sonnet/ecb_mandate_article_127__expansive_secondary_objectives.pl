% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Article 127 Reading: Expansive Secondary Objectives / Discretionary Balancing
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   This story instantiates the expansive-secondary-objectives reading of the
 *   ECB's Article 127 mandate: the price-stability primary objective is read
 *   narrowly enough, and the 'without prejudice' clause read broadly enough,
 *   that the Governing Council treats employment and growth as legitimate
 *   operational considerations whenever inflation is not judged under threat.
 *   This is a distinct constraint from the orthodox reading (which forecloses
 *   operational weight on secondary objectives) and from the
 *   climate-incorporation reading (which grounds discretion in Article 11
 *   TFEU environmental integration rather than employment/growth balancing).
 *   Each reading has its own beneficiary structure and its own ε; this file
 *   covers only the employment/growth balancing reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.42).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.38).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.42).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Article 127 Reading: Expansive Secondary Objectives / Discretionary Balancing").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, '16ee30c2-0ae9-428c-a8aa-52b0a686fc2c').
narrative_ontology:cs_kernel_codification('16ee30c2-0ae9-428c-a8aa-52b0a686fc2c', fixed_text).
narrative_ontology:cs_authority_grounding('16ee30c2-0ae9-428c-a8aa-52b0a686fc2c', lineage).
narrative_ontology:cs_interpretation_layer_present('16ee30c2-0ae9-428c-a8aa-52b0a686fc2c').
narrative_ontology:cs_reading_relation('16ee30c2-0ae9-428c-a8aa-52b0a686fc2c', ecb_mandate_article_127__orthodox_price_stability, forecloses).
narrative_ontology:cs_reading_relation('16ee30c2-0ae9-428c-a8aa-52b0a686fc2c', ecb_mandate_article_127__climate_incorporation, coexists_with).
narrative_ontology:cs_axiom('16ee30c2-0ae9-428c-a8aa-52b0a686fc2c', foundational, without_prejudice_clause_grants_operational_discretion).
narrative_ontology:cs_axiom_status(without_prejudice_clause_grants_operational_discretion, holdable).
narrative_ontology:cs_axiom_grounding('16ee30c2-0ae9-428c-a8aa-52b0a686fc2c', without_prejudice_clause_grants_operational_discretion, conventional).
narrative_ontology:cs_axiom('16ee30c2-0ae9-428c-a8aa-52b0a686fc2c', secondary, employment_growth_are_legitimate_operational_weights).
narrative_ontology:cs_axiom_status(employment_growth_are_legitimate_operational_weights, holdable).
narrative_ontology:cs_axiom_grounding('16ee30c2-0ae9-428c-a8aa-52b0a686fc2c', employment_growth_are_legitimate_operational_weights, instrumental).
narrative_ontology:cs_reference_frame('16ee30c2-0ae9-428c-a8aa-52b0a686fc2c', maastricht_price_stability_primacy).
narrative_ontology:cs_drift_state('16ee30c2-0ae9-428c-a8aa-52b0a686fc2c', post_sovereign_debt_crisis_and_pandemic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('16ee30c2-0ae9-428c-a8aa-52b0a686fc2c', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, wage_dependent_workers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, sovereign_debtors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, employment_sensitive_regions).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, fixed_income_savers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, creditor_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, price_stability_hawks_within_governing_council).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, operational_discretion_doctrine).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, without_prejudice_clause_authorizes_balancing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 127(1)'s primary objective of price stability and its secondary 'without prejudice' clause. Under this reading, the Council treats the clause as authorizing genuine operational weight on employment and growth whenever inflation is judged not threatened, giving it wide discretion to calibrate policy toward broader economic outcomes rather than a narrow inflation target alone.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, arbitrage, continental).

% Benefit when the ECB tolerates somewhat looser policy to support employment, since job security and wage growth are more sensitive to real-economy slack than to marginal inflation deviations. Cannot influence the Council's interpretive choice directly and have no exit from the currency union's monetary stance.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, wage_dependent_workers, beneficiary,
    powerless, biographical, trapped, continental).

% Heavily indebted member states benefit from an accommodative reading of the mandate that keeps borrowing costs lower and nominal growth higher, easing debt service. They lobby politically for this reading but cannot compel it; their exit would require leaving the currency union entirely.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, sovereign_debtors, beneficiary,
    organized, generational, constrained, continental).

% Regions and sectors with structurally higher unemployment sensitivity to monetary tightening gain from a reading that lets the ECB hold accommodative policy longer during recoveries. They have no formal channel into ECB decision-making beyond national government representation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, employment_sensitive_regions, beneficiary,
    moderate, biographical, constrained, regional).

% Savers and pension holders bear the cost when discretionary balancing tolerates higher inflation or prolonged low rates to support employment, eroding real returns. They can shift savings across asset classes but cannot exit the currency's monetary regime.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, fixed_income_savers, payer,
    moderate, biographical, constrained, continental).

% Fiscally conservative member states with net creditor positions bear reputational and real transfer costs when the mandate is read expansively, since it can be seen as subsidizing profligate debtor states through looser monetary conditions. They retain formal votes in EU institutions but cannot unilaterally overturn ECB legal interpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, creditor_member_states, payer,
    powerful, generational, constrained, continental).

% A minority faction within the Governing Council itself holds the orthodox reading and dissents from expansive balancing, but is structurally outvoted within the Council's own decision procedures once the majority interpretation is adopted.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, price_stability_hawks_within_governing_council, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__expansive_secondary_objectives, price_stability_hawks_within_governing_council, excluded).

% Reviews challenges to ECB actions (as in Gauweiler and Weiss) and has generally deferred to ECB discretion under a proportionality standard, effectively validating the expansive reading's legal viability without itself authoring the interpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, european_court_of_justice, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__expansive_secondary_objectives, diffuse).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__expansive_secondary_objectives, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates monetary policy across a heterogeneous currency union by allowing the central bank operational flexibility to weigh employment and growth conditions that differ sharply by member state, rather than mechanically targeting inflation in a way that could produce severe regional divergence in real outcomes.
% TRANSFER_FUNCTION: Moves real purchasing power and financing costs from savers and creditor states toward debtors, wage earners, and employment-sensitive regions, via the calibration of interest rates and asset purchase programs that this reading of the mandate authorizes.
% ABSENT_VOICES: Fixed-income savers and creditor-state taxpayers have no direct representation in Governing Council deliberations; their objections surface only indirectly through national finance ministries and academic/monetarist critique, not through a seat at the table where the interpretive choice is made.
% DISAPPEARANCE_RATIONALE: If this expansive reading were abandoned in favor of the orthodox reading, the ECB would lose discretionary latitude to tolerate above-target inflation or prolonged accommodation for employment support; heavily indebted states would face materially different financing conditions, and employment-sensitive regions would lose a channel of implicit support currently built into monetary calibration.
% FOUNDING_PROBLEM: The original Maastricht drafters needed to reconcile a hard anti-inflation mandate (demanded by Germany as a precondition for monetary union) with the reality that a single monetary policy would govern economically divergent states, some of which would periodically need employment support that a narrow inflation target alone could not provide.
% FOUNDING_PROBLEM_CORROBORATION: ECB officials and academic economists sympathetic to flexible inflation targeting attest the founding problem (regional divergence management) remains live and the expansive reading is a legitimate, intended tool for it. Independent legal scholars and creditor-state central bank officials (e.g., Bundesbank-aligned commentary) attest the founding problem was meant to be solved by strict subordination of secondary objectives, and that the expansive reading is an interpretive drift beyond what Article 127's text and drafting history support.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.42) and suppression (0.38) are moderate rather than severe: the reading redistributes real costs from savers and creditor states to debtors and workers through interest-rate and asset-purchase calibration, but it operates through legally sanctioned discretion rather than coercive suppression of alternatives — dissenting Council members retain votes, and legal challenge routes (ECJ) remain open, even though they have consistently failed. Theater ratio (0.3) reflects that the balancing function is substantively exercised, not merely performed, though the proportionality language used to justify particular decisions sometimes exceeds what the decisions actually weigh.
 *
 * DIRECTIONALITY LOGIC:
 *   Wage-dependent workers, sovereign debtors, and employment-sensitive regions are structural beneficiaries: the discretionary reading systematically tilts calibration toward conditions that serve their interests without requiring their direct participation in the interpretive choice. Fixed-income savers, creditor member states, and the hawkish minority within the Council itself bear the costs: their exit options are constrained (they cannot leave the currency union or dissent effectively within Council votes once outvoted). The Council itself sits as agenda-setter with the widest degrees of freedom — it authors the interpretation and is not itself extracted from.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (not snare) is deliberate: this reading does solve a genuine coordination problem — managing a single monetary policy across economically divergent member states without producing severe employment divergence — and is not merely extraction dressed as coordination. But it requires active enforcement (Council votes, ECJ deference) and produces identifiable payers (savers, creditor states) alongside identifiable beneficiaries (debtors, workers), which is precisely the tangled_rope signature. Treating this as pure Rope would erase the real distributional transfer; treating it as pure Snare would erase the real coordination function it performs for currency-union stability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    without_prejudice_clause_scope_ambiguity,
    'Does the ''without prejudice to the objective of price stability'' clause in Article 127(1) TFEU authorize genuine operational weight on employment/growth (this reading), or does it merely permit the ECB to support such objectives only when doing so has zero marginal effect on price stability (the orthodox reading)?',
    'A definitive ECJ ruling squarely addressing the operational (not merely rhetorical) weight given to secondary objectives in a case where price stability and employment considerations genuinely conflicted, rather than the proportionality-deference rulings issued to date (Gauweiler, Weiss).',
    'If the orthodox reading is correct, this constraint''s beneficiary/victim structure dissolves — the redistribution this reading authorizes would be legally unauthorized discretion rather than a legitimate reading of the mandate, reclassifying the underlying practice as ultra vires rather than tangled_rope coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(without_prejudice_clause_scope_ambiguity, conceptual, 'Whether the without-prejudice clause structurally authorizes operational balancing or only residual, non-conflicting accommodation.').

omega_variable(
    kernel_committer_structure,
    'This story is one reading of the contested ecb_mandate_article_127 kernel; what would adopting a sibling reading (orthodox_price_stability or climate_incorporation) change structurally?',
    'Compare the three linked constraint stories: orthodox_price_stability would foreclose this reading''s beneficiary set entirely (debtors/workers would no longer receive systematic operational tilt); climate_incorporation would add a parallel discretionary basis grounded in Article 11 TFEU that could operate alongside or in tension with employment/growth balancing depending on how climate and employment considerations interact in specific policy decisions.',
    'The disagreement is located in the interpretive scope given to ''without prejudice'' — whether it is read as a narrow safety valve (orthodox) or a genuine second mandate limb (this reading). Sibling readings are held by different factions within the ECB, academic commentary, and national governments simultaneously; no single framework currently resolves which reading is authoritative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Committer structure: this reading vs. orthodox_price_stability and climate_incorporation siblings of the same kernel.').

omega_variable(
    distributional_intent_vs_incidental_effect,
    'Is the redistribution from savers/creditor-states to debtors/workers an intended function of this reading, or an incidental side effect of pursuing legitimate stabilization that the Council does not itself treat as a distributional objective?',
    'Internal Governing Council deliberation records and speeches (to the extent published) indicating whether distributional considerations are explicitly weighed or whether officials frame all decisions purely in stabilization terms regardless of distributional outcome.',
    'If purely incidental, the tangled_rope characterization is weaker (closer to Rope with unintended externalities); if distributional weighing is explicit, the tangled_rope-with-active-enforcement characterization is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributional_intent_vs_incidental_effect, empirical, 'Whether distributional transfer is an object of the discretionary reading or a side effect of stabilization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 1999, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t1999, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 1999, 0.18).
narrative_ontology:measurement(ecb__tr_t2004, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2004, 0.2).
narrative_ontology:measurement(ecb__tr_t2009, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2009, 0.28).
narrative_ontology:measurement(ecb__tr_t2014, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2014, 0.32).
narrative_ontology:measurement(ecb__tr_t2019, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2019, 0.29).
narrative_ontology:measurement(ecb__tr_t2024, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(ecb__be_t1999, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 1999, 0.22).
narrative_ontology:measurement(ecb__be_t2004, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2004, 0.26).
narrative_ontology:measurement(ecb__be_t2009, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2009, 0.34).
narrative_ontology:measurement(ecb__be_t2014, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2014, 0.4).
narrative_ontology:measurement(ecb__be_t2019, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2019, 0.38).
narrative_ontology:measurement(ecb__be_t2024, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t1999, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 1999, 0.25).
narrative_ontology:measurement(ecb__su_t2004, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2004, 0.27).
narrative_ontology:measurement(ecb__su_t2009, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2009, 0.35).
narrative_ontology:measurement(ecb__su_t2014, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2014, 0.39).
narrative_ontology:measurement(ecb__su_t2019, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2019, 0.36).
narrative_ontology:measurement(ecb__su_t2024, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__expansive_secondary_objectives, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% This is one of three linked stories decomposing the natural-language claim 'the ECB's mandate under Article 127' into structurally distinct readings of a single contested kernel (ecb_mandate_article_127). orthodox_price_stability forecloses this reading's operational discretion; climate_incorporation coexists with it as a parallel discretionary basis. Each reading carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle — they are not the same constraint measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
