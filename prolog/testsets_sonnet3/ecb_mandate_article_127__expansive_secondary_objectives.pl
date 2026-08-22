% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: ECB Mandate — Expansive Secondary Objectives Reading (Article 127(1) 'Without Prejudice' Discretionary Balancing)
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   This story instantiates the expansive secondary objectives reading of
 *   Article 127(1) TFEU: the clause providing that the ECB shall support
 *   general economic policies in the Union 'without prejudice to the
 *   objective of price stability' is read as authorizing genuine
 *   discretionary balancing whenever the Governing Council judges price
 *   stability is not threatened. This is a distinct constraint from the
 *   orthodox reading (exclusive price-stability focus, secondary objectives
 *   strictly non-operational) and from the climate-incorporation reading
 *   (treaty-based obligation to integrate climate risk under Article 11
 *   TFEU). The three readings share a kernel — the same treaty text and the
 *   same institutional actor — but instantiate structurally different
 *   constraints with different beneficiary sets, different extraction
 *   profiles, and different suppression mechanisms. Under this reading, ε
 *   reflects the standing arrangement as this reading's own proponents would
 *   assess it: real but moderate extraction from savers and creditor-state
 *   constituencies, in service of a genuine cross-regional coordination
 *   function.
 *
 * KEY AGENTS:
 *   - governing_council_discretion: institutional agenda-setter, interprets and operationalizes the without-prejudice clause
 *   - wage_earning_households: powerless beneficiaries of accommodative tolerance during downturns
 *   - indebted_sovereign_states: moderate-power beneficiaries via eased financing conditions
 *   - fixed_income_savers: powerless payers via real-return erosion
 *   - creditor_member_states: institutional payers via subsidized peripheral financing
 *   - price_stability_hawks_within_governing_council: powerful but structurally outvoted dissenting minority
 *   - european_parliament: consultative observer with no binding corrective power
 *   - european_court_of_justice: analytical observer that has validated broad ECB discretion without adjudicating the interpretive contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.42).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.38).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.42).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Mandate — Expansive Secondary Objectives Reading (Article 127(1) 'Without Prejudice' Discretionary Balancing)").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, 'dd53c1e6-727d-4aeb-b560-88b8ca91b5f2').
narrative_ontology:cs_kernel_codification('dd53c1e6-727d-4aeb-b560-88b8ca91b5f2', fixed_text).
narrative_ontology:cs_authority_grounding('dd53c1e6-727d-4aeb-b560-88b8ca91b5f2', lineage).
narrative_ontology:cs_interpretation_layer_present('dd53c1e6-727d-4aeb-b560-88b8ca91b5f2').
narrative_ontology:cs_reading_relation('dd53c1e6-727d-4aeb-b560-88b8ca91b5f2', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('dd53c1e6-727d-4aeb-b560-88b8ca91b5f2', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('dd53c1e6-727d-4aeb-b560-88b8ca91b5f2', foundational, without_prejudice_clause_grants_genuine_operational_discretion).
narrative_ontology:cs_axiom_status(without_prejudice_clause_grants_genuine_operational_discretion, holdable).
narrative_ontology:cs_axiom_grounding('dd53c1e6-727d-4aeb-b560-88b8ca91b5f2', without_prejudice_clause_grants_genuine_operational_discretion, conventional).
narrative_ontology:cs_axiom('dd53c1e6-727d-4aeb-b560-88b8ca91b5f2', secondary, distributional_effects_of_monetary_policy_are_legitimate_balancing_inputs).
narrative_ontology:cs_axiom_status(distributional_effects_of_monetary_policy_are_legitimate_balancing_inputs, holdable).
narrative_ontology:cs_axiom_grounding('dd53c1e6-727d-4aeb-b560-88b8ca91b5f2', distributional_effects_of_monetary_policy_are_legitimate_balancing_inputs, instrumental).
narrative_ontology:cs_reference_frame('dd53c1e6-727d-4aeb-b560-88b8ca91b5f2', treaty_text_functional_flexibility).
narrative_ontology:cs_drift_state('dd53c1e6-727d-4aeb-b560-88b8ca91b5f2', post_sovereign_debt_crisis_qe_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dd53c1e6-727d-4aeb-b560-88b8ca91b5f2', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, wage_earning_households).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, indebted_sovereign_states).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, leveraged_corporate_borrowers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, governing_council_discretion).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, fixed_income_savers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, creditor_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, price_stability_hawks_within_governing_council).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, dual_mandate_functional_equivalence_doctrine).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, central_bank_discretion_as_democratic_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Governing Council interprets 'without prejudice to price stability' as authorizing it to weight employment, growth, and financial stability considerations whenever it judges inflation is not threatened. It sets its own threshold for when secondary objectives become operative, writes the forward guidance that operationalizes this reading, and faces no binding external check on where it draws that line.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, governing_council_discretion, agenda_setter,
    institutional, generational, analytical, continental).

% Benefit when the ECB tolerates modestly looser policy to support employment during downturns rather than treating any inflation risk as an absolute stop. Cannot participate in Governing Council deliberations, cannot exit the eurozone labor market, and depend entirely on this reading being the one that prevails in practice.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, wage_earning_households, beneficiary,
    powerless, biographical, trapped, continental).

% High-debt member states benefit from the discretionary balancing reading because it permits accommodative policy (lower rates, continued asset purchases) that eases sovereign debt service and financing conditions, even when this reading is contested by creditor states as beyond the mandate's proper scope. Their exit option — leaving the currency union — is catastrophic and effectively foreclosed.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, indebted_sovereign_states, beneficiary,
    moderate, generational, constrained, national).

% Firms carrying substantial euro-denominated debt benefit from the discretionary balancing reading's tolerance for accommodative conditions supporting growth and refinancing. They lobby through industry associations for the expansive reading to remain operative but cannot directly alter the mandate's interpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, leveraged_corporate_borrowers, beneficiary,
    organized, biographical, constrained, continental).

% Savers and pensioners holding fixed-income instruments bear the cost when the expansive reading tolerates policy accommodation that erodes real returns. They have no institutional voice in Governing Council deliberations and limited practical ability to reallocate into inflation-protected assets at scale.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, fixed_income_savers, payer,
    powerless, biographical, trapped, continental).

% Low-debt, export-oriented member states (historically Germany, the Netherlands) argue the expansive reading transfers real costs to their savers and taxpayers to subsidize peripheral sovereign financing conditions. They retain formal votes on the Governing Council and in Treaty reform processes but cannot unilaterally force a narrower interpretation without treaty-level consensus.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, creditor_member_states, payer,
    institutional, generational, constrained, national).

% A minority bloc within the Governing Council itself who read Article 127(1) as requiring exclusive or near-exclusive focus on price stability. They participate in votes but are structurally outvoted when the majority adopts the expansive discretionary-balancing reading; their orthodox interpretation becomes a dissenting minority position rather than the operative one.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, price_stability_hawks_within_governing_council, payer,
    powerful, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__expansive_secondary_objectives, price_stability_hawks_within_governing_council, excluded).

% Holds hearings and receives testimony on ECB policy but has no binding power to compel a particular mandate interpretation; can question but not overrule the Governing Council's reading of 'without prejudice.' Represents a democratic accountability channel that is consultative rather than corrective.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, european_parliament, observer,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__expansive_secondary_objectives, european_parliament, excluded).

% Has ruled (Gauweiler, Weiss) that the ECB retains broad discretion in interpreting its mandate so long as measures are proportionate and price stability is not structurally endangered — effectively validating wide latitude for the expansive reading without adjudicating between the competing interpretations on the merits.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, european_court_of_justice, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__expansive_secondary_objectives, diffuse).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__expansive_secondary_objectives, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single monetary authority with the interpretive flexibility to respond to divergent economic conditions across a heterogeneous currency union — allowing accommodative policy during downturns without requiring a treaty amendment each time economic circumstances change.
% TRANSFER_FUNCTION: Moves real purchasing power and financing-cost relief from creditor-state savers and fixed-income holders toward debtor states, leveraged borrowers, and cyclically-exposed wage earners, via the monetary policy stance the discretionary reading permits.
% ABSENT_VOICES: National fiscal authorities of creditor states, ordinary savers, and the orthodox minority within the Governing Council itself would object that this reading exceeds the price-stability-primacy the Treaty's text privileges; savers in particular have no direct representation in the interpretive process and experience the reading only through its effects on returns.
% DISAPPEARANCE_RATIONALE: If this reading were displaced by the orthodox price-stability reading, the ECB would lose the interpretive latitude to weight employment and financing conditions during downturns; asset purchase programs, forward guidance premised on growth support, and accommodative stances during sovereign debt stress would lose their mandate-based justification, forcing either treaty reform or a much narrower operating framework — a substantial rearrangement of eurozone monetary governance.
% FOUNDING_PROBLEM: The original problem was designing a monetary mandate for a currency union spanning economically heterogeneous member states, where rigid single-objective rules risked producing policy too tight for depressed regions and requiring frequent treaty renegotiation to accommodate changing economic conditions.
% FOUNDING_PROBLEM_CORROBORATION: The Governing Council and debtor-state governments attest the flexibility remains necessary given persistent asymmetric shocks across the union. Independent monetary economists and creditor-state central bank officials (outside the ECB's own governance) have testified in Bundestag and academic fora that the 'without prejudice' clause was drafted as a narrow safety valve, not a basis for systematic secondary-objective operationalization — suggesting the founding problem's scope has been read more broadly over time than the drafters intended.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction (0.42 at interval end) reflects a real but moderate transfer — the discretionary reading does redistribute real costs from savers and creditor states toward debtors and cyclically-exposed workers, but the transfer rides on a genuine coordination function (a workable single monetary policy for a heterogeneous currency union) rather than being pure rent extraction. Suppression (0.38) is moderate: the orthodox reading's proponents are outvoted rather than silenced, and the ECJ's Gauweiler/Weiss jurisprudence has validated broad discretion without banning contestation. Theater ratio (0.28) is low-moderate: the Governing Council's public communications genuinely operationalize the balancing function rather than merely performing it, though some forward guidance has arguably drifted toward justifying decisions taken on other grounds.
 *
 * DIRECTIONALITY LOGIC:
 *   Wage-earning households, indebted states, and leveraged borrowers sit near the beneficiary end of directionality — the discretionary reading's accommodative tolerance flows to them as reduced financing costs and employment support. Fixed-income savers and creditor member states sit toward the target end — they bear the real-return erosion and implicit subsidy costs. The Governing Council itself sits at the agenda-setting extreme: it does not merely benefit from the reading, it constructs and administers it, giving it discretion no other seat holds. The price-stability hawks are the interesting case: same institutional power level as the majority, but structurally on the losing side of every vote where the expansive reading is applied — their directionality is target-like despite their formal power, because their preferred narrower reading is never operative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — designing flexibility into a currency-union mandate to avoid requiring treaty renegotiation for every economic cycle — remains partially live (asymmetric shocks persist) but is contested as to whether the CURRENT scope of discretionary balancing exceeds what the founders intended as a narrow safety valve. This is not classic mandatrophy (a dead mandate propped up by inertia): both the orthodox and expansive camps agree the underlying coordination problem is real; they disagree about how much interpretive latitude the without-prejudice clause was meant to license. Classifying this as tangled_rope rather than snare or piton reflects that judgment — the coordination function is genuine and contested, not fictional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy_expansive_vs_orthodox,
    'Is the expansive secondary-objectives reading a legitimate exercise of the discretion the without-prejudice clause grants, or a drift beyond what Article 127(1)''s text and drafting history support?',
    'This constraint is one reading (expansive_secondary_objectives) of the ecb_mandate_article_127 kernel. The sibling readings — orthodox_price_stability (exclusive, non-operational secondary objectives) and climate_incorporation (treaty-mandated climate integration under Article 11 TFEU) — are separate constraint stories with their own ε values and stakeholder structures, linked via network.affects_constraints. No single empirical test resolves which reading is ''correct''; the ECJ''s proportionality-based deference (Gauweiler, Weiss) has so far validated broad latitude without settling the interpretive contest on the merits.',
    'If the orthodox reading were to become dominant (e.g. via treaty amendment or a stricter ECJ ruling), the beneficiary set here (debtor states, wage earners, leveraged borrowers) would lose the mandate-based justification for accommodative tolerance, and this constraint''s extraction/suppression profile would not apply — the orthodox_price_stability constraint''s profile would govern instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy_expansive_vs_orthodox, conceptual, 'Committer-frame ambiguity: which of the three kernel readings is the operative one is itself contested and not resolvable by data internal to this story.').

omega_variable(
    discretion_as_democratic_deficit_or_necessary_flexibility,
    'Does the Governing Council''s unchecked interpretive discretion over ''without prejudice'' constitute a democratic accountability gap, or is it the necessary flexibility a technocratic, treaty-insulated central bank requires to function across a heterogeneous union?',
    'Comparative institutional analysis: track whether treaty reform proposals to formalize secondary-objective weighting (making explicit what is now discretionary) gain political traction, and whether the European Parliament''s consultative oversight produces any binding constraint over time.',
    'If discretion is judged a democratic deficit, the suppression metric understates the constraint''s coercive character (unaccountable power exercised over savers and creditor states without formal consent). If judged necessary flexibility, the coordination framing (tangled_rope, not snare) is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discretion_as_democratic_deficit_or_necessary_flexibility, preference, 'Whether unchecked interpretive discretion is illegitimate technocratic power or necessary institutional flexibility is a values question, not an empirical one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 5, 0.18).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 10, 0.21).
narrative_ontology:measurement(ecb__tr_t15, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 15, 0.24).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 20, 0.26).
narrative_ontology:measurement(ecb__tr_t25, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 5, 0.26).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(ecb__be_t15, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(ecb__be_t25, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 10, 0.31).
narrative_ontology:measurement(ecb__su_t15, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 15, 0.34).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(ecb__su_t25, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 25, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__expansive_secondary_objectives, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the ecb_mandate_article_127 kernel (Article 127(1) TFEU and its 'without prejudice' clause). orthodox_price_stability reads the clause as requiring exclusive, non-operational treatment of secondary objectives (low ε, narrow beneficiary set, Rope-leaning). climate_incorporation reads Article 11 TFEU as imposing a treaty obligation to integrate climate risk into asset purchase and collateral frameworks (distinct beneficiary set: green-transition-exposed sectors and future generations; distinct victim set: carbon-intensive incumbents). This reading (expansive_secondary_objectives) authorizes discretionary balancing toward employment/growth whenever price stability is not judged threatened, producing a broader beneficiary set (workers, debtors) and moderate suppression of the orthodox alternative within the Governing Council itself. Each reading has a stable, independently-authored ε; they are linked, not merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
