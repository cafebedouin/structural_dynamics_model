% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Discretionary Central Bank Authority Replacing Gold Anchor (Automatic Constraint Reading)
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the automatic_constraint_reading of
 *   the contested kernel gold_fiat_transition_mechanism. It treats the
 *   historical transition as the deliberate elimination of an automatic
 *   material constraint â the gold reserve limit on money creation â and
 *   its replacement with discretionary central bank authority. The resulting
 *   institutional constraint coordinates macroeconomic stabilization and
 *   lender-of-last-resort functions while asymmetrically extracting
 *   purchasing power from nominal creditors and fixed-income holders. Key
 *   agents by structural relationship: monetary authorities are the
 *   agenda-setter and primary beneficiary of discretion; creditor_class and
 *   fixed_income_holders are the payers bearing inflation-tax extraction;
 *   commercial_banks and sovereign_debtors are secondary beneficiaries;
 *   hard_money_advocates are excluded from the policy conversation;
 *   monetary_historians observe from an analytical seat.
 *
 * KEY AGENTS:
 *   - monetary_authorities: Primary agenda-setter and beneficiary (institutional/global) â gained discretionary power and administers the fiat framework
 *   - creditor_class: Primary payer (powerful/global) â holds nominal claims diluted by discretionary expansion
 *   - fixed_income_holders: Secondary payer (powerless/national) â bears inflation tax without hedging capacity
 *   - commercial_banks: Secondary beneficiary (institutional/global) â receives liquidity backstops and payment privileges
 *   - sovereign_debtors: Secondary beneficiary (institutional/national) â gains fiscal space and debt erosion
 *   - hard_money_advocates: Excluded voice (organized/global) â argues for commodity anchor but kept out of policy institutions
 *   - monetary_historians: Analytical observer (analytical/global) â documents regime operation without stake in its continuation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.79).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.68).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Discretionary Central Bank Authority Replacing Gold Anchor (Automatic Constraint Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "monetary_economics/political_economy").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, '7b157a76-97a7-4706-bb33-fbbb030e3f35').
narrative_ontology:cs_kernel_codification('7b157a76-97a7-4706-bb33-fbbb030e3f35', formalized).
narrative_ontology:cs_authority_grounding('7b157a76-97a7-4706-bb33-fbbb030e3f35', expertise).
narrative_ontology:cs_interpretation_layer_present('7b157a76-97a7-4706-bb33-fbbb030e3f35').
narrative_ontology:cs_reading_relation('7b157a76-97a7-4706-bb33-fbbb030e3f35', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b157a76-97a7-4706-bb33-fbbb030e3f35', gold_fiat_transition_mechanism__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('7b157a76-97a7-4706-bb33-fbbb030e3f35', foundational, material_anchor_automatic_constraint).
narrative_ontology:cs_axiom_status(material_anchor_automatic_constraint, holdable).
narrative_ontology:cs_axiom_grounding('7b157a76-97a7-4706-bb33-fbbb030e3f35', material_anchor_automatic_constraint, empirically_contingent).
narrative_ontology:cs_axiom('7b157a76-97a7-4706-bb33-fbbb030e3f35', foundational, discretionary_enforcement_dependence).
narrative_ontology:cs_axiom_status(discretionary_enforcement_dependence, holdable).
narrative_ontology:cs_axiom_grounding('7b157a76-97a7-4706-bb33-fbbb030e3f35', discretionary_enforcement_dependence, conventional).
narrative_ontology:cs_reference_frame('7b157a76-97a7-4706-bb33-fbbb030e3f35', discretionary_macro_stabilization).
narrative_ontology:cs_drift_state('7b157a76-97a7-4706-bb33-fbbb030e3f35', post_gfc_qe_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7b157a76-97a7-4706-bb33-fbbb030e3f35', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, commercial_banks).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_debtors).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_holders).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, discretionary_monetary_policy_doctrine).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, legal_tender_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the fiat monetary framework, setting interest rates and money supply without a material anchor. Gained discretionary power over money creation previously bounded by gold reserves. Cannot unilaterally restore a gold standard without systemic collapse, so their exit is constrained by the institutional structure they administer.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, beneficiary).

% Hold nominal debt instruments and financial claims denominated in fiat currency. Lost the automatic protection against debasement that gold convertibility provided; purchasing power is subject to discretionary monetary expansion.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    powerful, biographical, constrained, global).

% Rely on pensions, annuities, and savings deposits fixed in nominal terms. Bear the inflation tax silently; lack the financial sophistication or scale to hedge effectively against discretionary monetary expansion.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_holders, payer,
    powerless, biographical, constrained, national).

% Benefit from central bank liquidity facilities, payment system privileges, and the ability to create deposits within the fiat framework. Regulated by monetary authorities but structurally privileged by the discretionary regime.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, commercial_banks, beneficiary,
    institutional, generational, constrained, global).

% Governments and fiscal authorities gain fiscal space from the ability to borrow in nominally elastic currency. Debt burdens erode via inflation and monetary expansion; they are structurally protected from balance-of-payments crises that plagued gold-standard regimes.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_debtors, beneficiary,
    institutional, generational, constrained, national).

% Argue for restored commodity money or competing currencies to eliminate discretionary debasement. Structurally excluded from central bank governance, monetary policy committees, and mainstream macroeconomic institutions.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, hard_money_advocates, excluded,
    organized, generational, constrained, global).

% Analyze the transition and its distributional consequences across monetary regimes. Hold no stake in the constraint's continuation but document its operation and asymmetries.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an elastic money supply and lender-of-last-resort function that a rigid gold standard cannot: it accommodates banking panics, sovereign crisis finance, and secular growth in transactional demand without deflationary spirals or reserve shortages.
% TRANSFER_FUNCTION: Moves purchasing power from nominal creditors and fixed-income holders to sovereign debtors (via inflation tax and debt erosion) and to the banking system (via seigniorage and liquidity backstops), while concentrating discretionary power in monetary authorities.
% ABSENT_VOICES: Hard-money advocates, commodity-money theorists, and creditors from non-reserve-currency nations who would prefer an automatic anchor are excluded from monetary policy committees and mainstream macroeconomic institutions; their objections are treated as archaic or politically naive.
% DISAPPEARANCE_RATIONALE: If discretionary central bank authority vanished overnight and a gold anchor automatically reasserted, nominal debt contracts would reprice, sovereign fiscal space would contract sharply, banking systems would lose liquidity elasticity, and global trade settlement would reconverge on physical reserve movements â the monetary order would fundamentally rearrange.
% FOUNDING_PROBLEM: The gold standard imposed a rigid money supply that could not accommodate banking panics, cyclical credit contraction, or sovereign war finance without severe deflation and bank failures.
% FOUNDING_PROBLEM_CORROBORATION: Monetary authorities and sovereign debtors attest the problem remains live, citing the need for counter-cyclical policy. Heterodox economists and monetary historians outside the benefiting parties attest the founding problem was largely soluble through other mechanisms (fiscal policy, branch banking, clearinghouse certificates) and that the transition served fiscal extraction; independent historical scholarship documents that the automatic constraint was eliminated under emergency conditions that later became permanent.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.79, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.79) because discretionary fiat authority systematically transfers purchasing power from nominal creditors to issuers and banks through inflation tax and financial repression. Suppression is substantial (0.68) due to legal tender laws, tax denomination requirements, capital controls, and the suppression of commodity-money alternatives. Theater ratio is moderate (0.45): central bank press conferences, inflation targeting frameworks, and forward guidance perform legitimacy while genuine coordination (lender of last resort, elastic money) operates underneath. Accessibility collapse is moderate (0.60): gold and crypto alternatives exist but are heavily taxed, regulated, or legally excluded from core payment and tax functions. Resistance is moderate (0.55): hard-money movements, some creditor nations, and academic critics mount persistent but institutionally marginalized opposition. The temporal series show rising extraction and theater over the interval, with a suppression dip during the 1990s liberalization followed by renewed hardening in the QE era.
 *
 * PERSPECTIVAL GAP:
 *   From the monetary authority seat, the constraint is necessary coordination â without discretionary elasticity, banking panics and deflationary spirals would destroy value. From the creditor seat, the same structure is extraction without anchor â a discretionary tax on nominal holdings. The engine computes this divergence from the structural data; the authored claim does not adjudicate the dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities sit near the beneficiary end: they control the constraint and gain discretion from its operation. Sovereign debtors and commercial banks also draw low directionality because the constraint subsidizes their fiscal and liquidity positions. Creditor_class and fixed_income_holder sit near the full-target end: their nominal claims are the extraction surface. The asymmetry is structural, not incidental â the same monetary expansion that eases sovereign debt burdens dilutes creditor wealth.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling this constraint as either pure coordination (Rope) or pure extraction (Snare). Fiat discretion genuinely solves collective-action problems in liquidity provision and counter-cyclical policy that a rigid gold standard cannot. At the same time, it asymmetrically extracts from identifiable victims (creditors, fixed-income holders) through inflation and financial repression, and its persistence requires active legal and institutional enforcement. A pure Rope reading would ignore the creditor victimization; a pure Snare reading would ignore the genuine lender-of-last-resort and macro-stabilization functions. The dual requirement captures the hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automatic_reading_position,
    'Does the automatic constraint reading capture the causal core of the transition, or does the composite overdetermination reading better describe the historical process?',
    'Comparative historiography weighing the counterfactual: would fiat discretion have emerged without the telecommunications revolution, labor shifts, and legal tender maturation cited in the composite reading?',
    'If composite, the automatic reading overstates the structural discreteness of the constraint and should be merged into a broader institutional convergence story; if automatic, the elimination of the material anchor is the independent variable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automatic_reading_position, conceptual, 'Position of this reading within the contested kernel').

omega_variable(
    extraction_coordination_boundary,
    'What proportion of discretionary monetary expansion is necessary coordination (elasticity for growth and stability) versus extractive transfer (inflation tax and seigniorage)?',
    'Independent macroeconomic audit of counterfactual money demand under a revived automatic anchor, comparing realized paths to simulated rigid-money paths.',
    'A high coordination share would shift the constraint toward rope; a high extraction share would shift toward snare. The current tangled rope classification depends on this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_coordination_boundary, empirical, 'Coordination versus extraction in fiat discretion').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative monies structural (legal tender laws, tax codes, capital controls) or internalized (the belief that state fiat is the natural form of money)?',
    'Natural experiments from jurisdictions with weakened enforcement capacity: observe whether fiat remains dominant by habit or retreats to commodity substitutes when structural enforcement fails.',
    'If internalized, effective suppression is higher than structural measure suggests and the constraint is more deeply embedded; if purely structural, the constraint is brittle and its classification leans toward enforcement-dependent tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of monetary alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(gold_tr_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(gold_tr_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(gold_tr_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(gold_tr_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 40, 0.49).
narrative_ontology:measurement(gold_tr_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(gold_be_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gold_be_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(gold_be_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(gold_be_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(gold_be_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(gold_be_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(gold_su_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(gold_su_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(gold_su_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(gold_su_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(gold_su_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 50, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, resource_allocation).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The gold_fiat_transition_mechanism kernel decomposes into three readings. This file (automatic_constraint_reading) treats the transition as the discrete elimination of a material constraint and creation of an institutional extractive/coordinating mechanism. Creditor_discipline_reading reframes it as a geopolitical power shift from creditors to reserve-currency issuers. Composite_overdetermination_reading denies a single causal node and treats the transition as convergent overdetermination. Each reading instantiates a different constraint with different epsilon values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
