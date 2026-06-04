% ============================================================================
% CONSTRAINT STORY: procedural_due_process__mathews_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_procedural_due_process__mathews_balancing_reading, []).

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
 *   constraint_id: procedural_due_process__mathews_balancing_reading
 *   human_readable: Mathews Balancing Test: Procedural Due Process as Administrative Optimization
 *   domain: legal/constitutional_doctrine
 *
 * SUMMARY:
 *   Mathews v. Eldridge (1976) established a three-factor balancing test for
 *   procedural due process: (1) the private interest affected; (2) the risk
 *   of erroneous deprivation through existing procedures and the probable
 *   value of additional safeguards; (3) the government's interest, including
 *   fiscal and administrative burden. This reading of the procedural due
 *   process kernel instantiates due process as a constitutional optimization
 *   problem, not a fixed form. The Mathews balancing suppresses categorical
 *   hearing rights in low-stakes cases (small benefit amounts, minor
 *   professional consequences) by explicitly discounting the claimant's
 *   private interest against government burden. The constraint benefits
 *   administrative agencies through budgetary flexibility and reduces
 *   procedural costs. It extracts from categorical-hearing claimants who lose
 *   the right to pre-termination hearing that Goldberg v. Kelly (1970) seemed
 *   to establish. The Mathews reading coexists with the Goldberg reading —
 *   different circuits and different statutory schemes apply different
 *   doctrinal interpretations — but they emanate from the same constitutional
 *   text and generate different victim sets and beneficiary distributions.
 *   This is a kernel-reading constraint: the constitutional commitment (due
 *   process) is fixed; the reading (optimization vs. categorical rights)
 *   varies across institutional actors and jurisdictions.
 *
 * KEY AGENTS:
 *   - Categorical hearing claimants (powerless/trapped): victims of process-shaving in low-stakes cases; bear the cost of error and delay; no exit capacity
 *   - Administrative agencies (institutional/arbitrage): beneficiaries of the balancing framework; gain budgetary control and flexibility; experience it as pure coordination
 *   - Government fiscal interest (institutional/arbitrage): beneficiary; gains calculable procedure-sizing across benefit programs; solves budget allocation problem
 *   - Due process bar and advocacy community (organized/constrained): constrained by categorical-right suppression but retain leverage to litigate balance-shifting; see access to balancing arithmetic
 *   - Congressional legislator (powerful/mobile): retains override capacity through statute; sees Mathews as default, not immutable
 *   - Judicial interpreter (institutional/arbitrage): tasked with applying the balancing test across contexts; benefits from formulaic decision procedure; extracts interpretive authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(procedural_due_process__mathews_balancing_reading, 0.52).
domain_priors:suppression_score(procedural_due_process__mathews_balancing_reading, 0.65).
domain_priors:theater_ratio(procedural_due_process__mathews_balancing_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(procedural_due_process__mathews_balancing_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(procedural_due_process__mathews_balancing_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(procedural_due_process__mathews_balancing_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(procedural_due_process__mathews_balancing_reading, tangled_rope).
narrative_ontology:human_readable(procedural_due_process__mathews_balancing_reading, "Mathews Balancing Test: Procedural Due Process as Administrative Optimization").
narrative_ontology:topic_domain(procedural_due_process__mathews_balancing_reading, "legal/constitutional_doctrine").

domain_priors:requires_active_enforcement(procedural_due_process__mathews_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(procedural_due_process__mathews_balancing_reading, 'aaecb473-0eb8-45db-92a7-bfeeaabd8dd6').
narrative_ontology:cs_kernel_codification('aaecb473-0eb8-45db-92a7-bfeeaabd8dd6', fixed_text).
narrative_ontology:cs_authority_grounding('aaecb473-0eb8-45db-92a7-bfeeaabd8dd6', lineage).
narrative_ontology:cs_interpretation_layer_present('aaecb473-0eb8-45db-92a7-bfeeaabd8dd6').
narrative_ontology:cs_reading_relation('aaecb473-0eb8-45db-92a7-bfeeaabd8dd6', procedural_due_process__goldberg_hearing_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('aaecb473-0eb8-45db-92a7-bfeeaabd8dd6', procedural_due_process__new_property_reading, influences).
narrative_ontology:cs_axiom('aaecb473-0eb8-45db-92a7-bfeeaabd8dd6', foundational, due_process_is_optimization).
narrative_ontology:cs_axiom_status(due_process_is_optimization, holdable).
narrative_ontology:cs_axiom_grounding('aaecb473-0eb8-45db-92a7-bfeeaabd8dd6', due_process_is_optimization, deontological).
narrative_ontology:cs_axiom('aaecb473-0eb8-45db-92a7-bfeeaabd8dd6', foundational, process_scales_to_stakes_and_burden).
narrative_ontology:cs_axiom_status(process_scales_to_stakes_and_burden, holdable).
narrative_ontology:cs_axiom_grounding('aaecb473-0eb8-45db-92a7-bfeeaabd8dd6', process_scales_to_stakes_and_burden, instrumental).
narrative_ontology:cs_reference_frame('aaecb473-0eb8-45db-92a7-bfeeaabd8dd6', mathews_balancing_formula).
narrative_ontology:cs_drift_state('aaecb473-0eb8-45db-92a7-bfeeaabd8dd6', contemporary_circuits_and_statutes, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('aaecb473-0eb8-45db-92a7-bfeeaabd8dd6', '').
narrative_ontology:cs_kernel_id(procedural_due_process__mathews_balancing_reading, procedural_due_process).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(procedural_due_process__mathews_balancing_reading, administrative_agencies).
narrative_ontology:constraint_beneficiary(procedural_due_process__mathews_balancing_reading, government_budget_interests).
narrative_ontology:constraint_victim(procedural_due_process__mathews_balancing_reading, categorical_hearing_claimants).
narrative_ontology:constraint_victim(procedural_due_process__mathews_balancing_reading, low_stakes_benefit_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CATEGORICAL HEARING CLAIMANT (SNARE) — A person challenging termination of a government benefit (welfare, public employment, professional license) faces a Mathews-optimized process designed to minimize government burden rather than ensure accuracy. The optimization explicitly discounts their interest against administrative convenience. No meaningful exit: the benefit is typically their sole livelihood support. Trapped agent experiencing maximum extraction — the process calculus embeds their powerlessness as a weight in the optimization.
constraint_indexing:constraint_classification(procedural_due_process__mathews_balancing_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MODERATELY RESOURCED CLAIMANT (TANGLED ROPE) — A claimant with some financial cushion or alternative income can absorb process delay and cost. The Mathews test provides minimal process (written notice, opportunity to respond, post-termination hearing). These procedural safeguards do coordinate some verification of error, but the balancing explicitly discounts their utility against government burden. Mixed extraction: some coordination function (hearing occurs), but asymmetric — the claimant bears the cost of delay while the agency extracts budget savings.
constraint_indexing:constraint_classification(procedural_due_process__mathews_balancing_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ADMINISTRATIVE AGENCY (ROPE) — The agency experiences the Mathews framework as pure coordination. The balancing test provides a decision procedure for sizing process: how much hearing is required before terminating this benefit? The arithmetic is clear and administrable. The agency can optimize hearing depth to budget constraints. Net beneficiary from the constraint — it gains administrative flexibility and budgetary control while the constitutional requirement for 'due process' is satisfied through the optimization formula. Extractiveness runs toward the agency.
constraint_indexing:constraint_classification(procedural_due_process__mathews_balancing_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GOVERNMENT FISCAL INTEREST (ROPE) — At the fiscal-management level, the Mathews balancing coordinates budget allocation and process cost. Welfare agencies can calculate the expected hearing cost against the benefit amount and size the pre-termination hearing accordingly. High-value benefits receive richer process; low-value benefits receive minimal process. This is a coordination mechanism for allocating finite administrative resources. The constraint solves the collective action problem of process sizing across heterogeneous benefit programs.
constraint_indexing:constraint_classification(procedural_due_process__mathews_balancing_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DUE PROCESS BAR AND ADVOCACY (TANGLED ROPE) — Organized advocates (legal aid, public defenders, civil rights groups) experience Mathews as constraining and extractive: it legitimizes process-shaving in low-stakes cases (small benefits, minor professional penalties) that would have received fuller process under the categorical 'welfare rights' reading (Goldberg). However, the Mathews framework also enables predictable litigation strategy — advocates can litigate to shift the balance (increase the weight on claimant interest or error risk), creating organized leverage. Some advocacy benefit (access to balancing arithmetic), but asymmetric extraction (low-stakes claimants lose categorical protection).
constraint_indexing:constraint_classification(procedural_due_process__mathews_balancing_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONGRESSIONAL LEGISLATOR (SCAFFOLD) — Congress retains the power to override Mathews through statute: set mandatory hearing requirements, define due process minimums for specific benefits, allocate dedicated hearing resources. Legislators see Mathews as a default that they can displace through legislation. This perspective experiences the constraint as having a sunset or override mechanism — the balancing is a judicial holding, not immutable. Effective extraction is dampened by the legislator's mobile exit (write a statute). The constraint has a built-in escape hatch for organized political actors.
constraint_indexing:constraint_classification(procedural_due_process__mathews_balancing_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational view, Mathews instantiates a specific reading of procedural due process: that the constitution requires an optimization, not a fixed form. The constraint coordinates the sizing of process to case-specific stakes. It also extracts from low-stakes claimants (suppresses their categorical right to hearing). The constraint is not a mountain (not immutable natural law — it is a doctrinal choice) and not pure rope (not coordination without asymmetry — the balancing is designed to favor administrative interests). Tangled rope at the analytical/civilizational level: genuine coordination function (size process to stakes) plus asymmetric extraction (discount powerless-agent interests in the balance).
constraint_indexing:constraint_classification(procedural_due_process__mathews_balancing_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(procedural_due_process__mathews_balancing_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(procedural_due_process__mathews_balancing_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(procedural_due_process__mathews_balancing_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(procedural_due_process__mathews_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(procedural_due_process__mathews_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Mathews balancing explicitly optimizes process size to minimize government burden while maintaining threshold constitutionality. The optimization is asymmetric — it discounts the interests of low-stakes claimants. The extraction is not total (some process is provided; categorical hearing is not abolished) but substantial (many claimants receive less process than categorical approach would provide). The value reflects that the extraction is structurally embedded in the test itself, not contingent on agency misapplication. Suppression (0.65): Moderate-high. The Mathews framework suppresses the categorical hearing right that Goldberg appeared to establish. Claimants in low-stakes cases lose meaningful pre-termination hearing opportunity. The suppression is accomplished through doctrinal redefinition (reframing Goldberg's categorical right as one factor in a balance, not a floor). The mechanism is legalized procedure, not crude prohibition — hence 0.65 rather than 0.85. Theater ratio (0.58): Moderate. The Mathews test produces written decisions explaining the balancing, post-termination hearings, and a veneer of constitutional process. These are not purely performative — they do provide some verification of error. However, in low-stakes cases, the pre-termination process (written notice, opportunity to submit evidence) is minimal, and post-termination hearing occurs after the claimant has been deprived (high cost to claimant). The theater consists partly in the framing of this as 'due process' when the claimant's property interest was already extinguished.
 *
 * PERSPECTIVAL GAP:
 *   The Mathews reading generates a sharp perspectival gap. The agency sees rope (coordination mechanism for sizing process). The powerless claimant sees snare (extraction under constitutional cover). The advocacy community sees tangled rope (some coordination, but asymmetric cost allocation). The legislative actor sees scaffold (an override-able default). The analytical observer recognizes that Mathews instantiates a specific reading of the due process kernel — not an immutable natural law, but a doctrinal choice that benefits administrative interests by reframing the constitution as requiring optimization rather than categorical rights. The gap between Mathews and Goldberg readings is the gap between optimization and categorical boundaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The Mathews balancing explicitly asymmetries the structural relationship. Agencies are beneficiaries (arbitrage exit: they can budget process and reallocate resources). Categorical claimants are victims (trapped exit: they depend on the benefit and cannot opt out of Mathews minimization). The directionality is embedded in the test formula itself — discounting claimant interest against government burden. The analytical observer (d ≈ 0.72) sees the full structure: the reading chooses to weight government interests heavily, which is a legitimate constitutional interpretation, but it is a choice that extracts from powerless agents and benefits institutional actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mathews reading resolves the due process mandatrophy by accepting the trade-off between procedure and burden: not all claims merit full hearings before termination. This is a defensible reading of the constitutional text. The Goldberg reading resolves the same mandatrophy by prioritizing the 'brutal need' of the welfare recipient: the constitutional text requires process befitting the stakes, and welfare is categorical high-stakes. These are incommensurable resolutions — they rest on different axioms about what due process requires. The DR framework does not adjudicate between them; it maps the structural consequences. Mathews extracts from low-stakes claimants and benefits administrative actors. Goldberg protects all claimants categorically but imposes higher administrative burden. The mandatrophy is not resolved by choosing the 'correct' reading; it is resolved by mapping how each reading distributes extraction and coordination across the agent set. The Mathews reading is not incorrect — it is a legitimate doctrinal choice with specific distributional consequences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    error_risk_measurement_ambiguity,
    'How is ''probable value of accurate determination'' (error risk component of Mathews) operationalized in practice? Does judicial application of the test track genuine error probability or proxy institutional preferences?',
    'Empirical analysis of judicial Mathews applications: correlation between stated error-risk weight and actual error rates in post-termination hearings; comparison across benefit types and judicial circuits',
    'If error risk is genuine empirical assessment: Mathews balancing reflects true stakes optimization. If error risk is proxied to institutional preferences (agencies overstate error risk for high-value benefits, understate it for low-value): the balancing is disguised extraction, extractiveness rises to 0.68+, classification shifts toward pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(error_risk_measurement_ambiguity, empirical, 'Whether error-risk measurement reflects genuine probability or institutional preference').

omega_variable(
    counterfactual_hearing_denial_rate,
    'Under a Goldberg categorical-hearing regime (alternative reading), what fraction of current Mathews-minimized processes would receive fuller hearings? What is the suppression delta between readings?',
    'Counterfactual analysis: regression discontinuity at benefit-amount thresholds; comparison with jurisdictions that apply categorical hearing requirements; simulation of Goldberg regime applied to current caseloads',
    'If delta is large (>40% of cases receive thinner process under Mathews): suppression is 0.65+ and the victim set is large. If delta is small (<15%): Mathews aligns closely with Goldberg practice, extractiveness is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_hearing_denial_rate, empirical, 'Suppression gap between Mathews balancing and categorical hearing regime').

omega_variable(
    axiom_contestation_over_procedural_form,
    'Is the Mathews balancing reading''s foundational axiom (procedures should optimize to stakes) logically compatible with the Goldberg reading''s axiom (welfare is a stake worth uniform process) within a single constitutional framework?',
    'Doctrinal analysis: can a single interpretation of the Fifth/Fourteenth Amendment accommodate both axioms, or do they instantiate incompatible normative theories (process optimization vs. categorical rights)?',
    'If compatible: readings coexist_with (different courts hold different axioms). If incompatible: one reading forecloses the other (framework-level conflict). Determines the reading_relations entry in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_contestation_over_procedural_form, conceptual, 'Logical compatibility of Mathews and Goldberg axioms in single constitutional framework').

omega_variable(
    kernel_reading_identity,
    'Is this constraint ''the Mathews balancing reading of procedural due process'' or is it ''one operational instantiation of Mathews that gets overridden by statute or circuit splits''?',
    'Doctrinal genealogy: trace which constitutional text and authority grounds the Mathews holding; assess whether Mathews articulates a binding principle or a default rebuttable presumption',
    'If Mathews is a binding principle: this reading is a stable constraint with high authority_grounding score. If Mathews is a presumption: the reference_frame and drift_state should reflect its contestability and legislative override capacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Constitutional status of Mathews balancing test as binding principle or rebuttable default').

omega_variable(
    administrative_burden_weight_calibration,
    'In the Mathews arithmetic, how is ''probable value to the claimant of additional or substitute procedural safeguards'' weighted against ''burden on the government''? Is the weighting symmetric or systematically discounted?',
    'Systematic review of Mathews applications across benefit types: does a $100 benefit receive 1/10 the process of a $1,000 benefit, or is process depth decoupled from benefit size? Evidence of systematic under-weighting of low-value claimant interests.',
    'If weighting is symmetric: balancing is genuine optimization. If weighting systematically discounts powerless-agent interests: extractiveness rises, classification shifts from Tangled Rope toward Snare, suppression becomes 0.72+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_burden_weight_calibration, empirical, 'Whether Mathews weighting of claimant vs. government interests is symmetric or systematically discounted').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(procedural_due_process__mathews_balancing_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mathews_theater_1970_adoption, procedural_due_process__mathews_balancing_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(mathews_theater_1985_consolidation, procedural_due_process__mathews_balancing_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(mathews_theater_2000_refinement, procedural_due_process__mathews_balancing_reading, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(mathews_extractiveness_1970_adoption, procedural_due_process__mathews_balancing_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mathews_extractiveness_1985_consolidation, procedural_due_process__mathews_balancing_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(mathews_extractiveness_2000_refinement, procedural_due_process__mathews_balancing_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mathews_suppression_1970_adoption, procedural_due_process__mathews_balancing_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(mathews_suppression_1985_consolidation, procedural_due_process__mathews_balancing_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(mathews_suppression_2000_refinement, procedural_due_process__mathews_balancing_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(procedural_due_process__mathews_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(procedural_due_process__mathews_balancing_reading, procedural_due_process__goldberg_hearing_rights_reading).
narrative_ontology:affects_constraint(procedural_due_process__mathews_balancing_reading, procedural_due_process__new_property_reading).

% DUAL FORMULATION NOTE:
% The procedural_due_process kernel generates three distinct constraint stories corresponding to three readings: goldberg_hearing_rights_reading (categorical welfare protection), mathews_balancing_reading (optimization framing), and new_property_reading (government largesse as property). Each reading produces different extractiveness values, victim sets, and beneficiary distributions. Mathews influences both siblings: it limits the scope of Goldberg protection (low-stakes cases) and constrains the procedure even for Reich-protected property (balancing applies). Goldberg forecloses Mathews if welfare is categorical property requiring categorical process. New property coexists with Mathews: once property status is established, Mathews determines what process is due. These stories are linked by shared kernel but distinct readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
