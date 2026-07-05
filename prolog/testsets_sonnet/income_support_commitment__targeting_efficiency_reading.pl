% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__targeting_efficiency_reading, []).

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
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Targeted-Benefit Preservation Against Universal Basic Income Replacement
 *   domain: political economy / social policy / welfare state theory
 *
 * SUMMARY:
 *   This story instantiates the targeting_efficiency_reading of the
 *   income_support_commitment kernel: the claim that income support should be
 *   concentrated on demonstrated categorical need rather than distributed
 *   universally. The reading's structural core is a specific arithmetic
 *   example — a Queens parent qualifying for stacked categorical programs
 *   currently receives approximately $31,100 in combined benefits; under a
 *   UBI-replacement scheme funded by cannibalizing those same programs, the
 *   modeled household's transfer falls to roughly $19,100. The reading treats
 *   this delta as the decisive fact: the same population is both the nominal
 *   beneficiary of universalization rhetoric and the actual victim of
 *   universalization's funding mechanics, because a flat-rate scheme funded
 *   from the existing targeted-program pool necessarily redistributes away
 *   from the highest-need stacked-eligibility households toward the median
 *   household. This is evaluated here as a single, ε-invariant constraint —
 *   the targeting commitment as it currently operates and as it would be
 *   displaced by a specific rival funding mechanism. The sibling readings
 *   (freedom_floor_reading, dependency_trap_reading) are different
 *   constraints with different ε values and are not described further here;
 *   see cs_structure and the omegas for the committer structure.
 *
 * KEY AGENTS:
 *   - current_targeted_program_recipients: primary beneficiary of the status quo AND primary named victim of the rival reform (powerless/trapped) — bears the extraction under a funding-cannibalization frame
 *   - means_testing_administrative_apparatus: agenda_setter (institutional/arbitrage) — designs, defends, and staffs the eligibility verification infrastructure this reading protects
 *   - categorical_eligibility_advocacy_groups: beneficiary (organized/mobile) — political capital invested in preserving category-specific premiums
 *   - universal_benefit_proponents: excluded (organized/constrained) — advance a rival reform this reading frames as extractive toward the population it claims to help
 *   - non_categorical_working_poor: payer (powerless/trapped) in this reading's accounting — would gain under universalization but that gain is treated as incidental, not central, to this reading's claim
 *   - general_taxpayers: observer (organized/constrained) — funds whichever system prevails, largely indifferent to the intra-poor distributional fight
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.71).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.58).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Targeted-Benefit Preservation Against Universal Basic Income Replacement").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political economy / social policy / welfare state theory").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, 'df4c6247-048e-4557-bf2c-e73da05ca2be').
narrative_ontology:cs_kernel_codification('df4c6247-048e-4557-bf2c-e73da05ca2be', distributed).
narrative_ontology:cs_authority_grounding('df4c6247-048e-4557-bf2c-e73da05ca2be', distributed).
narrative_ontology:cs_reading_relation('df4c6247-048e-4557-bf2c-e73da05ca2be', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('df4c6247-048e-4557-bf2c-e73da05ca2be', income_support_commitment__dependency_trap_reading, influences).
narrative_ontology:cs_axiom('df4c6247-048e-4557-bf2c-e73da05ca2be', foundational, need_verification_improves_allocative_precision).
narrative_ontology:cs_axiom_status(need_verification_improves_allocative_precision, holdable).
narrative_ontology:cs_axiom_grounding('df4c6247-048e-4557-bf2c-e73da05ca2be', need_verification_improves_allocative_precision, empirically_contingent).
narrative_ontology:cs_axiom('df4c6247-048e-4557-bf2c-e73da05ca2be', secondary, fixed_transfer_pools_create_zero_sum_tradeoffs_across_recipient_classes).
narrative_ontology:cs_axiom_status(fixed_transfer_pools_create_zero_sum_tradeoffs_across_recipient_classes, holdable).
narrative_ontology:cs_axiom_grounding('df4c6247-048e-4557-bf2c-e73da05ca2be', fixed_transfer_pools_create_zero_sum_tradeoffs_across_recipient_classes, empirically_contingent).
narrative_ontology:cs_reference_frame('df4c6247-048e-4557-bf2c-e73da05ca2be', categorical_means_tested_welfare_state).
narrative_ontology:cs_drift_state('df4c6247-048e-4557-bf2c-e73da05ca2be', contemporary_ubi_pilot_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('df4c6247-048e-4557-bf2c-e73da05ca2be', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, means_testing_administrative_apparatus).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, categorical_eligibility_advocacy_groups).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, universal_benefit_proponents).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, non_categorical_working_poor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A parent qualifying for stacked categorical programs (housing, SNAP, Medicaid, TANF, childcare subsidy) can receive a combined package — the story's example totals $31,100 — that exceeds what a flat universal transfer would deliver. Under a UBI-replacement scheme funded by cannibalizing these programs, the same household's modeled benefit falls to roughly $19,100. They are named as both the class the current system protects and the class a rival reform would extract from — the same people occupy the beneficiary seat under targeting and the victim seat under universalization, which is the structural fact this reading exists to assert.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients, payer).

% Caseworkers, eligibility-verification contractors, and program administrators design and enforce the categorical tests that route benefits to demonstrated need. Their institutional continuity depends on need being measured, verified, and re-verified rather than distributed by flat formula; they set and defend the eligibility thresholds and staff the appeals process.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, means_testing_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Disability-rights, single-parent, and disease-specific advocacy organizations built decades of political capital around category-specific carve-outs (higher benefit levels for their constituency than a flat UBI would deliver). They lobby to preserve targeting because their constituency's categorical premium disappears under a universal flat rate.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, categorical_eligibility_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Economists and reform coalitions arguing for UBI point to administrative savings, elimination of benefit cliffs, and universality's political durability. In this reading's frame, their proposal is treated as extractive because it would fund universality by clawing back the categorical premium from exactly the households targeting currently protects — they are excluded from this reading's beneficiary calculus even though they claim to represent the poor's interest.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, universal_benefit_proponents, excluded,
    organized, civilizational, constrained, national).

% Low-wage workers who fall just outside categorical eligibility lines (no qualifying disability, no dependent children, income slightly above thresholds) receive little or nothing from the current targeted system. They would gain under a flat universal transfer, but this reading's stakeholder set treats their gain as incidental to the story's central claim: that reallocating from category-holders to non-category-holders is redistribution among the poor dressed as reform, not a solved coordination problem.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, non_categorical_working_poor, payer,
    powerless, biographical, trapped, national).

% Fund whichever system prevails through general taxation. They are largely indifferent to the targeting-versus-universal fight except insofar as administrative overhead or benefit-cliff labor distortions affect aggregate cost; they watch the fight over how the transfer pool is sliced rather than participating directly in it.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, general_taxpayers, observer,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__targeting_efficiency_reading, means_testing_administrative_apparatus).
narrative_ontology:fixing_cost_class(income_support_commitment__targeting_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Categorical targeting solves a real allocation problem: fixed transfer budgets go further per unit of need when concentrated on households verified to lack housing, food, healthcare access, or childcare, rather than spread as a flat per-capita payment that gives the same dollar amount to a household with zero need as to one with maximal need.
% TRANSFER_FUNCTION: Moves pooled tax revenue toward households that clear categorical eligibility tests (disability status, dependent children, income floor, asset limits), and moves political and administrative resources toward maintaining and verifying those tests. Under the rival UBI-replacement proposal this reading evaluates, the same funding pool would be flattened and redistributed away from stacked categorical recipients toward all citizens equally — the transfer this reading names as extractive.
% ABSENT_VOICES: Non-categorical working poor who fall outside eligibility lines are structurally quiet in the targeting debate — they neither built the categorical infrastructure nor organized advocacy around it, so their gain under flattening is discounted in this reading's frame. Universal benefit proponents are present in the discourse but treated here as advancing the interests of a different constituency than the one currently served.
% DISAPPEARANCE_RATIONALE: If categorical targeting disappeared overnight and were replaced by a flat universal transfer funded from the same pool, the modeled household loses roughly $19,100 relative to its $31,100 categorical package; caseworker and eligibility-verification institutions would lose their core function; advocacy groups organized around categorical premiums would lose their reason for existing in current form; non-categorical working poor would gain. The arrangement is deeply load-bearing for the households and institutions currently inside it.
% FOUNDING_PROBLEM: Fixed antipoverty budgets needed to be allocated where deprivation was demonstrated rather than spread evenly, on the premise that a flat payment under-serves severe need and over-serves those without need.
% FOUNDING_PROBLEM_CORROBORATION: Means-testing administrators and categorical advocacy groups attest the founding problem remains live — need is heterogeneous and flat distribution wastes scarce transfer dollars on the non-needy. UBI-oriented economists and administrative-cost researchers, outside the beneficiary set, attest that the founding problem has been substantially answered by simplification research showing benefit-cliff and verification-overhead costs now exceed the targeting precision gained, and that persistence of categorical structures increasingly reflects the political durability of category-specific constituencies rather than allocative necessity.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__targeting_efficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__targeting_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 at interval end) because the reading's central claim is that a specific, named funding mechanism — replacing stacked categorical benefits with a flat universal transfer drawn from the same pool — extracts roughly $12,000/year from the modeled high-need household to fund payments to households with less demonstrated need, including some with none. Suppression (0.58) reflects the political and administrative machinery required to keep this framing dominant: eligibility-verification infrastructure, categorical advocacy lobbying, and the discursive work of characterizing universalization as "cannibalization" rather than simplification. Theater ratio (0.44, rising) reflects that an increasing share of administrative and advocacy energy over the measured interval is directed at defending category boundaries against reform pressure rather than at verifying need per se — a Goodhart-style drift where boundary-defense increasingly substitutes for the original allocative function. Accessibility collapse is moderate (0.52): rival funding architectures (negative income tax hybrids, categorical-plus-floor designs) remain visible and debated, so alternatives have not fully collapsed. Resistance is substantial (0.61) because universal benefit proponents, non-categorical working poor, and administrative-simplification researchers actively contest the targeting framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Current targeted-program recipients carry the story's central directionality tension: their beneficiary role under the status quo (d near the beneficiary end, given the $31,100 package) inverts to victim role under the modeled rival scheme (d near the target end, given the $19,100 modeled outcome). This dual-seat structure is authored deliberately via the secondary_role field rather than smoothed into a single d value — the reading's entire claim is that this population is simultaneously the status quo's protected class and the reform's extraction target. Means-testing administrators and categorical advocacy groups sit at the clear beneficiary end (institutional continuity, political capital) with mobile-to-arbitrage exit. Universal benefit proponents and non-categorical working poor sit at the target end within this reading's frame, though the reading acknowledges (via omega) that this framing itself is contested.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fixed budgets needing allocation toward demonstrated need rather than flat distribution — is authored as contested rather than flatly dead or flatly live. Means-testing administrators and categorical advocates attest it remains live (heterogeneous need still exists); simplification researchers outside the beneficiary set attest the administrative and benefit-cliff costs of maintaining categorical verification now rival or exceed the precision gained, suggesting the mandate may have drifted from allocative necessity toward institutional and political self-preservation. The disappearance_verdict of world_rearranges combined with a contested founding_problem_status is exactly the mismatch pattern this framework's mandatrophy consumer is built to flag: an arrangement whose defenders say it still solves its founding problem and whose critics, situated outside the benefiting seats, say the problem has been substantially answered by cheaper mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    funding_mechanism_zero_sum_assumption,
    'Is the $31,100-to-$19,100 delta an inherent feature of universalizing income support, or an artifact of the specific assumption that UBI must be funded by cannibalizing existing targeted-program budgets rather than by new revenue?',
    'Compare fiscal designs: UBI-replacing-targeting (this reading''s assumption) versus UBI-as-supplement funded by new progressive taxation, financial transaction taxes, or land-value taxation, holding the modeled household''s baseline package constant across designs.',
    'If the zero-sum framing is not load-bearing — if alternative funding architectures preserve or exceed the $31,100 baseline for high-need households while extending a floor to non-categorical households — then this reading''s core extractive claim depends on a specific (contestable) fiscal design choice rather than on universalization per se, which would substantially weaken the snare classification and shift the constraint toward tangled_rope or rope depending on the funding source chosen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_mechanism_zero_sum_assumption, conceptual, 'Whether the extraction this reading names is inherent to universal income support or an artifact of the cannibalization funding assumption.').

omega_variable(
    categorical_verification_cost_offset,
    'Do the administrative costs of maintaining categorical verification (caseworker infrastructure, eligibility audits, benefit-cliff-induced labor market distortions) offset or exceed the allocative precision gained by targeting, such that a simplified universal design could deliver comparable net benefit to high-need households after administrative savings are redistributed?',
    'Comprehensive administrative-cost accounting comparing total system cost (transfers plus verification plus compliance plus foregone labor income from benefit cliffs) under current targeting versus a flat universal design funded at equivalent total cost.',
    'If administrative savings from simplification are large enough, the modeled $19,100 outcome under a naively-designed UBI-replacement may understate what a well-designed universal system could deliver, undermining the reading''s central arithmetic; if administrative costs are a small fraction of the transfer pool, the reading''s arithmetic holds largely as stated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_verification_cost_offset, empirical, 'Whether verification overhead in the current system is large enough to change the net comparison the reading relies on.').

omega_variable(
    kernel_framing_which_population_is_the_reference_class,
    'This reading treats current targeted-program recipients as the reference population whose loss defines extraction. The freedom_floor_reading and dependency_trap_reading instead center different populations (autonomy-seeking exit-capacity holders; work-disincentivized categorical dependents). Is the choice of reference population itself a neutral structural fact, or a framing choice that predetermines which reading looks extractive?',
    'Cross-reading comparison: hold the same underlying policy proposal constant and vary only which population''s outcome is treated as the primary evaluative metric (stacked-recipient loss vs. non-categorical-poor gain vs. labor-supply effect); observe whether ε changes purely as a function of reference-population choice.',
    'If ε is sensitive primarily to reference-population choice rather than to any structural fact about the transfer mechanism itself, this signals that the three sibling readings are not simply different empirical bets but partly constituted by which population each reading elects to center — a finding relevant to how the kernel contest itself should be adjudicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_which_population_is_the_reference_class, conceptual, 'Whether this reading''s high-ε verdict depends on centering current recipients as the reference class rather than on an ε-invariant structural fact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__targeting_efficiency_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(inco_tr_t4, income_support_commitment__targeting_efficiency_reading, theater_ratio, 4, 0.31).
narrative_ontology:measurement(inco_tr_t8, income_support_commitment__targeting_efficiency_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(inco_tr_t12, income_support_commitment__targeting_efficiency_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(inco_tr_t16, income_support_commitment__targeting_efficiency_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__targeting_efficiency_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(inco_tr_t24, income_support_commitment__targeting_efficiency_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(inco_be_t4, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(inco_be_t8, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(inco_be_t12, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(inco_be_t16, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(inco_be_t24, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 24, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(inco_su_t4, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 4, 0.49).
narrative_ontology:measurement(inco_su_t8, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(inco_su_t12, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(inco_su_t16, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(inco_su_t24, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__targeting_efficiency_reading, 0.12).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the income_support_commitment kernel, linked via network edges to the other two readings (freedom_floor_reading, dependency_trap_reading). Each reading has its own ε, its own beneficiary/victim structure, and its own claimed type — they are not measurement-parameter variants of one constraint but structurally distinct constraints that share a contested kernel. This reading's ε (0.71, snare-leaning) is substantially higher than what a freedom-floor reading would likely author (low ε, rope-leaning, given that reading centers autonomy gained rather than categorical-premium loss) and differs in kind from a dependency-trap reading's likely focus on labor-supply distortion rather than distributional zero-sum arithmetic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
