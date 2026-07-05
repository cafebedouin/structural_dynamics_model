% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__dependency_trap_reading, []).

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
 *   constraint_id: unconditional_income_support__dependency_trap_reading
 *   human_readable: Universal Basic Income as Incentive-Distorting Upward Redistribution (Dependency Trap Reading)
 *   domain: political_economy/social_policy/welfare_state
 *
 * SUMMARY:
 *   This story instantiates the dependency-trap reading of the
 *   unconditional-income-support kernel: unconditional transfers presented as
 *   a universal floor function structurally as an incentive-distorting
 *   subsidy that redistributes upward. Under this reading, the coordination
 *   story (removing stigma and administrative friction) is real but secondary
 *   to the extraction pattern — flat transfers reach non-needy households at
 *   full value while targeted programs delivering higher value to the working
 *   poor are cut or capped to fund universality, and aggregate labor supply
 *   falls (AEI meta-analysis: -3.2% employment in large pilots). This is a
 *   distinct constraint from the freedom_floor_reading (autonomy-enabling
 *   floor, same kernel, opposite ε and type) and the
 *   universality_paradox_reading (structural ambiguity across implementation
 *   paths). Per the ε-invariance principle, these are three separate
 *   constraint files sharing a kernel, not one constraint measured three
 *   ways.
 *
 * KEY AGENTS:
 *   - middle_upper_class_recipients: beneficiary (moderate/mobile) — collects transfer without need
 *   - ubi_advocacy_coalitions: beneficiary (organized/arbitrage) — collects political capital from universality
 *   - working_poor_former_program_recipients: primary victim (powerless/trapped) — loses higher-value targeted aid
 *   - general_taxpayers: victim (powerless/constrained) — bears net fiscal cost
 *   - program_administering_agencies: agenda_setter (institutional) — designs substitution tradeoff
 *   - labor_market_researchers: analytical observer — measures employment and substitution effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.71).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.42).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Universal Basic Income as Incentive-Distorting Upward Redistribution (Dependency Trap Reading)").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/social_policy/welfare_state").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, 'b0432012-d2eb-4109-bd6b-b4225a092f4f').
narrative_ontology:cs_kernel_codification('b0432012-d2eb-4109-bd6b-b4225a092f4f', distributed).
narrative_ontology:cs_authority_grounding('b0432012-d2eb-4109-bd6b-b4225a092f4f', distributed).
narrative_ontology:cs_reading_relation('b0432012-d2eb-4109-bd6b-b4225a092f4f', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0432012-d2eb-4109-bd6b-b4225a092f4f', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('b0432012-d2eb-4109-bd6b-b4225a092f4f', foundational, targeting_by_need_outperforms_universal_flat_transfer).
narrative_ontology:cs_axiom_status(targeting_by_need_outperforms_universal_flat_transfer, holdable).
narrative_ontology:cs_axiom_grounding('b0432012-d2eb-4109-bd6b-b4225a092f4f', targeting_by_need_outperforms_universal_flat_transfer, empirically_contingent).
narrative_ontology:cs_axiom('b0432012-d2eb-4109-bd6b-b4225a092f4f', secondary, unconditional_transfers_measurably_reduce_labor_supply).
narrative_ontology:cs_axiom_status(unconditional_transfers_measurably_reduce_labor_supply, holdable).
narrative_ontology:cs_axiom_grounding('b0432012-d2eb-4109-bd6b-b4225a092f4f', unconditional_transfers_measurably_reduce_labor_supply, empirically_contingent).
narrative_ontology:cs_reference_frame('b0432012-d2eb-4109-bd6b-b4225a092f4f', means_tested_targeted_welfare_baseline).
narrative_ontology:cs_drift_state('b0432012-d2eb-4109-bd6b-b4225a092f4f', post_pilot_evaluation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b0432012-d2eb-4109-bd6b-b4225a092f4f', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocacy_coalitions).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor_former_program_recipients).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, general_taxpayers).
narrative_ontology:constraint_vindicates(unconditional_income_support__dependency_trap_reading, universality_reduces_stigma_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive the flat unconditional transfer regardless of need, on top of existing earnings and assets. The check is not means-tested, so households with substantial income or wealth collect the same nominal amount as households in poverty. Their situation is unchanged by the transfer's existence or removal in any material way — it is supplemental income layered onto an already-adequate position.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients, beneficiary,
    moderate, biographical, mobile, national).

% Political and philanthropic organizations that built institutional capital, think-tank funding, and pilot-program leadership around promoting universal, unconditional transfers. Their organizational survival and prestige are tied to universality as a design principle rather than to measured outcomes for the poor; they collect political and reputational returns independent of whether the program net-benefits the population it claims to serve.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocacy_coalitions, beneficiary,
    organized, generational, arbitrage, national).

% Previously received targeted, means-tested benefits (housing vouchers, SNAP, EITC, childcare subsidies) whose combined value substantially exceeded the flat UBI amount for their specific circumstances. When the flat transfer replaces or is offset against targeted programs, they experience a net income loss despite being nominally 'included' in a universal program. They have no mechanism to opt back into the higher-value targeted benefit once program budgets are reallocated to fund universality.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor_former_program_recipients, payer,
    powerless, immediate, trapped, national).

% Bear the net fiscal cost after offsetting eliminated programs — estimated near $1.4 trillion annually in the AEI meta-analysis framing — through taxation or debt-financed deficits. Individual taxpayers have no practical exit from the tax obligation and no direct voice in the specific design tradeoffs between universality and targeting; their cost is diffuse and largely invisible relative to the concentrated, visible benefit to recipients.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, general_taxpayers, payer,
    powerless, generational, constrained, national).

% Legislatures and executive agencies design the transfer formula, decide whether it supplements or substitutes for targeted programs, and administer disbursement. They face pressure from UBI coalitions to maximize universality (political simplicity, broad constituency) and from fiscal watchdogs to control cost, and they resolve this tension by funding universality partly through targeted-program cuts.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, program_administering_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% Conduct and interpret pilot studies and meta-analyses (including the referenced AEI meta-analysis showing a -3.2% employment effect in large pilots) measuring labor-supply response, program substitution effects, and net distributional outcomes. Their findings are contested by both advocacy coalitions and critics, and are cited selectively by each side.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, labor_market_researchers, observer,
    analytical, generational, analytical, national).

% Administer means-tested programs being defunded or restructured to pay for universal transfers. They observe firsthand which former clients lose net income under the new regime but are not central parties to the political debate over program design; their operational knowledge of program-substitution harm rarely reaches the legislative record.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, targeted_program_case_workers, excluded,
    moderate, immediate, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients).
narrative_ontology:fixing_cost_class(unconditional_income_support__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces the administrative overhead and stigma cost of proving need for every recipient, and provides a income floor without a bureaucratic eligibility gate — in principle solving the real problem of means-testing complexity and benefit cliffs that discourage marginal work.
% TRANSFER_FUNCTION: Moves general tax revenue (and reallocated targeted-program budgets) to every resident regardless of need, with a large share landing on middle- and upper-income households who do not require it, while the working poor lose access to higher-value targeted benefits that are cut or capped to fund the universal transfer.
% ABSENT_VOICES: Case workers administering the targeted programs being defunded, and the specific working-poor households who lose net income under substitution, are rarely central witnesses in the political debate, which is dominated by advocacy coalitions and fiscal think tanks arguing from aggregate projections rather than household-level substitution effects.
% DISAPPEARANCE_RATIONALE: If the unconditional transfer were withdrawn and targeted programs restored to their prior funding and eligibility rules, the working poor would regain higher-value means-tested benefits, taxpayers would see reduced net fiscal burden from the program-replacement effect, and the political coalitions built around universality would lose their central organizing cause — the labor-supply and program-substitution effects documented in pilots would reverse.
% FOUNDING_PROBLEM: Means-tested welfare systems impose stigma, high marginal effective tax rates near benefit cliffs, and costly administrative gatekeeping that traps recipients in poverty and discourages work at the margin.
% FOUNDING_PROBLEM_CORROBORATION: UBI advocacy coalitions attest the founding problem (stigma and benefit-cliff disincentives) remains fully live and justifies universality. Independent labor economists conducting the AEI-cited meta-analysis and other pilot evaluations attest that the -3.2% employment effect and net program-substitution losses for the working poor are measured outcomes independent of advocacy framing, and that the reform as implemented does not resolve the founding problem for its intended beneficiaries — it primarily benefits non-needy recipients and advocacy organizations.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__dependency_trap_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unconditional_income_support__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) and rising over the measurement interval because the program-substitution mechanism (targeted aid cut to fund universal transfer) compounds as budget reallocation matures — early years show partial substitution, later years show full crowd-out as targeted-program infrastructure is dismantled and not restorable. Suppression is moderate (0.42) rather than extreme: there is no coercive barrier preventing working-poor households from voicing loss, but the political framing of universality as inherently progressive makes the substitution harm difficult to surface and contest within mainstream advocacy discourse — this is closer to narrative suppression than legal suppression. Theater ratio rises over time (0.20 to 0.38) as the program's public justification increasingly emphasizes universality/dignity messaging even as the underlying labor-market and distributional evidence accumulates against the stated anti-poverty rationale.
 *
 * DIRECTIONALITY LOGIC:
 *   Middle/upper-class recipients and UBI advocacy coalitions are coded as beneficiaries with low derived directionality — mobile/arbitrage exit options and no structural need for the transfer mean the constraint operates as a pure subsidy for them. Working poor former program recipients and general taxpayers are coded as victims with high derived directionality — trapped/constrained exit and demonstrable net income loss (working poor) or diffuse unrecoverable cost (taxpayers) place them near the full-target end. Program administering agencies sit as agenda_setter: institutional actors who could redesign the substitution mechanism but face political incentives (coalition pressure, simplicity of universal design) not to.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stigma and benefit-cliff disincentives in means-tested welfare) is real but contested-live: it exists but the unconditional-transfer solution as implemented does not target it — it solves stigma for non-needy recipients who never needed the transfer and imposes it on the working poor via crowd-out. The founding_problem_status is authored 'contested' rather than 'dead' because targeted programs' administrative burden is a genuine ongoing problem; the mismatch is between the founding problem and the chosen remedy's actual incidence, not the problem's existence. This divergence is exactly what the classification is built to surface: universality as a design principle vindicates a doctrine (stigma reduction) while the flow of funds runs to non-needy beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_vs_supplementation_design,
    'Does the unconditional transfer, as actually implemented in a given jurisdiction, substitute for targeted programs (funding the flat transfer partly by cutting means-tested aid) or supplement them (funded from new revenue, targeted programs held constant)? The dependency-trap reading assumes substitution; the freedom-floor reading typically assumes supplementation.',
    'Line-item budget analysis tracing whether targeted-program appropriations fell concurrently with UBI rollout, and whether the fall exceeds what caseload changes alone would predict.',
    'If the constraint is empirically supplementation rather than substitution, the victim set (working poor) shrinks substantially and the classification would move toward rope or scaffold; if substitution is confirmed, the snare classification in this reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_vs_supplementation_design, empirical, 'Whether program substitution or supplementation is the actual funding mechanism.').

omega_variable(
    labor_supply_effect_generalizability,
    'Does the -3.2% employment effect from large pilots (as reported in the cited AEI meta-analysis) generalize to permanent, economy-wide unconditional transfers, or is it an artifact of time-limited pilot design (participants reducing labor supply because the transfer is known to be temporary)?',
    'Compare labor-supply response in genuinely permanent, universal implementations (if any exist at sufficient scale) against time-limited pilot results; examine whether the effect size varies with transfer duration and universality of expectation.',
    'If the effect is a pilot-design artifact, the extractiveness score attributable to labor-supply distortion should be revised downward; if it holds or grows under permanent implementation, it corroborates this reading''s high ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_supply_effect_generalizability, empirical, 'Whether pilot-measured labor-supply effects generalize to permanent programs.').

omega_variable(
    kernel_framing_selection,
    'Is the dependency-trap framing (this story) the correct primary lens for unconditional income support, or does the freedom-floor framing (autonomy-enabling, low ε) better capture the constraint''s dominant structural effect? Both are coherent readings of the same underlying policy kernel and produce opposite classifications (snare vs. rope).',
    'This is inherently a conceptual/political framing question rather than a purely empirical one — resolution depends partly on which distributional and labor-market findings are weighted most heavily, and partly on normative priors about work incentives versus unconditional dignity. Route through the cs_structure reading_relations rather than treating as resolvable by a single study.',
    'The choice of reading determines whether this policy instrument is evaluated primarily on program-substitution/upward-redistribution grounds (this story) or on autonomy/coercion-removal grounds (sibling story) — the two readings coexist as live political positions rather than one superseding the other.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Whether the dependency-trap or freedom-floor framing is the dominant structural lens for this kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__dependency_trap_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__dependency_trap_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__dependency_trap_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__dependency_trap_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__dependency_trap_reading, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__dependency_trap_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__dependency_trap_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__dependency_trap_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__dependency_trap_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__dependency_trap_reading, base_extractiveness, 20, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(unco_su_t4, unconditional_income_support__dependency_trap_reading, suppression_requirement, 4, 0.3).
narrative_ontology:measurement(unco_su_t8, unconditional_income_support__dependency_trap_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__dependency_trap_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(unco_su_t16, unconditional_income_support__dependency_trap_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__dependency_trap_reading, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__dependency_trap_reading, 0.1).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, universality_paradox_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, targeted_welfare_program_design).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the unconditional_income_support kernel. freedom_floor_reading instantiates the same policy instrument as a low-ε autonomy-enabling coordination mechanism (rope-leaning); universality_paradox_reading instantiates it as a structurally ambiguous cross-ideological vehicle whose implementation paths converge fiscally despite incompatible justifications. All three share the kernel (the unconditional transfer instrument itself) but diverge in claimed type, ε, and beneficiary/victim structure because they read different empirical and normative premises as dominant. Per the ε-invariance principle, they are authored as three separate constraint files linked via affects_constraints rather than as one constraint with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
