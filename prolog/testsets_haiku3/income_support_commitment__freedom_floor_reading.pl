% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__freedom_floor_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested kernel: the
 *   commitment to income support. The freedom-floor reading holds that
 *   unconditional, universal income support enables autonomy, dignity, and
 *   labor-market exit capacity — particularly for caregivers, precarious
 *   workers, abuse survivors, and creative workers. It frames the constraint
 *   as a coordination solution to labor-market coercion and a dignity
 *   entitlement, not as poverty-targeted redistribution. The reading is
 *   distinguished from the dependency-trap reading (which holds that
 *   unconditional support erodes work incentives and increases state
 *   dependence) and the targeting-efficiency reading (which holds that
 *   support should concentrate on demonstrated need rather than
 *   universality). This story generates ONE constraint from the freedom-floor
 *   reading's framing; the sibling readings are separate constraint stories
 *   with different ε, beneficiary/victim structures, and types.
 *
 * KEY AGENTS:
 *   - Caregivers (unpaid): currently trapped; income support removes coercion to accept any wage
 *   - Precarious workers: currently constrained; income support raises reservation wage and enables negotiation
 *   - Abuse survivors exiting: currently trapped; income support funds exit capacity
 *   - Artists and entrepreneurs: currently constrained; income support provides runway for deferred-income work
 *   - All citizens: universal recipients carrying shared dignity value and collective funding obligation
 *   - Employers (excluded): lose wage-desperation coercion power; explicitly constrained by the reading
 *   - Tax-base affluent (payers): fund the arrangement through progressive taxation; carry citizenship obligations
 *   - State fiscal administrators (agenda-setters): maintain unconditional, universal access; administer the commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.18).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.12).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '47b1246b-6c0c-49e0-b8ae-da83a100d71f').
narrative_ontology:cs_kernel_codification('47b1246b-6c0c-49e0-b8ae-da83a100d71f', formalized).
narrative_ontology:cs_authority_grounding('47b1246b-6c0c-49e0-b8ae-da83a100d71f', lineage).
narrative_ontology:cs_interpretation_layer_present('47b1246b-6c0c-49e0-b8ae-da83a100d71f').
narrative_ontology:cs_reading_relation('47b1246b-6c0c-49e0-b8ae-da83a100d71f', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('47b1246b-6c0c-49e0-b8ae-da83a100d71f', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('47b1246b-6c0c-49e0-b8ae-da83a100d71f', foundational, autonomy_requires_unconditional_exit_capacity).
narrative_ontology:cs_axiom_status(autonomy_requires_unconditional_exit_capacity, holdable).
narrative_ontology:cs_axiom_grounding('47b1246b-6c0c-49e0-b8ae-da83a100d71f', autonomy_requires_unconditional_exit_capacity, deontological).
narrative_ontology:cs_axiom('47b1246b-6c0c-49e0-b8ae-da83a100d71f', foundational, universality_eliminates_stigmatizing_means_test_extraction).
narrative_ontology:cs_axiom_status(universality_eliminates_stigmatizing_means_test_extraction, holdable).
narrative_ontology:cs_axiom_grounding('47b1246b-6c0c-49e0-b8ae-da83a100d71f', universality_eliminates_stigmatizing_means_test_extraction, instrumental).
narrative_ontology:cs_axiom('47b1246b-6c0c-49e0-b8ae-da83a100d71f', secondary, labor_market_desperation_coercion_is_unjust_extraction).
narrative_ontology:cs_axiom_status(labor_market_desperation_coercion_is_unjust_extraction, holdable).
narrative_ontology:cs_axiom_grounding('47b1246b-6c0c-49e0-b8ae-da83a100d71f', labor_market_desperation_coercion_is_unjust_extraction, deontological).
narrative_ontology:cs_reference_frame('47b1246b-6c0c-49e0-b8ae-da83a100d71f', dignity_autonomy_framework).
narrative_ontology:cs_drift_state('47b1246b-6c0c-49e0-b8ae-da83a100d71f', contemporary_labor_market_precarity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('47b1246b-6c0c-49e0-b8ae-da83a100d71f', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, caregivers_unpaid).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors_exiting).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_entrepreneurs).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, all_citizens_dignity_value).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, all_citizens).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, all_citizens).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, tax_base_affluent).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, autonomy_requires_exit_capacity).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, labor_dignity_requires_choice).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, unconditional_support_coordination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Parents (disproportionately mothers), elder care providers, disability care workers — currently trapped in unpaid labor with zero income security. Income support removes the coercion to accept any wage to survive; enables choice about care work continuity. They exit poverty and gain voice in labor negotiations if they pursue paid work.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, caregivers_unpaid, beneficiary,
    powerless, biographical, trapped, national).

% Gig workers, seasonal laborers, part-time contingent employees — currently must accept any available wage because they have no income buffer. Income support raises the reservation wage and enables rejection of abusive conditions, wage theft, unsafe work. They gain structural power in labor negotiations.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    moderate, biographical, constrained, national).

% Domestic violence and economic coercion survivors — currently trapped because leaving means homelessness and destitution. Income support funds immediate exit, housing, and subsistence during recovery and re-employment. Removes the 'economic lock-in' mechanism that keeps them dependent on abusers.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors_exiting, beneficiary,
    powerless, biographical, trapped, national).

% Creative workers and small-business founders — currently forced into wage employment or gig work to survive, foreclosing the runway for creative projects with deferred income. Income support provides a stability floor to sustain early-stage creative and entrepreneurial work without immediate revenue.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_entrepreneurs, beneficiary,
    moderate, biographical, constrained, national).

% Universal recipients (no means-testing); all citizens carry shared dignity and equal claim. They also fund it collectively through progressive taxation. The reading holds that the dignity value and autonomy capacity are benefits that outweigh the tax cost for those with income to contribute, and that universality itself (no stigma, no bureaucratic means-test apparatus) is structurally simpler and more respectful than targeting.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, all_citizens, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__freedom_floor_reading, all_citizens, payer).

% Employers accustomed to zero-reservation-wage workers — those with no exit capacity, no buffer, no choice. Income support erodes the coercive power of survival desperation on wage-setting. Employers would argue this is economically damaging; the freedom-floor reading holds that labor coercion via desperation is the damage, and exit capacity is the fix. They are excluded from the reading's consent set but their interests are explicitly accounted for as constrained by the arrangement.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, employers_wage_discipline, excluded,
    institutional, generational, trapped, national).

% High-income earners who finance income support through progressive taxation. They bear the direct cost. The reading frames this as fair exchange (equal citizenship status, equal dignity floor) and economically justified (loosened labor coercion increases labor quality and reduces precarity-driven social costs). They have partial exit capacity (tax minimization, relocation) but also carry citizenship obligations framed as structural.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, tax_base_affluent, payer,
    powerful, generational, mobile, global).

% Governments that set funding levels, distribution mechanisms, and universality guarantees. The reading constrains them to maintain unconditional access (no means-test gate, no behavioral conditions). They administer a commitment to universality, not a discretionary poverty program.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, state_fiscal_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Comparative welfare-state researchers and international policy analysts observing the constraint's operation across jurisdictions and time periods. They measure labor-force participation, wage floors, precarity metrics, exit dynamics, and dignity/autonomy self-reports to test whether the freedom-floor reading's predictions hold.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, observer_comparative_welfare, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_commitment__freedom_floor_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Funds a dignity floor and labor-market exit capacity universally, removing the wage-desperation coercion that otherwise forces workers into abusive conditions and forces caregivers into unpaid dependency. Solves the problem that labor markets with unlimited reservation-wage desperation generate both worker exploitation and persistent low-wage traps.
% TRANSFER_FUNCTION: Moves income from tax-paying affluent citizens and organized economic participants to all citizens, structured as an unconditional, universal, equal payment (no means-test differentiation, no behavioral conditions). The transfer is not to 'the poor' — the reading rejects that framing — but to all citizens as a dignity entitlement and autonomy foundation.
% ABSENT_VOICES: Employers who depend on zero-reservation-wage workers are structurally excluded from setting income-support levels and are actively constrained by the reading. They would argue for lower payments or conditions (work requirements, duration limits) to preserve labor-market desperation and thereby maintain wage discipline. Libertarian property-rights positions that reject collective provisioning are also excluded from the reading's frame but their objection is documented as a philosophical disagreement, not ignored.
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared, precarious workers would lose bargaining power and wages would fall back to desperation minimums; caregivers would return to unpaid dependency; abuse survivors would lose exit capacity and re-entrapment would follow; artists and early entrepreneurs would be re-forced into wage work or gig survival. Labor coercion via desperation would re-tighten, and the dignity floor would collapse for anyone without independent means. The entire labor-market equilibrium and social dependence structure would reorganize around survival desperation again.
% FOUNDING_PROBLEM: Labor markets without exit capacity systematically coerce workers into accepting abusive conditions, wage theft, unsafe work, and poverty wages because the alternative is destitution. This coercion generates skill atrophy, health damage, family instability, and perpetual precarity. Unpaid care work (childcare, elder care, disability support) is structurally undervalued and pushed into dependency because there is no income floor for non-market work. Abuse survivors and vulnerable populations are trapped by economic lock-in. The founding problem is the structural extraction of labor value through desperation coercion.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists (Piketty, Roemer, Sen, Standing) document persistent wage desperation and its causal role in precarity. Domestic violence researchers (Evan Stark, Cathy Young cohort) document economic lock-in as a primary entrapment mechanism. Care-work researchers (Hochschild, Crittenden) document the unpaid-care coercion structure. These corroborations come from outside the beneficiary-advocacy set — they are independent structural analyses. Governments and employers that resist income support would dispute the framing but do not dispute the empirical facts of wage coercion; they dispute whether it should be corrected via income support versus labor regulation or market expansion.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__freedom_floor_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__freedom_floor_reading_tests).
:- end_tests(income_support_commitment__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.18) because the reading frames the constraint as a coordination solution without inherent asymmetric extraction. The beneficiary set is universal (all citizens), eliminating the means-test stigma and bureaucratic extraction that targeted programs carry. No party is coerced into bearing concentrated costs without compensating benefit — the affluent bear financial cost, but the reading frames this as a fair exchange for equal citizenship and social stability. Suppression is very low (0.12) because the constraint operates through positive provisions (universal payment) rather than coercion or restriction. Theater is minimal (0.08) — the constraint's actual function is its stated function; there is little performative cover because the mechanism is transparent. Accessibility collapse is low (0.25) because alternatives remain conceptually available (means-tested targeting, work requirements, conditional support); the reading does not claim to be a natural law but rather a normative choice grounded in autonomy and dignity principles. Resistance is moderate (0.35) because employers, libertarian property-rights positions, and fiscal-conservative positions actively resist the arrangement, but the reading has significant political and intellectual support. Measurements are stable across the interval because the reading's core normative commitments (universality, unconditionality, dignity) do not drift over time; minor fluctuations in theater and suppression reflect political pressure cycles and implementation noise, not structural changes in the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   From the state-administrator seat: the constraint is a sustainable commitment requiring stable funding, political will, and unconditionality guarantees — extractiveness appears minimal, suppression routine. From the employer seat: the constraint is an unjust extraction of wage-discipline power and a threat to labor-cost minimization — extractiveness appears much higher (to them), because the loss of coercive control reads as extraction. From the precarious-worker seat: the constraint is a freedom-enabling coordination that removes desperation — extractiveness appears negative (it is a subsidy to their autonomy). The engine computes these divergences from the power atoms and exit-options structure. The reading does NOT adjudicate which perspective is 'correct' — it asserts the structural claim that all parties benefit from removing wage desperation, and that the political disagreement is about whether desperation-coercion is justified, not about the constraint's mechanical effects.
 *
 * DIRECTIONALITY LOGIC:
 *   From the caregivers' seat: trapped exit options + zero income → d approaches 1.0 (full target of wage desperation). Income support moves them to d near 0.0 (beneficiary). From the precarious workers' seat: constrained exit + desperation wages → d high; income support lowers d. From the abuse survivors' seat: trapped by economic lock-in → d = 1.0; income support enables exit, lowers d to beneficiary range. From the all-citizens seat (universal): organized power, mobile exit (can leave the nation), biographical horizon → d near 0.5 (symmetric: they both fund and benefit from universality). From the affluent-payers seat: powerful institutional actors, global arbitrage exit → d lowers toward 0.2–0.3 (they bear financial cost but retain autonomy and do not lose exit capacity; they are constrained by the arrangement but not coerced by it). From the employer seat (excluded): institutional power, trapped in the constraint (cannot opt out of the wage-market reform) → d rises toward 0.7 (targeted by the arrangement, lose coercive power). The reading's structural claim is that no party is placed in a position of concentrated extraction risk — the affluent retain autonomy, the constrained gain it, employers lose unjust coercive power (reframed as constraint, not harm). This is why the reading claims rope, not tangled-rope: there is genuine coordination function (remove wage desperation) without asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (wage desperation coerces workers into abusive conditions) is live and documented. The founding problem status is NOT dead or attenuated — labor precarity persists across all measured jurisdictions, and the mechanisms (no exit capacity, no income buffer) are active. This prevents the constraint from drifting toward piton classification. The reading does NOT claim the founding problem is solved by income support alone — it claims income support is a necessary component of a solution that also requires labor regulation, employer accountability, and care-work revaluation. The mandatrophy check asks: has the founding problem's solution condition (removal of wage desperation) been achieved? The answer is 'contested' (depends on implementation scale and depth), which keeps the constraint in active rope classification, not resolved. If a future scenario showed wage desperation had been eliminated and the income-support level had been reduced to zero or near-zero without labor precarity re-emergence, the founding_problem_status would shift to 'dead' and the constraint would face reclassification. That scenario is not the present one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universality_vs_targeting_boundary,
    'Is universality (providing income support to all citizens equally) structurally necessary to achieve the reading''s goals (dignity, autonomy, removal of means-test stigma), or could the same autonomy gains be achieved through very generous means-testing with minimal bureaucratic overhead?',
    'Comparative analysis of dignity-metric, exit-capacity, and autonomy outcomes across jurisdictions with universal support (Finland, Iran pilot, proposed UK trial) versus generous-but-targeted support (EITC, negative income tax designs). Test whether means-testing infrastructure introduces new suppression mechanisms (stigma, bureaucratic control, benefit cliffs) even at high eligibility thresholds.',
    'If outcomes diverge (universality produces higher autonomy despite lower per-recipient funding), the reading''s universality commitment is structurally grounded. If outcomes converge (generous targeting achieves the same autonomy gains), the constraint might be reclassified as a tangled-rope (coordination plus targeting-induced extraction) rather than a pure rope. This is the core design question the reading is built to investigate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universality_vs_targeting_boundary, empirical, 'Whether universality is structurally necessary to achieve autonomy and dignity outcomes, or whether very generous means-tested support could achieve the same results.').

omega_variable(
    work_incentive_empirical_contingency,
    'Does unconditional income support at the levels proposed by the freedom-floor reading actually reduce labor-force participation, or does it enable higher-quality participation (exit from abusive work, retraining, entrepreneurship)?',
    'Longitudinal analysis of labor-force participation, wage trajectories, job-quality metrics, and career re-entry rates for recipients of unconditional income support, compared to control populations and historical baselines. Separate analysis by care-work engagement, precarious vs. stable employment, and exit-from-abuse cohorts.',
    'If labor-force participation is maintained or increases, the dependency-trap reading is falsified and the freedom-floor reading''s claim to low extractiveness and genuine coordination is strengthened. If participation declines significantly and income support becomes a permanent substitute rather than a transitional safety net, the classification might shift toward scaffold (transient support) or toward tangled-rope if the support is concentrated on those exiting the labor force rather than enabling better participation. This omega names the empirical dispute between the readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(work_incentive_empirical_contingency, empirical, 'Whether unconditional income support reduces labor-force participation (supporting the dependency-trap reading) or enables higher-quality participation and exit from desperation work (supporting the freedom-floor reading).').

omega_variable(
    reading_foreclosure_structural_test,
    'Do the core normative premises of the freedom-floor reading (unconditional support, universality, dignity entitlement) logically foreclose the dependency-trap reading''s core premise (work disincentives and state dependence are systemic harms), or can both readings be coherently held within different frameworks?',
    'Genealogical analysis of the readings'' axiom sets and their logical entailments. If the freedom-floor reading''s axiom of ''autonomy-requires-exit-capacity'' directly contradicts the dependency-trap reading''s axiom of ''work-incentive-preservation-is-paramount'', foreclosure holds. If both axioms can be held by different political frameworks (liberal-egalitarian vs. conservative-paternalist), coexistence holds.',
    'If foreclosure holds, one of the readings should eventually be eliminated from public discourse as the logical contradiction becomes clear. If coexistence holds, both readings will persist as competing frameworks held by different political coalitions, and the engine should classify the kernel as internally contested rather than resolved. This impacts how the constraint family is modeled in the network: do the readings form a logical hierarchy (one forecloses others) or a persistent parallel disagreement (coexistence)?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_structural_test, conceptual, 'Whether the freedom-floor and dependency-trap readings logically foreclose each other or can coexist as coherent framework-dependent positions.').

omega_variable(
    suppression_mechanism_empirical_check,
    'The freedom-floor reading claims low suppression (0.12) because the constraint operates through positive provision rather than coercion. But does the universal-income-support commitment require any suppression to maintain (e.g., restrictions on wealthy departure, tax compliance enforcement, eligibility verification)? If so, is the suppression structural or incidental to implementation?',
    'Audit of the constraint''s enforcement machinery: What restrictions are imposed to maintain universality? How much enforcement cost is carried? Are there identity-lock or exit-barriers for the affluent-payer seat that constitute structural suppression? Comparison across jurisdictions: does every implementation of unconditional income support require equivalent suppression levels, or do implementation designs vary significantly?',
    'If suppression is structural and required by the reading''s normative commitments (e.g., preventing wealthy exit to zero-tax jurisdictions), the reading might be reclassified as tangled-rope (coordination plus enforcement cost). If suppression is incidental to implementation and varies with design choices, the rope classification is confirmed and the omega documents the implementation-design space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_empirical_check, empirical, 'Whether the freedom-floor reading''s low-suppression claim is robust to the enforcement costs required to maintain universality and prevent wealthy exit.').

omega_variable(
    kernel_reading_relational_ambiguity,
    'The freedom-floor and dependency-trap readings both claim to be the coherent interpretation of the income-support-commitment kernel. Which reading better matches the kernel''s actual codification (the language of laws, policy documents, and formal commitments), and which reading is imposed by a particular political coalition?',
    'Textual analysis of the kernel''s formalization: What do the founding policy documents, statutory language, and formal justifications actually say about the commitment''s purpose? Do they use freedom/autonomy language (supporting the freedom-floor reading) or work-incentive language (supporting the dependency-trap reading), or are they silent/ambiguous? Cross-jurisdictional comparison: do different nations'' codifications of income support show consistent patterns, or does each reading capture different jurisdictions?',
    'If the kernel''s own codification is ambiguous or open (distributed interpretation), both readings remain live and the kernel itself is structurally contested. If the codification clearly endorses one reading''s normative framing, the other reading becomes a revisionist reinterpretation (same kernel, different axioms). This affects whether the readings are truly independent constraints (different ε, different types) or one is a contestation of the other''s framing without structural difference.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_relational_ambiguity, conceptual, 'Whether the income-support kernel''s own codification privileges one reading''s framing over another, or remains open to multiple coherent interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(inco_tr_t0, projected).
narrative_ontology:measurement(inco_tr_t5, income_support_commitment__freedom_floor_reading, theater_ratio, 5, 0.08).
narrative_ontology:measurement_basis(inco_tr_t5, observed).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__freedom_floor_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement_basis(inco_tr_t10, observed).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(inco_tr_t20, observed).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__freedom_floor_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement_basis(inco_tr_t30, projected).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__freedom_floor_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement_basis(inco_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(inco_be_t0, projected).
narrative_ontology:measurement(inco_be_t5, income_support_commitment__freedom_floor_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement_basis(inco_be_t5, observed).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__freedom_floor_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement_basis(inco_be_t10, observed).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(inco_be_t20, observed).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__freedom_floor_reading, base_extractiveness, 30, 0.17).
narrative_ontology:measurement_basis(inco_be_t30, projected).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__freedom_floor_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(inco_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__freedom_floor_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(inco_su_t0, projected).
narrative_ontology:measurement(inco_su_t5, income_support_commitment__freedom_floor_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement_basis(inco_su_t5, observed).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__freedom_floor_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement_basis(inco_su_t10, observed).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__freedom_floor_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement_basis(inco_su_t20, observed).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__freedom_floor_reading, suppression_requirement, 30, 0.13).
narrative_ontology:measurement_basis(inco_su_t30, projected).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__freedom_floor_reading, suppression_requirement, 40, 0.14).
narrative_ontology:measurement_basis(inco_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__targeting_efficiency_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, labor_market_desperation_coercion).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, care_work_unpaid_dependency).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, abuse_economic_lock_in).

% DUAL FORMULATION NOTE:
% Income-support-commitment kernel carries three structurally distinct constraint readings: (1) freedom-floor-reading (THIS FILE) — low extractiveness rope, universality-based, autonomy framing; (2) dependency-trap-reading — higher extractiveness tangled-rope, work-disincentive risks, state-dependence costs; (3) targeting-efficiency-reading — means-tested snare-risk, targeting stigma and bureaucratic extraction. Each reading instantiates a different ε, beneficiary/victim structure, and claimed type from the same kernel commitment. The readings coexist as coherent positions held by different political coalitions; none logically forecloses the others within their respective frameworks. The network links signal structural influence: universalizing the support (freedom-floor) reduces the targeting extraction that the efficiency-reading would introduce, while increasing the work-disincentive risk that the dependency-trap reading emphasizes. A shift in kernel codification (toward universality or toward means-testing) would shift which reading's ε is empirically validated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
