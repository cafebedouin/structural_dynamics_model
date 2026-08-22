% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: unconditional_income_support__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Incentive-Distorting Subsidy (Dependency Trap Reading)
 *   domain: political_economy/social_policy/welfare_state
 *
 * SUMMARY:
 *   This constraint story instantiates the dependency-trap reading of the
 *   unconditional income support kernel. Under this reading, unconditional
 *   income support is structured as a snare that extracts from the working
 *   poor by replacing targeted welfare programs with a universal payment that
 *   leaves them worse off, and transfers upward to middle/upper-income
 *   recipients who receive a windfall. The reading emphasizes the
 *   constraint's behavioral effects (employment reduction, labor market
 *   substitution) and its distributional mechanics (who gains, who loses, who
 *   bears the cost). The constraint is claimed as a snare because it
 *   disguises extraction as universality: it presents the elimination of
 *   targeted programs (an extractive choice) as a side effect of the
 *   universality principle (a cover story), when in fact the elimination IS
 *   the extraction mechanism. The three sibling readings (freedom-floor,
 *   universality-paradox) interpret the same kernel (unconditional income
 *   support as policy arrangement) differently and would author different ε
 *   values, different beneficiary/victim structures, and different types.
 *   This story is ONE reading only; the siblings are separate constraint
 *   stories in a linked family.
 *
 * KEY AGENTS:
 *   - working_poor: primary target (lose targeted programs worth more than universal payment)
 *   - middle_and_upper_income_recipients: primary beneficiary (net windfall from universality)
 *   - ubi_advocacy_organizations: agenda-setter (design and defend the constraint, gain political capital from universality frame)
 *   - taxpayers_and_general_revenue: secondary target (bear fiscal cost)
 *   - means_tested_program_administrators: excluded stakeholder (lose programs and function)
 *   - labor_market_and_employers: excluded stakeholder (bear employment reduction cost)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.78).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.71).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support as Incentive-Distorting Subsidy (Dependency Trap Reading)").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/social_policy/welfare_state").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, '357a8ec0-1d03-4fdb-b526-f46f4f65f4b9').
narrative_ontology:cs_kernel_codification('357a8ec0-1d03-4fdb-b526-f46f4f65f4b9', distributed).
narrative_ontology:cs_authority_grounding('357a8ec0-1d03-4fdb-b526-f46f4f65f4b9', extraction).
narrative_ontology:cs_interpretation_layer_present('357a8ec0-1d03-4fdb-b526-f46f4f65f4b9').
narrative_ontology:cs_reading_relation('357a8ec0-1d03-4fdb-b526-f46f4f65f4b9', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('357a8ec0-1d03-4fdb-b526-f46f4f65f4b9', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('357a8ec0-1d03-4fdb-b526-f46f4f65f4b9', foundational, employment_reduction_indicates_extraction).
narrative_ontology:cs_axiom_status(employment_reduction_indicates_extraction, holdable).
narrative_ontology:cs_axiom_grounding('357a8ec0-1d03-4fdb-b526-f46f4f65f4b9', employment_reduction_indicates_extraction, empirically_contingent).
narrative_ontology:cs_axiom('357a8ec0-1d03-4fdb-b526-f46f4f65f4b9', foundational, universality_principle_incompatible_with_targeting_efficiency).
narrative_ontology:cs_axiom_status(universality_principle_incompatible_with_targeting_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('357a8ec0-1d03-4fdb-b526-f46f4f65f4b9', universality_principle_incompatible_with_targeting_efficiency, instrumental).
narrative_ontology:cs_axiom('357a8ec0-1d03-4fdb-b526-f46f4f65f4b9', secondary, program_elimination_imposes_net_cost_on_poorest).
narrative_ontology:cs_axiom_status(program_elimination_imposes_net_cost_on_poorest, holdable).
narrative_ontology:cs_axiom_grounding('357a8ec0-1d03-4fdb-b526-f46f4f65f4b9', program_elimination_imposes_net_cost_on_poorest, empirically_contingent).
narrative_ontology:cs_reference_frame('357a8ec0-1d03-4fdb-b526-f46f4f65f4b9', targeted_welfare_state_efficiency_baseline).
narrative_ontology:cs_drift_state('357a8ec0-1d03-4fdb-b526-f46f4f65f4b9', post_unconditional_support_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('357a8ec0-1d03-4fdb-b526-f46f4f65f4b9', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_and_upper_income_recipients).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocacy_organizations).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor_losing_targeted_programs).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, taxpayers_bearing_aggregate_cost).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, working_poor).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, taxpayers_and_general_revenue).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, means_tested_program_administrators).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, labor_market_and_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Currently receive means-tested benefits (housing assistance, food support, childcare subsidies) that total $8,000–$15,000 annually depending on local program availability. Under unconditional income support at $1,000/month ($12,000/year), those targeted programs are eliminated or severely cut to offset the universal payment. Net outcome: lose specialized support (e.g., housing vouchers worth $6,000–$10,000) and receive a smaller, undifferentiated $12,000 check. The gap leaves them worse off. Their exit option is labor market participation, but low-skill employment offers wages often below regional cost of living; the substitution of universal payments for targeted aid removes the margin that made survival possible.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__dependency_trap_reading, working_poor, beneficiary).

% Receive the same $12,000/year unconditional payment as everyone else. For households already earning $80,000–$150,000+, this is a pure windfall; they face no income test, no work requirement, no behavioral condition. They deploy it as discretionary savings, investment, or supplemental consumption. The universality of the payment ensures they collect without stigma or verification burden. They are net beneficiaries: they pay higher taxes (average $2,000–$3,500 more annually on progressive schedules) but receive the $12,000 transfer, netting positive return or manageable negative return depending on their marginal rate.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_and_upper_income_recipients, beneficiary,
    powerful, biographical, arbitrage, national).

% Advocate for unconditional income support as policy, building political capital around the universality claim: everyone receives, no means test, no bureaucratic verification. This framing solves for their core goal (basic income redistribution) and their organizational interest (simple, universal programs are easier to scale, defend, and claim credit for than complex, targeted systems). They frame opposition as anti-redistribution and frame support as pro-universal-human-dignity. The elimination of targeted programs is treated as a feature (removing stigma) rather than an extraction mechanism (replacing more efficient support with less efficient universality).
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).

% Bear the aggregate fiscal cost: unconditional income support at $1,000/month for 260 million adults is approximately $3.12 trillion annually (gross). Offsets (elimination of some existing welfare programs, modest efficiencies) reduce net cost to approximately $1.4–$1.8 trillion. Taxpayers fund this through progressive income tax increases (marginal rates rise 5–8 percentage points) and/or payroll tax increases. Their extraction is diffuse: no single taxpayer controls the policy, and the cost is socialized across the tax base, making individual resistance difficult. Their exit option is capital mobility (high earners can relocate) or political voice (advocate for repeal), but the policy enjoys supermajority support among low-income voters, making electoral reversal unlikely.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, taxpayers_and_general_revenue, payer,
    organized, biographical, constrained, national).

% Administer housing assistance, SNAP, childcare subsidies, and other targeted programs. Unconditional income support crowds out their function: the programs are defunded to offset the universal payment, eliminating the administrative apparatus and the specialized knowledge to match aid to need (e.g., regional housing costs, family composition, medical expenses). They lose budgets, staff, and organizational mission. Their objection (that targeted programs are more efficient and reach only the needy) is overridden by the universality claim (everyone gets the same, no means test). They are excluded from the policy design process and are not recognized as having legitimate interests in program continuation.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, means_tested_program_administrators, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__dependency_trap_reading, means_tested_program_administrators, excluded).

% Unconditional income support operates as a labor-market subsidy to non-work: individuals can opt out of employment at a lower reservation wage (the $12,000/year floor). Pilots show a -3.2% employment reduction per AEI meta-analysis. Employers face reduced labor supply in low-wage sectors (hospitality, agriculture, retail), putting upward pressure on wages or forcing operational adjustments. They do not control the policy and cannot exit the jurisdiction, so their cost is absorbed as operational pressure or passed to consumers. They are treated as a payer (bearing the employment reduction cost) but are not recognized as affected parties in the policy discourse.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, labor_market_and_employers, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__dependency_trap_reading, labor_market_and_employers, payer).

% Analyze the policy's distributional and behavioral effects. From this reading's seat, the constraint is a snare: it replaces efficient, targeted transfers with a universal, undifferentiated transfer that leaves the poorest worse off and benefits the non-needy. The reading depends on empirical claims about employment elasticity (does labor supply decline?) and distributional analysis (who gains, who loses?). Their seat is analytical and they exercise voice through research and publication; they do not control implementation.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, policy_analysts_and_economists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__dependency_trap_reading, middle_and_upper_income_recipients).
narrative_ontology:fixing_cost_class(unconditional_income_support__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly coordinates a simplified redistribution mechanism: one single transfer replaces dozens of means-tested programs, reducing administrative overhead and stigma. The universality claim eliminates verification requirements and makes the payment universally available.
% TRANSFER_FUNCTION: Moves approximately $1.4–$1.8 trillion annually from high-income taxpayers to all adult citizens, distributed as a flat $1,000/month payment. In this reading, the constraint functionally transfers wealth FROM working poor and taxpayers TO middle/upper-income recipients and advocacy organizations (political capital gain). The working poor lose more targeted support than they gain in universal payment; middle-income recipients receive a windfall; taxpayers bear the full fiscal cost.
% ABSENT_VOICES: Means-tested program administrators, low-wage employers, working poor with specialized needs (disabled workers, regional cost-of-living variance, family-size adjustment). These parties would object that universality destroys program efficacy for the poorest and that the employment reduction is not offset by the payment floor. They are excluded because the universality framing treats their objection as opposition to redistribution itself, not recognition of distributional mechanics.
% DISAPPEARANCE_RATIONALE: If unconditional income support and its universal funding structure disappeared overnight, the world rearranges sharply: targeted welfare programs are restored (or rebuilt), labor supply in low-wage sectors recovers, the fiscal burden on high earners falls by $1.4–$1.8 trillion annually, and the working poor regain access to specialized support. The constraint is not natural; it is a chosen institutional arrangement that can be unmade.
% FOUNDING_PROBLEM: Welfare bureaucracy is inefficient, means-testing creates stigma and perverse incentives for program enrollment/disenrollment, and poverty persists despite targeted spending. The founding claim is that a single, universal payment bypasses the bureaucratic apparatus and delivers the same poverty-reduction outcome more efficiently.
% FOUNDING_PROBLEM_CORROBORATION: The constraint's advocates (UBI organizations, progressive economists building the universality model) attest the founding problem is live: bureaucracy is too complex, stigma is real, and means-testing traps workers in poverty cycles. Opposing economists (AEI, targeted-program researchers, working-poor advocacy groups) attest the founding problem is misframed: targeted programs are more efficient per dollar deployed, universality is fiscally unsustainable, and the constraint's designers have not solved the fundamental efficiency problem—they have moved it, not eliminated it. Independent analysis (Congressional Budget Office, GAO evaluations of pilot programs) from outside the advocacy-organization beneficiary set supports the contestation: efficiency gains are modest, employment reduction is measurable, and distributional effects are more negative for the poorest than the advocates claim.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__dependency_trap_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness at 0.78 reflects the high asymmetry: the constraint functionally transfers wealth from working poor and taxpayers to non-needy recipients and advocacy organizations. The measurement series shows extraction rising from 0.62 at t=0 (initial implementation, still navigating program transition) to 0.78 by t=16 (full maturation, all targeted programs eliminated, behavioral effects realized, political entrenchment high). Theater rises from 0.28 to 0.42 over the interval: initially, the universality justification is most credible (new program, administrative simplification narrative dominant); as time passes and targeted-program loss becomes visible, the theater ratio rises—more of the constraint's persistence becomes dependent on defending the universality narrative against distributional criticism. Suppression rises from 0.58 to 0.71: the constraint requires active suppression of working-poor objections (framed as opposition to redistribution), exclusion of means-tested administrators from policy voice, and prevention of alternative framings (e.g., a hybrid system retaining targeted programs and adding a modest UBI top-up). The time grid is aligned: every metric shares the same seven time points (0, 2, 4, 8, 12, 16, 20), enabling temporal coherence across the measurement series.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (UBI advocacy organizations), the constraint appears as a rope: a genuine coordination solution to welfare bureaucracy inefficiency, universally beneficial, morally aligned with redistribution principles. From the working-poor seat, the constraint appears as a snare: a mechanism that replaces valuable targeted support with a smaller universal check, harming the poorest while benefiting the non-needy. From the taxpayer seat, the constraint appears as extraction with moderate theater: the universality claim justifies a massive fiscal transfer to non-needy recipients, and resistance is suppressed by framing opposition as anti-redistribution. The three readings (this one, freedom-floor, universality-paradox) each anchor to a different seat's experience and generalize it as the constraint's true nature. This reading anchors to the working-poor and taxpayer-burden seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The working poor carry directionality near 1.0 (full target): they pay the extraction (lose targeted support), have trapped exit options (cannot exit the jurisdiction or the welfare system entirely), and face identity-locked dependency on the constraint's rules. The middle/upper-income recipients carry directionality near 0.0 (full beneficiary): they collect a net windfall, have arbitrage-grade exit (can relocate if tax rates rise, can reallocate income to lower tax bases), and face minimal suppression. The taxonomy diverges across power levels: a powerful, arbitrage-capable agent (middle income) experiences the same constraint as a beneficiary with near-zero directionality; a powerless, trapped agent (working poor) experiences the same constraint as a target with high directionality and high effective extraction. The UBI advocacy organizations carry directionality near 0.5 in abstract (they collect political capital, not fiscal transfer), but their structural position is as an agenda-setter—they design and enforce the constraint—so their directionality is better understood as asymmetric beneficiary with organized power. The engine derives d from beneficiary/victim + exit + power; the measurement reveals seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (welfare bureaucracy is inefficient, means-testing creates stigma) is claimed as LIVE by the constraint's advocates but is CONTESTED by independent analysis. The empirical evidence (CBO, GAO evaluations) suggests efficiency gains are modest and distributional effects are worse for the poorest than claimed. The theater ratio rising from 0.28 to 0.42 indicates the constraint's persistence increasingly depends on defending the universality narrative rather than on genuine program efficacy. The constraint begins to exhibit mandatrophy signature: the founding problem (bureaucratic inefficiency) is substantially addressed by administrative reform and targeted-program optimization (which occurs in parallel or independently), yet the constraint persists because it serves the political interest of the agenda-setters (universality claim, organizational power) and the fiscal interest of non-needy recipients. The disappearance verdict is world_rearranges (arrangements depend on the constraint), so mandatrophy does NOT declare here—mandatrophy requires world_unchanged or contested verdict. However, the rising theater ratio and the empirical contestation of the founding problem suggest ongoing drift toward mandatrophy condition if evidence continues to undermine the founding claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    employment_elasticity_empirical_contention,
    'What is the true employment elasticity of unconditional income support at the tested payment level ($1,000/month)? Do labor-market effects from small pilots ($200–$500/month, 1–3 year duration) generalize to full-scale, permanent implementation ($1,000/month)?',
    'Multi-year pilot studies with varied payment levels and durations; real-world implementation data from jurisdictions running unconditional income support long-term; meta-analysis of employment outcomes across multiple studies with consistent methodology.',
    'If employment elasticity is low (near zero), the dependency-trap reading''s core claim about incentive distortion weakens, and the constraint approaches rope or tangled-rope classification. If elasticity is high (>2%), the snare classification strengthens and the extraction claim is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_elasticity_empirical_contention, empirical, 'Employment behavioral response to full-scale unconditional income support.').

omega_variable(
    targeted_program_replacement_efficiency,
    'Is eliminating targeted programs to fund unconditional income support actually more efficient than operating both systems in parallel (hybrid approach: targeted programs + modest UBI top-up), or is the constraint''s motivation to eliminate targeted programs driven by the agenda-setter''s interest in simplification and political universality rather than genuine efficiency optimization?',
    'Comparative cost analysis of three scenarios: (1) baseline targeted programs only, (2) full universality + targeted-program elimination, (3) hybrid (targeted programs retained + modest UBI addition). Per-dollar poverty-reduction outcomes for each scenario. Analysis of why the policy designer chose elimination over addition.',
    'If elimination is less efficient than hybrid approaches, the constraint is not solving a genuine coordination problem—it is extracting from the poorest to satisfy the agenda-setter''s institutional interest in universality. This would support the snare classification. If elimination is substantially more efficient, the constraint approaches tangled-rope (coordination + extraction) or rope (genuine coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeted_program_replacement_efficiency, empirical, 'Whether the constraint''s elimination of targeted programs is optimality-driven or ideology-driven.').

omega_variable(
    middle_income_windfall_capture,
    'Is the upward redistribution to middle/upper-income recipients a design feature (intentional cross-subsidy of universality) or an unintended side effect discovered after implementation?',
    'Policy design documents, legislator testimony, advocacy organization communications from pre-implementation period. Post-implementation distributional analysis showing gain/loss by income quintile. Statements from designers about whether they anticipated the upward redistribution.',
    'If intentional, the constraint is a design choice to sacrifice poor-targeting for political universality—it is clearly extractive, supporting snare classification. If unintended, the constraint retains a more generous interpretation (honest mistake in design), but the persistence after discovery would indicate extraction-covering (mandatrophy drift).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(middle_income_windfall_capture, empirical, 'Whether the upward redistribution is an intended feature or an unintended consequence.').

omega_variable(
    reading_contention_over_behavioral_premise,
    'Do the freedom-floor reading and the universality-paradox reading accept the dependency-trap reading''s empirical premises about employment reduction and distributional loss, or do they dispute the premises themselves?',
    'Comparative reading analysis: do the sibling readings concede employment elasticity but claim it is offset by autonomy gains (freedom-floor), or do they dispute the elasticity itself (universality-paradox)? This determines whether the readings coexist or whether one forecloses the other on empirical grounds.',
    'If all readings accept the employment reduction but interpret its meaning differently (extraction vs. autonomy, efficiency vs. incoherence), they coexist. If the freedom-floor reading disputes the employment reduction entirely, it forecloses the dependency-trap reading on factual grounds. If the universality-paradox reading claims both readings'' empirical premises are simultaneously true but incoherent, it influences both without foreclosing either.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contention_over_behavioral_premise, conceptual, 'The relationship between readings is determined by whether they dispute premises or interpretations of shared premises.').

omega_variable(
    suppression_mechanism_structural_vs_rhetorical,
    'Is the measured suppression (0.71) structural (legal/institutional barriers to alternative welfare configurations, political entrenchment making program restoration difficult) or rhetorical (opposition framed as anti-redistribution, making dissent costly but not legally barred)?',
    'Post-exit suppression trajectory: if a jurisdiction dismantles unconditional income support and restores targeted programs without legal/constitutional barrier, the suppression is primarily rhetorical. If restoration is blocked by constitutional interpretation, political super-majority requirements, or path-dependent institutional lock-in, the suppression is structural. Survey of working-poor agents on whether their suppression persists after the constraint is removed.',
    'If structural, the constraint''s extractive force is higher than the measured suppression suggests (target cannot exit even if the arrangement is removed). If rhetorical/internalized, the working poor carry some suppression in their cognition even after the constraint''s removal, indicating identity-lock or ideology-driven internalization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_rhetorical, empirical, 'Whether suppression is structural or internalized/rhetorical.').

omega_variable(
    kernel_reading_foreclosure_test,
    'Does the dependency-trap reading logically foreclose the freedom-floor reading, or do they coexist as interpretatively incommensurable but simultaneously holdable positions?',
    'Formal logical analysis: if one reading''s core axiom directly contradicts the other''s (e.g., ''employment elasticity is zero'' vs. ''employment elasticity is nonzero''), they may coexist on empirical grounds (different factual premises). If they interpret the same facts through incompatible frameworks (e.g., employment reduction is extraction vs. autonomy enablement), they coexist. Foreclosure occurs only if one reading''s acceptance logically precludes the other''s framework, not merely its conclusion.',
    'If foreclosure: the engine computes one reading as invalid given the other''s adoption, routing to contradiction detection. If coexistence: both readings remain live in the corpus, modeling genuine political disagreement. This determines the reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Whether readings are logically incommensurable or merely empirically disputed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(unco_tr_t0, observed).
narrative_ontology:measurement(unco_tr_t2, unconditional_income_support__dependency_trap_reading, theater_ratio, 2, 0.31).
narrative_ontology:measurement_basis(unco_tr_t2, observed).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__dependency_trap_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement_basis(unco_tr_t4, observed).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__dependency_trap_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement_basis(unco_tr_t8, observed).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__dependency_trap_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement_basis(unco_tr_t12, observed).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__dependency_trap_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement_basis(unco_tr_t16, observed).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__dependency_trap_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(unco_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(unco_be_t0, observed).
narrative_ontology:measurement(unco_be_t2, unconditional_income_support__dependency_trap_reading, base_extractiveness, 2, 0.66).
narrative_ontology:measurement_basis(unco_be_t2, observed).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__dependency_trap_reading, base_extractiveness, 4, 0.7).
narrative_ontology:measurement_basis(unco_be_t4, observed).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__dependency_trap_reading, base_extractiveness, 8, 0.75).
narrative_ontology:measurement_basis(unco_be_t8, observed).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__dependency_trap_reading, base_extractiveness, 12, 0.77).
narrative_ontology:measurement_basis(unco_be_t12, observed).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__dependency_trap_reading, base_extractiveness, 16, 0.78).
narrative_ontology:measurement_basis(unco_be_t16, observed).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__dependency_trap_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(unco_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(unco_su_t0, observed).
narrative_ontology:measurement(unco_su_t2, unconditional_income_support__dependency_trap_reading, suppression_requirement, 2, 0.62).
narrative_ontology:measurement_basis(unco_su_t2, observed).
narrative_ontology:measurement(unco_su_t4, unconditional_income_support__dependency_trap_reading, suppression_requirement, 4, 0.65).
narrative_ontology:measurement_basis(unco_su_t4, observed).
narrative_ontology:measurement(unco_su_t8, unconditional_income_support__dependency_trap_reading, suppression_requirement, 8, 0.69).
narrative_ontology:measurement_basis(unco_su_t8, observed).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__dependency_trap_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement_basis(unco_su_t12, observed).
narrative_ontology:measurement(unco_su_t16, unconditional_income_support__dependency_trap_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement_basis(unco_su_t16, observed).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__dependency_trap_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(unco_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__dependency_trap_reading, 0.18).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'unconditional_income_support' (the policy arrangement itself). Three structurally distinct constraints instantiate three readings: dependency_trap_reading (this file, snare type, high extractiveness), freedom_floor_reading (rope/coordinate type, autonomy-enabling, low extractiveness from beneficiary seats), and universality_paradox_reading (tangled_rope type, political incoherence, contested effectiveness). The three readings share a referent (unconditional income support policy) but instantiate different constraints with different epsilon values, beneficiary/victim structures, and stakeholder experiences. All three are linked via this network field. The decomposition follows DP-001 (epsilon-invariance principle): measuring the constraint differently (employment effects, distributional outcomes, autonomy impacts) produces substantially different epsilon values, signaling multiple constraints, not one constraint viewed from multiple angles. Each reading is complete, internally coherent, and falsifiable on its own terms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_income_support__dependency_trap_reading, powerful, 0.15).
constraint_indexing:directionality_override(unconditional_income_support__dependency_trap_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
