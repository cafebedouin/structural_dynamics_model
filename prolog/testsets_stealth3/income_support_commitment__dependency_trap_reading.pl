% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__dependency_trap_reading, []).

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
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support — Dependency-Trap Reading
 *   domain: political economy/social policy/welfare state theory
 *
 * SUMMARY:
 *   A national unconditional income-support arrangement pays every resident a
 *   flat periodic amount financed by general taxation — no means test, no
 *   work requirement, no time limit. This story assesses that standing
 *   arrangement through the dependency-trap lens: the flat payment lowers the
 *   relative return to entry-level work, and for recipients who remain out of
 *   employment the gap between a constant payment and a depreciating earning
 *   capacity widens year over year. Financing falls on the working tax base;
 *   the net flow runs from labor-market-attached households toward detached
 *   ones, with the heaviest private cost — forgone skill formation at the
 *   career entry point — landing on the poor. The arrangement simultaneously
 *   performs a real pooling function (a universal subsistence floor with no
 *   gatekeeping) and runs a persistent asymmetric transfer; both halves are
 *   declared structural facts, not reconciled. KEY AGENTS (by structural
 *   relationship): - working_taxpayers: Primary target
 *   (organized/constrained) — bears the net fiscal levy with no offsetting
 *   receipt - long_term_recipients: Primary beneficiary (powerless/trapped) —
 *   collects the concentrated net transfers - at_risk_poor_youth:
 *   Dual-positioned target-beneficiary (powerless/constrained) — receives the
 *   payment while bearing forgone skill formation - welfare_ministry:
 *   Agenda-setter (institutional/constrained) — sets payment levels and the
 *   financing schedule - transfer_bureaucracy: Secondary beneficiary
 *   (organized/constrained) — staffing and budgets scale with program scope -
 *   future_taxpayers: Excluded bearer (powerless/trapped) — services deferred
 *   financing costs with no seat in deliberation -
 *   policy_evaluation_researchers: Analytical observer
 *   (analytical/analytical) — produces the evidence both directions cite
 *
 * KEY AGENTS:
 *   - working_taxpayers: Primary target (organized/constrained) — funds the universal payment while receiving little back; lever is electoral
 *   - long_term_recipients: Primary beneficiary (powerless/trapped) — payment is primary income; outside option depreciates yearly
 *   - at_risk_poor_youth: Dual-positioned payer-beneficiary (powerless/constrained) — stands at the skill-formation entry point the payment prices away
 *   - welfare_ministry: Agenda-setter (institutional/constrained) — sets parameters within statutory bounds; continuity depends on program stability
 *   - transfer_bureaucracy: Secondary beneficiary (organized/constrained) — administers enrollment and payment; scale tracks program scope
 *   - future_taxpayers: Excluded bearer (powerless/trapped) — named in actuarial tables, absent from deliberation
 *   - policy_evaluation_researchers: Analytical observer (analytical/analytical) — runs pilots and panels; influence runs through citation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.6).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.55).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support — Dependency-Trap Reading").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political economy/social policy/welfare state theory").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, 'a580bfc5-1bce-4960-ae46-85079d4818ee').
narrative_ontology:cs_kernel_codification('a580bfc5-1bce-4960-ae46-85079d4818ee', formalized).
narrative_ontology:cs_authority_grounding('a580bfc5-1bce-4960-ae46-85079d4818ee', distributed).
narrative_ontology:cs_reading_relation('a580bfc5-1bce-4960-ae46-85079d4818ee', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('a580bfc5-1bce-4960-ae46-85079d4818ee', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('a580bfc5-1bce-4960-ae46-85079d4818ee', foundational, reciprocity_conditions_legitimate_support).
narrative_ontology:cs_axiom_status(reciprocity_conditions_legitimate_support, holdable).
narrative_ontology:cs_axiom_grounding('a580bfc5-1bce-4960-ae46-85079d4818ee', reciprocity_conditions_legitimate_support, deontological).
narrative_ontology:cs_axiom('a580bfc5-1bce-4960-ae46-85079d4818ee', foundational, dependence_costs_exceed_floor_benefits).
narrative_ontology:cs_axiom_status(dependence_costs_exceed_floor_benefits, holdable).
narrative_ontology:cs_axiom_grounding('a580bfc5-1bce-4960-ae46-85079d4818ee', dependence_costs_exceed_floor_benefits, empirically_contingent).
narrative_ontology:cs_reference_frame('a580bfc5-1bce-4960-ae46-85079d4818ee', work_conditioned_safety_net).
narrative_ontology:cs_drift_state('a580bfc5-1bce-4960-ae46-85079d4818ee', contemporary_post_pilot_evidence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a580bfc5-1bce-4960-ae46-85079d4818ee', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, long_term_recipients).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, transfer_bureaucracy).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, at_risk_poor_youth).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, at_risk_poor_youth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Earn market income and remit the taxes that finance the universal payment. Their net position is negative: they fund the floor while receiving little back. Opting out is unavailable — residence and earning expose them to the levy regardless of consent. Their lever is electoral: shifting payment levels, tax schedules, or conditionality through legislation. Emigration exists for a mobile minority but severs livelihoods and family ties for most.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    organized, biographical, constrained, national).

% Receive the payment as their primary or sole income across many years. Out of steady employment, occupational skills depreciate, professional networks thin, and résumé gaps lengthen, so the wages available on re-entry fall over time while the payment stays constant. Leaving the arrangement means accepting a falling living standard against a shrinking outside option; staying means the gap widens further each year.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, long_term_recipients, beneficiary,
    powerless, biographical, trapped, national).

% Stand at the decision point where early jobs build the skills and work history that price later careers. The unconditional payment lowers the cost of postponing that first rung; those who postpone collect the payment now and enter the labor market later with weaker credentials. They receive the transfer like everyone else while bearing the forgone-formation cost privately.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, at_risk_poor_youth, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, at_risk_poor_youth, beneficiary).

% Sets the payment level, the tax schedule that finances it, and the review calendar. Commissions evaluations, answers to the legislature, and adjusts parameters within statutory bounds. Its institutional continuity depends on the program's legal stability; radical redesign requires coalition politics it does not control alone.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, welfare_ministry, agenda_setter,
    institutional, generational, constrained, national).

% Administers enrollment, payment processing, fraud screening, and reporting. Staffing and administrative budgets scale with program scope. Executes policy rather than writing it, and its employment persists exactly as long as the program does.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, transfer_bureaucracy, beneficiary,
    organized, biographical, constrained, national).

% Will service whatever financing gap today's payment levels accumulate. Named in actuarial tables but holding no vote, no lobby, and no seat in the deliberations that set current generosity; their interests enter only through the forecasts the ministry chooses to commission.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, future_taxpayers, excluded,
    powerless, generational, trapped, national).

% Run and analyze pilots, policy discontinuities, and longitudinal panels on labor supply, skill formation, and fiscal incidence. Publish findings that cut in both directions and hold no stake in either expansion or retrenchment; influence runs through citation into ministry reviews and legislative testimony.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, policy_evaluation_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__dependency_trap_reading, long_term_recipients).
narrative_ontology:fixing_cost_class(income_support_commitment__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools subsistence risk across the whole population: every resident receives a guaranteed floor regardless of employment status, eliminating means-testing gatekeeping, stigma, and take-up failure, and insuring against job loss and income volatility without administrative verification.
% TRANSFER_FUNCTION: Moves purchasing power from the taxed base — disproportionately working households and capital — to residents in equal per-capita amounts; the net flow concentrates on those with little or no market income and recurs year over year.
% ABSENT_VOICES: Future taxpayers bear the deferred financing costs but hold no seat in current deliberation; informal-economy workers and migrants excluded from the payout perimeter would contest who counts as a resident owed the floor; low-wage employers facing thinner entry-level labor supply would contest the disincentive design but enter only as lobbying outsiders.
% DISAPPEARANCE_RATIONALE: Household budgets, retail demand floors, and labor-supply patterns are configured around the payment. Overnight removal would force rapid re-entry into low-wage work, spike hardship and charity load, contract consumption in recipient-dense regions, and force emergency re-legislation within months — the world rearranges around its absence.
% FOUNDING_PROBLEM: Industrial-era mass unemployment and old-age poverty outpaced charitable and familial capacity, and means-tested relief carried heavy administrative cost, stigma, and take-up failure; the arrangement was built to guarantee subsistence independent of employment fluctuations without gatekeeping anyone.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: national statistical offices' poverty and food-insecurity series, international labor-body compilations of subsistence insecurity, and employer-association surveys documenting entry-level skill shortages all attest the underlying insecurity persists. No credible source attests the founding problem is solved; advocacy and critical seats agree on the problem's persistence while disputing the arrangement's effect on it.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.60: the net flow runs persistently from the productive base toward non-participation, and the poorest entrants bear a compounding private cost in forgone capability — substantial, but short of pure extraction because the floor function is real and every seat draws on it in adversity. Suppression is 0.55 as a raw structural property (unscaled by power or scope): financing is compulsory with no individual opt-out, but electoral exit exists and is exercised. Theater is 0.29: transfers are real and dominant, while evaluation commissions, reporting cycles, and pilot announcements grow faster than actual parameter changes. Accessibility collapse is 0.45: work, family support, charity, and migration persist as alternatives, but the flat payment erodes the pull of entry-level work and, cumulatively, the capacity to take it. Resistance is 0.55: persistent electoral contestation over levels and conditionality, plus intermittent work effort among recipients. The claim (tangled_rope) and the metrics are authored independently — the claim states this reading's structural assessment (genuine pooling plus asymmetric net flow); the metrics describe observed operation. All three tracked series share one time grid (t=0,10,20,30,40,50) so no metric row borrows another's end-state. Identity-lock dynamics: the mechanism binding long-term recipients is part economic (skill and network depreciation) and plausibly part internalized (household normalization, recipient self-concept); the split is unresolved and routed to the dependence_structural_vs_internalized omega rather than asserted.
 *
 * PERSPECTIVAL GAP:
 *   From the payer seats the arrangement computes as enforced transfer carrying compounding private costs; from the recipient seats the same structure is a lifeline whose removal is the catastrophe; from the ministry seat it is insurance priced against actuarial forecasts; from the bureaucracy seat it is employment. Identical metrics, divergent seats — the engine computes per-seat classifications from power, exit options, and directionality; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Working taxpayers sit near the full-target end: net levy, no offsetting receipt, exit limited to costly emigration or electoral reversal. Long-term recipients sit near the beneficiary end: constant receipt against a depreciating outside option. At-risk poor youth are declared in the victim set and derive a high d accordingly; their secondary receipt dampens but does not reverse the net position, because the forgone skill formation compounds against a flat payment. The ministry sits near symmetric — it trades fiscal cost for institutional stability — and the bureaucracy leans beneficiary, collecting scaled budgets without bearing the levy. Future taxpayers are pure targets with zero present voice: the widest directional gap in the story, and the reason the excluded seat matters despite holding no vote.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure extraction would erase the genuine pooling function every seat — payers included — draws on in adversity; reading it as pure coordination would erase the compounding costs the payer and at-risk seats bear. The tangled_rope claim holds both halves in view: coordination (universal floor, no gatekeeping, no take-up failure) alongside asymmetric net flow (productive base finances detachment while the poorest pay in forgone capability). The founding problem — subsistence insecurity without stigma or administrative gatekeeping — remains live, so no mandatrophy is declared. The open question the temporal series registers is whether costs have begun migrating from solving the founding problem to manufacturing a successor problem: base_extractiveness rises monotonically across the interval while the transfer function itself never lapses.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_income_support_kernel,
    'This constraint instantiates the dependency_trap_reading of the income_support_commitment kernel; at which structural element is the disagreement with the sibling readings (freedom_floor_reading, targeting_efficiency_reading) located?',
    'Adoption of a sibling reading re-indexes epsilon over the same standing arrangement: freedom_floor_reading authors low epsilon (the floor as autonomy and exit-capacity infrastructure), targeting_efficiency_reading localizes epsilon in the universality-overhead component. Long-run pilot, panel, and administrative data on labor-supply and skill-formation effects decide which reading''s descriptive claims hold.',
    'If the freedom-floor descriptive claims prevail, this constraint''s epsilon collapses toward the resource-allocation coordination floor and classification trends rope; if the targeting-efficiency framing prevails, the extraction decomposes into a targeted rope plus a universal surcharge, splitting this story in two.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_income_support_kernel, conceptual, 'Committer routing: one reading of the income_support_commitment kernel; the disagreement sits in the descriptive effect claims (cumulative dependence formation versus autonomy enablement) and in distribution-design premises.').

omega_variable(
    disincentive_causal_direction,
    'Does the unconditional payment itself cause labor-market exit and skill atrophy, or does it mainly cushion exits driven by pre-existing conditions the floor did not create?',
    'Longitudinal randomized and natural-experiment evidence (negative income tax trials, national basic-income pilots, benefit-reform discontinuities) tracking hours worked, re-entry wages, and skill proxies over five-plus years.',
    'Small durable disincentives drop epsilon toward the coordination floor and flatten the payer-seat asymmetry; large cumulative effects push epsilon upward and raise the risk of a tangled_rope-to-snare transition as dependence compounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disincentive_causal_direction, empirical, 'Causal weight of the unconditional floor in producing the dependence this reading alleges.').

omega_variable(
    dependence_structural_vs_internalized,
    'Is recipient-side exit suppression structural (skill and network depreciation raising re-entry cost) or internalized (household normalization of non-work and self-concept fused with support status)?',
    'Post-exit trajectory data where payments lapse: if search intensity and re-entry remain depressed after the fiscal barrier is removed, the internalized share is material.',
    'A large internalized share raises effective suppression above the structural measure and shifts the long-term recipient seat''s exit posture from trapped toward identity_locked, amplifying that seat''s effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependence_structural_vs_internalized, empirical, 'Mechanism split behind recipient-side suppression: economic lock-in versus identity fusion.').

omega_variable(
    intergenerational_cost_deferral,
    'How much of the arrangement''s current affordability rests on deferring financing cost to future taxpayers rather than covering it from current-year revenue?',
    'Actuarial decomposition of program financing into current revenue versus accumulated obligations, with sensitivity of solvency to demographic projections.',
    'High deferral raises the excluded future-taxpayer seat''s exposure and extends the constraint''s effective time horizon, increasing scope-amplified extraction; low deferral contains the burden within the paying generation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_cost_deferral, empirical, 'Share of the arrangement''s cost financed by intergenerational deferral rather than current revenue.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dep_trap_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dep_trap_tr_t10, income_support_commitment__dependency_trap_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(dep_trap_tr_t20, income_support_commitment__dependency_trap_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(dep_trap_tr_t30, income_support_commitment__dependency_trap_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(dep_trap_tr_t40, income_support_commitment__dependency_trap_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(dep_trap_tr_t50, income_support_commitment__dependency_trap_reading, theater_ratio, 50, 0.29).

% Extraction over time
narrative_ontology:measurement(dep_trap_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dep_trap_be_t10, income_support_commitment__dependency_trap_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(dep_trap_be_t20, income_support_commitment__dependency_trap_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(dep_trap_be_t30, income_support_commitment__dependency_trap_reading, base_extractiveness, 30, 0.57).
narrative_ontology:measurement(dep_trap_be_t40, income_support_commitment__dependency_trap_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement(dep_trap_be_t50, income_support_commitment__dependency_trap_reading, base_extractiveness, 50, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(dep_trap_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dep_trap_su_t10, income_support_commitment__dependency_trap_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(dep_trap_su_t20, income_support_commitment__dependency_trap_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(dep_trap_su_t30, income_support_commitment__dependency_trap_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement(dep_trap_su_t40, income_support_commitment__dependency_trap_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(dep_trap_su_t50, income_support_commitment__dependency_trap_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the income_support_commitment kernel decomposes into three reading-stories sharing one referent (the standing unconditional arrangement) with reading-indexed epsilon values. This member carries the dependency-trap assessment. Edges link the siblings because upstream descriptive findings (pilots, discontinuities, longitudinal panels) feed every member's epsilon and the members compete for the same legislative attention. Decomposition follows the epsilon-invariance rule: one reading, one epsilon, one beneficiary/victim structure per file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
