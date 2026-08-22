% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Autonomy-Enabling Floor
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'freedom_floor_reading' of the
 *   unconditional_income_support kernel. The reading frames universal basic
 *   income as an autonomy infrastructure — a coordination mechanism that
 *   solves the structural coercion embedded in labor markets dependent on
 *   desperation, the invisibility of care work, and the stigmatizing
 *   machinery of means-tested welfare. The claimed type is rope: a genuine
 *   coordination function with minimal extraction, where participants are net
 *   beneficiaries and alternatives (targeted welfare, charity, family
 *   dependence) are not suppressed but rendered unnecessary. The metrics
 *   reflect this: low extractiveness (0.28, mostly fiscal transfer from
 *   high-income), very low suppression (0.12, no behavioral conditionality),
 *   near-zero theater (0.08, administrative simplicity is real not
 *   performed). The kernel contest is documented in omegas and cs_structure;
 *   this story stands alone as an ε-invariant constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.28).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.12).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support as Autonomy-Enabling Floor").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, '0cb5cf5d-383a-4f5b-b78f-0d813ba61f77').
narrative_ontology:cs_kernel_codification('0cb5cf5d-383a-4f5b-b78f-0d813ba61f77', implicit).
narrative_ontology:cs_authority_grounding('0cb5cf5d-383a-4f5b-b78f-0d813ba61f77', distributed).
narrative_ontology:cs_reading_relation('0cb5cf5d-383a-4f5b-b78f-0d813ba61f77', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('0cb5cf5d-383a-4f5b-b78f-0d813ba61f77', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('0cb5cf5d-383a-4f5b-b78f-0d813ba61f77', foundational, autonomy_as_freedom_from_coercion).
narrative_ontology:cs_axiom_status(autonomy_as_freedom_from_coercion, holdable).
narrative_ontology:cs_axiom_grounding('0cb5cf5d-383a-4f5b-b78f-0d813ba61f77', autonomy_as_freedom_from_coercion, deontological).
narrative_ontology:cs_axiom('0cb5cf5d-383a-4f5b-b78f-0d813ba61f77', foundational, universal_provision_eliminates_stigma).
narrative_ontology:cs_axiom_status(universal_provision_eliminates_stigma, holdable).
narrative_ontology:cs_axiom_grounding('0cb5cf5d-383a-4f5b-b78f-0d813ba61f77', universal_provision_eliminates_stigma, empirically_contingent).
narrative_ontology:cs_reference_frame('0cb5cf5d-383a-4f5b-b78f-0d813ba61f77', post_war_welfare_state_conditionality).
narrative_ontology:cs_drift_state('0cb5cf5d-383a-4f5b-b78f-0d813ba61f77', contemporary_precarity_intensification, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('0cb5cf5d-383a-4f5b-b78f-0d813ba61f77', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, informal_caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists_creative_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, long_term_unemployed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, taxpayers_middle_income).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, taxpayers_high_income).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, taxpayers_middle_income).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, welfare_bureaucracy).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, employers_low_wage).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, universal_basic_income_autonomy_thesis).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, labor_decommodification_principle).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, welfare_stigma_elimination_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers in gig, contract, and low-wage jobs who face unpredictable schedules, no benefits, and immediate income loss from any disruption. The floor lets them refuse exploitative shifts, invest in skills, or bridge gaps between contracts without catastrophic loss. Exit from bad jobs becomes possible rather than theoretical.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, biographical, constrained, national).

% People (disproportionately women) providing unpaid care for children, elders, or disabled relatives. Current welfare systems either ignore this work or condition support on labor-market attachment. The floor recognizes care as socially necessary work and gives caregivers leverage to negotiate support or exit abusive dynamics.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, informal_caregivers, beneficiary,
    powerless, biographical, identity_locked, national).

% Cultural producers whose work has high social value but unreliable market returns. The floor decouples creative production from immediate commercial viability, enabling risk-taking and long-form work. Many currently rely on precarious side work or grant cycles that shape output toward funder priorities.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, artists_creative_workers, beneficiary,
    moderate, biographical, constrained, national).

% People in financially coercive relationships (domestic abuse, exploitative employment, trafficking-adjacent situations) who cannot leave because they have no independent income. The floor provides the material basis for exit — a rare structural intervention that directly reduces the coercion enabling abuse.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, immediate, trapped, local).

% Workers displaced by structural change (automation, trade, regional decline) who face stigma, skill atrophy, and welfare conditionality that penalizes retraining. The floor removes the 'job search' performance requirement and lets them pursue genuine reintegration on their own timeline.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, long_term_unemployed, beneficiary,
    powerless, biographical, constrained, national).

% High-income earners and capital owners who bear the primary fiscal incidence of funding the floor through progressive taxation. They have political voice, capital mobility, and access to tax optimization — their exit option is structural (lobbying, relocation, avoidance) rather than personal.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, taxpayers_high_income, payer,
    powerful, biographical, arbitrage, national).

% Middle-income households who pay net positive taxes but also face precarity risk. They experience the floor as both a contribution and insurance — the net position depends on life trajectory. Their exit is constrained by lack of mobility and dependence on the same labor market the floor modifies.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, taxpayers_middle_income, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, taxpayers_middle_income, beneficiary).

% Administrative apparatus of means-tested programs (caseworkers, eligibility systems, compliance enforcement). The floor threatens their institutional rationale and headcount — but also absorbs the most punitive, stigmatizing, and error-prone functions. Their situation is ambivalent: loss of mission vs. relief from administering cruelty.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, welfare_bureaucracy, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, welfare_bureaucracy, payer).

% Employers in sectors relying on desperate labor (hospitality, agriculture, care, logistics). The floor raises the reservation wage — workers can refuse dangerous, degrading, or underpaid jobs. This is experienced as a cost increase and 'labor shortage' narrative, but structurally it forces productivity investment or business model change.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, employers_low_wage, payer,
    organized, biographical, mobile, national).

% Researchers evaluating labor supply effects, fiscal sustainability, inflation dynamics, and distributional outcomes. Their seat is analytical — they do not collect or pay but shape the evidence base that legitimizes or delegitimizes the floor. Key debate: whether Alaska Permanent Fund and Kenya UBI trial minimal labor supply effects generalize to national-scale implementation.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, policy_analysts_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of providing a universal, non-stigmatizing income floor that enables genuine labor market exit without means-testing bottlenecks, administrative exclusion errors, or moral hazard traps. The floor coordinates: (1) risk-pooling across the population for market-income volatility, (2) recognition of unpaid care and creative work as socially necessary, (3) a baseline bargaining position for every worker.
% TRANSFER_FUNCTION: Moves fiscal resources from progressive taxation (primarily high-income/wealth) to universal individual payments. Net transfer is negative for top deciles, positive for bottom half, near-zero for middle deciles depending on tax design. No behavioral conditionality — the transfer is not contingent on job search, training, family structure, or disability certification.
% ABSENT_VOICES: Future generations (fiscal sustainability debate), undocumented migrants (excluded from most proposals despite high precarity), people with high disability-related costs (floor may not cover extraordinary needs without supplements), Global South workers whose labor produces the wealth taxed for Northern floors. The excluded are those the universal claim 'everyone' does not structurally reach.
% DISAPPEARANCE_RATIONALE: If the floor vanished overnight, millions of precarious workers would lose refusal power, caregivers would lose leverage, abuse survivors would lose exit capital, and the welfare bureaucracy would revert to full means-testing with its stigma, errors, and coercion. The labor market would re-coerce at the bottom. The world rearranges because the floor changes the structural conditions of participation, not just distribution.
% FOUNDING_PROBLEM: The post-war welfare state was built around the male breadwinner in stable industrial employment. It fails: (1) the rise of precarious, fragmented, low-wage labor; (2) the invisibility of unpaid care work; (3) the stigmatizing, coercive machinery of means-testing; (4) the inability of targeted programs to reach the most excluded. The floor was proposed as a structural update — replacing conditionality with universality, stigma with rights, bureaucracy with infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by: (a) OECD labor market insecurity indicators (rising non-standard work, 2010-2024), (b) feminist political economy (care work undervaluation, Nancy Folbre, Marilyn Waring), (c) welfare rights organizations documenting means-test exclusion errors (UK DWP own estimates: 1M+ eligible non-recipients), (d) abuse survivor advocacy (financial coercion as primary barrier to exit). No major institution outside the UBI advocacy coalition disputes the problem diagnosis; the contest is over whether a floor solves it or creates worse problems.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.28) because the floor is funded by progressive taxation — a transfer from high-income/wealth to universal recipients. This is not extractive in the DR sense (no party collects rents from constraint operation); the 'extraction' measured is the fiscal cost to payers. Suppression is minimal (0.12) because the constraint operates by expanding options, not foreclosing them — no work requirements, no means-tests, no compliance machinery. Theater is near-zero because the administrative apparatus is genuinely simpler than the means-tested system it replaces (no eligibility verification, no caseworker discretion, no appeals system). Accessibility collapse is low (0.25) because targeted programs, charity, and family support remain legally and practically available — the floor makes them less necessary, not impossible. Resistance is moderate (0.42) from fiscal conservatives, low-wage employers, and welfare bureaucracy — but this is political contestation, not resistance from the constrained (who are beneficiaries).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (especially high-income taxpayers and low-wage employers) will compute higher effective extraction than beneficiary seats. The analytical observer seat sees the full structure: a transfer that is Pareto-improving in autonomy terms but redistributive in fiscal terms. The engine's per-seat classification should show: beneficiaries → rope (coordination with subsidy), high-income payers → rope with mild extraction, low-wage employers → tangled_rope (coordination of labor standards via floor, but they pay), welfare bureaucracy → piton (atrophied means-test mission, theatrical maintenance of relevance). The claimed type 'rope' is the analytical observer's synthesis; seat divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (precarious workers, caregivers, artists, abuse survivors, long-term unemployed) are structurally powerless or moderate with constrained/identity-locked/trapped exit — they gain autonomy, bargaining power, and exit capacity. The floor subsidizes them (d ≈ 0.0-0.2). Payers: high-income taxpayers (powerful, arbitrage exit) bear net fiscal cost but gain social stability and reduced coercion externalities — d ≈ 0.4-0.5 (near-symmetric). Middle-income taxpayers (moderate, constrained exit) are near-neutral net (payer + beneficiary) — d ≈ 0.5. Low-wage employers (organized, mobile exit) face higher reservation wages — they experience the constraint as extractive (d ≈ 0.7) but their exit is capital mobility, not personal coercion. Welfare bureaucracy (institutional, constrained exit) is ambivalent: loses mission but sheds worst functions — d ≈ 0.4. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war welfare mismatch with contemporary labor markets and care structures) remains live — corroborated by independent labor economists, feminist political economy, welfare rights orgs, and abuse survivor advocates. The floor is not a solution to a dead problem; it is a structural response to intensifying precarity. Mandatrophy risk is low for this reading: the coordination function (autonomy floor) strengthens as labor markets fragment further. The risk is capture by sibling readings — dependency_trap_reading frames the same policy as extraction; universality_paradox_reading warns the coalition is unstable. This reading's mandatrophy status: unresolved but live, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the freedom_floor_reading a distinct constraint from its siblings, or a different measurement of the same constraint?',
    'Apply the ε-invariance test: if changing the reading changes the beneficiary/victim structure, the extractiveness referent, or the coordination/extraction decomposition, they are different constraints. This reading has beneficiaries (precarious workers, caregivers, artists, abuse survivors), no victims, ε=0.28, type=rope. Dependency_trap_reading would have victims (taxpayers, ''deserving poor'' crowded out), beneficiaries (alleged ''idle''), higher ε. Universality_paradox_reading would have contested beneficiaries, implementation-dependent ε. The structural deltas are disjoint — these are three constraints linked by network.affects_constraints, not one constraint with three measurements.',
    'If readings are distinct constraints, the engine classifies each independently and contamination analysis traces influence across the family. If they are one constraint, the classification becomes observer-relative and the framework''s core principle (ε-invariance) fails. This omega documents the decomposition commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are structurally distinct constraints (per ε-invariance) or observer perspectives on one constraint.').

omega_variable(
    labor_supply_effects_generalizability,
    'Do the minimal labor supply effects from Alaska Permanent Fund and Kenya UBI trials generalize to a national-scale, permanent, livable floor?',
    'Large-scale saturation pilots (e.g., 3-5 year universal pilots in diverse OECD regions) with rigorous labor market tracking, or natural experiments from policy adoption in smaller nations. Key variables: permanence (vs. temporary), level (survival vs. supplemental), universality (vs. targeted), and macroeconomic feedback (inflation, wage floors, sectoral shifts).',
    'If labor supply effects are substantially negative at scale, extractiveness rises (fiscal cost increases, political sustainability drops) and the constraint may shift toward tangled_rope (coordination function eroded by extraction). If effects remain minimal, the rope classification holds. This is the central empirical uncertainty for the reading''s metric stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_effects_generalizability, empirical, 'Generalizability of minimal labor supply response evidence to national permanent floor.').

omega_variable(
    care_work_recognition_vs_commodification,
    'Does an unconditional floor recognize care work as socially necessary, or does it commodify care by pricing it at the floor level and enabling state withdrawal from care infrastructure?',
    'Comparative analysis of care infrastructure investment pre/post floor implementation in jurisdictions with strong vs. weak pre-existing care systems. Track: public childcare/eldercare funding, care worker wages, family leave policy, and care quality metrics.',
    'If the floor triggers care infrastructure retrenchment (state says ''families now have money, they can buy care''), the coordination function for caregivers degrades — the floor becomes a subsidy to private care markets rather than recognition of unpaid care. This would shift the constraint toward tangled_rope (coordination + extraction via privatization). If care infrastructure expands (floor enables political demand for quality care), the rope classification strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(care_work_recognition_vs_commodification, empirical, 'Whether the floor structurally recognizes care or enables its commodification and public retreat.').

omega_variable(
    fiscal_sustainability_under_shocks,
    'Can the floor''s funding mechanism (progressive taxation) withstand correlated macroeconomic shocks (recession + inflation + demographic shift) without degrading the floor''s real value or triggering austerity that converts it into a targeted, stigmatized residual?',
    'Stress-test modeling of floor financing under 2008-style financial crisis, 1970s-style stagflation, and 2030s-style demographic aging scenarios. Key variables: tax base elasticity, sovereign currency issuance capacity, political coalition durability, and automatic stabilizer design.',
    'If the floor collapses into means-tested residual under stress, the coordination function fails — the constraint becomes a scaffold that didn''t sunset but degraded. If it holds as universal, the rope classification is robust. This omega captures the temporal dimension: the floor''s type may be path-dependent on crisis performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_under_shocks, empirical, 'Whether the floor''s universal character survives correlated macro-fiscal shocks.').

omega_variable(
    abuse_survivor_exit_actualization,
    'Does the floor materially enable abuse survivors to exit coercive situations, or do structural barriers (housing shortage, legal dependency, immigration status, trauma) prevent actualization of the theoretical exit option?',
    'Mixed-methods studies tracking abuse survivor outcomes in jurisdictions with vs. without income floors: shelter utilization, return-to-abuser rates, housing stability, legal independence, child custody outcomes. Control for co-occurring services (legal aid, counseling, housing vouchers).',
    'If the floor''s autonomy promise for abuse survivors is largely theoretical (exit blocked by other constraints), the beneficiary claim for this group is overstated. The constraint remains a rope for other beneficiaries but has a snare-like gap for the most vulnerable. This would require decomposing the constraint by population segment — violating ε-invariance unless the floor is modeled as a constraint family with population-specific ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(abuse_survivor_exit_actualization, empirical, 'Whether the floor''s theoretical exit provision for abuse survivors translates to actualized autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__freedom_floor_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__freedom_floor_reading, theater_ratio, 16, 0.08).
narrative_ontology:measurement(unco_tr_t24, unconditional_income_support__freedom_floor_reading, theater_ratio, 24, 0.07).
narrative_ontology:measurement(unco_tr_t32, unconditional_income_support__freedom_floor_reading, theater_ratio, 32, 0.08).
narrative_ontology:measurement(unco_tr_t40, unconditional_income_support__freedom_floor_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__freedom_floor_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__freedom_floor_reading, base_extractiveness, 16, 0.29).
narrative_ontology:measurement(unco_be_t24, unconditional_income_support__freedom_floor_reading, base_extractiveness, 24, 0.28).
narrative_ontology:measurement(unco_be_t32, unconditional_income_support__freedom_floor_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(unco_be_t40, unconditional_income_support__freedom_floor_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__freedom_floor_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(unco_su_t8, unconditional_income_support__freedom_floor_reading, suppression_requirement, 8, 0.18).
narrative_ontology:measurement(unco_su_t16, unconditional_income_support__freedom_floor_reading, suppression_requirement, 16, 0.14).
narrative_ontology:measurement(unco_su_t24, unconditional_income_support__freedom_floor_reading, suppression_requirement, 24, 0.12).
narrative_ontology:measurement(unco_su_t32, unconditional_income_support__freedom_floor_reading, suppression_requirement, 32, 0.12).
narrative_ontology:measurement(unco_su_t40, unconditional_income_support__freedom_floor_reading, suppression_requirement, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__universality_paradox_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, means_tested_welfare_conditionality).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, labor_market_coercion_floor).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, care_infrastructure_public_provision).

% DUAL FORMULATION NOTE:
% This reading decomposes the unconditional_income_support kernel into an autonomy-enabling floor (rope). The dependency_trap_reading models the same policy as extraction (snare/tangled_rope). The universality_paradox_reading models the political coalition instability. All three are linked here. The means_tested_welfare_conditionality constraint is the status quo this floor would replace (coordination via coercion). The labor_market_coercion_floor constraint is the structural coercion this floor dismantles. The care_infrastructure_public_provision constraint is a complementary (or competing) coordination mechanism for care work recognition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_income_support__freedom_floor_reading, institutional, 0.35).
constraint_indexing:directionality_override(unconditional_income_support__freedom_floor_reading, organized, 0.7).
constraint_indexing:directionality_override(unconditional_income_support__freedom_floor_reading, powerful, 0.45).
constraint_indexing:directionality_override(unconditional_income_support__freedom_floor_reading, powerless, 0.1).
constraint_indexing:directionality_override(unconditional_income_support__freedom_floor_reading, moderate, 0.5).
constraint_indexing:directionality_override(unconditional_income_support__freedom_floor_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
