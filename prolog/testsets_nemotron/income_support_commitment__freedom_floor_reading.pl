% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Universal Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_floor_reading of the
 *   income_support_commitment kernel. The kernel is the persistent societal
 *   commitment to provide income support; the contest is over *what kind* of
 *   support and *what it is for*. This reading holds that the support must be
 *   universal, unconditional, and sufficient to function as a freedom floor —
 *   a material baseline that makes exit from coercive relationships
 *   (employer, partner, patron) structurally possible. The sibling readings
 *   (dependency_trap_reading, targeting_efficiency_reading) are different
 *   constraints with different beneficiary/victim structures and different ε,
 *   generated from the same kernel commitment. This story authors only this
 *   reading's structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.08).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.12).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Universal Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '5b2bc08c-4791-4bd1-b411-e5b77aabb221').
narrative_ontology:cs_kernel_codification('5b2bc08c-4791-4bd1-b411-e5b77aabb221', implicit).
narrative_ontology:cs_authority_grounding('5b2bc08c-4791-4bd1-b411-e5b77aabb221', distributed).
narrative_ontology:cs_reading_relation('5b2bc08c-4791-4bd1-b411-e5b77aabb221', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b2bc08c-4791-4bd1-b411-e5b77aabb221', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('5b2bc08c-4791-4bd1-b411-e5b77aabb221', foundational, autonomy_precondition_for_agency).
narrative_ontology:cs_axiom_status(autonomy_precondition_for_agency, holdable).
narrative_ontology:cs_axiom_grounding('5b2bc08c-4791-4bd1-b411-e5b77aabb221', autonomy_precondition_for_agency, deontological).
narrative_ontology:cs_axiom('5b2bc08c-4791-4bd1-b411-e5b77aabb221', foundational, universality_eliminates_stigma).
narrative_ontology:cs_axiom_status(universality_eliminates_stigma, holdable).
narrative_ontology:cs_axiom_grounding('5b2bc08c-4791-4bd1-b411-e5b77aabb221', universality_eliminates_stigma, conventional).
narrative_ontology:cs_reference_frame('5b2bc08c-4791-4bd1-b411-e5b77aabb221', postwar_conditional_welfare_settlement).
narrative_ontology:cs_drift_state('5b2bc08c-4791-4bd1-b411-e5b77aabb221', precarity_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5b2bc08c-4791-4bd1-b411-e5b77aabb221', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_entrepreneurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, taxpayers_high_income).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, employers_low_wage_sectors).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, autonomy_as_precondition_for_agency).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, dignity_as_non_instrumental_good).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, exit_capacity_disciplines_labor_market_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provide full-time care for dependents without formal compensation. The income floor enables them to sustain this labor without dependence on a partner or familial coercion, and to re-enter paid work on their own terms when care demands shift.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, unpaid_caregivers, beneficiary,
    moderate, biographical, constrained, national).

% Cycle through gig, seasonal, and zero-hours contracts with no unemployment insurance. The floor converts intermittent wages into a livable baseline, giving them credible refusal power against exploitative schedules and unsafe conditions.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, constrained, national).

% Depend financially on an abusive partner or household. Universality means no means-test interview with the abuser present, no stigma of 'welfare', and an exit fund that exists before the decision to leave — the floor is the material condition of escape.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, immediate, identity_locked, local).

% Pursue creative or speculative ventures with high failure risk and delayed returns. The floor absorbs the cost of experimentation without forcing premature commercialization or dependence on gatekeeping patrons.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_entrepreneurs, beneficiary,
    moderate, biographical, mobile, national).

% Bear the marginal tax incidence of funding the floor. Their exit option is capital mobility and tax planning, but the universality of the benefit reduces the political salience of targeted resistance — no 'deserving/undeserving' fracture to exploit.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, taxpayers_high_income, payer,
    powerful, biographical, arbitrage, national).

% Face upward wage pressure because workers no longer accept poverty wages to survive. They lobby for conditionality and work requirements to restore the discipline of desperation; their structural interest is in a disciplined reserve army of labor.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, employers_low_wage_sectors, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__freedom_floor_reading, employers_low_wage_sectors, agenda_setter).

% Administer the universal payment with minimal gatekeeping — no eligibility verification, no sanction regimes, no caseworker discretion. Their institutional role shifts from moral policing to operational throughput, reducing bureaucratic theater.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, welfare_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Object to the fiscal cost and moral hazard narrative. They would means-test, condition, and sanction if present; their exclusion from the design is what makes the floor a floor rather than a ladder with missing rungs.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, fiscal_conservatives, excluded,
    organized, generational, mobile, national).

% Track labor market attachment, health outcomes, care labor valuation, and entrepreneurial activity under the floor. Their read is contested: some see validated coordination, others see latent dependency.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of funding a social floor that no individual can afford to provide alone but everyone benefits from existing in: a society where no one's survival depends on submitting to another's power. The tax base coordinates the resource pool; the universal payment coordinates the distribution without exclusion errors.
% TRANSFER_FUNCTION: Moves resources from the progressive tax base (concentrated on high income/wealth) to every resident as an unconditional monthly payment. No transfer is conditioned on labor status, household composition, or moral assessment — the transfer *is* the coordination, not a leak from it.
% ABSENT_VOICES: Future generations who inherit the fiscal architecture; non-citizen residents in jurisdictions where the floor is citizen-only (a common compromise that reintroduces exclusion at the border); employers who would prefer a sub-floor wage floor enforced by desperation.
% DISAPPEARANCE_RATIONALE: If the universal floor vanished overnight, caregivers would lose the only income recognizing their labor; precarious workers would lose refusal power; abuse survivors would lose the pre-positioned exit fund; artists and entrepreneurs would lose the risk-absorption buffer. Employers would regain unilateral wage-setting power in low-wage sectors. The social contract would revert to conditional, stigmatized, administratively complex transfers.
% FOUNDING_PROBLEM: The post-war welfare compromise tied income support to labor market attachment and family form, leaving caregivers, precarious workers, and abuse survivors dependent on relational or bureaucratic gatekeepers. The founding problem was: how to secure material existence without conditioning it on subordination?
% FOUNDING_PROBLEM_CORROBORATION: The 1970s negative income tax experiments (US/Canada) documented labor supply effects far smaller than predicted; the Alaska Permanent Fund (universal, unconditional, 40+ years) shows no dependency atrophy; Finnish basic income trial (2017-2018) found improved well-being and no employment reduction. Corroboration comes from empirical evaluation literature outside the advocacy coalition.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__freedom_floor_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.08) because the constraint operates as a pure coordination mechanism: a progressive tax base funds a universal payment with no means-test overhead, no sanction regime, and no moral gatekeeping. The suppression metric (0.12) reflects only the tax enforcement required — no behavioral compliance is demanded of recipients. Theater ratio (0.15) captures the residual administrative performance of 'verifying universality' (identity checks, residency). Accessibility collapse is low (0.25) because alternatives (targeted welfare, charity, familial dependence) remain legally and practically available — the floor does not forbid them, it makes them optional. Resistance (0.35) comes from organized employer interests and fiscal conservatives who benefit from the discipline of desperation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (caregivers, precarious workers, abuse survivors, artists/entrepreneurs) are structural beneficiaries — they receive the transfer without conditions and gain exit capacity. Their directionality d is near the beneficiary end (low d, low effective χ). Payers (high-income taxpayers, low-wage employers) bear the cost but with very different exit options: taxpayers have arbitrage-grade exit (capital mobility), employers have constrained exit (cannot relocate low-wage labor markets easily). The engine derives d from these structural positions. No victims are declared — universality eliminates the means-test stigma that makes targeted welfare extractive toward its recipients.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question is live: the founding problem (securing existence without subordination) persists because labor market precarity has increased, care labor remains unpaid, and intimate partner violence remains endemic. The constraint is not a degraded version of a past solution — it is the solution that was never fully tried. The dependency_trap_reading claims the floor *would* create a new mandatrophy (state dependence replacing market dependence); this reading holds that exit capacity *prevents* dependence by making all dependence voluntary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_floor_vs_dependency_trap,
    'Does a universal unconditional floor empirically produce net autonomy gains (exit capacity, risk-taking, care recognition) or net dependency (labor withdrawal, skill atrophy, state reliance)?',
    'Longitudinal panel data from universal pilot programs (Finland, Kenya, US negative income tax follow-ups) tracking labor supply, entrepreneurship, care hours, health, and subjective autonomy over 10+ years, compared to matched conditional-transfer populations.',
    'If autonomy gains dominate, the rope classification holds and the floor is a genuine coordination solution. If dependency effects dominate, the constraint reclassifies toward tangled_rope (coordination + extraction from future taxpayers) or snare (if the floor becomes a trap). This is the core empirical contest between this reading and the dependency_trap_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_floor_vs_dependency_trap, empirical, 'The central empirical disagreement between freedom_floor_reading and dependency_trap_reading.').

omega_variable(
    universality_fiscal_sustainability,
    'Can a universal floor at a dignity-sufficient level be funded by a progressive tax base without either (a) crowding out other public goods or (b) requiring regressive consumption taxes that erode the floor''s own value?',
    'Dynamic fiscal modeling incorporating behavioral responses (labor supply, tax avoidance, capital flight) under alternative tax mixes (wealth, carbon, financial transaction, VAT). Compare to status quo welfare + tax expenditure costs.',
    'If unsustainable without regressive financing, the constraint acquires extractive structure (burden shifts to low-income consumers) and ε rises. If sustainably progressive, the rope classification is stable. This omega links to the targeting_efficiency_reading''s claim that universality is fiscally irresponsible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universality_fiscal_sustainability, empirical, 'Whether the coordination problem (funding level + tax base) has a stable solution at dignity-sufficient universality.').

omega_variable(
    employer_power_countervailing,
    'Does the exit capacity provided by the floor structurally constrain employer wage-setting power in low-wage sectors, or do employers adapt through automation, offshoring, or labor market segmentation that preserves monopsony power?',
    'Sectoral wage and employment dynamics after floor implementation: compare low-wage sectors with high automation potential vs. high care/content dependence; track vacancy durations, wage growth, and working condition improvements.',
    'If employer power is constrained, the floor''s coordination function extends to labor market discipline (vindicated_proposition: exit_capacity_disciplines_labor_market_power). If employers adapt without conceding, the floor becomes a subsidy to low-wage employers (extraction from taxpayers to employers) — tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_power_countervailing, empirical, 'Whether the floor''s labor market exit effect materializes as employer discipline or employer subsidy.').

omega_variable(
    committer_framing_delta,
    'This constraint is one reading of the income_support_commitment kernel. How does the freedom_floor_reading''s beneficiary structure (caregivers, precarious workers, abuse survivors, artists/entrepreneurs; no victims) differ structurally from the sibling readings, and where is the disagreement located?',
    'Compare the three readings'' base_properties: dependency_trap_reading declares victims (future taxpayers, workers who withdraw) and higher ε; targeting_efficiency_reading declares beneficiaries (administrators, fiscal conservatives) and victims (universal recipients who ''don''t need it''). The disagreement is located in: (1) who the constraint recognizes as legitimate claimants, (2) whether universality is a feature or bug, (3) whether exit capacity is autonomy or moral hazard.',
    'The three readings instantiate different constraints with different ε, different beneficiary/victim sets, different claimed types. They are not the same constraint measured differently — they are different constraints generated from the same kernel commitment. This omega documents the committer-frame decomposition required by DP-001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_delta, conceptual, 'Structural delta between this reading and its siblings in the kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__freedom_floor_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__freedom_floor_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__freedom_floor_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__freedom_floor_reading, base_extractiveness, 10, 0.1).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.09).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__freedom_floor_reading, base_extractiveness, 30, 0.08).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__freedom_floor_reading, base_extractiveness, 40, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__freedom_floor_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__freedom_floor_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__freedom_floor_reading, suppression_requirement, 20, 0.13).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__freedom_floor_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__freedom_floor_reading, suppression_requirement, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__freedom_floor_reading, 0.05).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__targeting_efficiency_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, labor_market_regulation).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, care_infrastructure_provision).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, housing_affordability_floor).

% DUAL FORMULATION NOTE:
% The income_support_commitment kernel decomposes into three constraint stories (this file plus two siblings). Each has its own ε, beneficiaries/victims, and claimed_type. This reading claims rope (coordination); dependency_trap_reading claims tangled_rope (coordination + extraction from future taxpayers); targeting_efficiency_reading claims rope (coordination) but with different beneficiaries (administrators, fiscal disciplinarians) and victims (excluded universal recipients). All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__freedom_floor_reading, institutional, 0.15).
constraint_indexing:directionality_override(income_support_commitment__freedom_floor_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
