% ============================================================================
% CONSTRAINT STORY: income_driven_repayment_cap_evasion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_driven_repayment_cap_evasion, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_driven_repayment_cap_evasion
 *   human_readable: Income-Driven Repayment Cap Evasion
 *   domain: economic/higher_education_policy
 *
 * SUMMARY:
 *   Income-driven repayment (IDR) cap evasion represents a structural
 *   constraint where federal loan policy creates an apparent coordination
 *   mechanism (income-driven payments prevent catastrophic defaults) but is
 *   systematically undermined by mechanisms that block relief below policy
 *   intent. The constraint embeds extraction within a coordination facade:
 *   borrowers receive nominally income-responsive payments, but cap evasion
 *   techniques (income recalculation opacity, consolidation restrictions,
 *   servicer administrative delays, payment formula complexity) ensure that
 *   actual relief falls far below statutory intent. The constraint affects
 *   two distinct victim groups: low-income borrowers in general, and public
 *   service workers promised but denied loan forgiveness. The extractiveness
 *   value (0.58) reflects that the mechanism is neither pure coordination
 *   (genuine income response exists) nor pure extraction (too much
 *   coordination function remains), but rather a hybrid where the
 *   coordination function is actively undermined. The suppression value
 *   (0.65) reflects high barriers to exit: borrowers cannot default without
 *   permanent credit damage, cannot escape through income increase (graduated
 *   repayment formulas may increase payments), and face administrative
 *   opacity that blocks information access needed to navigate the system.
 *   Theater ratio (0.48) is moderate-low, indicating that the cap evasion is
 *   not purely performative — real payment reductions occur — but the
 *   performance (annual certification ritual) obscures how much relief is
 *   actually blocked.
 *
 * KEY AGENTS:
 *   - Low-Income Borrowers: Primary victim (powerless/trapped) — bear direct extraction through suppressed relief despite policy promises; no viable exit options
 *   - Public Service Workers: Secondary victim (moderate/constrained) — promised PSLF forgiveness blocked by administrative and substantive cap evasion; career-locked to low-wage service sectors
 *   - Loan Servicers: Primary beneficiary (institutional/arbitrage) — profit from extended repayment periods, income calculation opacity, and payment formula complexity; have market exit options through contract competition
 *   - Education Department Budget: Beneficiary (institutional/constrained) — cap evasion preserves fiscal predictability by deferring write-downs; constrained by legislative mandates and budget rules
 *   - Borrower Advocacy Coalition: Organized agents (organized/constrained) — see cap evasion as temporary institutional problem with sunset clause via federal servicing expansion and regulatory reform
 *   - Loan Repayment Certification Ritual: Institutional artifact (institutional/arbitrage) — annual income recertification performs coordination function but is substantially performative theater, maintained by inertia rather than necessity
 *   - Analytical Observer: Structural analysis (analytical/analytical) — identifies the constraint as a tangled rope at civilizational scale: genuine coordination exists but is systematically undermined by extraction mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_driven_repayment_cap_evasion, 0.58).
domain_priors:suppression_score(income_driven_repayment_cap_evasion, 0.65).
domain_priors:theater_ratio(income_driven_repayment_cap_evasion, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_driven_repayment_cap_evasion, extractiveness, 0.58).
narrative_ontology:constraint_metric(income_driven_repayment_cap_evasion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(income_driven_repayment_cap_evasion, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_driven_repayment_cap_evasion, tangled_rope).
narrative_ontology:human_readable(income_driven_repayment_cap_evasion, "Income-Driven Repayment Cap Evasion").
narrative_ontology:topic_domain(income_driven_repayment_cap_evasion, "economic/higher_education_policy").

domain_priors:requires_active_enforcement(income_driven_repayment_cap_evasion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_driven_repayment_cap_evasion, federal_loan_servicers).
narrative_ontology:constraint_beneficiary(income_driven_repayment_cap_evasion, education_department_budget).
narrative_ontology:constraint_victim(income_driven_repayment_cap_evasion, low_income_borrowers).
narrative_ontology:constraint_victim(income_driven_repayment_cap_evasion, public_service_workforce).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME BORROWER (SNARE) — Trapped by education debt with minimal exit capacity. Income-driven repayment offers apparent protection but the cap evasion mechanisms (income recalculation, consolidation restrictions, payment formula opacity) force payments well above sustainable levels. No meaningful alternatives: default damages credit permanently; deferment accrues interest; forbearance extends the trap. Experiences maximum extraction with high suppression.
constraint_indexing:constraint_classification(income_driven_repayment_cap_evasion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC SERVICE WORKER (SNARE) — Constrained by career choice (teaching, nursing, government work) tied to loan forgiveness promises (Public Service Loan Forgiveness). The cap evasion mechanisms (administrative processing delays, servicer income calculation errors, forgiveness eligibility narrowing) create de facto extraction by blocking promised relief. Constrained by professional identity and geographic ties; cannot easily switch sectors. High extraction with institutional suppression of PSLF pathways.
constraint_indexing:constraint_classification(income_driven_repayment_cap_evasion, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LOAN SERVICER (ROPE) — Benefits from cap evasion as a coordination mechanism. Servicers profit from extended repayment periods (servicing fees scale with loan balance and repayment duration). The constraint coordinates borrower behavior (maintains payment compliance, prevents defaults that would require write-downs) while extracting value through extended loan life and interest accrual. Has exit options (contract renegotiation, market competition for servicer contracts). Net beneficiary experiencing low effective extraction (they coordinate and benefit).
constraint_indexing:constraint_classification(income_driven_repayment_cap_evasion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EDUCATION DEPARTMENT (TANGLED ROPE) — Constrained by fiscal pressures and legislative mandates. The cap evasion serves dual coordination functions: (1) manages federal budget by extending repayment periods and deferring write-downs, (2) maintains loan servicer ecosystem through profitable contract structures. Simultaneously extracts by blocking income-driven relief, narrowing forgiveness eligibility, and using administrative opacity to discourage claims. Active enforcement of cap evasion through budget accountability measures. Department has institutional constraints (legislative mandates, fiscal rules) that limit exit options.
constraint_indexing:constraint_classification(income_driven_repayment_cap_evasion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: BORROWER ADVOCACY COALITION (SCAFFOLD) — Organized agents (student debt advocacy groups, labor unions, progressive policy organizations) see cap evasion as a temporary regulatory problem with a sunset clause: Direct Loan consolidation reforms, servicer contract reformation, and income verification transparency are gradually building toward relief mechanisms that bypass the cap evasion architecture. The constraint appears as a coordination failure (servicer incentive misalignment) rather than permanent extraction. Sunset logic: as political pressure increases and direct federal servicing expands, the private servicer extraction model becomes obsolete. Constrained by budget politics but seeing exit paths through policy change.
constraint_indexing:constraint_classification(income_driven_repayment_cap_evasion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LOAN REPAYMENT CERTIFICATION RITUAL (PITON) — Annual income certification and payment recalculation is substantially performative theater. The ritual persists (servicers require yearly forms, borrowers submit updated income documents) but the coordination function has largely atrophied — cap evasion mechanisms ensure that certification produces minimal actual payment reduction despite its intent. The mechanism is maintained through institutional inertia (loan servicers benefit from the ritual's opacity) rather than functional necessity. Borrowers perceive it as bureaucratic theater with minimal payoff.
constraint_indexing:constraint_classification(income_driven_repayment_cap_evasion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Zooming to full institutional scope, income-driven repayment cap evasion is a hybrid coordination-extraction system. Genuine coordination function exists: income-driven repayment prevents catastrophic default cascades, keeps borrowers engaged with the system, and provides fiscal predictability. Simultaneous extraction: cap evasion mechanisms (income recalculation gaming, consolidation restrictions, servicer profit incentives, administrative delays) systematically reduce relief below policy intent. The constraint is neither pure coordination (has extractive components) nor pure extraction (has genuine coordination function). Asymmetric: benefits servicers and education budget; costs low-income borrowers and public service workers.
constraint_indexing:constraint_classification(income_driven_repayment_cap_evasion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_driven_repayment_cap_evasion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(income_driven_repayment_cap_evasion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(income_driven_repayment_cap_evasion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_driven_repayment_cap_evasion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(income_driven_repayment_cap_evasion, TR),
    TR >= 0.70.

:- end_tests(income_driven_repayment_cap_evasion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from borrowers through suppressed relief, but not maximally — some income-driven reduction does occur, preventing pure snare classification. The extraction is active (mechanisms designed to limit relief) rather than passive (relief simply difficult to access). The trajectory from 0.35 to 0.58 reflects increasing game sophistication: early IDR implementations were more coordinated; subsequent policy iterations added cap evasion mechanisms to protect the federal budget. Suppression (0.65): Moderate-high. Barriers to relief include: administrative opacity (borrowers don't understand payment formula), servicer fee structures (incentivize extended repayment), credit damage from default (catastrophic cost of exit), lack of alternative education financing (trapped into existing loans), and bureaucratic complexity (certification burden). Suppression is not total — some borrowers do navigate the system and receive relief — but the barriers are substantial enough to trap most low-income and public service borrowers in extended repayment. Theater ratio (0.48): Moderate. The annual income certification ritual is partially performative — it creates the appearance of income-responsive adjustment without delivering promised relief magnitude. But it is not pure theater like traditional peer review (some real relief does flow) or credential signaling (actual payment calculations occur). The theater ratio being moderate reflects that the constraint combines real coordination function with performative obscuration of cap evasion mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the diagnostic power of indexed classification. The same base properties (ε=0.58, suppression=0.65) produce six different classification types depending on structural position. The low-income borrower's Snare view emphasizes that they are trapped without exit. The public service worker's Snare view emphasizes that PSLF promises are systematically blocked. The servicer's Rope view emphasizes that the constraint coordinates borrower behavior while benefiting the servicer. The department's Tangled Rope view emphasizes simultaneous coordination (prevent defaults) and extraction (suppress relief). The advocacy coalition's Scaffold view emphasizes that federal servicing expansion creates a sunset path. The certification ritual's Piton view emphasizes that the annual recertification is substantially performative. The perspectival gap reveals that the constraint is experienced very differently by different agents — some see extraction, some see coordination, some see degraded ritual, some see a temporary problem being solved. No single classification captures the full structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from: (1) beneficiary/victim status — servicers and budget are beneficiaries, borrowers are victims; (2) exit options — borrowers are trapped (high d), servicers have market exit (low d), department is constrained by law (moderate d), advocacy coalition is constrained by political change speed (moderate d); (3) power level relative to THIS constraint — powerless borrowers cannot negotiate, institutional actors (servicers, department) have negotiating power. The engine computes d from these structural inputs and applies the sigmoid f(d) to determine experienced extractiveness. Beneficiaries with exit options experience low effective extraction (they benefit and can leave). Victims without exit experience high extraction (they bear costs and cannot leave). Victims with exit options experience moderate-high extraction (they bear costs but could escape at significant price). This produces the perspectival gap: borrowers see high extraction (low exit options amplify f(d)), servicers see low extraction (beneficiary status + exit options suppresses f(d)).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY QUESTION: Can this constraint be classified as pure extraction (Snare) or is it fundamentally a coordination mechanism (Rope) with extraction complications? RESOLUTION: The constraint is genuinely Tangled Rope, not a misclassified Rope. The coordination function is real: income-driven repayment genuinely prevents default cascades and keeps borrowers engaged with the system. The extraction is also real and active: mechanisms are specifically designed to suppress relief below statutory intent. Both components exist and are structurally necessary — removing the coordination would cause default waves; removing the extraction would require expanding forgiveness beyond fiscal capacity (or redefining fiscal constraints). The mandatrophy resolves by accepting that the constraint is an asymmetric hybrid: coordination function + active extraction. Some perspectives see primarily extraction (borrowers), others see primarily coordination (servicers), others see the hybrid (analytical scope). No single type is a mislabeling — all are correct from their structural positions. The system's actual form is: coordination function (income-responsive payments) + extraction mechanism (cap evasion) = hybrid constraint requiring Tangled Rope classification at civilizational analytical scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cap_evasion_mechanism_identification,
    'Are the cap evasion mechanisms (income calculation variance, consolidation restrictions, servicer delays, payment formula opacity) intentional policy or emergent gaming of ambiguous regulatory rules?',
    'Historical analysis of regulatory intent vs. servicer implementation; internal education department emails and guidance; comparative analysis of servicer behavior across similar regulatory contexts',
    'If intentional: constraint is a deliberate tangled rope — department benefits from extraction while maintaining coordination facade. If emergent: constraint is institutional gaming of coordination rules, shifting classification toward rope (coordination breakdown rather than extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cap_evasion_mechanism_identification, empirical, 'Whether cap evasion is intentional policy or emergent regulatory gaming').

omega_variable(
    pslf_forgiveness_sufficiency,
    'Do borrowers actually receive promised PSLF relief when administrative barriers are overcome, or is the forgiveness itself structurally limited (loans too large, income too high, service years miscounted)?',
    'Longitudinal tracking of PSLF applicants from certification through forgiveness; comparison of promised relief vs. actual forgiveness amounts; analysis of denial reasons (administrative vs. substantive eligibility)',
    'If relief is substantive: PSLF is genuine coordination mechanism with administrative friction (scaffold dynamics). If blocked at multiple substantive gates: PSLF is architectural snare — forgiveness promised but made unattainable by design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pslf_forgiveness_sufficiency, empirical, 'Whether PSLF relief is substantive or architecturally blocked').

omega_variable(
    servicer_profit_extraction_scale,
    'What percentage of loan servicer profits derive directly from cap evasion mechanisms (extended repayment periods, payment formula opacity, income calculation errors) vs. baseline servicing operations?',
    'Financial analysis of servicer revenue streams; forensic accounting of profit attribution; comparison with competing servicing models (direct federal servicing, nonprofit servicers)',
    'If >30% of profits from evasion: servicer incentive is structural, making snare classification appropriate. If <10%: evasion is side effect of broader business model, shifting toward tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(servicer_profit_extraction_scale, empirical, 'Profit margin attribution to cap evasion mechanisms').

omega_variable(
    direct_federal_servicing_scalability,
    'Can direct federal loan servicing (scaling up the Department''s own servicing operations) actually replace private servicers without reducing coordination function or increasing administrative costs?',
    'Pilot data from Federal Student Aid direct servicing; comparative cost analysis; borrower outcome tracking (default rates, payment compliance, forgiveness processing time)',
    'If scalable and cost-competitive: scaffold sunset is real — federal servicing becomes viable exit path, collapsing the servicer extraction model. If costly or coordination-degrading: private servicer ecosystem remains locked in despite extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(direct_federal_servicing_scalability, empirical, 'Viability of direct federal servicing as replacement for private servicers').

omega_variable(
    borrower_identity_lock_mechanism,
    'To what extent do public service borrowers remain trapped by PSLF promises despite cap evasion blocking relief? Is the binding structural (career costs of exit) or identity-locked (self-concept fused with service commitment)?',
    'Qualitative interviews with PSLF-trapped borrowers; analysis of career switching rates post-forgiveness denial; survey of identity fusion (how much is service commitment vs. loan entanglement motivation)',
    'If structurally trapped: classification as snare is appropriate (barriers to exit make this extractive). If identity-locked: special vulnerability — even if material barriers removed, cognitive frames prevent exit. Affects treatment strategy (material relief vs. identity reframing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(borrower_identity_lock_mechanism, conceptual, 'Identity-lock binding in PSLF-trapped public service workers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_driven_repayment_cap_evasion, 0, 21).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(idr_tr_t0, income_driven_repayment_cap_evasion, theater_ratio, 0, 0.38).
narrative_ontology:measurement(idr_tr_t7, income_driven_repayment_cap_evasion, theater_ratio, 7, 0.43).
narrative_ontology:measurement(idr_tr_t14, income_driven_repayment_cap_evasion, theater_ratio, 14, 0.48).
narrative_ontology:measurement(idr_tr_t21, income_driven_repayment_cap_evasion, theater_ratio, 21, 0.48).

% Extraction over time
narrative_ontology:measurement(idr_be_t0, income_driven_repayment_cap_evasion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(idr_be_t7, income_driven_repayment_cap_evasion, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(idr_be_t14, income_driven_repayment_cap_evasion, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(idr_be_t21, income_driven_repayment_cap_evasion, base_extractiveness, 21, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_driven_repayment_cap_evasion, resource_allocation).
narrative_ontology:affects_constraint(income_driven_repayment_cap_evasion, student_loan_default_cascade).
narrative_ontology:affects_constraint(income_driven_repayment_cap_evasion, public_service_labor_supply).
narrative_ontology:affects_constraint(income_driven_repayment_cap_evasion, higher_education_access_inequality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_driven_repayment_cap_evasion, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
