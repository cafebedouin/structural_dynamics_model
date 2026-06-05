% ============================================================================
% CONSTRAINT STORY: conditional_cash_transfer_gates
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conditional_cash_transfer_gates, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: conditional_cash_transfer_gates
 *   human_readable: Conditional Cash Transfer Gates as Behavioral Control Mechanism
 *   domain: development_economics/social_policy
 *
 * SUMMARY:
 *   Conditional Cash Transfer (CCT) programs have become the dominant poverty
 *   alleviation mechanism in the Global South, transferring ~$100 billion
 *   annually and affecting 300+ million households. The stated coordination
 *   problem: poor households face intergenerational poverty traps because
 *   they cannot invest in child human capital (schooling, health) while
 *   meeting immediate subsistence needs. The solution: cash transfers
 *   conditional on child school attendance, health checkups, and
 *   (increasingly) behavioral compliance. This constraint story analyzes the
 *   extractive dimension of conditionality that development discourse has
 *   naturalized as incentive alignment. The core tension: conditions bind
 *   households through payment withholding (suppression=0.65), enforcing
 *   behaviors that may or may not reflect recipient priorities or actual
 *   welfare outcomes. The extractiveness grows over the interval (0.35→0.58)
 *   as programs layer additional behavioral conditions (nutrition, parenting
 *   practices, financial literacy) beyond the original school attendance
 *   requirement. The theater ratio remains moderate (0.48) because CCT
 *   programs are rigorously evaluated compared to many development
 *   interventions, but programs increasingly report impact through proxy
 *   metrics (enrollment, attendance) rather than actual outcome measures
 *   (learning, health status, agency).
 *
 * KEY AGENTS:
 *   - Recipient Households: Primary victim (powerless/trapped) — structurally dependent on transfer, cannot exit without subsistence loss, must comply with conditions regardless of alignment with priorities
 *   - School System: Secondary beneficiary and victim (moderate/constrained) — benefits from stable enrollment, bears extraction through compliance verification and discipline infrastructure
 *   - Program Administrators: Primary beneficiary (institutional/arbitrage) — experience conditionality as functional coordination mechanism, have full discretion to reorient programs
 *   - Donor Coalition: Organized beneficiary (organized/constrained) — benefits from accountability narrative and perceived effectiveness, constrained by geopolitical legitimacy and results reporting requirements
 *   - Child Welfare Metric: Abstract victim (powerful/mobile) — has agency through measurement requirements but constrained by program design that uses behavioral proxies rather than outcome measures
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies conditionality as behavioral control mechanism, perceives extraction masked as development incentive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conditional_cash_transfer_gates, 0.58).
domain_priors:suppression_score(conditional_cash_transfer_gates, 0.65).
domain_priors:theater_ratio(conditional_cash_transfer_gates, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conditional_cash_transfer_gates, extractiveness, 0.58).
narrative_ontology:constraint_metric(conditional_cash_transfer_gates, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(conditional_cash_transfer_gates, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conditional_cash_transfer_gates, tangled_rope).
narrative_ontology:human_readable(conditional_cash_transfer_gates, "Conditional Cash Transfer Gates as Behavioral Control Mechanism").
narrative_ontology:topic_domain(conditional_cash_transfer_gates, "development_economics/social_policy").

domain_priors:requires_active_enforcement(conditional_cash_transfer_gates).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(conditional_cash_transfer_gates, program_administrators).
narrative_ontology:constraint_beneficiary(conditional_cash_transfer_gates, donor_institutions).
narrative_ontology:constraint_beneficiary(conditional_cash_transfer_gates, behavioral_compliance_enforcers).
narrative_ontology:constraint_victim(conditional_cash_transfer_gates, recipient_households).
narrative_ontology:constraint_victim(conditional_cash_transfer_gates, child_welfare_outcomes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RECIPIENT HOUSEHOLD (SNARE) — Structurally trapped. The cash transfer itself is essential for subsistence, but conditionality binds compliance regardless of actual impact on child welfare. Conditions (school attendance, health checkups) are enforced through payment withholding, creating maximum suppression. The household cannot exit without sacrificing basic survival. Extraction: they must perform behavioral compliance whether or not it serves the child's welfare. Zero degrees of freedom.
constraint_indexing:constraint_classification(conditional_cash_transfer_gates, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SCHOOL SYSTEM (TANGLED ROPE) — Constrained but not trapped. Schools benefit from guaranteed enrollment (coordination function: stable pupil numbers, fundable positions, infrastructure investment). Simultaneously, they bear extraction: they must maintain enrollment compliance infrastructure, process attendance verification for CCT administrators, and discipline students for non-compliance without additional resources. Exit is costly (losing enrollment-based funding) but theoretically possible through alternative revenue sources.
constraint_indexing:constraint_classification(conditional_cash_transfer_gates, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROGRAM ADMINISTRATION (ROPE) — Pure coordination from their position. CCTs solve the legitimate development problem: how to transfer cash to households while incentivizing human capital investment. The conditionality mechanism is experienced as a coordination tool, not as extraction. Administrators have full arbitrage capacity (can shift programs, reorient goals, exit if funding dries up). They see the constraint as functional.
constraint_indexing:constraint_classification(conditional_cash_transfer_gates, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DONOR COALITION (TANGLED ROPE) — Organized but constrained by geopolitical legitimacy and results accountability. Donors have coordination function: they've funded 2+ generations of CCT rollout, creating institutional infrastructure for poverty alleviation. But they also extract: they enforce behavioral requirements on recipients that serve donor theories of development (school attendance, health behaviors) without necessarily matching recipient priorities or local knowledge. Donors' constraints are political (they must show impact to domestic taxpayers) and programmatic (they must monitor conditions to claim effectiveness).
constraint_indexing:constraint_classification(conditional_cash_transfer_gates, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CHILD WELFARE METRIC (TANGLED ROPE) — The stated goal of conditionality is improving child outcomes (school completion, health status). From this abstract perspective, the constraint is hybrid: it coordinates household behavior toward human capital investment (genuine coordination) but extracts by imposing specific behavioral proxies (school attendance) rather than measuring actual welfare (learning, health status, agency). The metric has agency through measurement — programs must track it to claim impact — but is powerless to redesign conditions when proxies fail.
constraint_indexing:constraint_classification(conditional_cash_transfer_gates, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational analytical perspective, conditionality functions as a hidden behavioral control mechanism. The cash transfer (essential for subsistence) becomes lever for enforcing donor-preferred behaviors regardless of recipient agency or actual welfare outcomes. This is snare-classified at the analytical level: the mechanism exists to regulate behavior of the powerless according to theories of development designed by the powerful. No coordination benefit justifies the behavioral extraction. The theater is moderate (0.48) because programs are genuinely monitored for outcomes, not purely ceremonial.
constraint_indexing:constraint_classification(conditional_cash_transfer_gates, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conditional_cash_transfer_gates_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(conditional_cash_transfer_gates, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conditional_cash_transfer_gates, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(conditional_cash_transfer_gates, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(conditional_cash_transfer_gates_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The core extraction is behavioral compliance enforcement through payment withholding. Households receive cash (benefits) but must perform donor-preferred behaviors (school attendance, health checkups, increasingly detailed parenting practices). The household cannot choose to forgo these conditions in exchange for unconditional cash — the conditions are non-negotiable. The extractiveness is not as severe as pure snares (0.80+) because: (a) the cash transfer itself provides genuine material benefit, and (b) some conditions may align with household priorities (school attendance often increases child welfare). But extractiveness exceeds pure rope (0.35) because conditions constrain household agency and redistribute discretion upward to administrators. The trajectory shows increasing extractiveness (0.35→0.58) as programs layer behavioral conditions beyond cash assistance. Suppression (0.65): High. Multiple barriers prevent exit: (a) material dependence — recipients need the cash for subsistence, (b) payment withholding as enforcement — non-compliance triggers payment loss, (c) behavioral verification — schools and clinics become surveillance points for compliance monitoring, (d) lack of alternatives — other assistance programs typically have equal or higher conditions. Suppression is not maximal (0.90+) because some households have partial alternatives (informal labor, family networks, seasonal migration) and some conditions are less enforceable in practice. Theater ratio (0.48): Moderate. CCT programs are among the most rigorously evaluated development interventions (RCTs, impact evaluations), creating genuine outcome measurement pressure. But measurement theater emerges through metric substitution: programs report school attendance (verifiable, easy to measure) rather than learning outcomes (difficult to measure, often show weak condition-effect). The theater rises over the interval as programs shift from cash-plus-conditions to increasingly detailed behavioral prescriptions (nutrition, parenting practices) whose actual impact is harder to verify.
 *
 * PERSPECTIVAL GAP:
 *   Recipients perceive maximum extraction (snare from powerless/trapped position) because they have zero exit capacity and payment withholding is credible punishment. Administrators perceive zero extraction (rope from institutional/arbitrage position) because they designed the mechanism and experience it as solving a coordination problem. The gap reveals that the same mechanism (conditions + payment withholding) is experienced as extractive by those it constrains and coordinative by those who control it. This is the diagnostic signature of a tangled rope: genuine coordination function (incentivizing human capital investment) coupled with genuine extraction (behavioral control over unrelated dimensions). The school system's tangled rope perspective shows intermediate extraction — schools gain enrollment but lose autonomy over discipline and attendance enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the cash and conditions. Recipients are full targets (d→1.0): they receive cash (benefit) but have zero negotiating power over conditions and face maximum suppression (payment withholding). Program administrators are full beneficiaries (d→0.0): they experience conditions as solving the coordination problem with minimal enforcement cost (schools and clinics already exist and perform verification). Donors are partial beneficiaries with geopolitical constraints (d→0.25-0.35): they benefit from accountability narrative and program rollout, but face political constraints from recipient countries and domestic taxpayers. Schools are mixed (d→0.50-0.60): they benefit from enrollment stability but bear extraction through compliance monitoring. The school's constraint exit options (constrained rather than trapped) reflect that schools can theoretically exit by refusing to verify attendance, but face enrollment loss penalty. The analytical observer's d is derived from the universal civilizational scope and observational position (d→0.72).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION via perspectival decomposition: The constraint resolves mandatrophy by showing that CCT programs are genuinely hybrid mechanisms. They are NOT pure coordination (rope) because recipients have zero agency over conditions and payment withholding creates maximum suppression. They are NOT pure extraction (snare) because the cash transfer provides material benefit that some recipients might not receive otherwise, and some conditions may align with recipient priorities. They are tangled rope (mixed coordination and extraction) because: (a) the program solves a genuine coordination problem (incentivizing human capital investment when households face poverty traps), (b) the beneficiaries (donors, administrators, schools) experience coordination benefit, (c) the enforcement mechanism (behavioral conditions, payment withholding) extracts behavioral compliance beyond what the coordination problem requires, and (d) the recipients bear the full cost of suppression (cannot exit, cannot renegotiate conditions). The analytical observer classification as snare reveals the hidden extraction: from a civilizational perspective, using cash-transfer leverage to enforce donor-preferred behaviors on the poorest populations is behavioral control. But the tangled rope classification from institutional and recipient perspectives preserves the genuine coordination dimension — without conditions, many donors would not fund programs that might be spent on consumption rather than capital investment. The resolution: CCTs are tangled ropes that have drifted toward snares as conditions have accumulated and become less tethered to the original coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditionality_impact_ambiguity,
    'Do conditions actually improve child welfare outcomes, or do they merely sort households by existing capacity and pre-existing compliance willingness?',
    'Randomized trials comparing CCT with conditions vs CCT without conditions vs no transfer. Longitudinal tracking of child outcomes beyond program duration. Analysis of selection bias in which households accept conditionality.',
    'If conditions improve outcomes: constraint is tangled rope (mixed coordination and extraction). If conditions only sort: constraint is snare (extraction masquerading as development). If conditions harm outcomes (via labor reallocation, school quality degradation): constraint is pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditionality_impact_ambiguity, empirical, 'Whether conditions improve outcomes or merely sort households').

omega_variable(
    compliance_mechanism_substitution,
    'When households cannot meet conditions (child illness, school closure, transport barriers), does payment withholding actually measure non-compliance or measure household poverty depth?',
    'Granular analysis of compliance failures: distinguish voluntary non-compliance from inability (medical exemptions, school closures, accessibility barriers). Track outcomes of payment withholding during involuntary non-compliance periods.',
    'If withholding targets voluntary non-compliance: mechanism is extraction to enforce norms (snare). If withholding punishes involuntary failure: mechanism is punitive rather than incentivizing (pure extraction, high snare). If most failures are involuntary: the suppression measure is misaligned with the stated incentive goal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_mechanism_substitution, empirical, 'Whether compliance failures are voluntary or involuntary').

omega_variable(
    recipient_agency_and_identity_lock,
    'Are recipients locked into compliance through material dependence (trapped exit) or through internalized acceptance of conditionality as legitimate (identity_locked exit)?',
    'Qualitative interviews comparing recipient framing of compliance (''I have no choice'' vs ''it''s for my child''s own good''). Post-program trajectory of behavioral change: if identity-locked, behaviors persist after cash ends; if materially trapped, behaviors revert.',
    'If primarily trapped: suppression measure (0.65) is accurate. If partly identity-locked: suppression may underestimate binding strength (internalized norms persist after barriers removed). If identity-locked dominates: classification shifts to emphasize internalized control over material control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recipient_agency_and_identity_lock, empirical, 'Recipient exit mechanism: material dependence vs identity lock').

omega_variable(
    extractiveness_attribution_boundary,
    'How much of the measured extractiveness (0.58) reflects donor imposition of preferred behaviors vs legitimate coordination problem-solving for poverty alleviation?',
    'Decompose extractiveness into: (a) donor-stipulated conditions vs locally-prioritized conditions, (b) conditions that increase household welfare vs conditions that redistribute welfare to preferences of measurer, (c) coordination benefit to recipients vs pure transfer with behavioral control premium.',
    'If extractiveness is primarily donor-imposed: tangled rope classification stands. If extractiveness is primarily coordination benefit as perceived by recipients: constraint may be rope. If extractiveness is primarily measurement theater: constraint may be piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_attribution_boundary, conceptual, 'Whether extractiveness reflects donor control or legitimate coordination').

omega_variable(
    conditionality_theater_trajectory,
    'Is the moderate theater ratio (0.48) sustainable, or will programs increasingly rely on performative metrics as actual impact evidence becomes harder to defend?',
    'Longitudinal analysis of program reporting: measure divergence between stated impact metrics and independent outcome evaluation. Track metric substitution over time (shift from actual welfare measures to proxy measures).',
    'If theater remains stable: tangled rope classification holds. If theater rises toward 0.70+: constraint degrades toward piton. If theater falls below 0.30: constraint may simplify toward pure snare (less performative, more nakedly extractive).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conditionality_theater_trajectory, empirical, 'Sustainability of theater ratio under outcome pressure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conditional_cash_transfer_gates, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cct_tr_t0, conditional_cash_transfer_gates, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cct_tr_t5, conditional_cash_transfer_gates, theater_ratio, 5, 0.42).
narrative_ontology:measurement(cct_tr_t10, conditional_cash_transfer_gates, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(cct_be_t0, conditional_cash_transfer_gates, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cct_be_t5, conditional_cash_transfer_gates, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(cct_be_t10, conditional_cash_transfer_gates, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conditional_cash_transfer_gates, resource_allocation).
narrative_ontology:boltzmann_floor_override(conditional_cash_transfer_gates, 0.12).
narrative_ontology:affects_constraint(conditional_cash_transfer_gates, behavioral_conditionality_escalation).
narrative_ontology:affects_constraint(conditional_cash_transfer_gates, surveillance_infrastructure_embedding).

% DUAL FORMULATION NOTE:
% CCT constraints decompose into multiple structurally distinct claims: (1) cash transfer as poverty alleviation (rope, ε≈0.08), (2) conditionality as incentive alignment (tangled_rope, ε≈0.58), (3) behavioral verification infrastructure (snare, ε≈0.72). This story focuses on the conditionality mechanism specifically. The broader CCT program is the union of all three constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(conditional_cash_transfer_gates, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
