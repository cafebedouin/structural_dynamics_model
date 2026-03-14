% ============================================================================
% CONSTRAINT STORY: welfare_cliff_effects
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_welfare_cliff_effects, []).

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
 *   constraint_id: welfare_cliff_effects
 *   human_readable: Welfare Cliff Effects: Poverty Trap Through Benefit Discontinuity
 *   domain: economic_policy/social_welfare
 *
 * SUMMARY:
 *   Welfare cliff effects occur when benefit eligibility criteria create
 *   sharp income thresholds where earning slightly more income results in
 *   losing benefit access, producing effective marginal tax rates of 300-500%
 *   and trapping workers in low-earning equilibrium. This constraint operates
 *   across most developed nations' welfare systems and affects millions of
 *   beneficiaries. The cliff is justified as an administrative efficiency
 *   mechanism (categorical eligibility reduces means-testing complexity) but
 *   functions as a pure extraction mechanism from the perspective of trapped
 *   workers. The constraint demonstrates how indexical classification reveals
 *   competing narratives: fiscal conservatives see coordination (efficient
 *   categorical design), administrators see institutional inertia (piton),
 *   reform coalitions see a temporary problem with solution pathway
 *   (scaffold), and trapped workers see structural extraction with no exit
 *   (snare). The extractiveness score (0.58) reflects that the mechanism is
 *   moderately severe — the benefit loss is real and disruptive, but not
 *   total impoverishment. The suppression score (0.72) reflects high barriers
 *   to exit: childcare costs, transportation, skill gaps, and identity-locked
 *   caregiving roles constrain workers' ability to find employment that pays
 *   more than the cliff threshold.
 *
 * KEY AGENTS:
 *   - Low-income workers earning near benefit threshold: Primary victims (powerless/trapped) — face 300-500% marginal tax rates at threshold crossing
 *   - Single parents and caregivers: Secondary victims (powerless/identity_locked) — structurally mobile but identity-fused to caregiving roles that keep income below threshold
 *   - Fiscal conservative policy coalitions: Primary beneficiaries (institutional/arbitrage) — benefit from administrative simplicity and cost control; experience constraint as coordination
 *   - Welfare administration bureaucracy: Secondary beneficiary (institutional/arbitrage) — maintains categorical systems due to path dependency; sees own process as degraded (piton perspective)
 *   - Reform coalitions (nonprofits, progressive economists): Organized agents (organized/mobile) — see cliff as temporary problem with solution pathway; building earned income tax credit expansion and smooth phase-out alternatives
 *   - Healthcare/childcare systems: Mixed (moderate/constrained) — provide genuine coordination function at low-income level while creating cliff through benefit discontinuity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(welfare_cliff_effects, 0.58).
domain_priors:suppression_score(welfare_cliff_effects, 0.72).
domain_priors:theater_ratio(welfare_cliff_effects, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(welfare_cliff_effects, extractiveness, 0.58).
narrative_ontology:constraint_metric(welfare_cliff_effects, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(welfare_cliff_effects, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(welfare_cliff_effects, snare).
narrative_ontology:human_readable(welfare_cliff_effects, "Welfare Cliff Effects: Poverty Trap Through Benefit Discontinuity").
narrative_ontology:topic_domain(welfare_cliff_effects, "economic_policy/social_welfare").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(welfare_cliff_effects, fiscal_conservatism_coalition).
narrative_ontology:constraint_beneficiary(welfare_cliff_effects, administrative_overhead_reduction).
narrative_ontology:constraint_victim(welfare_cliff_effects, low_income_workers).
narrative_ontology:constraint_victim(welfare_cliff_effects, benefit_recipients_at_threshold).
narrative_ontology:constraint_victim(welfare_cliff_effects, single_parents_earning_near_cutoff).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME WORKER AT CLIFF (SNARE) — Faces sharp discontinuity in take-home income as earnings cross benefit threshold. Trapped by the structure: earning $1 more results in losing $3-5 in benefits, creating effective marginal tax rate of 300-500%. No exit option exists within the constraint. Maximum extraction experienced — the worker bears full cost of the threshold mechanism.
constraint_indexing:constraint_classification(welfare_cliff_effects, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SINGLE PARENT WITH IDENTITY LOCK (SNARE) — Structurally mobile (could relocate, retrain) but identity-locked to 'primary caregiver' role. Perceives cliff through fused lens of parental identity and economic dependency. Cannot imagine leaving childcare arrangement or part-time status that keeps income below threshold without experiencing identity dissolution. Suppression is both structural (childcare costs, transportation barriers) and internalized (guilt about working more, cultural narratives about motherhood).
constraint_indexing:constraint_classification(welfare_cliff_effects, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: FISCAL CONSERVATIVE POLICY COALITION (ROPE) — Benefits from sharp cliff because it reduces administrative overhead and controls benefit roll-off. Experiences the constraint as coordination mechanism: the cliff is an efficient (if harsh) rule that solves the categorical problem of 'who qualifies?' without means-tested phase-out complexity. Net beneficiary — the mechanism serves their policy objective of cost control.
constraint_indexing:constraint_classification(welfare_cliff_effects, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HEALTHCARE AND CHILDCARE DEPENDENT (TANGLED ROPE) — Faces both coordination benefit (access to subsidized childcare/healthcare while below threshold) and extraction (loss of subsidies creates high marginal cost to earning slightly more). The constraint serves genuine coordination (matching childcare supply to demand below threshold) while simultaneously extracting from those near the boundary. Constrained exit: can work more, but at severe cost. This perspective experiences the mixed function characteristic of tangled rope.
constraint_indexing:constraint_classification(welfare_cliff_effects, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WELFARE ADMINISTRATION BUREAUCRACY (PITON) — The cliff mechanism persists largely through institutional inertia: welfare systems were designed with discrete eligibility categories decades ago, and the administrative apparatus (staff training, computer systems, benefit determination rules) remains locked into these categories despite recognition that the cliff creates perverse incentives. The theater is high (eligibility determinations, verification processes) but the function has atrophied — modern alternatives (negative income tax, earned income tax credit phase-out) would serve the coordination goal better. The bureaucracy maintains the cliff through path dependency, not active necessity.
constraint_indexing:constraint_classification(welfare_cliff_effects, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: EARNINGS SUPPLEMENT REFORM COALITION (SCAFFOLD) — Organized agents (nonprofits, progressive economists, state welfare offices) see the cliff as a temporary coordination failure with a genuine sunset: earned income tax credit expansion, negative income tax pilots, and gradual phase-out mechanisms are building alternative architectures that preserve work incentives while maintaining support. This coalition has identified the exit path (smooth phase-out rather than cliff) and is constructing it. Low effective extraction from this perspective because the structure has a visible horizon and the coalition has agency in building alternatives.
constraint_indexing:constraint_classification(welfare_cliff_effects, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — Tempting to argue that welfare cliffs are inherent to any categorical benefit system: once you define eligibility categories, discontinuities appear at boundaries. This is a false summit. The cliff is not inherent to the concept of categorical benefits — it is a contingent choice about how to design the boundary. Smooth phase-out (which accomplishes the same coordination goal of connecting support to need) has the same logical foundation. The mountain classification reveals when 'this is just how welfare works' has naturalized a design choice.
constraint_indexing:constraint_classification(welfare_cliff_effects, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(welfare_cliff_effects_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(welfare_cliff_effects, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(welfare_cliff_effects, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(welfare_cliff_effects, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(welfare_cliff_effects, TR),
    TR >= 0.70.

:- end_tests(welfare_cliff_effects_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The welfare cliff creates genuine extraction from trapped workers through the effective marginal tax rate mechanism. However, the extractiveness is not as severe as pure predatory extraction (0.75+) because the mechanism operates through administrative rule rather than overt coercion, and because some workers can exit by changing employment or location. The value reflects that the extraction is systematic and intentional (fiscal policy design choice) but not total impoverishment. Suppression (0.72): High. Multiple barriers prevent exit: childcare costs rise sharply with work hours (reducing net income gains), transportation and work-related expenses absorb additional earnings, skill gaps limit job search options, and identity-based commitments (primary caregiver role) constrain mobility. These barriers are both structural and internalized. The suppression has increased over the measurement interval (0.65 in year 0 to 0.72 in year 15) as childcare and healthcare costs have risen faster than wage growth, making the cliff more binding. Theater ratio (0.55): Moderate. The welfare administration process involves substantial theater (eligibility verification, recertification processes) but the core mechanism (the cliff) is structurally real, not purely performative. The theater reflects administrative overhead to determine categorical eligibility, but this doesn't hide the extraction — it enables it. The theater ratio remains relatively flat across the interval because the categorical administration process hasn't fundamentally changed, even as awareness of the problem has grown.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint reveals how the same structural mechanism appears radically different from different positions. From the fiscal conservative institutional view, the welfare cliff is coordination mechanism solving an important problem (categorical targeting without expensive verification). From the trapped low-income worker view, the cliff is a pure snare preventing upward mobility. From the single parent identity-locked view, the cliff is a snare compounded by cognitive entrapment — the structural barrier is only half the problem; the other half is the internalized identity that makes exiting the caregiving role unthinkable. From the reform coalition organized view, the cliff is a temporary problem with a visible solution path (smooth phase-out mechanisms). From the administration bureaucratic view, the cliff is an inertial piton — the system was designed this way decades ago and persists because alternative architecture is complex, not because it's optimal. From the false analytical summit view, the cliff appears inherent to any benefit system (mountain), revealing when 'this is just how welfare works' has naturalized a contingent design choice. The gap is diagnostic: it shows that the constraint's classification depends entirely on the observer's structural relationship to the benefit flow and their exit options. No single perspective is 'wrong' — they're all accurate descriptions of what the constraint looks like from that position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the structural flow of extraction. Low-income workers are victims of the cliff — they lose income when crossing the threshold. Their d is high (≈0.90-0.95) because the constraint extracts from them. Fiscal conservative coalitions are beneficiaries — the cliff solves their coordination problem and doesn't apply to them. Their d is low (≈0.05-0.15) because the constraint benefits them. Single parents appear as victims structurally (d ≈ 0.90) but the identity_locked exit option captures that their cognitive frame prevents exercise of the structural exit path, which modulates their experienced extraction upward (they perceive greater immutability than constrained agents). Administrators and welfare bureaucracies are in ambiguous position: they benefit from simpler systems (low d) but also bear costs of dealing with consequences (systems strain, appeals, legislative pressure). The directionality override mechanism could be used here to fine-tune administrative d upward from the default beneficiary position (0.10-0.15) to (0.30-0.40) if analysis shows the administrative burden is high enough to create partial victimhood. Earnings supplement reform coalitions have analytical or organized power with mobile exit, placing them at d ≈ 0.50-0.60 (neither pure beneficiary nor victim, but seeing the problem from the powerless perspective).
 *
 * MANDATROPHY ANALYSIS:
 *   The welfare cliff resolves potential mandatrophy by showing that all six types are simultaneously valid from different structural positions. The mandatrophy is not 'which type is correct?' but 'which stakeholder position are you analyzing from?' The fiscal conservative coalition genuinely experiences coordination (rope). The trapped worker genuinely experiences extraction (snare). The reform coalition genuinely sees a temporary problem with solution (scaffold). The bureaucracy genuinely operates through inertia (piton). The false summit perspective (mountain) reveals when 'this is how welfare works' naturalizes a contingent design. The classification framework prevents the false reduction to a single 'true' type by requiring that all perspectives be validated against their structural position. This prevents the standard policy error where reformers claim the cliff is 'obviously' bad (snare perspective) while conservatives claim it's 'obviously' necessary (rope perspective) — both are correct from their position. The mandatrophy resolves by showing the constraint is a **mixed coordination-extraction hybrid** (tangled rope from moderate perspectives, snare from powerless perspectives) with genuine functions (targeting benefit to low income) and genuine harms (trapping people in low-earnings equilibrium). This mixed diagnosis suggests that simple removal of the cliff (satisfying the snare perspective) risks breaking the coordination function (understood from the rope perspective) — the solution is not to eliminate the cliff but to replace it with a mechanism that preserves targeting while eliminating the discontinuity (smooth phase-out), which is exactly what the scaffold perspective proposes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_response_elasticity,
    'How much do workers actually reduce hours/earnings in response to the cliff versus how much do they remain trapped in the low-earnings trap?',
    'Quasi-experimental analysis using state-level cliff discontinuities; labor supply response analysis comparing regions with sharp vs gradual phase-outs; longitudinal tracking of worker trajectories at threshold',
    'If high behavioral response: the snare classification is correct — workers are actively constrained by the disincentive. If low behavioral response: extraction mechanism may be institutional inertia rather than active suppression; Piton classification becomes more prominent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_response_elasticity, empirical, 'Labor supply elasticity response to cliff discontinuities').

omega_variable(
    threshold_awareness_and_cognition,
    'Do workers understand the cliff effect exists and can accurately calculate their take-home income at different earnings levels?',
    'Survey of benefit recipients near threshold; cognitive testing of perceived marginal tax rates; analysis of whether actual earnings patterns cluster below threshold (indicating awareness) or cross freely (indicating lack of awareness)',
    'If high awareness: suppression is active and intentional (workers organize around the constraint). If low awareness: suppression works through structural complexity rather than transparent disincentive — extraction mechanism relies on cognitive capture/information asymmetry rather than clear tradeoff.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_awareness_and_cognition, empirical, 'Recipient awareness and understanding of welfare cliff structure').

omega_variable(
    administrative_cost_savings_verification,
    'Do cliff-based categorical systems actually cost less to administer than graduated phase-out systems, or is the claimed efficiency gain a justification story?',
    'Comparative cost accounting: total administrative spending (staff, systems, verification) in categorical systems vs smooth phase-out systems; per-beneficiary cost analysis; overhead burden analysis',
    'If categorical systems are genuinely cheaper: the fiscal conservative coalition''s rope classification reflects real coordination benefit. If costs are comparable: the cliff''s existence is not justified by efficiency — it is pure extraction mechanism maintained by organizational path dependency (stronger piton signal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_cost_savings_verification, empirical, 'True administrative cost comparison between categorical and graduated systems').

omega_variable(
    internalized_suppression_persistence,
    'For beneficiaries with identity_locked exit options, does the suppression they experience persist after earning enough to exit the system, or does it dissolve?',
    'Longitudinal tracking of former beneficiaries who escaped the cliff: do they maintain low earnings/hours patterns months/years after cliff is no longer binding? Do they report reduced identity-based barriers after crossing threshold permanently?',
    'If suppression persists: the identity lock is genuinely internalized; the constraint travels with the agent even after the structural mechanism is removed. This indicates deeper extraction mechanism (psychological entrapment) than the structural cliff alone. If suppression dissolves: the snare classification captures the mechanism — it was the external barrier, not identity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_persistence, empirical, 'Whether suppression is structural or internalized for identity-locked agents').

omega_variable(
    phase_out_design_political_feasibility,
    'Are gradual phase-out mechanisms (which solve the coordination problem without cliffs) politically achievable, or are they blocked by ideology/coalition dynamics?',
    'Legislative history analysis; comparative analysis of states that implemented smooth phase-outs; stakeholder interviews with fiscal conservatives about why cliff-based design is defended; modeling of political economy of reform',
    'If politically feasible: scaffold classification is structurally valid — sunset mechanism exists and can be activated. If blocked: the scaffold perspective is aspirational; the constraint may be locked in by political equilibrium rather than technical inevitability. Implies mandatrophy is not resolvable through reform — the snare is sustained by political choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phase_out_design_political_feasibility, preference, 'Political feasibility of smooth phase-out alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(welfare_cliff_effects, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(welf_tr_t0, welfare_cliff_effects, theater_ratio, 0, 0.48).
narrative_ontology:measurement(welf_tr_t5, welfare_cliff_effects, theater_ratio, 5, 0.52).
narrative_ontology:measurement(welf_tr_t10, welfare_cliff_effects, theater_ratio, 10, 0.55).
narrative_ontology:measurement(welf_tr_t15, welfare_cliff_effects, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(welf_be_t0, welfare_cliff_effects, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(welf_be_t5, welfare_cliff_effects, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(welf_be_t10, welfare_cliff_effects, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(welf_be_t15, welfare_cliff_effects, base_extractiveness, 15, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(welfare_cliff_effects, resource_allocation).
narrative_ontology:affects_constraint(welfare_cliff_effects, unemployment_benefits_discontinuity).
narrative_ontology:affects_constraint(welfare_cliff_effects, medicaid_coverage_thresholds).
narrative_ontology:affects_constraint(welfare_cliff_effects, housing_subsidy_cliffs).
narrative_ontology:affects_constraint(welfare_cliff_effects, child_tax_credit_phase_out).

% DUAL FORMULATION NOTE:
% The welfare cliff is one instance of a broader class of benefit discontinuity constraints affecting multiple welfare systems. Each system (SNAP, Medicaid, housing subsidies, EITC phase-out) has its own extractiveness profile and temporal dynamics, but they share the same structural mechanism: categorical eligibility creates sharp boundaries with high marginal costs to crossing. The stories are linked because workers often face multiple overlapping cliffs from different benefit programs simultaneously, compounding the suppression and extraction effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(welfare_cliff_effects, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
