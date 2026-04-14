% ============================================================================
% CONSTRAINT STORY: global_ubi_movement_rhetoric
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_ubi_movement_rhetoric, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: global_ubi_movement_rhetoric
 *   human_readable: Global UBI Movement Rhetoric as Coordination-Extraction Hybrid
 *   domain: political_economy/discourse_analysis
 *
 * SUMMARY:
 *   The global UBI movement rhetoric represents a constraint that
 *   simultaneously coordinates genuine concerns about technological
 *   unemployment, social fragmentation, and dignity in labor markets, while
 *   extracting by suppressing analysis of alternative policy designs and
 *   their distributional consequences. The constraint operates through
 *   discursive closure: UBI rhetoric frames itself as the natural,
 *   inevitable, or only viable response to automation and precarity, thereby
 *   suppressing serious public deliberation about employment protection, wage
 *   floors, job guarantees, sectoral bargaining, or other mechanisms. The
 *   movement includes sincere advocates who genuinely believe UBI is optimal
 *   policy, technology sector actors who benefit from normalization of
 *   workforce displacement, intellectuals whose careers depend on UBI
 *   framework salience, fiscal conservatives who see UBI as cheaper than
 *   targeted welfare, and precarious workers who are promised liberation but
 *   experience rhetorical extraction. The constraint's theater ratio (0.65)
 *   reflects that the movement maintains performative engagement with
 *   empirical evidence (pilot programs, academic studies) while core
 *   rhetorical commitments persist regardless of evidence quality or
 *   generalizability.
 *
 * KEY AGENTS:
 *   - Precarious workers and low-wage laborers: Primary victims (powerless/trapped) — experience wage suppression and employment anxiety while being promised rhetorical solutions that defer material protection to an uncertain future
 *   - Means-tested welfare recipients: Secondary victims (moderate/constrained) — risk losing targeted programs designed for their specific needs in exchange for universal cash transfers that may be inadequate
 *   - Technology sector and automation advocates: Primary beneficiaries (institutional/arbitrage) — gain legitimacy for workforce displacement and social stability without wage regulation, while framing labor displacement as inevitable
 *   - UBI intellectual coalition: Institutional beneficiaries (institutional/arbitrage) — careers, funding, and status depend on UBI framework remaining dominant in policy discourse
 *   - Progressive policy entrepreneurs: Secondary beneficiaries (powerful/mobile) — advance political profiles by championing UBI rhetoric without committing to implementation
 *   - Labor movements and employment-protection advocates: Counter-coalition (organized/constrained) — face suppression of alternative policy frames but retain agency to build counter-narratives
 *   - Fiscal conservative governments: Constrained institutional actors (powerful/mobile) — experience pressure to adopt UBI from rhetorical consensus but bear implementation costs and face fiscal constraints
 *   - Analytical observer: Global civilizational perspective (analytical/analytical) — can identify both coordination and extraction functions, risking false equivalence or false naturalness depending on observational frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_ubi_movement_rhetoric, 0.52).
domain_priors:suppression_score(global_ubi_movement_rhetoric, 0.48).
domain_priors:theater_ratio(global_ubi_movement_rhetoric, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_ubi_movement_rhetoric, extractiveness, 0.52).
narrative_ontology:constraint_metric(global_ubi_movement_rhetoric, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(global_ubi_movement_rhetoric, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_ubi_movement_rhetoric, tangled_rope).
narrative_ontology:human_readable(global_ubi_movement_rhetoric, "Global UBI Movement Rhetoric as Coordination-Extraction Hybrid").
narrative_ontology:topic_domain(global_ubi_movement_rhetoric, "political_economy/discourse_analysis").

domain_priors:requires_active_enforcement(global_ubi_movement_rhetoric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_ubi_movement_rhetoric, ubi_intellectual_coalition).
narrative_ontology:constraint_beneficiary(global_ubi_movement_rhetoric, technology_sector_advocates).
narrative_ontology:constraint_beneficiary(global_ubi_movement_rhetoric, progressive_policy_entrepreneurs).
narrative_ontology:constraint_victim(global_ubi_movement_rhetoric, low_wage_workers_employment_anxiety).
narrative_ontology:constraint_victim(global_ubi_movement_rhetoric, means_tested_welfare_recipients).
narrative_ontology:constraint_victim(global_ubi_movement_rhetoric, fiscal_constraint_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Low-wage workers with no exit from the labor market face extraction through UBI rhetoric that simultaneously promises liberation and normalizes job displacement. The rhetoric suppresses alternative employment-protection strategies by framing job loss as inevitable and autonomous income as the solution. Maximum suppression: workers cannot exit the labor precarity, cannot exit the discourse promising to solve it, and bear the costs of wage compression during the transition rhetoric phase.
constraint_indexing:constraint_classification(global_ubi_movement_rhetoric, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WELFARE-DEPENDENT HOUSEHOLD (TANGLED ROPE) — Means-tested welfare recipients experience both genuine coordination (UBI rhetoric acknowledges dignity, reduces stigma) and extraction (the movement's framing can justify dismantling existing targeted programs in favor of uniform cash transfers that may leave them worse off). The constraint is active enforcement — UBI policy proposals require suppression of alternative welfare designs through rhetorical claims about universal simplicity.
constraint_indexing:constraint_classification(global_ubi_movement_rhetoric, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: TECHNOLOGY SECTOR (ROPE) — Tech companies and automation advocates benefit from UBI rhetoric that legitimizes workforce displacement and provides social stability without wage-regulation constraints. The constraint functions as pure coordination from their position: UBI rhetoric enables them to pursue automation strategies while maintaining social license. Exit option is arbitrage — they can exit to other labor regimes or markets if needed.
constraint_indexing:constraint_classification(global_ubi_movement_rhetoric, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FISCAL CONSERVATIVE GOVERNMENT (TANGLED ROPE) — State actors experience coordination (UBI rhetoric offers a legible policy frame) and extraction (the rhetoric suppresses acknowledgment of fiscal constraints and the distributional choices UBI requires). They can exit by refusing to adopt UBI, but bear political cost of opposing the rhetorical consensus. Moderate extraction because powerful agents retain agency — they experience constraint but not imprisonment.
constraint_indexing:constraint_classification(global_ubi_movement_rhetoric, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR MOVEMENT COUNTER-COALITION (SCAFFOLD) — Organized labor sees UBI rhetoric as temporary scaffolding that must be surpassed. The constraint is: UBI rhetoric suppresses employment-protection strategies during its rhetorical dominance, but labor organizing has a genuine sunset clause — worker power mobilization can shift the frame back to job guarantees, sectoral bargaining, and wage floors. Organized agents see the extraction as time-limited because they retain agency to build counter-power.
constraint_indexing:constraint_classification(global_ubi_movement_rhetoric, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ACADEMIC UBI INSTITUTION (PITON) — The academic UBI movement (research centers, journal articles, conference circuits) maintains performative engagement with empirical pilots while institutional careers depend on UBI rhetoric remaining salient. The movement's core function — coordination of intellectual discourse — has been largely replaced by theater: pilot programs are designed to produce supportive findings, empirical failures are compartmentalized, and the rhetorical consensus persists despite mixed or negative evidence. Theater ratio (0.65) reflects this degradation.
constraint_indexing:constraint_classification(global_ubi_movement_rhetoric, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global civilizational view, UBI rhetoric coordinates legitimate concerns about technological unemployment and social fragmentation while simultaneously extracting by suppressing analysis of its own distributional assumptions (who pays, who benefits, what replaces targeted programs). The constraint's extractiveness derives not from fraud but from the rhetorical closure that treats UBI as a solution rather than a choice among competing designs. The coordination function is real; the extraction is real.
constraint_indexing:constraint_classification(global_ubi_movement_rhetoric, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_ubi_movement_rhetoric_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_ubi_movement_rhetoric, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_ubi_movement_rhetoric, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_ubi_movement_rhetoric, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_ubi_movement_rhetoric, TR),
    TR >= 0.70.

:- end_tests(global_ubi_movement_rhetoric_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The rhetoric extracts value primarily by suppressing alternative policy frames and deferring material protection to an uncertain future while normalizing present displacement. The extraction is not maximal because genuine coordination function exists: UBI rhetoric does articulate real concerns about labor market fragmentation and dignity. The value reflects that the benefit/cost ratio is unfavorable for low-wage workers and welfare recipients, but the constraint is not pure extraction — some agents genuinely believe in UBI's optimality. Suppression (0.48): Moderate-high. The rhetorical constraint suppresses discussion of employment-protection strategies, sectoral bargaining, wage floors, and targeted welfare programs by treating UBI as inevitable or natural. However, suppression is not total — counter-rhetoric exists, labor movements continue organizing, and some scholars produce critical analyses. The value reflects active enforcement (UBI advocates actively suppress alternatives through discourse dominance) alongside incomplete suppression (alternatives persist despite rhetorical pressure). Theater ratio (0.65): Moderate-high. The UBI movement maintains substantial performative content: pilot programs are often small and atypical; academic consensus is overstated relative to empirical strength; policy proposals frequently gloss over distributional details and replacement architecture; carrier rhetoric focuses on utopian futures rather than implementation specifics. However, theater is not dominant — genuine intellectual and policy work occurs within the movement, and some analyses are rigorous. The value reflects degradation of empirical verification against rhetorical consensus, but not complete theatrical replacement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival gaps driven by directionality. The technology sector sees rope (pure coordination enabling their preferred labor regime) with d ≈ 0.15, experiencing negative effective extraction. Precarious workers see snare (complete suppression of alternatives with no exit) with d ≈ 0.95, experiencing maximal extraction. The UBI intellectual coalition sees rope (coordination of scientific discourse) but the piton classification reveals this is largely performative — they experience low chi because institutional actors with arbitrage options always do. The welfare-dependent household sees tangled rope because they benefit from UBI's dignity framing while losing targeted services — mixed experience reflecting d ≈ 0.50. Labor movements see scaffold because they retain agency to build counter-power with a sunset clause on UBI dominance. The gap reflects real differences in structural position, not disagreement about facts. All perspectives observe the same rhetorical output; they differ in their exit costs and benefit flows.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their beneficiary/victim status and exit options. Technology beneficiaries with arbitrage options (can exit to other labor regimes) experience low d ≈ 0.15, producing negative f(d) ≈ -0.01, so χ ≈ ε × (-0.01) = slightly negative extraction (they benefit). Precarious workers trapped by labor market necessity experience d ≈ 0.95, producing f(d) ≈ 1.42, so χ ≈ 0.52 × 1.42 × 1.0 ≈ 0.74 — maximum experienced extraction. UBI intellectuals with institutional power and arbitrage (can exit to other research topics) experience d ≈ 0.20, producing f(d) ≈ 0.02, so χ ≈ 0.52 × 0.02 × 1.2 ≈ 0.01 — minimal extraction, consistent with piton classification (they experience the constraint as low-cost theater). Welfare recipients with constrained exit (cannot easily exit means-tested dependence) experience d ≈ 0.50, producing f(d) ≈ 0.65, so χ ≈ 0.52 × 0.65 × 1.0 ≈ 0.34 — moderate extraction, consistent with tangled rope. Scope modifier σ(S) = 1.0 for national scope, 1.2 for global scope. The gap between institutional perspectives reflects that the same constraint produces positive χ (beneficiary) for tech sector and negative or minimal χ for workers — the arithmetic reveals why the constraint persists despite appearing suboptimal to those bearing extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   COORDINATION VS. EXTRACTION RESOLUTION: The UBI movement rhetoric avoids mandatrophy classification by exhibiting genuine dual function. The coordination component is measurable: UBI rhetoric successfully coordinates previously fragmented concerns about automation, social fragmentation, and dignity into a unified policy frame. This reduces transaction costs for political organizing and policy discussion. The extraction component is equally measurable: UBI rhetoric suppresses alternative frames (employment protection, wage regulation, sectoral bargaining) by treating them as inferior or impossible, thereby extracting political and discursive labor from actors who would prefer different mechanisms. The tangled rope classification holds because both functions are essential to the constraint's operation. Remove the coordination function (make UBI purely extractive) and the constraint would require explicit coercion; remove the extraction function (make UBI purely coordinative) and the constraint would simply be a policy proposal subject to normal democratic deliberation. The constraint's power derives from performing coordination while performing extraction simultaneously. The mandatrophy is resolved by recognizing that this is not a misclassification but a correct diagnosis of hybrid mechanisms that use genuine coordination value to suppress alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pilot_program_selection_bias,
    'Do UBI pilot programs systematically select for favorable initial conditions that do not generalize to permanent, universal implementation?',
    'Comparative analysis of pilot contexts (typically small, bounded, time-limited, often in developed economies) vs. structural requirements of permanent national UBI; examination of which pilots are cited vs. which are obscured in rhetorical consensus.',
    'If selection bias confirmed: UBI rhetoric extracts by generalizing from non-generalizable evidence. Extractiveness rises from 0.52 to 0.65+. If pilots are representative: coordination function is stronger, extraction lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pilot_program_selection_bias, empirical, 'Whether UBI pilot programs select for favorable initial conditions').

omega_variable(
    replacement_welfare_architecture,
    'Does UBI rhetoric suppress serious analysis of what welfare architecture replaces targeted programs, and does this suppression serve specific beneficiary interests?',
    'Content analysis of UBI advocacy materials regarding means-tested programs (disability, housing, health); tracking of policy proposals that explicitly phase out existing programs vs. additive proposals; longitudinal discourse analysis of program-replacement framing.',
    'If suppression confirmed and traceable to beneficiary interests (tech sector, fiscal conservatives): extraction component is deliberate institutional enforcement. If displacement is accidental or genuinely navigated: constraint is coordination-dominant. Classification may shift toward snare if suppression is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replacement_welfare_architecture, empirical, 'Whether UBI rhetoric suppresses analysis of welfare program replacement').

omega_variable(
    employment_displacement_timeline,
    'What is the empirical relationship between automation adoption and UBI adoption timelines? Does rhetoric assume simultaneous implementation when empirical history shows technology displaces faster than policy?',
    'Historical analysis of technology adoption curves vs. policy adoption windows; modeling of lag times between job displacement and effective UBI implementation; examination of what happens to workers during the gap.',
    'If significant lag confirmed: UBI rhetoric extracts by promising future solutions to present precarity. Suppression component rises. If timelines can align: coordination function strengthens. This determines whether the constraint''s beneficiaries experience genuine coordination or receive extraction-enabling ideology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_displacement_timeline, empirical, 'Temporal relationship between automation and UBI policy adoption').

omega_variable(
    identity_lock_in_intellectual_coalition,
    'Is the UBI intellectual coalition''s commitment to universal cash transfer framing driven by empirical analysis or by identity fusion with the UBI movement itself?',
    'Longitudinal tracking of intellectual positions: do scholars who have publicly committed to UBI rhetoric adjust positions when new evidence contradicts the framework? Analysis of career trajectories of UBI-movement defectors vs. movement advocates; examination of organizational costs to changing positions within the UBI infrastructure.',
    'If identity lock confirmed: the constraint''s extractiveness derives from institutional inertia + intellectual capture, not from the rhetorical merits. The piton classification becomes central. If intellectual flexibility preserved: the movement retains a genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_intellectual_coalition, conceptual, 'Whether UBI intellectual coalition exhibits identity lock').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_ubi_movement_rhetoric, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ubi_rhet_tr_t0, global_ubi_movement_rhetoric, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ubi_rhet_tr_t3, global_ubi_movement_rhetoric, theater_ratio, 3, 0.58).
narrative_ontology:measurement(ubi_rhet_tr_t6, global_ubi_movement_rhetoric, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(ubi_rhet_be_t0, global_ubi_movement_rhetoric, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ubi_rhet_be_t3, global_ubi_movement_rhetoric, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(ubi_rhet_be_t6, global_ubi_movement_rhetoric, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_ubi_movement_rhetoric, identity_coordination).
narrative_ontology:affects_constraint(global_ubi_movement_rhetoric, automation_labor_displacement).
narrative_ontology:affects_constraint(global_ubi_movement_rhetoric, welfare_state_architecture).
narrative_ontology:affects_constraint(global_ubi_movement_rhetoric, worker_bargaining_power_erosion).

% DUAL FORMULATION NOTE:
% UBI movement rhetoric is downstream of technological unemployment anxiety and upstream of policy implementation decisions. Separate constraint stories should model automation displacement (the technical constraint) vs. rhetorical framing (this story) vs. specific UBI policy proposals (electoral constraint). The rhetoric mediates between the technical reality and policy choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
