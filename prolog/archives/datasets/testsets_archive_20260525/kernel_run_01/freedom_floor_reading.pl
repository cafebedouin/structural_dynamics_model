% ============================================================================
% CONSTRAINT STORY: freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_freedom_floor_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor and Labor Market Exit Capacity
 *   domain: political_economy/welfare_policy/labor_markets
 *
 * SUMMARY:
 *   Unconditional income support (UIS) — a regular cash transfer to all
 *   citizens with no work requirement or behavioral condition — instantiates
 *   a contested commitment to what income policy is 'for'. This constraint
 *   models ONE reading of that kernel: the freedom_floor_reading, which
 *   claims UIS primarily enables autonomy, dignity, and labor market exit
 *   capacity by removing the coercive desperation that forces acceptance of
 *   exploitative arrangements. Under this reading, UIS solves a coordination
 *   problem: it aligns the recognition of human capacity (for labor, care,
 *   creativity) with material survival, removing the false necessity that
 *   market prices alone determine human value. The constraint exhibits pure
 *   coordination dynamics (Rope) across all perspectives — beneficiaries
 *   experience it as enabling choice, employers experience it as improving
 *   labor market efficiency despite losing coercive power, and the tax system
 *   experiences it as funding labor productivity and reducing
 *   desperation-driven costs. The measured extractiveness (0.12) is low,
 *   reflecting that the constraint's primary function is coordination rather
 *   than extraction. However, the existential contest with sibling readings
 *   (dependency_trap_reading and targeting_efficiency_reading) means the
 *   freedom_floor_reading itself rests on empirically contestable claims:
 *   whether UIS actually enhances autonomy (vs creating dependence), whether
 *   universality actually reduces stigma (vs targeting efficiency), and
 *   whether the fiscal limits that may constrain the benefit reflect economic
 *   reality or political choice. These ambiguities are the kernel context —
 *   the readings coexist because the underlying commitment is genuinely
 *   under-determined by evidence alone.
 *
 * KEY AGENTS:
 *   - Precarious workers: Primary beneficiary (powerless/mobile) — gain exit capacity from coercive employment; experience the constraint as pure coordination enabling choice
 *   - Caregivers (unpaid labor): Primary beneficiary (moderate/constrained) — gain recognition and material support for care work; experience the constraint as coordination validating non-market labor
 *   - Entrepreneurs / Artists: Primary beneficiary (powerful/mobile) — gain funding for high-risk or non-commercializable productive activity; experience the constraint as coordination enabling innovation
 *   - Employers / Wage-setters: Secondary actor (powerful/constrained) — lose monopsony power but gain coordination benefits (worker retention, productivity, reduced turnover); experience constraint as tangled rope
 *   - Labor market participants (wage-earning public): Diffuse beneficiary (moderate/mobile) — benefit from reduced desperation-driven wage suppression and improved skill matching; experience the constraint as coordination improving market efficiency
 *   - Tax base / Revenue system: Institutional actor (institutional/arbitrage) — benefit from reduced desperation-driven costs and higher productivity; experience the constraint as coordination funding economic health
 *   - Analytical observer: Civilizational view (analytical/analytical) — sees the constraint as resolving a fundamental separation of human capacity from material survival; risk of naturalizing contingent political choices as structural necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(freedom_floor_reading, 0.12).
domain_priors:suppression_score(freedom_floor_reading, 0.08).
domain_priors:theater_ratio(freedom_floor_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(freedom_floor_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(freedom_floor_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(freedom_floor_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(freedom_floor_reading, rope).
narrative_ontology:human_readable(freedom_floor_reading, "Unconditional Income Support as Freedom Floor and Labor Market Exit Capacity").
narrative_ontology:topic_domain(freedom_floor_reading, "political_economy/welfare_policy/labor_markets").

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(freedom_floor_reading, formalized).
narrative_ontology:cs_authority_grounding(freedom_floor_reading, distributed).
narrative_ontology:cs_kernel_id(freedom_floor_reading, income_support_commitment).
narrative_ontology:cs_reading_relation(freedom_floor_reading, dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation(freedom_floor_reading, targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom(freedom_floor_reading, foundational, income_support_enables_autonomy).
narrative_ontology:cs_axiom_status(income_support_enables_autonomy, holdable).
narrative_ontology:cs_axiom(freedom_floor_reading, foundational, universality_eliminates_stigma_suppression).
narrative_ontology:cs_axiom_status(universality_eliminates_stigma_suppression, holdable).
narrative_ontology:cs_reference_frame(freedom_floor_reading, human_dignity_floor).
narrative_ontology:cs_drift_state(freedom_floor_reading, contemporary_welfare_state, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(freedom_floor_reading, artists_entrepreneurs).
narrative_ontology:constraint_beneficiary(freedom_floor_reading, wage_earners_generally).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (ROPE) — Unconditional income support solves a genuine coordination problem: the worker can now exit exploitative employment without facing homelessness or starvation. The constraint removes the coercive dependence that forces acceptance of wage theft, unsafe conditions, or degradation. The worker experiences this as pure coordination — a framework enabling genuine choice. No extraction: the worker benefits fully from the exit capacity the program provides.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: CAREGIVER (ROPE) — Unconditional income support enables the caregiver (parent, elder-care provider, community support worker) to maintain autonomy while performing essential care labor that would otherwise force dependence on earnings from market work or on a spouse/partner. The constraint coordinates the valuation and sustainability of care work. No extraction: the caregiver gains genuine dignity and choice capacity they lacked when income and care were decoupled.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENTREPRENEUR / ARTIST (ROPE) — Unconditional income support enables risk-taking and creative labor that market forces alone would not fund. The constraint coordinates the funding of non-commercializable or high-risk productive activity. The beneficiary experiences pure coordination — the floor enables the innovation or cultural production that markets systematically underinvest in. No extraction to the program's beneficiary.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR MARKET PARTICIPATION (ROPE) — From an institutional/market perspective, unconditional income support coordinates labor supply and demand at a higher welfare equilibrium. It removes the coercive desperation that forces workers into low-productivity, high-stress positions. It enables skill matching: workers can afford to search for work that fits their capacities rather than accepting the first job that prevents destitution. It increases bargaining power, raising wages at the margin for low-wage work. The constraint improves market coordination without extracting from market actors — it changes the boundary conditions that markets operate within.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EMPLOYER / WAGE-SETTER (TANGLED ROPE) — Unconditional income support constrains the employer's monopsony power by giving workers a credible exit option. The employer can no longer extract through desperation-driven wage suppression. However, the employer also benefits from the program: higher worker productivity, lower turnover costs, reduced social instability that disrupts supply chains, and a more skilled workforce enabled by the floor. The constraint is hybrid — it removes one extraction mechanism (desperation-driven wages) while providing genuine coordination benefits (worker retention, productivity, stability). From this perspective, effective extraction is moderate and positive: the employer loses coercive power but gains coordination efficiency.
constraint_indexing:constraint_classification(freedom_floor_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TAX BASE / REVENUE SYSTEM (ROPE) — Unconditional income support solves a coordination problem for the revenue system: it allocates resources to enable labor market participation and productivity, reducing costs elsewhere (emergency rooms for untreated poverty-driven illness, criminal justice for desperation-driven crime, mental health crisis response). The constraint coordinates funding levels with economic productivity. Pure coordination: the program enables the tax base itself by supporting workers and removing corrosive desperation.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (ROPE) — From a civilizational analytical perspective, unconditional income support solves a structural coordination problem: the separation of labor capacity from market valuation creates systemic desperation that forces acceptance of extractive arrangements. The constraint coordinates the recognition of human capacity for labor and care with material survival. It removes the false premise that market prices = human value. The analytical observer sees this as pure coordination — no extraction, only the resolution of a fundamental market failure where desperation is produced endogenously by the system itself.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(freedom_floor_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(freedom_floor_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(freedom_floor_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. Under the freedom_floor_reading, UIS is primarily a coordination mechanism — it solves the problem of sustaining human capacity and dignity when market wages alone would force desperation. The measured extractiveness reflects that no agent bears disproportionate costs relative to benefits; the program redistributes resources but does not extract in the sense of concentrating gains in beneficiary hands while imposing losses on victims. The modest rise from 0.05 to 0.12 over the interval reflects growing administrative overhead and potential inflation effects as the program scales, not growing extractive intent. Suppression (0.08): Very low. The freedom_floor_reading explicitly claims that universality (no means-test, no behavioral conditions) eliminates suppression by removing stigma and surveillance. Workers are not forced into the program; it is always available without shame or condition. The low suppression value reflects the design principle of unconditional access. Theater ratio (0.25): Low. Under the freedom_floor_reading, the program's function is transparent: income support enables exit capacity. Administrative performance metrics (take-up rates, benefit levels, payment timeliness) measure the actual coordination function, not a substitute. The modest theater reflects some administrative ritual and bureaucratic performance, but the core function is non-performative — the cash transfer is itself the entire mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives reveal remarkable coherence under the freedom_floor_reading — all seven perspectives classify as Rope or Tangled Rope, and all except the employer experience pure coordination benefits. This coherence is the reading's structural signature: if UIS is a freedom floor, it solves a coordination problem benefiting all participants except those who benefited from the previous desperation-driven regime. The perspectival gap is NOT within this reading but ACROSS readings — between the freedom_floor_reading (Rope) and the dependency_trap_reading (which would show Snare or Piton from beneficiary perspectives, with suppression rising over time) and the targeting_efficiency_reading (which would emphasize resource reach and optimize for fiscal efficiency, not autonomy). The gap between readings is documented in omega variables and cs_structure.reading_relations, not in perspectival divergence within a single reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) under this reading reflect the beneficiary relationship to the constraint's core function (enabling exit capacity and autonomy). Beneficiaries with trapped or desperation-driven exit options gain the most — for them, d is near 1.0 (full target of the benefit). But the reading interprets this as positive transformation, not extraction — the constraint is removing coercion, not imposing it. Employers and institutional actors benefit from improved coordination efficiency — their d is moderate (0.40–0.60) reflecting mixed effects (loss of monopsony power, gain in productivity). The analytical observer with arbitrage exit options experiences d around 0.50 (neutral) because they benefit from the improved labor market efficiency but are not the primary beneficiary. The freedom_floor_reading's directional structure is unusual: beneficiaries with high d (victims of desperation) experience positive transformation, while those with low d (institutional actors) experience efficiency gains. This inverts the usual extraction pattern where high d = negative experience. The inversion is the reading's analytical claim: that coercive desperation is the baseline being corrected, not a neutral starting point.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by claiming that the freedom_floor characterization is empirically grounded: if UIS genuinely enables exit capacity and autonomy, then the constraint is coordination (Rope), not extraction (Snare) or degradation (Piton). The mandatrophy is whether that claim is true — whether the empirical effects match the normative aspirations. The omega variables document the resolvable ambiguities: behavioral response (inflation), labor supply elasticity (whether exit is from exploitation or from participation), and sustainability (whether fiscal limits are structural or political). These are not resolved — they are the irreducible uncertainties of the reading. The sibling readings would produce different mandatrophy resolutions: dependency_trap_reading would show Snare (the constraint constrains agency despite appearing to enable it), and targeting_efficiency_reading would show Tangled Rope (genuine coordination of resource reach coupled with asymmetric targeting burden on the non-poor). The mandatrophy is resolved not by choosing a single correct reading but by explicitly holding all three and documenting their empirical and axiological differences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    funding_sustainability_threshold,
    'At what level does unconditional income support become fiscally unsustainable, and does that fiscal threshold constitute a structural limit (mountain) or a political choice (rope)?',
    'Comparative analysis of implemented UIS schemes (Finland, Kenya, Stockton CA) showing sustainable funding models; economic modeling of tax-base growth under varying UIS levels; historical analysis of whether fiscal limits reflect economic reality or political choice',
    'If fiscal limits are structural (independent of policy choice): classify aspects of the constraint as mountain. If fiscal limits are political (funding is technically feasible but politically contested): classification remains rope — the constraint is coordination, not immutable law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_sustainability_threshold, empirical, 'Distinction between fiscal sustainability (structural) and political choice in funding level').

omega_variable(
    behavioral_response_inflation,
    'Does unconditional income support cause inflation in rental markets, consumer goods, or service sectors that erodes the real purchasing power of the benefit?',
    'Comparison of price inflation in UIS-recipient areas vs non-recipient control areas; longitudinal tracking of real purchasing power for same-consumption basket; analysis of landlord rent-setting behavior in response to UIS deployment',
    'If inflation fully erodes benefit: effective extractiveness rises significantly (landlords, merchants extract the subsidy). If inflation is modest: the coordination reading holds. If inflation is zero or negative: the rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_response_inflation, empirical, 'Whether prices rise to capture UIS benefits, eroding real exit capacity').

omega_variable(
    labor_supply_elasticity_threshold,
    'At what level of UIS does labor force participation decline to economically unsustainable levels, and is that decline evidence of extraction (people choosing non-participation over exploitation) or market failure (people withdrawing from productive activity)?',
    'Comparative labor supply elasticity estimates across UIS deployments; distinction between workers exiting exploitative arrangements vs declining market participation; productivity and output effects; analysis of reallocation to care work, community participation, and non-market production',
    'If labor withdrawal reflects exit from exploitation: rope reading is confirmed — the constraint enables choice. If withdrawal represents collapse of market participation: the constraint may shift toward snare (if withdrawal is involuntary) or scaffold (if withdrawal is transitional during labor market restructuring).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_elasticity_threshold, empirical, 'Interpretation of labor supply changes under UIS: exit from exploitation vs market participation collapse').

omega_variable(
    reading_versus_dependency_trap,
    'This reading (''freedom floor'') claims UIS enables exit from coercive arrangements. The sibling ''dependency_trap_reading'' claims UIS creates psychological dependence and erodes motivation. Are these empirically distinct claims or merely different normative framings of the same phenomenon?',
    'Longitudinal psychological and behavioral studies of UIS recipients; comparison of motivation profiles and autonomy measures for same individuals before/after UIS receipt; analysis of whether observed behavior changes reflect rational response to changed incentives (rope) or internalized learned helplessness (snare)',
    'If readings are empirically distinct: the constraint''s classification depends on which effect dominates. If readings are normative framings of the same mechanism: both readings coexist in the kernel, and the committer frame (Rule 2 — omega variables) is the appropriate place to document the contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_dependency_trap, conceptual, 'Whether dependency effects and freedom-enabling effects are distinct or differently-framed aspects of the same mechanism').

omega_variable(
    universality_versus_targeting_coverage,
    'This reading assumes universality (UIS for all, eliminating stigma). The sibling ''targeting_efficiency_reading'' claims means-tested targeting maximizes resource reach. Is the universality claim empirically grounded or normatively prior?',
    'Comparative analysis of stigma effects in universal vs means-tested programs; measurement of take-up rates and deadweight loss; analysis of whether stigma is an artifact of implementation (surmountable via design) or intrinsic to targeting logic',
    'If stigma is empirically measurable and substantial: universality strengthens the rope classification by reducing suppression. If stigma is minimal: the distinction between universality and targeting becomes primarily a question of resource allocation efficiency, shifting the debate to the targeting_efficiency_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universality_versus_targeting_coverage, empirical, 'Whether universality provides measurable dignity/autonomy benefits beyond targeting efficiency').

omega_variable(
    committer_frame_axiom_disagreement,
    'This reading is ONE of three contestable readings of the income_support_commitment kernel. The readings disagree on whether UIS is primarily about freedom (this reading), dependency risk (sibling), or targeting efficiency (sibling). Is the disagreement resolvable through empirical evidence, or does each reading rest on irreducible normative premises about what income support is ''for''?',
    'Examination of the foundational axioms each reading holds (documented in cs_structure.axioms): if axioms are empirically testable claims about human behavior or social effects, disagreement is empirical. If axioms are normative commitments about what welfare policy should prioritize, disagreement is preference-based.',
    'If resolvable: provide empirical resolution path. If preference-based: document the axiom sets and the conditions under which each reading holds sway (e.g., freedom_floor_reading dominates if autonomy is prioritized; dependency_trap_reading dominates if labor market participation is prioritized).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_axiom_disagreement, conceptual, 'Whether the reading contest is empirical or axiomatically grounded').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(freedom_floor_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ffr_tr_t0, freedom_floor_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ffr_tr_t5, freedom_floor_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ffr_tr_t10, freedom_floor_reading, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(ffr_be_t0, freedom_floor_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(ffr_be_t5, freedom_floor_reading, base_extractiveness, 5, 0.1).
narrative_ontology:measurement(ffr_be_t10, freedom_floor_reading, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(freedom_floor_reading, dependency_trap_reading).
narrative_ontology:affects_constraint(freedom_floor_reading, targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% The freedom_floor_reading is one of three structurally distinct readings of the income_support_commitment kernel. The sibling readings (dependency_trap_reading and targeting_efficiency_reading) share the same policy object but differ in their claims about what the policy primarily does and whether those effects are beneficial or harmful. Each reading has its own ε value, perspectives, and beneficiary/victim structure, reflecting the genuine disagreement about the policy's actual effects and purposes. The network link documents that these are sibling readings of a common kernel, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
