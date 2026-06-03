% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Border Control Legitimacy: Freedom of Movement Reading
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of a contested kernel in
 *   political philosophy and international law: the relationship between
 *   territorial sovereignty and border control authority. The reading
 *   presented here — freedom_of_movement_primary — asserts that freedom of
 *   movement is a fundamental human right and that territorial sovereignty
 *   does NOT entail authority to absolutely exclude non-citizens. Under this
 *   reading, state authority is limited to jurisdictional regulation:
 *   determining what rights and obligations apply to persons within the
 *   territory, not whether persons may enter at all. The constraint describes
 *   a snare from the perspective of the powerless (workers, asylum seekers)
 *   who are absolutely excluded; a rope from the perspective of capital which
 *   benefits from labor scarcity and precarity; a tangled rope from the
 *   perspective of receiving-nation citizens who experience both coordination
 *   benefits and extraction costs; a piton from the perspective of the border
 *   apparatus which maintains performative legitimacy despite degraded
 *   humanitarian function; and a tangled rope from the analytical observer
 *   who sees this reading as one coherent interpretation of the sovereignty
 *   kernel competing with the sovereignty_primary and
 *   jurisdictional_sovereignty readings. The measurement trajectory shows
 *   increasing extractiveness (0.38→0.58), suppression (0.52→0.72), and
 *   theater (0.48→0.65) over the 40-year interval, reflecting intensifying
 *   border enforcement apparatus, rising enforcement budgets, expanding
 *   digital surveillance and biometric systems, and simultaneously rising
 *   humanitarian rhetoric alongside rising exclusions — a classic pattern of
 *   extraction masked by performative exception-making.
 *
 * KEY AGENTS:
 *   - Displaced Workers: Primary victims (powerless/trapped) — excluded from labor markets and fundamental freedom of movement; no voice in legitimacy of closure decisions
 *   - Asylum Seekers & Stateless Persons: Primary victims (powerless/trapped) — systematically excluded despite humanitarian obligations; suppression includes legal delegitimation
 *   - Capital Demanding Labor: Primary beneficiaries (institutional/arbitrage) — benefits from labor shortage, wage suppression, and workforce precarity; can arbitrage through lobbying for worker visas, points systems, exemptions
 *   - Receiving Nation Citizens: Mixed position (organized/constrained) — benefit from perceived labor market stability and public service capacity; constrained by political participation costs and identification with border enforcement
 *   - Border Enforcement Apparatus: Institutional actor (institutional/arbitrage) — maintains performative legitimacy (humanitarian exceptions, due process) while systematically excluding vulnerable populations; high arbitrage options for redirecting focus
 *   - Freedom of Movement Principle: Abstract victim — the normative principle itself is suppressed and delegitimized by being declared subordinate to sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.58).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.72).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, snare).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Border Control Legitimacy: Freedom of Movement Reading").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, '081a6347-9f4c-467b-b9a9-eea1f08a3ae0').
narrative_ontology:cs_kernel_codification('081a6347-9f4c-467b-b9a9-eea1f08a3ae0', distributed).
narrative_ontology:cs_authority_grounding('081a6347-9f4c-467b-b9a9-eea1f08a3ae0', extraction).
narrative_ontology:cs_interpretation_layer_present('081a6347-9f4c-467b-b9a9-eea1f08a3ae0').
narrative_ontology:cs_reading_relation('081a6347-9f4c-467b-b9a9-eea1f08a3ae0', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('081a6347-9f4c-467b-b9a9-eea1f08a3ae0', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_axiom('081a6347-9f4c-467b-b9a9-eea1f08a3ae0', foundational, freedom_of_movement_fundamental).
narrative_ontology:cs_axiom_status(freedom_of_movement_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('081a6347-9f4c-467b-b9a9-eea1f08a3ae0', freedom_of_movement_fundamental, deontological).
narrative_ontology:cs_axiom('081a6347-9f4c-467b-b9a9-eea1f08a3ae0', foundational, sovereignty_not_closure_authority).
narrative_ontology:cs_axiom_status(sovereignty_not_closure_authority, holdable).
narrative_ontology:cs_axiom_grounding('081a6347-9f4c-467b-b9a9-eea1f08a3ae0', sovereignty_not_closure_authority, deontological).
narrative_ontology:cs_reference_frame('081a6347-9f4c-467b-b9a9-eea1f08a3ae0', universal_human_rights_regime).
narrative_ontology:cs_drift_state('081a6347-9f4c-467b-b9a9-eea1f08a3ae0', contemporary_border_intensification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('081a6347-9f4c-467b-b9a9-eea1f08a3ae0', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, capital_demanding_labor).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, receiving_nation_employers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, displaced_workers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, stateless_persons).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, freedom_of_movement_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED WORKER (SNARE) — Faces absolute exclusion enforced by border apparatus with no legitimate voice in closure decisions. Extraction mechanism: capital benefits from labor shortage/wage suppression while workers bear enforcement costs and exclusion. No exit option; cannot challenge the legitimacy of their own exclusion within the closure framework.
constraint_indexing:constraint_classification(border_control_legitimacy__freedom_of_movement_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ASYLUM SEEKER (SNARE) — Faces closure authority presented as absolute sovereign right despite humanitarian obligations and freedom of movement principle. Cannot credibly claim rights or exit; suppression includes legal delegitimation of their exit status (classification as illegal, criminal, or security threat). Extraction: states benefit from maintaining labor control and border apparatus; asylum seekers bear full cost of exclusion including death in transit.
constraint_indexing:constraint_classification(border_control_legitimacy__freedom_of_movement_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CAPITAL DEMANDING LABOR (ROPE) — Experiences border closure as enabling labor shortage, wage suppression, and workforce precarity. Coordination function: border control solves the coordination problem of preventing worker organization across borders (capital cannot organize globally but labor can). The constraint legitimates labor market segmentation. Net beneficiary with full arbitrage options (can lobby for exceptions, points-based systems, temporary worker programs).
constraint_indexing:constraint_classification(border_control_legitimacy__freedom_of_movement_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RECEIVING NATION CITIZENS (TANGLED ROPE) — Mix of genuine coordination (labor market stability, public services allocation) and extraction. Citizens face genuine uncertainty about wage/employment effects and public service capacity. Constraint provides coordination function (allocates entry rights) alongside asymmetric extraction (workers bear costs of exclusion that citizens experience as benefits). Exit options constrained by political participation costs and border enforcement dependence.
constraint_indexing:constraint_classification(border_control_legitimacy__freedom_of_movement_primary, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: BORDER ENFORCEMENT APPARATUS (PITON) — Maintains elaborate procedural legitimacy (due process, humanitarian exceptions, appeals mechanisms) that is substantially performative. The apparatus claims to balance freedom of movement with sovereignty, but the balance is theater — enforcement systematically excludes absent genuine humanitarian override. Theater persists through institutional inertia despite recognition that the apparatus is degraded relative to its stated mission. High arbitrage options (can redirect toward immigration control, security focus, or selective labor migration).
constraint_indexing:constraint_classification(border_control_legitimacy__freedom_of_movement_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FREEDOM OF MOVEMENT READING (TANGLED ROPE) — From the freedom-of-movement frame, border closure extracts from workers (denies fundamental right) while coordinating labor market segmentation for capital. The sibling sovereignty readings would classify this as mountain (closure is inherent right) or rope (jurisdiction without closure authority). From this reading, the constraint is structurally extraction-and-coordination hybrid: genuine coordination of labor supply AND asymmetric extraction from those excluded. This perspectival gap reveals the kernel contest.
constraint_indexing:constraint_classification(border_control_legitimacy__freedom_of_movement_primary, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(border_control_legitimacy__freedom_of_movement_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(border_control_legitimacy__freedom_of_movement_primary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, TR),
    TR >= 0.70.

:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from excluded workers (denies them labor opportunities, income, freedom of movement) while benefiting capital (suppressed wages, precarious workforce) and capital-receiving states (labor control, political advantage). The extraction is not maximal (0.66+) because some legitimate public goods coordination exists — societies do coordinate public service capacity and labor market adjustment. But the extraction is substantial because the coordination justification is applied selectively: wealthy, skilled workers face far lower barriers than poor, low-skilled workers, revealing that the stated coordination function masks labor market stratification. Suppression (0.72): High. Barriers to exit include: (1) legal prohibition enforced by state violence, (2) criminalization of undocumented migration, (3) exclusion from legal remedies and appeals, (4) lack of organized voice for excluded populations, (5) physical barriers (walls, checkpoints, maritime blockades), (6) documentation requirements that stateless/displaced persons cannot meet. Suppression rising over the interval reflects increasing enforcement infrastructure, biometric systems, employer sanctions, and digital surveillance. Theater ratio (0.65): Moderate-high. The border apparatus maintains elaborate humanitarian exceptions (asylum, refugee status, family reunification), humanitarian rhetoric, and procedural legitimacy (due process, appeals) while systematically denying protection and creating conditions that push vulnerable populations toward dangerous routes. The ratio rises over the interval as the gap widens between stated humanitarian commitments and exclusion outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The freedom_of_movement_primary reading produces snare/tangled_rope/piton classifications from different agents, while the sibling sovereignty_primary reading would produce rope/mountain/piton from the same agent positions. The perspectival gap reveals the kernel contest: whether border closure is a legitimate exercise of sovereignty (sovereignty_primary, mountain from the state), a mechanism of labor market coordination (jurisdictional_sovereignty, tangled_rope from the state), or extraction masked by sovereignty rhetoric (freedom_of_movement_primary, snare from workers). The analytical observer at the tangled_rope level sees this reading as coherent but structurally contested — the other readings are not incoherent, they are alternative readings of the same kernel with different normative grounding.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values for each perspective are derived from structural relationship to the exclusion mechanism. Displaced workers (powerless/trapped) have d≈0.95 (full target): they bear all costs of closure and receive no benefits. Asylum seekers (powerless/trapped) have d≈0.93 (slight variation due to humanitarian exception possibility): similarly positioned but with marginally non-zero exit possibility through exception channels. Capital (institutional/arbitrage) has d≈0.12 (partial beneficiary): benefits from labor scarcity and precarity but also experiences some costs (skilled worker retention, brain drain, consumer base reduction). Receiving-nation citizens (organized/constrained) have d≈0.52 (symmetric): both benefit from perceived security/service provision and bear costs of labor shortage, cultural friction, and enforcement apparatus burden. The border apparatus (institutional/arbitrage) has d≈0.25 (partial beneficiary): benefits from budget expansion and political importance but experiences costs of humanitarian criticism and operational complexity. These d values feed the sigmoid f(d) to produce experienced extractiveness (χ) for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the potential mandatrophy by clarifying that the classification (snare for workers, tangled_rope for citizens, piton for apparatus, tangled_rope for analytical observer) is perspectival and kernel-dependent. If the sovereignty_primary reading were adopted, the same constraint would classify as rope or mountain (closure is a natural right). If the jurisdictional_sovereignty reading were adopted, the same constraint would classify more clearly as tangled_rope with lower extraction (legitimacy is conditional, not absolute). The mandatrophy is not 'what is the true classification' but 'which kernel reading are you adopting?' The structure is stable; the classification is reading-relative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamental_right_grounding,
    'Is freedom of movement grounded in natural rights (deontological) or in instrumental human flourishing (consequentialist)?',
    'Philosophical coherence analysis; review of international humanitarian law grounding statements; examination of whether restrictions are justified instrumentally or denied as categorically impermissible',
    'If deontological: sovereignty reading forecloses freedom-of-movement reading (cannot restrict fundamental rights). If consequentialist: both readings coexist (right can be weighed against state interests). This omega determines reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamental_right_grounding, conceptual, 'Philosophical grounding of freedom of movement as fundamental right').

omega_variable(
    sovereignty_entailment,
    'Does territorial sovereignty logically entail authority to exclude non-citizens, or is sovereignty only jurisdictional (power to regulate within territory)?',
    'Analysis of state practice vs. legal doctrine; examination of whether states without closure authority (Schengen zone, historical open borders) lose sovereignty; assessment of whether closure is definitional or contingent',
    'If closure is entailed: sovereignty_primary reading is logically necessary (forecloses freedom-of-movement). If sovereignty is only jurisdictional: freedom_of_movement_primary and jurisdictional_sovereignty coexist as alternative readings of the same sovereignty foundation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_entailment, conceptual, 'Whether sovereignty logically entails border closure authority').

omega_variable(
    extraction_vs_coordination_boundary,
    'Does the border control apparatus primarily coordinate labor markets / public services, or primarily extract from excluded workers?',
    'Empirical analysis of wage effects, employment stability, and public service provision with/without closure; examination of whether documented benefits flow to capital vs. receiving-nation workers; assessment of counterfactual outcomes under open borders',
    'If primarily coordination: tangled_rope classification holds for moderate agents. If primarily extraction: snare classification rises; coordination function becomes cover story for pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether border control primarily serves coordination or extraction function').

omega_variable(
    humanitarian_exception_efficacy,
    'Do humanitarian exceptions (asylum, refugee status, family reunification) constitute genuine escape valves from the border closure snare, or are they theater that systematically excludes vulnerable populations?',
    'Empirical analysis of exception grant rates, processing times, and outcomes for vulnerable populations; comparison of humanitarian exception criteria against actual displacement drivers; assessment of whether exceptions reduce overall extraction magnitude',
    'If genuine escape valves: suppression metric (0.72) is overstated; constraint reclassifies toward tangled_rope. If theater: suppression is understated and extraction floor rises; constraint reclassifies toward pure snare (ε > 0.66, χ > 0.75).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_exception_efficacy, empirical, 'Whether humanitarian exceptions provide genuine relief or function as performative theater').

omega_variable(
    reading_kernel_identity,
    'Is this reading instantiating a genuine alternative kernel interpretation, or is it a normative claim about what the kernel SHOULD entail?',
    'Examination of whether freedom-of-movement-primary is a coherent reading of actual state practice and legal doctrine, or a prescriptive claim about what legitimacy requires. Assessment of whether any state system has adopted this reading as its operative principle.',
    'If coherent reading: the kernel contest is real and both sides claim the same underlying authority structure. If prescriptive: the reading is a reformist claim, not an alternative kernel interpretation — should be modeled as a distinct prescriptive constraint rather than a sibling reading of the same kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Whether freedom-of-movement-primary is a kernel reading or a normative prescription').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bclfm_tr_t0, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0, 0.48).
narrative_ontology:measurement(bclfm_tr_t20, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 20, 0.58).
narrative_ontology:measurement(bclfm_tr_t40, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(bclfm_be_t0, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bclfm_be_t20, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(bclfm_be_t40, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bclfm_su_t0, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(bclfm_su_t20, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(bclfm_su_t40, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, resource_allocation).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__jurisdictional_sovereignty).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__sovereignty_primary).

% DUAL FORMULATION NOTE:
% The border_control_legitimacy kernel decomposes into three constraint stories, one for each reading. Each story instantiates a different kernel interpretation with different ε values and victim/beneficiary structures. The freedom_of_movement_primary reading (this story) treats displaced workers as victims and produces ε=0.58, snare; the sovereignty_primary reading treats sovereignty as immutable and produces ε≈0.15, mountain/rope; the jurisdictional_sovereignty reading treats closure as contingent on consent and produces ε≈0.40, tangled_rope. These are not the same constraint viewed from different angles — they are genuinely different structural claims about which agents are victimized and whether closure is entailed by sovereignty. Link them via affects_constraints to show they are alternative readings of one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_control_legitimacy__freedom_of_movement_primary, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
