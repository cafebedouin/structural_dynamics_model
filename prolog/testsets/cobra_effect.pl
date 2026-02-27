% ============================================================================
% CONSTRAINT STORY: cobra_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cobra_effect, []).

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
 *   constraint_id: cobra_effect
 *   human_readable: The Cobra Effect (Perverse Bounty Incentive)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Cobra Effect exemplifies how incentive mechanisms designed to solve
 *   public problems become vehicles for extraction when the gap between
 *   stated intent and verifiable outcome is wide. The original case — the
 *   British colonial government offering bounties for dead cobras in Delhi,
 *   leading to cobra farming — reveals a structural phenomenon: when
 *   compensation is tied to a proxy (dead cobra skins) rather than the actual
 *   goal (reduced cobra population), agents optimize for the proxy, rendering
 *   both the bounty and the stated problem-solving orthogonal. The constraint
 *   exhibits high extractiveness (0.58) and suppression (0.65) because
 *   fabricators (those creating fake bounty claims) have lower setup costs
 *   than legitimate problem-solvers, and the verification burden falls on a
 *   powerless public resource commons. Theater ratio (0.68) reflects that
 *   bounty programs continue partly as performative policy despite known
 *   perverse effects. The constraint is tangled rope at the core: it has a
 *   genuine coordination function (it does coordinate many agents around a
 *   simple rule) but that coordination produces extraction (fabricators
 *   benefit, legitimate solvers and public resources lose). This story
 *   demonstrates how a constraint can classify as six different types from
 *   six different structural positions — a diagnostic exemplar for indexical
 *   classification.
 *
 * KEY AGENTS:
 *   - Public Resource Commons: Primary victim (powerless/trapped) — budget diverted to fabricated claims
 *   - Legitimate Problem Solvers: Secondary victim (moderate/constrained) — face higher costs than fabricators; crowded out of the market
 *   - Bounty Claim Fabricators: Primary beneficiary (institutional/arbitrage) — exploit gap between stated bounty criterion and verification capability
 *   - Program Administrators: Secondary beneficiary (institutional/arbitrage) — maintain institutional legitimacy by executing the program as designed
 *   - Policy Reformers and Second-Order Institutions: Organized actors (organized/constrained) — see both coordination and extraction; constrained by political barriers to reform
 *   - The Bounty System as Institution: Piton actor (institutional/arbitrage) — persists through inertia and theater despite degraded function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cobra_effect, 0.58).
domain_priors:suppression_score(cobra_effect, 0.65).
domain_priors:theater_ratio(cobra_effect, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cobra_effect, extractiveness, 0.58).
narrative_ontology:constraint_metric(cobra_effect, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cobra_effect, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cobra_effect, tangled_rope).
narrative_ontology:human_readable(cobra_effect, "The Cobra Effect (Perverse Bounty Incentive)").
narrative_ontology:topic_domain(cobra_effect, "economic/political").

domain_priors:requires_active_enforcement(cobra_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cobra_effect, bounty_claim_fabricators).
narrative_ontology:constraint_beneficiary(cobra_effect, program_administrators).
narrative_ontology:constraint_victim(cobra_effect, public_resource_allocation).
narrative_ontology:constraint_victim(cobra_effect, legitimate_problem_solvers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC RESOURCE COMMONS (SNARE) — The bounty program's budget becomes diverted to fabricated claims rather than solving the stated problem. Public resources are trapped in a mechanism designed to help but structured to extract: no exit, no recourse, maximum experienced extraction as funds flow to counterfeit solutions.
constraint_indexing:constraint_classification(cobra_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGITIMATE PROBLEM SOLVERS (SNARE) — Constrained by high setup costs (R&D, iteration, proof of genuine solution) versus low setup costs for fabricators (manufacturing fake cobra skins, creating false reports). Forced to compete on terms that make real solutions uneconomical. Extraction runs toward the fabricators; legitimate solvers bear the cost of program degradation.
constraint_indexing:constraint_classification(cobra_effect, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BOUNTY CLAIM FABRICATORS (ROPE) — Experience the constraint as pure coordination: collect whatever qualifies as a bounty claim (fabricated cobra skins, falsified reports) within the rules as written. The mechanism coordinates their behavior around a simple rule. Net beneficiary — they arbitrage the gap between stated intent and contractual specification.
constraint_indexing:constraint_classification(cobra_effect, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PROGRAM ADMINISTRATORS (ROPE) — Coordinate on a simple rule: pay verified bounty claims. The rule works perfectly. Extraction from their perspective is minimal — they see themselves as efficient allocators following clear contract specifications. Beneficiary because the mechanism allows them to distribute funds as designed, even if design has perverse effects.
constraint_indexing:constraint_classification(cobra_effect, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: POLICY REFORMERS (TANGLED ROPE) — See the cobra effect as a genuine coordination problem (bounty verification requires ex-post inspection capability that doesn't exist) AND an extraction mechanism (the gap between stated and actual bounty targets benefits fabricators at the expense of real solutions). Organized but constrained by political barriers to redefining bounty criteria. Must balance reducing fabrication against side effects of tighter verification (higher transaction costs, reduced participation from legitimate solvers).
constraint_indexing:constraint_classification(cobra_effect, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: BOUNTY SYSTEM AS INSTITUTIONAL FORM (PITON) — The bounty mechanism persists as a performative ritual: announcing a bounty signals government concern and action (theater) while the mechanism itself has degraded function. Post-cobra-effect, the system continues not because it solves problems but because alternatives (direct government research, mandates, regulatory reform) require higher political costs. Theater ratio is high — public announcement of 'bounty program' creates appearance of incentive alignment despite known perverse effects.
constraint_indexing:constraint_classification(cobra_effect, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: VERIFICATION-AUGMENTED BOUNTY REFORM (SCAFFOLD) — A sunset clause exists: if verification mechanisms can be implemented (lab testing, third-party auditing, randomized inspections), the cobra effect is structurally solvable. Extraction declines as verification capability increases. Organized actors (auditing firms, universities, regulatory bodies) can build verification infrastructure that bypasses the fabrication mechanism. High suppression currently (fabricators can evolve methods faster than verification), but declining over time as infrastructure matures.
constraint_indexing:constraint_classification(cobra_effect, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cobra_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cobra_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cobra_effect, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cobra_effect, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cobra_effect, TR),
    TR >= 0.70.

:- end_tests(cobra_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The bounty mechanism extracts value from the public commons and diverts it to fabricators. The value is not consumed — fabricators are not solving the stated problem, so resources are wasted rather than transferred. Measured relative to the original stated goal (cobra reduction or problem-solving), extractiveness is high. But measured relative to the bounty-as-written (payment for verified claims), extractiveness is lower — the mechanism performs as specified, which is the problem. Suppression (0.65): High. Multiple barriers prevent legitimate problem-solvers from competing: (1) fabricators have lower setup costs, (2) verification is expensive and delayed, (3) risk of program collapse deters real investment, (4) publication of the cobra effect itself signals that bounties are unreliable. Theater ratio (0.68): Moderate-high. Bounty programs continue partly as public performance of concern and action, despite structural evidence they don't work. The ritual of announcing a bounty has signaling value (appears responsive) even when the mechanism is known to be perverse.
 *
 * PERSPECTIVAL GAP:
 *   The cobra effect is the canonical example of how a constraint can be rope (simple coordination rule) and snare (structural extraction) simultaneously, depending on perspective. The constraint resolves the mandatrophy by showing that the beneficiaries' rope experience and the victims' snare experience are both structurally real. The program coordinates fabricators, administrators, and claim evaluators perfectly around a simple rule: 'Pay verified claims.' That rule works. But the rule's design (claim-based rather than outcome-based) creates extraction relative to the stated goal. This is tangled rope: genuine coordination mixed with genuine extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is determined by their structural relationship to the bounty-extraction flow. Fabricators have d ≈ 0.05 (full beneficiary, arbitrage exits) — they face no extraction, they arbitrage the gap between stated criterion and verification gap. Administrators have d ≈ 0.10 (beneficiary, arbitrage exits) — they maintain institutional legitimacy by executing the program; extraction is minimal from their perspective. Legitimate problem-solvers have d ≈ 0.75 (victim, constrained exits) — they face extraction but retain some agency (can invest in verification, can seek alternative funding). Public resource commons has d ≈ 0.95 (full victim, trapped) — abstract collective good with no exit and no recourse; bears maximum experienced extraction. Policy reformers have d ≈ 0.65 (partial victim, constrained exits) — trapped between acknowledging program failure and political cost of reform.
 *
 * MANDATROPHY ANALYSIS:
 *   COBRA EFFECT AS TANGLED ROPE RESOLUTION: The cobra effect resolves the mandatrophy between 'This is pure coordination' and 'This is pure extraction' by being both. From the beneficiary perspective, it is pure coordination — a clear rule that coordinates multiple agents. From the victim perspective, it is pure extraction — the rule extracts value from legitimate problem-solvers and public resources. The constraint's extractiveness and suppression are high enough to classify as tangled rope at the core analytical level: it has a genuine coordination function (rule-following for fabricators) AND asymmetric extraction (legitimate solvers and public resources lose). The classification prevents both false-positive detection of pure coordination (mistaking the mechanism's efficient rule-following for beneficial coordination) and false-positive detection of pure coercion (mistaking the mechanism for a snare when beneficiaries actually experience it as rope). The mandatrophy is resolved: tangled rope is the correct type because the constraint has both structural components — coordination that enables extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    claim_verification_cost_threshold,
    'At what verification cost does the bounty program become economically inviable compared to direct government action?',
    'Cost-benefit analysis comparing: bounty program with verification overhead vs direct government provision vs regulatory mandate; break-even verification cost calculation',
    'If verification cost < 20% of bounty pool: tangled rope (mixed coordination/extraction) persists. If verification cost > 50%: bounty becomes pure theater (piton). If > 80%: entire mechanism collapses and becomes uneconomical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(claim_verification_cost_threshold, empirical, 'The threshold at which verification costs exceed bounty efficiency').

omega_variable(
    fabrication_evolution_rate,
    'Can verification mechanisms evolve faster than fabrication techniques, or do fabricators perpetually stay ahead?',
    'Historical case study of bounty programs with active verification arms (India rat eradication, measles vaccination verification); comparison of time-to-detection for new fabrication methods vs time-to-deploy countermeasures',
    'If verification faster: scaffold sunset is real, system becomes rope over time. If fabrication perpetually ahead: snare is permanent, spiral of escalating verification theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fabrication_evolution_rate, empirical, 'Whether verification can outpace fabrication evolution').

omega_variable(
    political_exit_cost_for_reformers,
    'What is the political cost of admitting a bounty program failed, and does that cost prevent reform even when efficiency analysis shows it should?',
    'Historical analysis of bounty program reforms; political economy of sunk-cost commitment to failed programs; comparison of political risk of reform vs continuation',
    'If exit cost is high: piton classification is permanent (theater persists despite knowing dysfunction). If exit cost is low: policy can shift to rope or scaffold structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_exit_cost_for_reformers, preference, 'Political barriers to admitting and reforming failed bounty incentives').

omega_variable(
    information_asymmetry_fundamental,
    'Is the cobra effect fundamentally rooted in information asymmetry (verifier cannot tell fake from real) or in misalignment of incentives (real solutions are more costly than fakes)?',
    'Decomposition analysis: can perfect information without incentive realignment solve the problem? Can incentive realignment without information solve it? Which is the binding constraint?',
    'If information-binding: tangled rope is core, scaffolding verification solves it. If incentive-binding: snare is core, only changing the bounty structure (e.g., outcome-based vs claim-based) solves it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_fundamental, conceptual, 'Whether cobra effect is rooted in information asymmetry or incentive misalignment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cobra_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cobra_tr_t0, cobra_effect, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cobra_tr_t3, cobra_effect, theater_ratio, 3, 0.48).
narrative_ontology:measurement(cobra_tr_t6, cobra_effect, theater_ratio, 6, 0.62).
narrative_ontology:measurement(cobra_tr_t10, cobra_effect, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(cobra_be_t0, cobra_effect, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cobra_be_t3, cobra_effect, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(cobra_be_t6, cobra_effect, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(cobra_be_t10, cobra_effect, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cobra_effect, resource_allocation).
narrative_ontology:affects_constraint(cobra_effect, verification_bottleneck).
narrative_ontology:affects_constraint(cobra_effect, perverse_incentive_propagation).

% DUAL FORMULATION NOTE:
% The Cobra Effect is a family name for a class of perverse incentive structures. Specific instantiations (cobra bounties in Delhi, rat tail bounties in Vietnam, vaccination falsification in measurement-based programs) are separate constraint stories with domain-specific extractiveness values but share the same structural topology: claim-based bounty design creates lower-cost path via fabrication than via legitimate problem-solving. The general constraint story models the structural phenomenon; instantiation stories model specific extraction values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cobra_effect, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
