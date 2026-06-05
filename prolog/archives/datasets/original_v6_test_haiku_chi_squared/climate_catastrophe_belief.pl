% ============================================================================
% CONSTRAINT STORY: climate_catastrophe_belief
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_catastrophe_belief, []).

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
 *   constraint_id: climate_catastrophe_belief
 *   human_readable: Belief in Inevitable Near-Term Climate Catastrophe
 *   domain: social/political/environmental
 *
 * SUMMARY:
 *   Widespread belief in inevitable near-term climate catastrophe has emerged
 *   as a structural constraint in contemporary society, measurably affecting
 *   individual planning horizons, political mobilization, psychological
 *   wellbeing, and institutional resource allocation. Polling data
 *   (YouGov/Economist, Feb 2026) shows ~47% of Americans believe they will
 *   personally experience catastrophic climate impacts within their lifetime.
 *   This constraint manifests as a tangled rope: it coordinates climate
 *   action (genuine coordination function) while simultaneously extracting
 *   psychological and economic costs from those who internalize the belief
 *   (extraction function). The tension between these functions — whether the
 *   catastrophe framing is necessary to motivate action or represents
 *   motivational inflation — defines the perspectival gap. From the viewpoint
 *   of climate advocacy infrastructure, the constraint is a rope
 *   (coordination mechanism enabling donor mobilization and policy urgency).
 *   From the viewpoint of individuals holding the belief, the constraint is a
 *   snare (no exit from the narrative, psychological burden without personal
 *   agency). The constraint's theater ratio (0.58) reflects increasingly
 *   performative climate coverage: dramatic framing and existential language
 *   replace technical accuracy and probabilistic updates. The extractiveness
 *   has risen from ~0.28 (2010, when catastrophe framing was new) to 0.52
 *   (2026, when it is institutionalized), indicating progressive
 *   centralization of the constraint's enforcement mechanisms.
 *
 * KEY AGENTS:
 *   - Catastrophe-Believing Citizen: Victim (powerless/trapped) — psychological cost without exit; cannot unsee the narrative or opt out of societal messaging
 *   - Climate Advocacy Organization: Beneficiary (institutional/arbitrage) — funds, political urgency, donor mobilization from catastrophe framing
 *   - Progressive Political Coalition: Mixed (organized/constrained) — benefits from electoral mobilization via catastrophe messaging; constrained by dependence on maintaining belief intensity
 *   - Mainstream Media: Institutional actor (institutional/arbitrage) — catastrophe framing drives engagement and audience; maintenance is performative rather than epistemic (piton)
 *   - Green Energy Investors: Secondary beneficiary (powerful/mobile) — capital allocation toward renewables; can exit catastrophe narrative if profitable alternatives emerge
 *   - Household Planners: Victim (moderate/constrained) — cannot plan long-term (mortgages, retirement, children's education) under near-term catastrophe belief
 *   - Climate Science Community: Analytical observer — produces evidence that warming is real and anthropogenic; but observes gap between scientific projections and political catastrophe framing
 *   - Conservative Political Opposition: Victim (organized/mobile) — constrained by inability to credibly contest climate science without appearing to deny anthropogenic warming; high exit cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_catastrophe_belief, 0.52).
domain_priors:suppression_score(climate_catastrophe_belief, 0.65).
domain_priors:theater_ratio(climate_catastrophe_belief, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_catastrophe_belief, extractiveness, 0.52).
narrative_ontology:constraint_metric(climate_catastrophe_belief, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_catastrophe_belief, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_catastrophe_belief, tangled_rope).
narrative_ontology:human_readable(climate_catastrophe_belief, "Belief in Inevitable Near-Term Climate Catastrophe").
narrative_ontology:topic_domain(climate_catastrophe_belief, "social/political/environmental").

domain_priors:requires_active_enforcement(climate_catastrophe_belief).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_catastrophe_belief, climate_advocacy_infrastructure).
narrative_ontology:constraint_beneficiary(climate_catastrophe_belief, green_energy_investors).
narrative_ontology:constraint_beneficiary(climate_catastrophe_belief, environmental_policy_advocates).
narrative_ontology:constraint_victim(climate_catastrophe_belief, psychological_wellbeing_general_population).
narrative_ontology:constraint_victim(climate_catastrophe_belief, long_term_economic_planning).
narrative_ontology:constraint_victim(climate_catastrophe_belief, intergenerational_trust).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CATASTROPHE-BELIEVING CITIZEN (SNARE) — Individual who internalizes near-term catastrophe belief has no exit: cannot unsee the narrative, cannot opt out of exposure to climate messaging, cannot alter systemic outcomes through individual action alone. Bears psychological cost (anxiety, despair, reduced planning horizon) without control over the constraint. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.80.
constraint_indexing:constraint_classification(climate_catastrophe_belief, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLIMATE ADVOCACY ORGANIZATION (ROPE) — Benefits from catastrophe framing: mobilizes donor funding, increases media attention, creates political urgency. Experiences the belief structure as a coordination mechanism for motivating action. Can arbitrage between donor pools (doom-responsive vs solution-focused). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(climate_catastrophe_belief, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PROGRESSIVE POLITICAL COALITION (TANGLED ROPE) — Uses catastrophe framing to coordinate climate policy coalitions and mobilize electoral bases (coordination benefit). But also depends on maintaining belief intensity to sustain political power — reducing catastrophe rhetoric weakens the coalition's enforcement mechanism. Constrained by reliance on catastrophe messaging; cannot fully exit. d≈0.52, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(climate_catastrophe_belief, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAINSTREAM MEDIA (PITON) — Climate catastrophe framing drives audience engagement and editorial urgency. But the media's capacity to verify specific predictions or update confidence intervals has degraded — coverage is increasingly ritualistic/performative (dramatic imagery, expert quotes, existential framing) rather than technical. Theater ratio 0.58 reflects mixed performative/functional content. Media maintains catastrophe narrative through inertia despite declining predictive accuracy. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(climate_catastrophe_belief, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GREEN ENERGY INVESTORS (SCAFFOLD) — Catastrophe belief mobilizes capital allocation to renewables, electrification, and climate tech (coordination function). But investors have high exit options: capital flows toward profitable solutions and can shift to other narratives. Sunset implicit in transition's success — as renewable energy becomes dominant and economically superior, the need for catastrophe framing diminishes. d≈0.35, f(d)≈0.28, σ=1.2 → χ≈0.17.
constraint_indexing:constraint_classification(climate_catastrophe_belief, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: HOUSEHOLD PLANNERS (SNARE) — Catastrophe belief shortens planning horizons (30-year mortgages, retirement savings, children's education planning become irrational under near-term doom). Individual households cannot exit the belief (ubiquitous messaging) but also cannot coordinate to change systemic outcomes. d≈0.85, f(d)≈1.22, σ=0.8 → χ≈0.51.
constraint_indexing:constraint_classification(climate_catastrophe_belief, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Empirically, climate science consensus is robust: warming is real, anthropogenic, and requires action. But the specific near-term catastrophe timeline (impacts 'within lifetime') conflates scientific projections with political rhetoric. Coordination function (mobilizing action) coexists with extraction function (generating compliance through fear). The observer sees both: legitimate urgency + motivational inflation. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.40.
constraint_indexing:constraint_classification(climate_catastrophe_belief, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_catastrophe_belief_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_catastrophe_belief, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_catastrophe_belief, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_catastrophe_belief, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_catastrophe_belief, TR),
    TR >= 0.70.

:- end_tests(climate_catastrophe_belief_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The catastrophe framing mobilizes real resources (policy, capital, labor) toward decarbonization — a genuine collective action outcome. But it also extracts psychological costs (anxiety, despair, shortened planning horizons) and distorts economic decision-making at household level. The extraction is real but not totalizing: households can take incremental actions, investors can profit from green transitions, and policy can advance. The 0.52 value reflects the coexistence of both functions. Suppression (0.65): High. Catastrophe messaging is ubiquitous (media, schools, political rhetoric, activist discourse). Alternative framings (climate change is real but manageable; transition is technically solvable; risk is moderate not extinction) are systematically underrepresented. Exit from exposure to catastrophe framing is nearly impossible in developed nations. Theater ratio (0.58): Moderate. Climate coverage has shifted from technical analysis toward dramatic/emotional framing. Predictions are frequently stated without probabilistic confidence intervals. But the underlying climate science remains rigorous — the theater is in communication, not in the scientific knowledge base itself. The rise in theater ratio over the interval reflects progressive mediazation of climate discussion.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The climate advocacy organization sees a rope (coordination tool). The believer sees a snare (no exit). The political coalition sees a tangled rope (coordination + dependence). The media see a piton (performative ritual). The investor sees a scaffold (temporary mobilization with exit). The household planner sees a snare (shortened planning horizon). The analytical observer sees a tangled rope (justified concern + motivational inflation coexisting). The disagreement is not about whether climate change is real (broad agreement exists) but about whether near-term catastrophe belief is necessary, functional, or extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Catastrophe-Believing Citizen: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. No exit from narrative; psychological cost imposed. Climate Advocacy Organization: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Can arbitrage between donor pools and advocacy messages. Progressive Political Coalition: Mixed + constrained → d≈0.52, f(d)≈0.65. Moderate extraction. Depends on catastrophe framing for electoral mobilization but cannot completely exit without losing power. Media: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Piton classification comes from theater gate, not extraction. Green Energy Investors: Beneficiary + mobile → d≈0.35, f(d)≈0.28. Low extraction; can exit if profitability changes. Household Planners: Victim + constrained → d≈0.85, f(d)≈1.22. High extraction; trapped in local decision-making under irrational timeline. Analytical Observer: balanced position → d≈0.50, f(d)≈0.65. Sees both coordination and extraction functions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint has BOTH a genuine coordination function AND genuine asymmetric extraction. This is precisely what Tangled Rope exists to capture. The coordination function: near-term catastrophe framing does mobilize action. Without it, political will for decarbonization might be insufficient. The asymmetric extraction: the mobilization is purchased by imposing psychological costs on believers, creating shortened planning horizons, and generating dependence on catastrophe messaging to sustain political coalitions. The false dichotomy would be: 'Is this coordination (Rope) or extraction (Snare)?' The true answer: 'It is both, and the balance between them is the strategic tension.' The analytical observer's role is to measure which function is primary (does climate policy advance without catastrophe framing? do believers benefit from the mobilization? do households plan better under the belief?) and to track whether theater ratio (now 0.58) continues rising, indicating progressive degradation of functional verification in favor of performative ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_timeline_precision,
    'At what specific timeline (5/10/20/30 years) do climate impacts transition from ''projected risk'' to ''observable catastrophe'', and how does this threshold compare to public belief?',
    'Longitudinal tracking of public belief statements + attribution to specific impact claims; comparison against climate model output and observed weather/climate changes; analysis of which predictions were accurate vs falsified',
    'If belief timeline much shorter than scientific evidence: extraction mechanism dominates (Snare wins). If belief aligns with lower-confidence tail risks: tangled rope confirmed (justified concern + motivational inflation coexist).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophe_timeline_precision, empirical, 'Precision of near-term catastrophe timeline in public belief vs climate science').

omega_variable(
    psychological_wellbeing_extraction_magnitude,
    'What is the quantified cost (anxiety, depression, reduced planning, reproductive decline, health outcomes) imposed by catastrophe belief on populations that hold it?',
    'Longitudinal mental health data; comparison of psychological outcomes between high-belief and low-belief cohorts, controlling for actual climate exposure; analysis of behavioral changes (savings, fertility, health investment)',
    'If cost is severe and uncompensated: victim status of believers confirmed (Snare strengthens). If cost is low or offset by mobilization benefits: extraction component diminishes (Rope vs Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_wellbeing_extraction_magnitude, empirical, 'Psychological and economic cost of climate catastrophe belief').

omega_variable(
    advocacy_infrastructure_counterfactual,
    'Would climate mitigation policy advance at similar pace under ''scientifically accurate but non-catastrophic'' framing (e.g., ''climate change is real and requires managed transition'') vs current catastrophe framing?',
    'Comparison of policy outcomes in jurisdictions using catastrophe vs neutral/technical framing; analysis of donor funding and political mobilization under alternate narratives; case studies of regions with lower catastrophe messaging but higher climate policy action',
    'If policy outcomes are similar under neutral framing: catastrophe framing is unnecessary extraction (pure Snare). If policy requires catastrophe: framing is functional (Tangled Rope confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(advocacy_infrastructure_counterfactual, conceptual, 'Whether climate policy requires catastrophe framing or responds equally to technical urgency').

omega_variable(
    belief_updating_mechanisms,
    'When near-term catastrophe predictions fail to materialize, do believers update their timelines or strengthen their commitment to the original belief?',
    'Tracking of repeated predictions and public response when deadlines pass (from 2012 predictions to 2026); analysis of narrative evolution (do advocates shift to longer timelines or double down?); comparison with other doom-responsive belief systems',
    'If believers update smoothly: belief system has epistemic feedback (Rope/Scaffold). If believers entrench: belief system resists falsification (Snare/Piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(belief_updating_mechanisms, empirical, 'Belief updating when catastrophe predictions fail').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_catastrophe_belief, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_catastrophe_belief, theater_ratio, 0, 0.35).
narrative_ontology:measurement(clim_tr_t8, climate_catastrophe_belief, theater_ratio, 8, 0.48).
narrative_ontology:measurement(clim_tr_t15, climate_catastrophe_belief, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_catastrophe_belief, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(clim_be_t8, climate_catastrophe_belief, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(clim_be_t15, climate_catastrophe_belief, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_catastrophe_belief, enforcement_mechanism).
narrative_ontology:affects_constraint(climate_catastrophe_belief, carbon_pricing_efficacy).
narrative_ontology:affects_constraint(climate_catastrophe_belief, renewable_energy_adoption_rate).
narrative_ontology:affects_constraint(climate_catastrophe_belief, long_term_economic_planning).
narrative_ontology:affects_constraint(climate_catastrophe_belief, intergenerational_moral_hazard).

% DUAL FORMULATION NOTE:
% Climate catastrophe belief should be decomposed into at least three structurally distinct constraints: (1) empirical claim about warming (ε≈0.05, Mountain from all perspectives — climate science consensus); (2) near-term attribution claim ('impacts within lifetime') with ε≈0.35 (contested, Tangled Rope); (3) mobilization function ('catastrophe framing is necessary for policy') with ε≈0.52 (present story). These three have different empirical status and different perspectival structures. The present story focuses on (3), the belief structure and its consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_catastrophe_belief, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
