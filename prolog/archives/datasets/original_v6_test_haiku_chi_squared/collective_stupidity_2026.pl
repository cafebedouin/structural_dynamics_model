% ============================================================================
% CONSTRAINT STORY: collective_stupidity_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collective_stupidity_2026, []).

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
 *   constraint_id: collective_stupidity_2026
 *   human_readable: The Cipolla-Galloway Stupidity Snare
 *   domain: social/behavioral
 *
 * SUMMARY:
 *   Carlo Cipolla's classical framework identifies stupidity as a category of
 *   human action distinct from malice, incompetence, and rational
 *   self-interest. The Cipolla-Galloway Stupidity Snare models stupidity as a
 *   structural constraint: stupidity is defined as causing damage to others
 *   without deriving personal gain. This definition captures why stupidity is
 *   particularly pernicious: it is not subject to the deterrents that work
 *   against malice (retaliation, reputation loss, punishment). The stupid
 *   agent gains nothing, so punishing them creates a second-order stupidity
 *   without preventing the first. The snare operates because: (1) stupidity
 *   is cognitively invisible — stupid agents do not perceive their own
 *   stupidity, (2) harm diffuses across the collective, preventing
 *   concentrated retaliation or incentive reversal, (3) organizational
 *   theater (compliance, process, oversight) attempts to prevent stupidity
 *   but functions largely performatively, and (4) no feedback mechanism
 *   reliably alerts stupid agents to their stupidity or changes their
 *   behavior. The extractiveness score (0.58) reflects that the stupid
 *   agent's lack of personal gain does NOT prevent damage — the constraint is
 *   as extractive as a snare despite the absence of selfish motivation. The
 *   theater ratio (0.68) captures the performative character of
 *   organizational anti-stupidity measures: training programs, oversight
 *   committees, and decision-review processes are substantially theatrical,
 *   addressing the symptom (documented stupidity) rather than the cause
 *   (cognitive blindness).
 *
 * KEY AGENTS:
 *   - Stupid Agent: Primary stupid actor (moderate/constrained) — commits damage without personal gain; cognitively blind to stupidity; trapped in patterns that produce stupid acts
 *   - Harmed Collective: Primary victim (powerless/trapped) — society or organization that absorbs damage from stupid acts; cannot organize against or punish stupidity effectively
 *   - Organization: Institutional actor (institutional/constrained) — attempts to prevent stupidity through policies, hierarchies, and oversight (piton view); theater ratio indicates low functional efficacy
 *   - Epistemic Community: Scholars and analysts (organized/mobile) — benefit from studying stupidity (extract through expertise) while coordinating on frameworks for understanding and reducing it
 *   - Observant Analyst: Cipolla-style analyst (powerful/mobile) — sees stupidity as a definitional/analytical problem to be solved through recognition and classification
 *   - Analytical Observer: Structural view (analytical/analytical) — sees stupidity as a universal cognitive and social constraint that is nearly inescapable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_stupidity_2026, 0.58).
domain_priors:suppression_score(collective_stupidity_2026, 0.72).
domain_priors:theater_ratio(collective_stupidity_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_stupidity_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(collective_stupidity_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(collective_stupidity_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_stupidity_2026, snare).
narrative_ontology:human_readable(collective_stupidity_2026, "The Cipolla-Galloway Stupidity Snare").
narrative_ontology:topic_domain(collective_stupidity_2026, "social/behavioral").

% --- Structural relationships ---
narrative_ontology:constraint_victim(collective_stupidity_2026, collective_welfare).
narrative_ontology:constraint_victim(collective_stupidity_2026, organizational_efficiency).
narrative_ontology:constraint_victim(collective_stupidity_2026, social_trust).
narrative_ontology:constraint_victim(collective_stupidity_2026, economic_productivity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HARMED COLLECTIVE (SNARE) — Victims of stupid acts suffer damage without recourse. Cannot exit the social/organizational system that contains stupid agents. No mechanism to prevent or punish stupidity (unlike fraud or malice, which carry legal liability). d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.67.
constraint_indexing:constraint_classification(collective_stupidity_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: STUPID AGENT (SNARE) — Commits damaging acts without personal gain (definition: stupidity ≠ selfish harm). But agent is trapped in the cognitive patterns that produce stupidity. Cannot easily exit or reform. Suppression of stupidity is low because stupidity is often invisible to the stupid agent. d≈0.88, f(d)≈1.35, σ=0.9 → χ≈0.55.
constraint_indexing:constraint_classification(collective_stupidity_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ORGANIZATION (PITON) — Attempts to prevent stupidity through policies, hierarchies, and oversight mechanisms. But theater_ratio≈0.68 indicates that much organizational 'stupidity prevention' is performative: compliance theater, process documentation, regulatory checkboxes. The actual function (preventing stupid decisions) is weak. Organizations maintain elaborate anti-stupidity theater (ethics committees, sign-off procedures, peer review) that persists despite low functional efficacy. d≈0.45, f(d)≈0.47, σ=1.0 → χ≈0.27.
constraint_indexing:constraint_classification(collective_stupidity_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EPISTEMIC COMMUNITY (TANGLED ROPE) — Scholars, researchers, and organizational theorists benefit from studying stupidity (publication, grant funding, consulting). But also coordinate to expose and reduce collective stupidity through analysis and norm-setting. Both extraction (gatekeeping of expertise) and coordination (shared frameworks for understanding stupidity). d≈0.52, f(d)≈0.67, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(collective_stupidity_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: OBSERVANT ANALYST (ROPE) — From Cipolla's civilizational perspective, recognizing stupidity is a pure coordination good. The four-quadrant framework (stupid, knave, rational, helpless) enables analysis without blame. Coordinating on the definition of stupidity itself (damage to others without personal gain) is the primary function. d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.18.
constraint_indexing:constraint_classification(collective_stupidity_2026, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — Sees stupidity as a structural trap with universal scope. Stupid acts are intrinsically difficult to prevent because: (1) stupidity is invisible to the stupid agent, (2) society provides no feedback loop to alert stupid agents to their stupidity, (3) the harm diffuses across the collective, preventing concentrated retaliation. The constraint is universal because human cognition has hard limits. d≈0.78, f(d)≈1.18, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(collective_stupidity_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_stupidity_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collective_stupidity_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_stupidity_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(collective_stupidity_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(collective_stupidity_2026, TR),
    TR >= 0.70.

:- end_tests(collective_stupidity_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Stupidity causes significant damage to organizational and collective welfare — production is lost, resources are wasted, trust is eroded. Unlike malicious extraction (where the extractor gains), stupid damage is pure loss to the system. The extractiveness reflects the cumulative harm rather than personal gain to the stupid agent. The measurement trajectory (0.42→0.58) indicates that as organizational complexity increases, opportunities for stupidity to cause damage multiply without corresponding increases in prevention capacity. Suppression (0.72): High. Multiple mechanisms suppress recognition of and response to stupidity: (a) cognitive blindness — the stupid agent cannot see their own stupidity, (b) diffuse harm — damage spreads across many victims, weakening any single agent's incentive to organize, (c) attribution error — organizations misattribute stupid outcomes to bad luck or incompetence rather than stupidity, (d) theater — compliance measures obscure the actual stupidity-causation mechanism. Theater ratio (0.68): Moderate-high. Organizations implement elaborate anti-stupidity theater: ethics committees sign off on decisions, peer review processes, hierarchical approval chains, training programs on critical thinking. But the theater is substantially performative because it addresses documented/obvious stupidity rather than the cognitive blindness that produces stupidity. Theater increases from 0.55 to 0.68 over the interval as organizations respond to visible stupidity with more procedural theater rather than structural reform.
 *
 * PERSPECTIVAL GAP:
 *   The harmed collective sees pure snare (unmitigated damage, no escape). The stupid agent sees snare (trapped in cognitive patterns, no personal gain from stupid acts). The organization sees piton (degraded institutional theater masking low functional prevention). The epistemic community sees tangled rope (both coordination benefit and extraction through expertise-gatekeeping). The observant analyst sees rope (pure coordination good in defining/recognizing stupidity). The analytical observer sees structural snare (universal cognitive trap). The perspectival gap arises because different agents experience the same stupidity constraint differently: some as victims, some as trapped agents, some as institutional theater-maintainers, some as analysts benefiting from study. The gap is widest between the powerless victim (snare) and the powerful analyst (rope) — they experience opposite effective extractiveness despite the same ε.
 *
 * DIRECTIONALITY LOGIC:
 *   Harmed Collective: Victim + trapped → d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.67. Maximum extraction experience (structural victim with no exit). Stupid Agent: Victim (of own stupidity) + constrained → d≈0.88, f(d)≈1.35, σ=0.9 → χ≈0.55. High extraction but not maximum — the agent is both victim and cause, creating an ambiguous structural position. Organization: Neither clear beneficiary nor victim; constrained exit → d≈0.45, f(d)≈0.47, σ=1.0 → χ≈0.27. Institutional actors experience the stupidity snare as a manageable compliance problem (piton). Epistemic Community: Both beneficiary (study stipends, publications) and victim (organizational stupidity damages research environment) + mobile → d≈0.52, f(d)≈0.67, σ=1.2 → χ≈0.47. Mixed experience. Observant Analyst: Beneficiary + mobile → d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.18. Low extraction from this perspective because defining/analyzing stupidity is a coordination good. Analytical Observer: Victim of universal cognitive constraint + analytical → d≈0.78, f(d)≈1.18, σ=1.0 → χ≈0.68. High extraction from the structural/civilizational view.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that stupidity is both a cognitive property (individual agents have varying stupidity levels) and a structural constraint (social/organizational systems are traps for stupid decision-making). The snare classification is appropriate because: (1) the constraint extracts heavily (ε=0.58) despite the absence of personal gain to the extractor, (2) suppression is high (0.72) because stupidity is structurally invisible and diffuse, and (3) the constraint relies on the victim's inability to organize or punish (core snare mechanism). The snare is NOT a false positive for 'pure extraction' — stupid acts are extractive even though the stupid agent gains nothing. The theater ratio (0.68) indicates that organizations treat stupidity prevention as a symbolic problem (compliance theater) rather than addressing the cognitive/structural root causes. This is not a snare pretending to be a rope; it is a snare that spawns piton-like performative responses. The analytical perspective sees the snare as structural/universal (cognitive blindness is a hard limit). The organizational perspective sees piton-like degradation (theater outpacing function). Both are consistent with the base snare classification — they differ in whether stupidity is seen as escapable (it is not, analytically) or as a manageable compliance issue (it is, organizationally, via theater).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stupidity_vs_informed_disagreement,
    'How do we distinguish stupidity (damage without gain) from informed disagreement about outcomes?',
    'Counterfactual analysis: did the agent know or should have known the act would harm others? Post-hoc assessment of available information.',
    'If threshold is strict (agent must know harm beforehand): most organizational failures are stupidity. If threshold is loose (agent could have discovered harm): many failures are reframed as negligence or incompetence, not stupidity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stupidity_vs_informed_disagreement, conceptual, 'Boundary between stupidity and informed disagreement').

omega_variable(
    personal_gain_boundary,
    'Does ''personal gain'' include psychic/status benefits (ego satisfaction, group loyalty) or only material gain?',
    'Definition refinement via case study analysis; examination of whether agents report subjective satisfaction from stupid acts.',
    'If material only: many stupid acts are self-interested (agent gains status/ego). If psychic included: definitional boundary is cleaner but harder to verify empirically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(personal_gain_boundary, conceptual, 'Scope of ''personal gain'' in stupidity definition').

omega_variable(
    feedback_loop_effectiveness,
    'Can organizational or social mechanisms create feedback loops that alert stupid agents to their stupidity?',
    'Pilot intervention analysis: test reputation systems, peer feedback, delayed-consequence transparency, and collaborative decision-making to measure behavior change.',
    'If feedback can work: stupidity snare can be partially escaped through institutional design. If feedback fails: the snare is structural and nearly inescapable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_loop_effectiveness, empirical, 'Whether feedback loops can alert stupid agents and reduce stupidity').

omega_variable(
    collective_vs_individual_stupidity,
    'Is stupidity primarily an individual cognitive property or an emergent property of group dynamics?',
    'Comparison of individual error rates vs collective decision pathologies; analysis of groupthink, cascade failures, and structural stupidity in organizations.',
    'If individual: stupid agents are the primary target for prevention. If collective: social/organizational structure is the target, and individual agents may be cognitive normal but trapped in stupid systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_vs_individual_stupidity, empirical, 'Whether stupidity is individual or emergent collective property').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_stupidity_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stupidity_tr_t0, collective_stupidity_2026, theater_ratio, 0, 0.55).
narrative_ontology:measurement(stupidity_tr_t5, collective_stupidity_2026, theater_ratio, 5, 0.62).
narrative_ontology:measurement(stupidity_tr_t10, collective_stupidity_2026, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(stupidity_be_t0, collective_stupidity_2026, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stupidity_be_t5, collective_stupidity_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(stupidity_be_t10, collective_stupidity_2026, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_stupidity_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(collective_stupidity_2026, organizational_decision_fatigue).
narrative_ontology:affects_constraint(collective_stupidity_2026, groupthink_cascade).
narrative_ontology:affects_constraint(collective_stupidity_2026, institutional_blame_diffusion).

% DUAL FORMULATION NOTE:
% The stupidity snare is upstream of specific organizational failures (decision fatigue, groupthink cascades) but represents a distinct structural constraint. The stupidity constraint describes the mechanism by which stupid acts produce damage without personal gain; downstream constraints describe how that mechanism manifests in specific contexts (cascades, diffusion, fatigue). Both constraints are linked because organizational stupidity amplifies the effects of individual cognitive biases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
