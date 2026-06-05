% ============================================================================
% CONSTRAINT STORY: collective_stupidity_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   human_readable: Cipolla-Galloway Stupidity Snare: Uncompensated Harm Without Beneficiary
 *   domain: social/behavioral
 *
 * SUMMARY:
 *   The Cipolla-Galloway Stupidity Snare describes a structural constraint
 *   where certain agents perform actions that cause damage to others without
 *   deriving personal benefit. This is not irrationality — rational agents
 *   operating under incomplete information still pursue goals (however
 *   badly). Stupidity in this framework is the specific pathology where an
 *   action reduces both the actor's and the collective's welfare
 *   simultaneously. The snare persists because no beneficiary exists to
 *   maintain it: the extraction is pure harm-shedding, with the stupid agent
 *   as an inadvertent vector and the collective as an unwilling host. The
 *   constraint operates at population scale, driven by the inevitable
 *   distribution of cognitive capacity and attention across any large group.
 *   Those in the bottom decile of capability or awareness will predictably
 *   cause uncompensated harm; no institutional arrangement eliminates this
 *   floor. The suppression mechanism is structural rather than intentional —
 *   stupid agents are not deterred by standard feedback (reputation cost,
 *   legal penalty, social ostracism) because they lack the causal reasoning
 *   to connect their action to the response. Feedback mechanisms designed for
 *   rational agents fail catastrophically against stupidity. The theater
 *   ratio (0.58) reflects that institutional responses to stupid actors often
 *   become performative: compliance training, risk assessment protocols,
 *   bureaucratic procedures that create appearance of control without
 *   reducing underlying harm distribution.
 *
 * KEY AGENTS:
 *   - Stupid Agents (powerless/trapped in own cognitive capacity): Inadvertent vectors of uncompensated harm; lack causal reasoning to recognize damage or respond to feedback; cannot exit their own lower quartile of capability
 *   - Harm-Bearing Agents (powerless/trapped): Direct recipients of stupid actions; no ability to escape or prevent damage; bear extraction with zero reciprocal benefit
 *   - Organized Victim Coalitions (moderate/constrained): Attempt to coordinate defensive screening, warning networks, ostracism; face structural limitation that stupid agents are immune to reputational cost
 *   - Collective Welfare (abstract/powerless): Diffuse concept of aggregate benefit; cannot organize or defend itself; bears distributed micro-harms from population-scale stupidity
 *   - Institutional Governance Structures (institutional/arbitrage): Design feedback mechanisms, screening protocols, liability rules; create performative theater around stupidity management; benefit from appearance of control
 *   - Analytical Observer (analytical/analytical): Observes that stupidity snare is a permanent feature of any population with distributed agency; recognizes futility of standard deterrence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_stupidity_2026, 0.68).
domain_priors:suppression_score(collective_stupidity_2026, 0.72).
domain_priors:theater_ratio(collective_stupidity_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_stupidity_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(collective_stupidity_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(collective_stupidity_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_stupidity_2026, snare).
narrative_ontology:human_readable(collective_stupidity_2026, "Cipolla-Galloway Stupidity Snare: Uncompensated Harm Without Beneficiary").
narrative_ontology:topic_domain(collective_stupidity_2026, "social/behavioral").

domain_priors:requires_active_enforcement(collective_stupidity_2026).

% --- Structural relationships ---
narrative_ontology:constraint_victim(collective_stupidity_2026, collective_welfare).
narrative_ontology:constraint_victim(collective_stupidity_2026, harm_bearing_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HARM-BEARING AGENT (SNARE) — Agents who experience damage from stupid actions have no exit and no appeal. The stupidity is uncompensated — the actor gains nothing, yet the victim bears full cost. Maximum extraction because the binding mechanism is not rational incentive but structural inevitability: stupid agents exist and act, and their targets cannot escape.
constraint_indexing:constraint_classification(collective_stupidity_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORGANIZED VICTIM COALITION (SNARE) — Victims can coordinate defensive strategies (social ostracism, institutional screening, distributed warning networks), but the stupidity snare persists because stupid agents are not deterred by reputational cost — they lack the awareness to recognize causation between their action and the response. Coalition has limited leverage; the constraint's suppression derives from the actor's immunity to standard feedback mechanisms.
constraint_indexing:constraint_classification(collective_stupidity_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (SNARE) — From a universal civilizational view, stupidity as uncompensated harm-infliction is a permanent feature of distributed agency. No beneficiary exists to extract value; the constraint persists because coordination cannot prevent the bottom decile of capability from causing damage. Snare classification is invariant across all contexts — this is the rare uniform-type constraint.
constraint_indexing:constraint_classification(collective_stupidity_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_stupidity_2026_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(collective_stupidity_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(collective_stupidity_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising over the interval (0.45→0.68). The snare's extractiveness increases as populations grow and coordination becomes more difficult. Stupid agents cause damage at a rate proportional to their proportion in the population; as societies scale, the absolute number of stupidity-induced harms accumulates faster than institutional capacity to respond. The rising trajectory reflects growing consciousness of the problem (measurement sensitivity) rather than accelerating harm rates, but both contribute. Suppression (0.72): High and stable. The binding mechanism is structural — agents in the bottom quartile of cognitive capacity or attention will inevitably cause harm, and no standard feedback mechanisms deter them (they lack causal reasoning to connect action to response). Targets cannot exit because the stupid agents are distributed throughout the population, not concentrated in avoidable regions. Theater ratio (0.58): Moderate and rising. Institutional responses — mandatory safety training, risk assessment protocols, incident reporting systems — create appearance of control without reducing the underlying problem. The stupid agent completes the training but does not gain causal reasoning; the protocol documents the action but does not prevent it. Theater increases as institutions respond to visible stupidity crises by adding more performative procedures.
 *
 * PERSPECTIVAL GAP:
 *   The Cipolla-Galloway stupidity snare is a rare uniform-type constraint: it classifies as Snare from every perspective. There is no beneficiary to see coordination or legitimate exchange. There is no coalition powerful enough to reframe the constraint. Even the analytical observer, looking at civilizational scale, sees an invariant structural floor — stupidity is an immutable property of distributed agency in large populations. The perspectival gap is not about classification type but about hope: the harm-bearing agent experiences despair (uncompensated, unprevented); the organized coalition experiences frustration (we can coordinate but cannot eliminate); the analytical observer experiences acceptance (this is how populations work). The snare's uniformity makes it diagnostically unusual and theoretically important — it demonstrates that not all constraints benefit someone, and that extraction can be genuinely uncompensated.
 *
 * MANDATROPHY ANALYSIS:
 *   The stupidity snare resolves the mandatrophy by confronting the classical oracle gap: can an analytical observer design a system that prevents the bottom decile of capability from causing harm? The answer from the constraint framework is 'no, not without sacrificing distributed agency.' Any system that grants action authority to agents must accept that some fraction will exercise it stupidly. This is not a coordination failure (which could be fixed with better incentives) but a capacity floor. The Cipolla-Galloway framework reframes stupid actions as not failures of rationality but failures of capability — and capability distributions are immutable across populations. The mandatrophy resolves by recognizing that the snare is not a design flaw but a feature: accepting stupidity (suppression through institutional tolerance, harm distribution through insurance, harm-reduction rather than prevention) is more realistic than attempting to eliminate it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stupidity_agency_threshold,
    'At what threshold of capacity does an agent''s action transition from ''stupid'' (uncompensated harm) to ''malicious'' (intended harm with implicit benefit to actor) or ''incompetent'' (failure to achieve actor''s own goals)?',
    'Causal analysis of actor''s intent, awareness, and capability; post-hoc examination of whether actor could have predicted harm; assessment of whether any secondary benefit to actor emerges from the harm',
    'If threshold is low (capacity < 20th percentile): snare classification holds across wider range of behaviors. If high: only the most egregiously damaging actions count as stupid; most poor outcomes are reclassified as incompetence or rationality under constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stupidity_agency_threshold, conceptual, 'Definition boundary between stupidity, malice, and incompetence').

omega_variable(
    harm_quantification_ambiguity,
    'How are distributed micro-harms (each stupid action causes small damage to many agents) compared to concentrated macro-harms (one stupid action causes severe damage to few agents)?',
    'Utilitarian aggregation across harm recipients; empirical measurement of whether distributed micro-harm has higher total impact than concentrated macro-harm in specific domains',
    'If distributed harm dominates: stupidity snare operates at population scale with diffuse suppression. If concentrated harm dominates: snare is concentrated on small victim sets with intense suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_quantification_ambiguity, empirical, 'Distribution of harm across victim populations').

omega_variable(
    counterfactual_benefit_detection,
    'How do we distinguish ''stupid action with zero benefit to actor'' from ''action with benefit to actor that we have not yet identified''?',
    'Longitudinal tracking: does the actor repeat the behavior? Do they defend it if questioned? Do they show pleasure/satisfaction post-action? Do secondary institutional benefits accrue (career advancement, status gain)?',
    'If secondary benefits exist: action reclassifies as strategic extraction (snare→tangled_rope or snare→snare-with-beneficiary). If truly zero benefit: stupidity classification holds and extractiveness increases (pure uncompensated harm).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_benefit_detection, empirical, 'Distinguishing zero-benefit actions from unidentified secondary benefits').

omega_variable(
    collective_welfare_definition,
    'Whose welfare counts as ''collective welfare'' in the stupidity definition? Sum of all agents? Only organized communities? Humanity? Future generations?',
    'Philosophical analysis of constituency; empirical testing of whether harm-bearing populations can organize and define themselves; institutional recognition of harm boundaries',
    'If narrow definition (only organized agents count): many stupid actions fall outside snare classification. If broad definition (all sentient beings): stupidity snare encompasses ecosystem damage, animal harm, long-term civilizational risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_welfare_definition, preference, 'Definition of collective welfare constituency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_stupidity_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stupidity_tr_t0, collective_stupidity_2026, theater_ratio, 0, 0.52).
narrative_ontology:measurement(stupidity_tr_t2, collective_stupidity_2026, theater_ratio, 2, 0.54).
narrative_ontology:measurement(stupidity_tr_t4, collective_stupidity_2026, theater_ratio, 4, 0.56).
narrative_ontology:measurement(stupidity_tr_t6, collective_stupidity_2026, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(stupidity_be_t0, collective_stupidity_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(stupidity_be_t2, collective_stupidity_2026, base_extractiveness, 2, 0.54).
narrative_ontology:measurement(stupidity_be_t4, collective_stupidity_2026, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(stupidity_be_t6, collective_stupidity_2026, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stupidity_su_t0, collective_stupidity_2026, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(stupidity_su_t3, collective_stupidity_2026, suppression_requirement, 3, 0.69).
narrative_ontology:measurement(stupidity_su_t6, collective_stupidity_2026, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(collective_stupidity_2026, institutional_coordination_failure).
narrative_ontology:affects_constraint(collective_stupidity_2026, distributed_harm_accountability).

% DUAL FORMULATION NOTE:
% Stupidity snare is upstream of many institutional failure modes. Specific stupid actions (environmental damage, medical errors, financial missteps) generate their own constraint stories with ε values tied to the specific domain. The stupidity snare describes the structural floor — that some fraction of agents will always cause uncompensated harm — while domain-specific stories describe how that floor manifests in particular contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
