% ============================================================================
% CONSTRAINT STORY: collective_action_deadlock
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collective_action_deadlock, []).

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
 *   constraint_id: collective_action_deadlock
 *   human_readable: The Infinite Deliberation Loop
 *   domain: political/social
 *
 * SUMMARY:
 *   The infinite deliberation loop emerges when collective action requires a
 *   response to a critical threshold event (resource scarcity, security
 *   threat, ecological tipping point) but internal governance rules grant
 *   veto power to any stakeholder. The constraint operates as a hybrid: it
 *   serves a genuine coordination function (minority protection, inclusive
 *   decision-making) while simultaneously enabling extraction (veto-holders
 *   block action to preserve status quo rents or strategic advantage). The
 *   deadlock is not inevitable—it reflects a specific institutional choice:
 *   requiring unanimity or consensus instead of using supermajority,
 *   qualified majority, or delegated decision-making with sunset clauses. The
 *   theatrical component has grown over the measurement interval:
 *   deliberation is increasingly performed as inclusive and legitimate while
 *   actual responsive capacity has degraded. This is the Piton signal: the
 *   ritual persists through inertia despite loss of function.
 *
 * KEY AGENTS:
 *   - Threshold-Responsive Group: Primary victim (powerless/trapped) — faces imminent collective action problem; cannot exit consensus-seeking structure; bears full cost of blockage
 *   - Veto-Holding Factions: Primary beneficiary (moderate/constrained) — enjoy voice and protection via veto privilege; also trapped in the structure they use to block others
 *   - Status Quo Defender: Secondary beneficiary (institutional/arbitrage) — faction profiting from inaction or current allocation; uses veto to preserve position
 *   - Reform Coalition: Organized actors (organized/mobile) — NGOs, transnational bodies, norm entrepreneurs advocating decision-rule reform with clear exit logic
 *   - Consensus Ritual: Institutional structure (institutional/arbitrage) — deliberation mechanism maintained through legitimacy theater despite degraded function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the deadlock as an inevitable consequence of preference aggregation rather than a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_action_deadlock, 0.52).
domain_priors:suppression_score(collective_action_deadlock, 0.68).
domain_priors:theater_ratio(collective_action_deadlock, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_action_deadlock, extractiveness, 0.52).
narrative_ontology:constraint_metric(collective_action_deadlock, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(collective_action_deadlock, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_action_deadlock, tangled_rope).
narrative_ontology:human_readable(collective_action_deadlock, "The Infinite Deliberation Loop").
narrative_ontology:topic_domain(collective_action_deadlock, "political/social").

domain_priors:requires_active_enforcement(collective_action_deadlock).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collective_action_deadlock, status_quo_defenders).
narrative_ontology:constraint_beneficiary(collective_action_deadlock, veto_holding_factions).
narrative_ontology:constraint_victim(collective_action_deadlock, collective_action_capacity).
narrative_ontology:constraint_victim(collective_action_deadlock, threshold_responsive_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THRESHOLD-RESPONSIVE GROUP (SNARE) — Faces imminent collective action problem (resource scarcity, security threat, ecosystem collapse) but cannot exit the deliberation structure. Veto rules prevent rapid response. Bears full cost of constraint inaction while locked in consensus-seeking. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(collective_action_deadlock, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VETO-CONSTRAINED COALITION (TANGLED ROPE) — Organized minority factions with legitimate interests in shared resource governance. Benefit from deliberation framework (their voice matters; decisions require their assent). But also bear extraction cost: any single member can paralyze the collective. Veto power is both shield and trap. d≈0.58, f(d)≈0.68, σ=0.9 → χ≈0.32.
constraint_indexing:constraint_classification(collective_action_deadlock, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATUS QUO DEFENDER (ROPE) — Faction benefiting from current allocation or inaction. Veto power ensures their interests are preserved; consensus requirement blocks unfavorable redistributions. Sees the deadlock as pure coordination: 'We maintain order by requiring unanimity.' d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary through veto privilege.
constraint_indexing:constraint_classification(collective_action_deadlock, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized agents (NGOs, international bodies, norm-setters) advocating for decision-rule reform: supermajority rules, issue-specific thresholds, or delegated authority with sunset clauses. See the deadlock as a temporary institutional failure with a clear exit path — modify the veto rule. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.19.
constraint_indexing:constraint_classification(collective_action_deadlock, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSENSUS RITUAL (PITON) — The deliberation structure itself persists through institutional inertia and legitimacy theater. Consensus-based decision-making is performatively associated with democratic inclusivity and legitimacy, but the functional verification (actual responsive collective action) has degraded. Veto rules that once enabled minority protection now enable paralysis. theater_ratio=0.58 reflects moderate performative content: deliberation is staged as inclusive but increasingly produces inaction. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.005.
constraint_indexing:constraint_classification(collective_action_deadlock, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, Arrow's Impossibility Theorem and Condorcet Voting Paradoxes establish that no decision rule can simultaneously satisfy unanimity, responsiveness, and non-dictatorship. Veto-based consensus is thus framed as an inevitable feature of democratic preference aggregation — a mathematical law. However, the structural data (ε=0.52, suppression=0.68, theater=0.58) contradicts mountain thresholds. The engine will detect this as a false summit: the impossibility is about abstract preference aggregation, not about THIS constraint's concrete institutional deadlock.
constraint_indexing:constraint_classification(collective_action_deadlock, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_action_deadlock_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collective_action_deadlock, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_action_deadlock, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(collective_action_deadlock, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(collective_action_deadlock, TR),
    TR >= 0.70.

:- end_tests(collective_action_deadlock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from threshold-responsive groups by preventing timely collective response, creating lag costs, mitigation delays, and forced suboptimal private adaptation. Not maximal (0.70+) because veto-holders are also somewhat constrained — their veto power gives them influence but not full control of outcomes, and blockage often triggers institutional reform or workaround governance. The extraction is real (threshold groups bear concentr costs) but not total (some actors benefit from deliberation's legitimacy, some escape via parallel structures). Suppression (0.68): High. Alternatives to consensus-based veto are suppressed through: (a) legitimacy framing ('true democracy requires unanimous assent'), (b) institutional path dependence (changing the rule requires consensus to change consensus), (c) fear of setting precedent (veto-holders know reform could turn against them). Threshold-responsive actors have minimal exit options within the formal structure, though they may create parallel governance. Theater ratio (0.58): Moderate. Deliberation is increasingly performed as legitimate and inclusive while functional responsiveness declines. The gap between stated decision-rule values (unanimity = fairness) and outcome (paralysis = injustice) creates performative content. Not as high as a pure ritual (0.80+) because deliberation sometimes produces real decisions, and the constraint is not purely theatrical.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a full perspectival gap across all six types. Threshold-responsive groups see a snare: they are trapped in a decision structure that cannot respond to their urgent needs. Veto-holding factions see tangled rope: they benefit from voice and protection, but are also constrained by reciprocal veto rights of others. Status quo defenders see rope: pure coordination, unanimity protects everyone's interests. Reform coalitions see scaffold: a temporary institutional failure with a clear path forward (change the decision rule). The consensus ritual persists as piton: performative legitimacy masks degraded function. The analytical observer risks seeing mountain: Arrow's theorem naturalizes the deadlock as inevitable. The engine's false summit detector will flag this: the theorem applies to abstract preference aggregation, not to the concrete institutional choice to require unanimity.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold-responsive group: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction relative. They cannot exit, bear concentr costs, have no veto. Veto-holding factions: Beneficiary + constrained → d≈0.58, f(d)≈0.68. Moderate extraction; they have power (veto) but are also bound by reciprocal veto of others, and blockage eventually triggers reform that may undermine their position. Status quo defender: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can exit if preferred (exit to status quo continuation or parallel governance). Reform coalition: Organized + mobile → d≈0.35, f(d)≈0.32. Low effective extraction; they have agency and see a clear exit path (institutional reform). Consensus ritual: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Piton classification comes from theater gate (theater=0.58), not from high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival risk — observer naturalizes contingent institutional choice as mathematical law.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RISK: The constraint sits at the tangled rope / snare boundary (ε=0.52, near the 0.46 threshold). The natural error is to misclassify it as either (a) pure rope—'unanimous assent is how we maintain coordination'—or (b) pure snare—'this is just veto-holder extraction.' The mandatrophy is resolved by recognizing that both aspects are structurally real: the veto rule does serve a coordination function (minority protection, preventing majoritarian overreach), AND it does enable extraction (blockage, status quo bias, lag costs). The distinction is not about the presence of coordination vs extraction, but about which agents perceive which function and at what cost. Threshold-responsive groups perceive extraction; status quo defenders perceive coordination; veto-holding factions perceive both. The theater ratio (0.58) indicates the legitimacy performance is moderating—not fully sustaining the false-coordination narrative. This measurement data supports the tangled rope classification over piton: the constraint is still performing a real function (voice, minority protection), not yet degraded to pure theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_duration_threshold,
    'What duration of blockage distinguishes legitimate deliberation from extractive deadlock?',
    'Historical case analysis: correlation between veto-blocking duration and (a) quality of eventual decision, (b) social cost of delay, (c) emergence of workaround governance structures.',
    'If threshold < 6 months: most vetoes appear extractive. If threshold > 3 years: chronic blockage may be misclassified as mountain. Classification shifts from Snare/Tangled Rope to Piton as duration increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_duration_threshold, empirical, 'Duration threshold distinguishing deliberation from deadlock extraction').

omega_variable(
    alternative_governance_availability,
    'Do parallel governance structures (subsidiarity, decentralized decision-making, issue-specific coalitions) genuinely enable response, or do they simply shift the deadlock to a different level?',
    'Comparative case study: cases where veto groups blocked centralized action but coalitions succeeded vs cases where deadlock replicated at every governance level.',
    'If alternatives work: scaffold perspective is realistic — reform has a genuine sunset path. If deadlock replicates: scaffold becomes aspirational theater, and the constraint is structurally deeper (possibly mountain-adjacent in some domains).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_governance_availability, empirical, 'Whether parallel governance structures bypass the deadlock or replicate it').

omega_variable(
    veto_actor_incentive_alignment,
    'Are veto-holders extracting rents (benefiting from blockage) or are they genuinely uncertain about optimal policy and using veto as a hedge against uncertain outcomes?',
    'Structural analysis: correlation between veto usage and (a) material benefit to veto-holder, (b) veto-holder''s stated preference distribution, (c) counterfactual modeling of payoffs under each decision rule.',
    'If primarily extractive: snare classification strengthens for threshold-responsive groups. If primarily uncertainty-driven: tangled rope classification strengthens — deadlock reflects legitimate coordination problem, not pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_actor_incentive_alignment, conceptual, 'Whether veto usage reflects extraction or legitimate uncertainty hedging').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_action_deadlock, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cad_tr_t0, collective_action_deadlock, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cad_tr_t5, collective_action_deadlock, theater_ratio, 5, 0.45).
narrative_ontology:measurement(cad_tr_t10, collective_action_deadlock, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(cad_be_t0, collective_action_deadlock, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cad_be_t5, collective_action_deadlock, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(cad_be_t10, collective_action_deadlock, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_action_deadlock, enforcement_mechanism).
narrative_ontology:affects_constraint(collective_action_deadlock, tragedy_of_the_commons).
narrative_ontology:affects_constraint(collective_action_deadlock, regulatory_capture_gridlock).
narrative_ontology:affects_constraint(collective_action_deadlock, multiparty_coalition_fragmentation).

% DUAL FORMULATION NOTE:
% The collective action deadlock decomposes into two related but structurally distinct constraints: (1) THE VETO MECHANISM itself (this story, ε=0.52) — the rule structure that enables blockage, (2) THE THRESHOLD MISMATCH (ε=0.68, Snare-only) — the lag between critical event timing and decision-making capacity. Both are causally upstream of specific failure modes (commons collapse, regulatory capture stabilization, coalition breakdown). The veto mechanism story represents the generic institutional structure; threshold mismatch represents how the structure fails under urgency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(collective_action_deadlock, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
