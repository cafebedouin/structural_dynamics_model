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
 *   constraint_id: collective_stupidity_2026
 *   human_readable: The Cipolla-Galloway Stupidity Snare
 *   domain: social/behavioral
 *
 * SUMMARY:
 *   The Cipolla-Galloway Stupidity Snare describes a structural constraint
 *   where agents cause damage to others without deriving personal benefit.
 *   This is not irrationality per se — rational agents can fail to achieve
 *   goals through ignorance or bad luck. Stupidity in this framework is
 *   specifically the class of actions that harm collective welfare while
 *   providing zero benefit to the actor. The constraint exhibits the
 *   essential properties of a snare: high suppression (victims cannot exit
 *   systems with stupid actors; stupid actors cannot be reliably identified
 *   before harm; institutional mechanisms amplify individual stupidity into
 *   systemic damage), high extractiveness (the rational victim bears
 *   uncompensated cost), and minimal theater (stupidity is fundamentally
 *   functional harm, not performative). The theater ratio is relatively low
 *   (0.38) because stupid harm is real, not ritualized. However,
 *   institutional responses to stupidity (bureaucratic oversight, ritual
 *   compliance checking) introduce performative elements. The constraint is a
 *   civilizational problem because heterogeneous cognitive capacity is
 *   intrinsic to large populations; no system can eliminate stupid actors
 *   entirely. Rational agents are trapped in the commons with them.
 *
 * KEY AGENTS:
 *   - Rational Agents: Primary victims (powerless/trapped) — bear costs of stupid harm without compensation or exit option
 *   - Collective Welfare: Structural victim (powerless/trapped) — shared resources and public goods degrade through accumulated stupid actions; intergenerational harm
 *   - Stupid Agents: Paradoxical actors (moderate/constrained) — provide no benefit to themselves yet operate within systems that sometimes reward or tolerate them; embedded in social/institutional structures
 *   - Institutional Systems: Degraded enforcement (institutional/arbitrage) — bureaucracies embed stupid requirements; maintain high-theater responses to stupidity (audits, regulations) without addressing root causes
 *   - Epistemic Communities: Organized monitors (organized/constrained) — attempt to solve stupidity through information, design, and institutional reform; see the constraint as solvable coordination problem
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing stupidity as irreducible feature of bounded rationality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_stupidity_2026, 0.68).
domain_priors:suppression_score(collective_stupidity_2026, 0.72).
domain_priors:theater_ratio(collective_stupidity_2026, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_stupidity_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(collective_stupidity_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(collective_stupidity_2026, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_stupidity_2026, snare).
narrative_ontology:human_readable(collective_stupidity_2026, "The Cipolla-Galloway Stupidity Snare").
narrative_ontology:topic_domain(collective_stupidity_2026, "social/behavioral").

% --- Structural relationships ---
narrative_ontology:constraint_victim(collective_stupidity_2026, collective_welfare).
narrative_ontology:constraint_victim(collective_stupidity_2026, rational_agents).
narrative_ontology:constraint_victim(collective_stupidity_2026, shared_resources).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RATIONAL AGENT (SNARE) — Bears costs of others' destructive actions without benefit. Cannot exit the system of stupidity; harm is involuntary. Maximum extraction without direct coercion — the stupid agent provides no benefit to the victim, yet the victim cannot escape. Trapped in shared systems (institutions, commons, social groups) where stupid actors inflict damage.
constraint_indexing:constraint_classification(collective_stupidity_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COLLECTIVE WELFARE (SNARE) — Structural victim of cascading stupidity. Public goods (atmosphere, institutions, trust networks, commons) degrade through accumulated stupid actions. No exit mechanism; no way to quarantine harm. Intergenerational extraction: future generations inherit institutional damage and depleted commons without consent or compensation.
constraint_indexing:constraint_classification(collective_stupidity_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: STUPID AGENT (TANGLED ROPE) — Mixed structure. The agent derives no personal gain from destructive action (pure stupidity per definition), yet operates within social/institutional systems that tolerate, enable, or accidentally reward them. Some benefit accrues: attention, social membership, resource access through coalitions of the stupid. Constrained exit — stupidity is often socially embedded; exiting requires cognitive or social resources the stupid agent may lack.
constraint_indexing:constraint_classification(collective_stupidity_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL STUPIDITY SYSTEMS (PITON) — Bureaucracies, organizations, and policy regimes often embed stupid requirements that produce damage without institutional benefit. Theater ratio high: extensive rituals (compliance procedures, reporting requirements, approval chains) whose primary function has atrophied, maintained through inertia. Institutions have arbitrage options (policy reform, structural change) but exhibit degraded decision-making capacity. Pure institutional inertia.
constraint_indexing:constraint_classification(collective_stupidity_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZED EPISTEMIC COMMUNITIES (ROPE) — Scientists, engineers, auditors, and oversight bodies see the constraint as a coordination problem to be solved through better information, institutional design, and incentive structures. Moderate extractiveness: the monitoring/intervention function itself requires resources and creates asymmetries, but the net benefit is stabilization of commons. Constrained by knowledge/resource limits.
constraint_indexing:constraint_classification(collective_stupidity_2026, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — From a civilizational frame, stupidity is an irreducible feature of any system with bounded rationality and cognitive heterogeneity. No agent can be fully rational; information asymmetries, attention limits, and cognitive biases are universal constraints on human decision-making. The existence of agents who cause harm without personal gain is a structural property of large-scale systems, not a contingent institutional arrangement. However, this risks naturalizing what is partly remediable through institutional design.
constraint_indexing:constraint_classification(collective_stupidity_2026, mountain,
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
    constraint_indexing:constraint_classification(collective_stupidity_2026, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.68): High. The constraint concentrates harm on rational victims who cannot identify or exit stupid actors in advance. The victim bears full cost; the stupid actor derives zero benefit. This is pure extraction in structural terms. The value reflects that stupidity is persistent (increases over the measurement interval from 0.45 to 0.68) and that institutional responses to stupidity (audits, regulations) add overhead without eliminating the underlying harm. Suppression (0.72): High. Mechanisms preventing escape: (1) Stupid actors are typically identifiable only post-hoc, after harm occurs; (2) Rational agents often cannot exit shared systems (institutions, commons, families) without substantial cost; (3) Institutional responses to stupidity can themselves be stupid, creating cascades; (4) Information asymmetries prevent rational agents from sorting away from stupid actors in advance. Theater ratio (0.38): Moderate-low. Institutional responses to stupidity (compliance procedures, regulatory audits, reporting requirements) are partly performative, but stupidity itself produces real harm that cannot be purely theatrical. The theater ratio has increased modestly over the interval (0.25 to 0.38) as institutions have added procedural overhead in response to visible stupidity, without addressing the underlying constraint.
 *
 * PERSPECTIVAL GAP:
 *   The Snare classification is dominant across perspectives, but with variation. The rational victim and collective welfare see maximal extraction (trapped, global scope). The stupid agent sees a tangled rope — some social/institutional systems enable their participation or provide indirect rewards (belonging, attention) while also confining them. The institutional system sees its own degradation (piton) — bureaucratic responses to stupidity are high-theater, low-function. The organized epistemic community sees a solvable coordination problem (rope) — better information, institutional design, and incentive alignment could reduce stupid harm. The civilizational analyst risks seeing an immutable natural law (mountain) — bounded rationality implies some stupidity in all systems — but this naturalizes what is partly remediable. The perspectival gap reveals whether the constraint is seen as structural/inevitable or as a design failure amenable to institutional reform.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural relationships. Rational victims have high d (0.85+) because they bear uncompensated cost with no benefit; trapped exit status maximizes their experienced extraction. Collective welfare has high d because it is a powerless abstract entity bearing intergenerational harm. Stupid agents occupy a paradoxical position: definitionally, they provide zero personal benefit (low d per naive reading), yet they operate within systems that sometimes reward or enable them (moderate d from institutional embeddedness). Institutional systems that respond to stupidity have moderate d because they deploy resources to manage externalities but often compound the problem. Epistemic communities attempting to solve stupidity through design have low d (arbitrage options, organized power) — they experience the constraint as solvable. The analytical observer has analytical directionality, risking naturalization of a contingent social problem.
 *
 * MANDATROPHY ANALYSIS:
 *   The Cipolla-Galloway constraint resolves the mandatrophy by distinguishing between stupidity as irreducible cognitive heterogeneity (mountain) and stupidity as an institutional trap that concentrates damage on rational victims (snare). The false summit lies in the claim that 'bounded rationality implies stupidity is inevitable, therefore the harm is a natural law.' Empirically, stupid harm is not uniformly distributed: institutional design, information systems, and incentive structures determine how much stupid action causes damage. Some institutions minimize stupid harm (redundant systems, error-correction, separation of duties); others amplify it (blame-shifting, cascading failures, perverse incentives). This is not a natural law. The snare classification is therefore robust: the constraint is a real structural trap that extracts uncompensated harm from rational agents, and it is remediable through institutional design. The piton perspective (institutional inertia) and rope perspective (solvable through design) are the actionable insights — stupidity is not immutable, but institutions often degrade in their ability to contain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stupidity_definition_boundary,
    'Does an agent who causes harm while pursuing a selfish goal that fails constitute stupidity, or is stupidity definitionally restricted to actions that provide zero personal gain?',
    'Empirical classification of harm-causing agents: partition by whether intended benefit accrued vs. harm outweighed intended benefit. Analysis of rational choice failure vs. value misalignment.',
    'If boundary is loose: stupidity becomes nearly synonymous with failed rationality (ε → 0.35). If boundary is strict: stupidity remains rare and structural (ε → 0.75).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stupidity_definition_boundary, conceptual, 'Boundary between stupidity and failed rational action').

omega_variable(
    harm_measurability_asymmetry,
    'Is the asymmetry between measurable harm to others and unmeasurable (zero) benefit to the stupid agent stable, or do stupid agents often receive unmeasured social/psychological rewards (belonging, attention, sense of impact) that partly offset their definition?',
    'Psychological study of stupid actors'' subjective experience; social network analysis of stupid agents'' position and ties; longitudinal tracking of compensation mechanisms (social status, resource flows) accruing to visibly stupid actors.',
    'If hidden rewards are significant: the Snare classification weakens (effective extraction lower than structural extraction). If rewards are negligible: Snare is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_measurability_asymmetry, empirical, 'Whether unmeasured rewards offset stupidity''s zero-benefit definition').

omega_variable(
    institutional_amplification_mechanism,
    'Does stupidity primarily cause damage through individual action, or through institutional mechanisms that amplify and concentrate stupid choices (scaling them into public harm)?',
    'Comparative analysis of stupid actions with vs. without institutional amplification; causal decomposition of harm into direct (individual) vs. systemic (institutional) components.',
    'If primarily individual: the constraint is a coordination problem (Rope from more perspectives). If institutional amplification dominates: the constraint is an extractive trap (Snare structural property).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_amplification_mechanism, empirical, 'Role of institutional amplification in scaling stupidity to collective harm').

omega_variable(
    exit_cost_heterogeneity,
    'Can rational agents effectively exit stupid systems or institutions, or is exit itself costly/dangerous (exit costs are endogenous to stupidity)?',
    'Historical case analysis of exit from stupid institutions (colonies, cults, regimes, corporations); measurement of exit costs vs. cost of staying; correlation between exit barriers and stupidity persistence.',
    'If exit is genuinely free: the snare is weaker (not all victims are trapped). If exit costs are high and endogenous: the snare is structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_heterogeneity, empirical, 'Whether rational agents can exit systems with high stupidity density').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_stupidity_2026, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cstupid_tr_t0, collective_stupidity_2026, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cstupid_tr_t25, collective_stupidity_2026, theater_ratio, 25, 0.32).
narrative_ontology:measurement(cstupid_tr_t50, collective_stupidity_2026, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(cstupid_be_t0, collective_stupidity_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cstupid_be_t25, collective_stupidity_2026, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(cstupid_be_t50, collective_stupidity_2026, base_extractiveness, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_stupidity_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(collective_stupidity_2026, institutional_degradation).
narrative_ontology:affects_constraint(collective_stupidity_2026, commons_tragedy).
narrative_ontology:affects_constraint(collective_stupidity_2026, rational_actor_assumption).

% DUAL FORMULATION NOTE:
% The stupidity snare is structurally upstream of institutional degradation (piton forms when bureaucracies become stupid) and commons tragedy (stupid actors overexploit shared resources). The constraint is also a critique of the rational actor assumption in economics and political theory — stupidity reveals that bounded rationality is not merely a complication but a structural feature that enables extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(collective_stupidity_2026, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
