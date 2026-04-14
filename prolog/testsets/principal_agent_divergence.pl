% ============================================================================
% CONSTRAINT STORY: principal_agent_divergence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_principal_agent_divergence, []).

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
 *   constraint_id: principal_agent_divergence
 *   human_readable: Principal-Agent Divergence
 *   domain: economic_governance/institutional_dynamics
 *
 * SUMMARY:
 *   Principal-agent divergence is the structural misalignment that arises
 *   when authority is delegated without perfect monitoring and without
 *   perfect incentive alignment. An owner hires a manager; a government
 *   appoints a bureaucrat; a fund invests capital with an asset manager; a
 *   corporation employs workers. In each case, the agent's incentives may
 *   diverge from the principal's due to information asymmetry (the agent
 *   knows more than the principal about effort or outcomes) and hidden action
 *   (the principal cannot costlessly observe what the agent is actually
 *   doing). This fundamental structural feature creates a hybrid constraint:
 *   genuine coordination function (delegation enables specialization and
 *   scale) overlaid with extraction opportunity (information asymmetry
 *   enables rent capture). The classification depends entirely on the
 *   observer's position: the principal sees extraction; the agent sees
 *   coordination; the institution sees both; the regulator sees a temporary
 *   problem being solved; the analyst risks naturalizing a contingent
 *   institutional pattern as fundamental law. The theater ratio (0.58)
 *   reflects that principal-agent mitigation mechanisms (compensation
 *   committees, audit committees, fiduciary duties) often operate
 *   performatively rather than substantively — they demonstrate that the
 *   problem is being 'managed' while structural asymmetries persist.
 *
 * KEY AGENTS:
 *   - Principal (Shareholder/Owner/Government): Primary victim (powerless/trapped) — bears residual risk, pays monitoring costs, cannot costlessly verify agent actions
 *   - Agent (Manager/Executive/Bureaucrat/Worker): Primary beneficiary (organized/arbitrage) — captures information rents, hidden action rents, and specialization benefits; has exit options
 *   - Institutional Actor (Board/Regulator/Market): Intermediate (powerful/mobile) — experiences the constraint as genuine hybrid of coordination and extraction
 *   - Stakeholder Class (Workers/Shareholders/Citizens): Secondary victim (moderate/constrained) — bears distributed divergence costs; partially organized
 *   - Regulatory Coalition (Governance Reformers/Activists): Organized beneficiary of sunset mechanisms (organized/constrained) — building transparency and accountability mechanisms that shift classification toward scaffold
 *   - Institutional System (Corporate Governance/Public Administration): Piton perspective (institutional/arbitrage) — maintains performative monitoring and compensation ritual
 *   - Analytical Observer: Risks naturalizing a contingent institutional pattern as fundamental constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(principal_agent_divergence, 0.52).
domain_priors:suppression_score(principal_agent_divergence, 0.65).
domain_priors:theater_ratio(principal_agent_divergence, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(principal_agent_divergence, extractiveness, 0.52).
narrative_ontology:constraint_metric(principal_agent_divergence, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(principal_agent_divergence, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(principal_agent_divergence, tangled_rope).
narrative_ontology:human_readable(principal_agent_divergence, "Principal-Agent Divergence").
narrative_ontology:topic_domain(principal_agent_divergence, "economic_governance/institutional_dynamics").

domain_priors:requires_active_enforcement(principal_agent_divergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(principal_agent_divergence, agent_with_information_asymmetry).
narrative_ontology:constraint_beneficiary(principal_agent_divergence, agent_with_hidden_actions).
narrative_ontology:constraint_victim(principal_agent_divergence, principal_bearing_residual_risk).
narrative_ontology:constraint_victim(principal_agent_divergence, collective_welfare_if_agent_extracts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRINCIPAL (SNARE) — The principal is structurally trapped. They have delegated authority but cannot costlessly monitor or verify agent actions. Information asymmetry is structural — the agent's incentives diverge from the principal's by design. The principal bears residual risk and must pay escalating monitoring costs. Exit is costly (replacing the agent, rebuilding trust structures, restructuring incentives). This is pure extraction: the agent captures rents through hidden information and hidden action.
constraint_indexing:constraint_classification(principal_agent_divergence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE AGENT (ROPE) — The agent experiences the constraint as a coordination mechanism: they are solving the principal's delegation problem. The incentive alignment required (through wages, bonuses, equity, profit-sharing) is a genuine coordination device that enables both parties to benefit from specialization and scale. The agent has arbitrage options (alternative principals, alternative employment) and can exit. They experience this as voluntary coordination, not extraction.
constraint_indexing:constraint_classification(principal_agent_divergence, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE INSTITUTIONAL ACTOR (TANGLED ROPE) — At institutional scale, principal-agent divergence is a genuine hybrid: it solves the coordination problem of delegation while simultaneously enabling extraction through information asymmetry. Modern institutional agents (CEOs, asset managers, bureaucrats with specialized knowledge) have both genuine coordination function and access to rent extraction. They are mobile enough to negotiate better terms but constrained by the institutional context. This is the canonical tangled rope: essential coordination function overlaid with asymmetric extraction.
constraint_indexing:constraint_classification(principal_agent_divergence, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: THE STAKEHOLDER CLASS (TANGLED ROPE) — Shareholders and workers experience agent divergence as both coordination and extraction. The agent's specialization and effort create value (coordination function). The agent's ability to extract rents reduces that value flow to stakeholders. Stakeholders cannot easily exit (capital is trapped, labor is locally constrained) but have some capacity to organize (shareholder activism, unionization). They experience significant but not total extraction.
constraint_indexing:constraint_classification(principal_agent_divergence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE REGULATORY COALITION (SCAFFOLD) — Organized regulatory and activist groups see principal-agent divergence as a temporary coordination failure being addressed through transparency mandates, fiduciary duties, clawback provisions, and stakeholder governance reforms. These sunset mechanisms (say-on-pay votes, ESG disclosure, worker representation on boards) are gradually shifting the constraint. As monitoring technology improves and transparency norms mature, the extraction component fades and the coordination function hardens. This is genuinely scaffold: high-suppression temporary mechanism with built-in sunset.
constraint_indexing:constraint_classification(principal_agent_divergence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: INSTITUTIONAL MOMENTUM (PITON) — Principal-agent theory itself has become substantially performative. Compensation committees structure pay according to optimization logic (efficiency wages, incentive alignment) that economics predicts, while actual mechanisms operate on political and social legitimacy. Boards perform fiduciary duty through ritual compliance. The theater ratio is high because the formal mechanisms (monitoring, incentives, contracts) ostensibly address divergence while structural asymmetries persist through institutional inertia. The framework persists because alternatives haven't fully displaced it, not because the compensation and monitoring apparatus actually solves the divergence problem.
constraint_indexing:constraint_classification(principal_agent_divergence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FUNDAMENTAL VIEW (MOUNTAIN) — From a civilizational perspective, principal-agent divergence appears to be an irreducible feature of delegation itself: whenever authority is separated from consequences, incentive misalignment follows from information asymmetry and the impossibility of comprehensive contracting. This appears as a fundamental constraint on institutional design. However, the structural data contradicts the mountain classification — the constraint is addressing a real coordination problem (agents specializing), and the extraction component is contingent on institutional arrangements (monitoring capacity, transparency technology, governance norms). The mountain perspective naturalizes what is structurally a tangled rope.
constraint_indexing:constraint_classification(principal_agent_divergence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(principal_agent_divergence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(principal_agent_divergence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(principal_agent_divergence, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(principal_agent_divergence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(principal_agent_divergence, TR),
    TR >= 0.70.

:- end_tests(principal_agent_divergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Principal-agent divergence enables real extraction through information asymmetry and hidden action, but the extraction is constrained by: (1) the principal's ability to eventually discover divergence (though with lag), (2) the agent's need to maintain reputation and relationship continuity, (3) competitive pressure (principals can replace agents, agents can find alternative principals), and (4) institutional oversight. The extractiveness value reflects that the mechanism is real and persistent but not total — constraints operate even in the worst-case scenarios. Suppression (0.65): Moderate-high. Significant structural barriers prevent the principal from monitoring and correcting divergence: information asymmetry is by definition, specialized knowledge creates epistemic barriers, tacit understanding of operations is agent-controlled, and replacing an agent is costly. However, suppression is not maximum — some transparency exists, some monitoring is possible, and some exit options are available. Theater ratio (0.58): Moderate. Principal-agent mitigation mechanisms (compensation committees, audit committees, clawback provisions, fiduciary duty declarations) have genuine function but also substantial performative content. Boards structure CEO compensation according to economic theory but continue paying rents that theory would eliminate. Audit committees report on controls while principal-agent problems persist. The theater ratio increases slightly over the interval (0.42 to 0.58) as governance mechanisms accumulate without proportionate reduction in actual divergence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The principal (powerless/trapped) sees a snare — information asymmetry that cannot be costlessly closed, hidden action that cannot be prevented, and residual risk that must be borne. The agent (organized/arbitrage) sees a rope — they are solving the principal's coordination problem through specialization, and their compensation aligns incentives. The institution (powerful/mobile) sees a tangled rope — genuine coordination function (agent effort and knowledge create value) alongside genuine extraction (agent captures rents through asymmetry). The regulator (organized/constrained) sees a scaffold — transparency mandates, clawback provisions, and stakeholder governance are gradually shifting the constraint toward zero divergence with a sunset. The institutional system (institutional/arbitrage) sees a piton — monitoring and compensation mechanisms persist performatively despite persistent divergence, maintained by institutional inertia. The analyst (analytical/analytical) risks seeing a mountain — principal-agent divergence as an irreducible feature of delegation itself. The perspectival gap reveals that the constraint's classification depends critically on the observer's structural relationship: beneficiaries see coordination, victims see extraction, institutions see hybrids, reformers see temporary problems, and analysts risk naturalizing contingency.
 *
 * DIRECTIONALITY LOGIC:
 *   The principal's high d (trapped victim with information asymmetry and residual risk) derives from three structural facts: (1) the principal has delegated authority and cannot costlessly recover it, (2) the principal cannot verify agent effort or choices without incurring monitoring costs, and (3) the principal bears the consequences of agent failures or extraction. These facts do not change based on the principal's power level — a powerful principal (a large corporation's board) and a powerless principal (an individual relying on a professional advisor) both experience trapped information asymmetry. The agent's low d (arbitrage beneficiary with specialized knowledge) derives from: (1) the agent possesses information the principal needs and cannot easily acquire, (2) the agent can exit to alternative principals if the current principal tightens incentives, and (3) the agent's specialized effort is genuinely valuable. Institutional actors split the difference — they benefit from agent work but also bear costs when agents diverge. Stakeholders (workers, shareholders) have constrained rather than arbitrage exit — they can eventually find alternative employment or reallocate capital, but at significant cost, placing them in the high-d range despite not being the immediate principal.
 *
 * MANDATROPHY ANALYSIS:
 *   Principal-agent divergence is a canonical tangled rope, and the JSON resolves the mandatrophy by clarifying the dual structure. The coordination function is genuine: delegation enables specialization and scale, and incentive alignment (wages, bonuses, equity) solves the problem of motivating effort in specialized tasks. The extraction function is also genuine: information asymmetry enables the agent to capture rents through hidden information and hidden action, and suppression (monitoring costs, replacement costs, information barriers) prevents the principal from costlessly eliminating divergence. Both functions coexist. The classical error is to misclassify this as pure rope (seeing only the coordination benefit) or pure snare (seeing only the extraction mechanism). The six-perspective analysis reveals that the classification depends entirely on the observer's structural position: the principal accurately sees snare (they cannot exit cheaply and bear full residual risk), the agent accurately sees rope (they are solving a real problem and have exit options), the institution accurately sees tangled rope (both functions operate), and the analyst accurately sees the risk of false mountain (naturalizing the contingent distribution of monitoring technology and transparency norms as fundamental law). No single type is correct — the presheaf over observation positions IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    monitoring_technology_threshold,
    'At what level of monitoring technology (real-time auditing, algorithmic compliance, continuous verification) does information asymmetry collapse and the constraint shift from snare to rope?',
    'Empirical measurement of agency cost reduction as monitoring technology improves; comparison of divergence metrics across organizations with different monitoring infrastructure',
    'If threshold is reachable: divergence is contingent on monitoring capacity and will fade with technology. If threshold is unreachable: information asymmetry is structural even with perfect monitoring (agents can still hide action), and divergence persists as an irreducible feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monitoring_technology_threshold, empirical, 'Technology threshold for information asymmetry collapse').

omega_variable(
    incentive_alignment_sufficiency,
    'Can any compensation structure (bonuses, equity, clawbacks, long-term vesting) actually align agent incentives with principal interests, or is divergence inherent to the delegation relationship?',
    'Historical analysis of compensation reforms; correlation between incentive structure tightness and actual divergence outcomes; identification of persistent divergence even under high-powered incentives',
    'If solvable: divergence is a coordination problem with technical solutions (better contracts, better monitoring). If unsolvable: divergence is inherent to agency relationships and requires acceptance of residual extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incentive_alignment_sufficiency, empirical, 'Whether incentive alignment can sufficiently reduce divergence').

omega_variable(
    institutional_capture_feedback,
    'Does the agent''s control over the principal''s information (regulatory capture, agency capture) create positive feedback that locks the principal into extraction paths they would otherwise exit?',
    'Analysis of institutional change trajectories; identification of cases where principals reorganized to reduce information asymmetry vs. cases where asymmetry self-reinforced',
    'If positive feedback dominates: divergence can transition from snare to structural extraction trap. If feedback is weak: organizational exits and reforms are possible even when monitoring is expensive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_feedback, empirical, 'Positive feedback between capture and institutional lock-in').

omega_variable(
    stakeholder_exit_costs,
    'For workers and shareholders, are divergence-induced losses recoverable through exit (finding alternative employment, reallocating capital) or are they structurally locked in to bearing residual risk?',
    'Labor market fluidity analysis; capital reallocation speed; comparison of exit costs across labor markets and capital markets with different institutional structures',
    'If exit is cheap: stakeholders can arbitrage away divergence costs and the snare classification is overstated. If exit is expensive: stakeholders are trapped and the snare classification is appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stakeholder_exit_costs, empirical, 'Stakeholder exit cost and availability').

omega_variable(
    divergence_visibility_threshold,
    'At what magnitude of agent divergence do transparency and accountability mechanisms activate to enforce correction? Is there a tipping point where hidden extraction becomes visible and triggers principal countermeasures?',
    'Historical case studies of divergence discovery and correction; measurement of detection lag between divergence onset and principal response; analysis of visibility thresholds that trigger regulatory or governance intervention',
    'If low visibility threshold: divergence is ephemeral and corrects quickly (closer to rope). If high visibility threshold: divergence can persist for extended periods before correction (closer to snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divergence_visibility_threshold, empirical, 'Visibility threshold for divergence detection and correction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(principal_agent_divergence, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prin_tr_t0, principal_agent_divergence, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prin_tr_t3, principal_agent_divergence, theater_ratio, 3, 0.5).
narrative_ontology:measurement(prin_tr_t6, principal_agent_divergence, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(prin_be_t0, principal_agent_divergence, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prin_be_t3, principal_agent_divergence, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(prin_be_t6, principal_agent_divergence, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(principal_agent_divergence, resource_allocation).
narrative_ontology:affects_constraint(principal_agent_divergence, regulatory_capture).
narrative_ontology:affects_constraint(principal_agent_divergence, information_asymmetry_in_markets).
narrative_ontology:affects_constraint(principal_agent_divergence, executive_compensation_ratchet).
narrative_ontology:affects_constraint(principal_agent_divergence, bureaucratic_discretion).

% DUAL FORMULATION NOTE:
% Principal-agent divergence is a parent constraint that drives multiple institutional and economic downstream constraints. Regulatory capture occurs when the agent (regulator or regulated firm) exploits information asymmetry to extract from the principal (public interest). Executive compensation ratchet occurs when agents compete to extract rents through compensation structuring. Information asymmetry in markets occurs when agents (traders, brokers, sellers) diverge from principal (retail investor, buyer). Bureaucratic discretion occurs when administrative agents diverge from legislative principal. Each downstream constraint has its own ε and its own perspectives, but all are manifestations of principal-agent divergence applied to specific institutional contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
