% ============================================================================
% CONSTRAINT STORY: organizational_trust_degradation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organizational_trust_degradation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: organizational_trust_degradation
 *   human_readable: Organizational Trust Degradation and Control Mechanism Substitution
 *   domain: organizational_sociology/institutional_dynamics
 *
 * SUMMARY:
 *   Organizational trust degradation represents a structural constraint where
 *   trust — the implicit social capital that enables coordination without
 *   constant monitoring — erodes, triggering substitution by formal control
 *   systems. These control systems (compliance monitoring, performance
 *   metrics, documentation requirements, surveillance technologies) genuinely
 *   solve a coordination problem: they make behavior legible and reduce legal
 *   liability. But they simultaneously extract from frontline agents and
 *   tacit knowledge systems that depend on informal coordination. This
 *   constraint exhibits a perspectival cascade: frontline agents experience
 *   pure extraction (snare); middle managers experience mixed coordination
 *   and extraction (tangled_rope); compliance departments experience pure
 *   coordination (rope); the organization as a whole experiences tangled_rope
 *   (both problems solved and new problems created). The theater ratio (0.68)
 *   reflects that much compliance activity is performative — it produces
 *   documentation that demonstrates due diligence but does not actually
 *   prevent misconduct or improve performance. The constraint has grown over
 *   20 years as organizations have accumulated compliance layers in response
 *   to legal and reputational risks.
 *
 * KEY AGENTS:
 *   - Frontline Agents: Primary victims (powerless/constrained) — experience surveillance, documentation burden, loss of autonomy; tacit knowledge and informal coordination degraded by formalization
 *   - Middle Managers: Secondary victims and partial beneficiaries (moderate/mobile) — coordinate organizational work but spend increasing time on compliance theater and documentation
 *   - Compliance Departments: Primary beneficiaries (institutional/arbitrage) — expand scope and justification as trust degrades; function grows because the problem they solve keeps growing
 *   - Control Apparatus Professionals: Secondary beneficiaries (institutional/arbitrage) — monitoring vendors, auditors, HR specialists whose services expand with formalization
 *   - Organization (Collective Actor): Victim of extraction (institutional/constrained) — incurs compliance cost, bureaucratic drag, tacit knowledge loss; cannot exit without legal risk
 *   - Organizational Redesign Advocates: Organized challengers (organized/constrained) — building alternative pathways (psychological safety, trust-based scheduling, flat hierarchies) with sunset logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing control as inherent to organization rather than recognizing it as contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organizational_trust_degradation, 0.58).
domain_priors:suppression_score(organizational_trust_degradation, 0.62).
domain_priors:theater_ratio(organizational_trust_degradation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organizational_trust_degradation, extractiveness, 0.58).
narrative_ontology:constraint_metric(organizational_trust_degradation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(organizational_trust_degradation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organizational_trust_degradation, tangled_rope).
narrative_ontology:human_readable(organizational_trust_degradation, "Organizational Trust Degradation and Control Mechanism Substitution").
narrative_ontology:topic_domain(organizational_trust_degradation, "organizational_sociology/institutional_dynamics").

domain_priors:requires_active_enforcement(organizational_trust_degradation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organizational_trust_degradation, management_control_apparatus).
narrative_ontology:constraint_beneficiary(organizational_trust_degradation, compliance_monitoring_vendors).
narrative_ontology:constraint_victim(organizational_trust_degradation, frontline_agents).
narrative_ontology:constraint_victim(organizational_trust_degradation, organizational_cohesion).
narrative_ontology:constraint_victim(organizational_trust_degradation, tacit_knowledge_transfer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE AGENT (SNARE) — Experiences trust degradation as a trap. Survives on tacit knowledge, informal networks, and relational coordination with peers. As formal monitoring and control systems expand, the agent faces increased surveillance, documentation burden, and behavioral constraints. Exit via job change incurs retraining costs and career reputation risk. The agent bears extraction without corresponding benefit — compliance work crowds out productive work, and the enforcement apparatus views informal coordination as a threat rather than an asset.
constraint_indexing:constraint_classification(organizational_trust_degradation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE MANAGER (TANGLED ROPE) — Experiences both coordination and extraction. The formal control systems provide genuine clarity on accountability and reduce ambiguity about performance criteria. But enforcement mechanisms also require the manager to document everything, report on subordinates, and spend significant time on compliance theater rather than coaching and development. Mobile exit options (lateral moves, consulting, retirement) exist but carry costs. The manager sees the constraint as necessary but excessive.
constraint_indexing:constraint_classification(organizational_trust_degradation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPLIANCE DEPARTMENT (ROPE) — Experiences the constraint as pure coordination. The department's function is to translate legal and regulatory requirements into operational rules. As trust degrades, the compliance function grows (more rules, more monitoring, more documentation). The department benefits from expanded scope and justifies its existence through the very problems it is hired to solve. From this perspective, trust degradation is a coordination challenge — the organization needs clarity on what is allowed, and compliance provides that clarity. Low extraction experienced by this institutional actor.
constraint_indexing:constraint_classification(organizational_trust_degradation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: CONTROL RITUAL SYSTEM (PITON) — The apparatus of performance reviews, compliance audits, and formal accountability metrics persists through institutional inertia. Much of the monitoring is performative — it produces documentation that looks like accountability but does not actually predict performance or prevent misconduct. The theater ratio is high (0.68) because time-and-motion studies, certification processes, and compliance reporting are largely about demonstrating due diligence rather than preventing problems. The system is maintained because alternatives have not fully replaced it and because legal liability concerns make formal documentation feel safer than trust.
constraint_indexing:constraint_classification(organizational_trust_degradation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZATIONAL REDESIGN MOVEMENT (SCAFFOLD) — Organized agents (HR reformers, management theorists, some forward-thinking executives) see trust degradation as a temporary coordination failure solvable through structural redesign: psychological safety initiatives, transparent communication, flat hierarchies, peer accountability. These agents view the compliance apparatus as a problem to be overcome, not a solution to be perfected. They are building alternative pathways (OKRs, agile governance, trust-based scheduling) with explicit sunset logic — the idea is that trust can be rebuilt and formal control systems gradually reduced. High suppression during the transition period (forcing adoption of new models, risk of failed redesigns) but with a clear exit path.
constraint_indexing:constraint_classification(organizational_trust_degradation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LARGE ORGANIZATION (INSTITUTIONAL ACTOR, TANGLED ROPE) — At the level of the organization itself, the constraint exhibits both coordination and extraction. Formal control systems genuinely reduce legal liability and ensure regulatory compliance — they solve a real coordination problem across thousands of agents with heterogeneous incentives. But the organization also experiences extraction through compliance cost, bureaucratic drag, and loss of tacit knowledge. The organization's exit options are constrained — it cannot simply trust its agents without incurring legal and reputational risk — but it also cannot fully escape the trust problem by piling on controls. This perspective bridges the beneficiary's and victim's experiences.
constraint_indexing:constraint_classification(organizational_trust_degradation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, the coordination of large organizations with diverse incentives is a genuine problem. Some level of formal accountability, documentation, and monitoring is inherent to organizing humans at scale. Trust alone cannot coordinate strangers; you need rules, transparency, and consequences. From this view, the constraint is a coordination mechanism (rope) that enables large-scale cooperation. However, the structural data reveals asymmetric extraction hidden in the coordination frame — the formal control apparatus extracts from frontline agents while benefiting management and compliance functions. The rope classification at this perspective risks naturalizing what is actually a tangled extraction system.
constraint_indexing:constraint_classification(organizational_trust_degradation, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organizational_trust_degradation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organizational_trust_degradation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organizational_trust_degradation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organizational_trust_degradation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organizational_trust_degradation, TR),
    TR >= 0.70.

:- end_tests(organizational_trust_degradation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from frontline agents through compliance burden and autonomy loss, and from the organization through bureaucratic drag and tacit knowledge degradation. But the extraction is not maximum because compliance systems do solve real coordination problems (legal liability, regulatory requirement, behavioral clarity across diverse agents). The increase from 0.25 to 0.58 over the interval reflects accumulation of compliance layers and loss of trust-based informal coordination — as extractiveness rose, so did theater and suppression. Suppression (0.62): High. Frontline agents face significant barriers to exit (job loss, retraining costs, career risk). The organization faces barriers to reducing controls (legal liability, reputational risk, regulatory requirement). But suppression is not total because some organizations have successfully reduced compliance burden, and exit at individual level is possible at high cost. Theater ratio (0.68): High and increasing. Performance reviews measure what is easy to measure (hours logged, documents filed, compliance boxes checked) rather than what matters (actual performance, problem-solving, innovation). Compliance audits produce documentation that looks like accountability but does not predict or prevent misconduct. The theater has increased as organizations have shifted from trust-based to documentation-based accountability. Claimed type (tangled_rope) is driven by dual function: genuine coordination (solving legal/behavioral clarity problems) plus asymmetric extraction (from frontline agents and tacit knowledge systems).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is profound and reveals the constraint's nature. The frontline agent sees a snare — they have no choice but to comply, the system extracts their autonomy and tacit knowledge, and alternatives are blocked by exit costs. The compliance department sees a rope — they are solving a genuine coordination problem (making behavior legible), the system enables the organization to operate at scale without legal risk, and their work is valuable. The organization sees a tangled_rope — it needs the controls but it also knows the controls are degrading tacit knowledge, slowing decision-making, and creating bureaucratic drag. The analytical observer at civilizational scope risks seeing this as an inherent feature of large organizations (implicit mountain) — the idea that trust is impossible at scale and formal control is necessary. But the structural data reveals this as a false summit: trust-based alternatives exist (psychological safety models, flat hierarchies, peer accountability), many smaller organizations operate with minimal formal control, and historical organizations operated with far less compliance apparatus. The degradation is contingent on institutional choices, not inherent to organization itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by structural position relative to the extraction flow. Frontline agents (powerless/constrained) experience high d — they are targets of extraction, face barriers to exit, and bear the compliance burden without corresponding benefit. Compliance departments (institutional/arbitrage) experience low d — they are beneficiaries, have arbitrage options (can move to consulting, other companies, new roles), and benefit from the constraint's existence. Middle managers (moderate/mobile) experience moderate d — they both coordinate others' work (beneficiary function) and report on them (enforcement function), and have exit options but at moderate cost. The organization (institutional/constrained) experiences moderate-high d — it is both problem-solver (using controls to solve coordination problem) and victim (bearing compliance cost and tacit knowledge loss), with constrained exit (cannot simply abandon controls without legal risk). The analytical observer (analytical/analytical) experiences canonical d around 0.73, which risks treating the constraint as immutable rather than contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that trust degradation is not a mountain of organizational necessity but a tangled_rope of institutional choice. The analytical observer's implicit mountain ('you cannot coordinate without control') is a false summit. The constraint exhibits genuine coordination function (making behavior legible, enabling large-scale cooperation) AND asymmetric extraction (from frontline agents, tacit knowledge systems, organizational responsiveness). The resolution path is NOT to eliminate controls entirely (which would reintroduce coordination problems) but to differentiate: high-stakes decisions (legal compliance, safety-critical processes) require formal accountability; low-stakes decisions (routine work, team coordination, problem-solving) can rely on trust and informal coordination. The redesign perspective (scaffold) is realistic not as a complete sunset (the constraint cannot be eliminated) but as a transition: reduce theater ratio by automating compliance documentation, rebuild trust through psychological safety initiatives, clarify which decisions genuinely require formal control vs which are over-controlled. The piton perspective captures real degradation — many compliance rituals persist through inertia rather than function — but the constraint's core function (solving coordination at scale) remains necessary. The mandatrophy does not require choosing a single type but recognizing that all types are legitimate readings of the same structural data: what looks like coordination to the compliance department looks like extraction to the frontline agent, and what looks like necessity to the analytical observer looks like institutional choice to the redesign advocates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trust_baseline_definition,
    'What level of organizational trust is ''baseline normal'' vs degraded?',
    'Historical comparison: organizations in the 1960s-1980s vs modern organizations; comparison of surveillance intensity, documentation burden, and informal coordination across sectors and time periods',
    'If baseline was already low-trust: current degradation is continuation. If baseline was high-trust: current degradation represents a structural shift with real extraction accumulation. This affects whether the constraint is a recent snare or a chronic tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trust_baseline_definition, empirical, 'Definition of baseline organizational trust level').

omega_variable(
    control_mechanism_necessity,
    'How much of the formal control apparatus is necessary for legal/regulatory compliance vs how much is defensive theater?',
    'Comparative analysis across regulatory regimes: organizations in low-regulation environments vs high-regulation environments; longitudinal analysis of compliance documentation vs actual misconduct incidents',
    'If mostly necessary: the snare classification is incorrect — the apparatus genuinely solves a coordination problem. If mostly theater: the extraction is real and the apparatus is maintained through liability fears rather than functional requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_mechanism_necessity, empirical, 'Proportion of formal controls that are necessary vs defensive').

omega_variable(
    identity_lock_in_compliance,
    'Do compliance-department professionals and control-oriented managers experience the apparatus as functionally binding or as identity-constitutive?',
    'Qualitative interviews: can compliance professionals imagine operating without the control systems, or has their professional identity fused with the apparatus? Do they resist alternative governance models because they are incompatible with the rules, or because they would require abandoning the compliance identity?',
    'If identity_locked: the control apparatus persists even after trust could be rebuilt — professionals continue advocating for rules not because rules are necessary but because rules are who they have become. If merely constrained: the control apparatus could shift with organizational redesign.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_compliance, conceptual, 'Whether compliance professionals are identity-locked to the control apparatus').

omega_variable(
    tacit_knowledge_irreplaceability,
    'How much of organizational performance depends on tacit knowledge and informal coordination that formal systems either capture poorly or destroy through formalization?',
    'Performance analysis before and after major compliance overhauls; comparison of productivity, error rates, and innovation velocity; exit interviews tracking whether departing employees cite bureaucratic burden',
    'If irreplaceable: formal controls are extracting from the organization itself by degrading tacit knowledge flows. If replaceable: formal documentation is a necessary cost that can be reduced through better system design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tacit_knowledge_irreplaceability, empirical, 'Whether tacit knowledge is irreplaceable or documentable').

omega_variable(
    trust_rebuilding_feasibility,
    'Under what conditions can organizational trust be rebuilt, and is the scaffold perspective realistic or aspirational?',
    'Case studies of organizations that successfully reduced compliance burden and rebuilt trust; analysis of why some redesigns succeed and others fail; identification of preconditions (stable leadership, low legal vulnerability, sector culture)',
    'If feasible: the scaffold perspective is realistic and the constraint has a genuine sunset path. If rare: the scaffold is aspirational and the constraint is more persistent (closer to piton or chronic tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trust_rebuilding_feasibility, empirical, 'Feasibility of trust rebuilding and organizational redesign').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organizational_trust_degradation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(otd_tr_t0, organizational_trust_degradation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(otd_tr_t10, organizational_trust_degradation, theater_ratio, 10, 0.52).
narrative_ontology:measurement(otd_tr_t20, organizational_trust_degradation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(otd_be_t0, organizational_trust_degradation, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(otd_be_t10, organizational_trust_degradation, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(otd_be_t20, organizational_trust_degradation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organizational_trust_degradation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(organizational_trust_degradation, 0.12).
narrative_ontology:affects_constraint(organizational_trust_degradation, tacit_knowledge_erosion).
narrative_ontology:affects_constraint(organizational_trust_degradation, bureaucratic_drag_accumulation).
narrative_ontology:affects_constraint(organizational_trust_degradation, employee_engagement_degradation).

% DUAL FORMULATION NOTE:
% Trust degradation is upstream of multiple organizational pathologies. Tacit knowledge erosion (constraint story: knowledge_formalization_loss, ε=0.35) is a direct consequence of the drive to documentize informal coordination. Bureaucratic drag (constraint story: compliance_overhead_accumulation, ε=0.52) emerges from layering multiple compliance systems without sunset. Employee engagement degradation (constraint story: autonomy_loss_from_monitoring, ε=0.64) is the subjective experience of frontline agents under high suppression. Each downstream constraint has its own extractiveness value reflecting different measurement basis, but all three are structurally dependent on the trust degradation constraint. Network edges flow downstream because trust degradation is the root mechanism triggering the pathologies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(organizational_trust_degradation, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
