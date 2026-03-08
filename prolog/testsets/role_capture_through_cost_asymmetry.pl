% ============================================================================
% CONSTRAINT STORY: role_capture_through_cost_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_role_capture_through_cost_asymmetry, []).

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
 *   constraint_id: role_capture_through_cost_asymmetry
 *   human_readable: Role Capture Through Cost Asymmetry in Organizational Ethics
 *   domain: organizational_ethics/systems_theory/moral_psychology
 *
 * SUMMARY:
 *   Role capture through cost asymmetry describes how institutional positions
 *   impose differential costs for moral action versus inaction, creating
 *   stable complicity equilibria. When an employee recognizes organizational
 *   misconduct, the cost of intervention (job loss, career damage, social
 *   isolation, retaliation) is immediate, personal, and certain, while the
 *   cost of complicity (moral injury, guilt, identity damage) is delayed,
 *   diffuse, and uncertain. This asymmetry is not accidental — it is
 *   structurally embedded in hierarchical role design. Junior employees face
 *   maximum extraction: they have the least power to intervene safely and the
 *   most to lose from retaliation. Senior leadership benefits from the
 *   stability the asymmetry provides: it prevents constant internal challenge
 *   and maintains operational continuity. The constraint exhibits both
 *   genuine coordination (role structures enable collective action and
 *   efficient decision-making) and extraction (the cost structure suppresses
 *   moral agency and enables institutional complicity). The theater ratio
 *   (0.58) reflects that many organizational ethics mechanisms — hotlines,
 *   training programs, ethics departments — are performative: they signal
 *   compliance without genuinely equalizing intervention costs. The
 *   constraint has accumulated extraction over time as organizations have
 *   become more legally sophisticated in managing retaliation (making it
 *   harder to prove) while ethics infrastructure has become more theatrical.
 *
 * KEY AGENTS:
 *   - Junior Employees: Primary victims (powerless/trapped) — face maximum cost asymmetry with no exit options; intervention destroys career, complicity extracts moral agency
 *   - Mid-Level Managers: Secondary victims (moderate/constrained) — experience both coordination value and extraction; can exit at significant cost; role embeddedness creates biographical lock-in
 *   - Senior Leadership: Primary beneficiaries (institutional/arbitrage) — benefit from stability mechanism; experience cost asymmetry as coordination that prevents disruptive challenge
 *   - Whistleblower Protection Movement: Organized actors (organized/mobile) — building legal frameworks to reduce intervention costs; see constraint as temporary with sunset logic
 *   - Corporate Ethics Departments: Institutional actors (institutional/constrained) — often performative; exist to manage liability rather than equalize costs; see own function as degraded
 *   - Professional Ethics Boards: Organized actors (organized/mobile) — have power to challenge structure but face institutional resistance; mobile across organizations but constrained within fields
 *   - Analytical Observer: Systems perspective (analytical/analytical) — sees both coordination and extraction as empirically real; recognizes contingency of cost asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(role_capture_through_cost_asymmetry, 0.48).
domain_priors:suppression_score(role_capture_through_cost_asymmetry, 0.62).
domain_priors:theater_ratio(role_capture_through_cost_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(role_capture_through_cost_asymmetry, extractiveness, 0.48).
narrative_ontology:constraint_metric(role_capture_through_cost_asymmetry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(role_capture_through_cost_asymmetry, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(role_capture_through_cost_asymmetry, tangled_rope).
narrative_ontology:human_readable(role_capture_through_cost_asymmetry, "Role Capture Through Cost Asymmetry in Organizational Ethics").
narrative_ontology:topic_domain(role_capture_through_cost_asymmetry, "organizational_ethics/systems_theory/moral_psychology").

domain_priors:requires_active_enforcement(role_capture_through_cost_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(role_capture_through_cost_asymmetry, institutional_continuity).
narrative_ontology:constraint_beneficiary(role_capture_through_cost_asymmetry, senior_leadership).
narrative_ontology:constraint_beneficiary(role_capture_through_cost_asymmetry, organizational_status_quo).
narrative_ontology:constraint_victim(role_capture_through_cost_asymmetry, individual_moral_agency).
narrative_ontology:constraint_victim(role_capture_through_cost_asymmetry, whistleblowers).
narrative_ontology:constraint_victim(role_capture_through_cost_asymmetry, junior_employees).
narrative_ontology:constraint_victim(role_capture_through_cost_asymmetry, organizational_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JUNIOR EMPLOYEE (SNARE) — Trapped by economic dependency and career vulnerability. Intervention costs are immediate and personal (job loss, blacklisting, retaliation), while complicity costs are diffuse and delayed (guilt, moral injury). The cost asymmetry is maximally extractive: speaking up destroys the individual; staying silent preserves their position but extracts their moral agency. No exit without severe penalty.
constraint_indexing:constraint_classification(role_capture_through_cost_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MID-LEVEL MANAGER (TANGLED ROPE) — Constrained by career investment and organizational embeddedness. Experiences both coordination (the role structure enables collective action and organizational function) and extraction (the cost asymmetry suppresses intervention even when misconduct is recognized). Can exit at significant cost. The role provides genuine coordination value while simultaneously binding moral agency through differential cost structures.
constraint_indexing:constraint_classification(role_capture_through_cost_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SENIOR LEADERSHIP (ROPE) — Benefits from the cost asymmetry as a stability mechanism. The differential cost structure prevents disruptive interventions and maintains operational continuity. Experiences the constraint as coordination: role differentiation enables efficient decision-making and protects the organization from constant internal challenge. Can exit to equivalent positions elsewhere. Net beneficiary of the extraction flow.
constraint_indexing:constraint_classification(role_capture_through_cost_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED COLLECTIVE ACTORS (TANGLED ROPE) — Professional associations, ethics boards, and labor unions see both the coordination function (role structures enable professional standards and collective bargaining) and the extraction mechanism (cost asymmetries suppress individual intervention). Have organized power to challenge the structure but face institutional resistance. Mobile across organizations but constrained within professional fields.
constraint_indexing:constraint_classification(role_capture_through_cost_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: WHISTLEBLOWER PROTECTION ADVOCATES (SCAFFOLD) — Organized actors building legal and institutional protections that reduce intervention costs. See the cost asymmetry as a temporary coordination failure with a sunset: as whistleblower protections mature, anonymous reporting channels expand, and retaliation becomes legally costly, the extraction mechanism loses force. Estimated sunset: 15-25 years for comprehensive legal frameworks to equalize intervention costs across organizational hierarchies.
constraint_indexing:constraint_classification(role_capture_through_cost_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CORPORATE ETHICS DEPARTMENTS (PITON) — Internal ethics functions are often performative: they exist to signal compliance and manage liability rather than to genuinely equalize intervention costs. The theater ratio is high because ethics hotlines, training programs, and reporting mechanisms rarely protect employees from retaliation in practice. The function persists through institutional inertia and regulatory requirement, not because it resolves the cost asymmetry.
constraint_indexing:constraint_classification(role_capture_through_cost_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a systems perspective, role-based cost asymmetries serve both coordination (enabling hierarchical decision-making and operational stability) and extraction (suppressing moral agency and enabling institutional complicity). The constraint is not a natural law — alternative organizational structures (flat hierarchies, rotating leadership, collective decision-making) demonstrate that the cost asymmetry is contingent. The analytical classification is tangled_rope because the structure genuinely coordinates while extracting, and both functions are empirically observable.
constraint_indexing:constraint_classification(role_capture_through_cost_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(role_capture_through_cost_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(role_capture_through_cost_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(role_capture_through_cost_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(role_capture_through_cost_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(role_capture_through_cost_asymmetry, TR),
    TR >= 0.70.

:- end_tests(role_capture_through_cost_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The cost asymmetry extracts moral agency from individuals while benefiting institutional continuity. The extraction is substantial but not maximal because some intervention does occur (whistleblowers exist, some organizations do protect them), and the coordination function is genuine (role structures do enable collective action). The value reflects that the career asymmetry is partly extractive rent-seeking (protecting leadership from accountability) and partly coordination overhead (preventing constant internal disruption). Suppression (0.62): High. Significant barriers to intervention include economic dependency, career vulnerability, retaliation risk, social isolation, and legal complexity. But suppression is not total — whistleblower protections exist (even if imperfectly enforced), some organizations have genuine ethics cultures, and collective action (unions, professional associations) can challenge the structure. Theater ratio (0.58): Moderate-high. Corporate ethics infrastructure is substantially performative: hotlines that don't protect reporters, training that doesn't change behavior, ethics officers without enforcement power. The theater has increased over the interval as legal compliance requirements have grown while actual protection mechanisms have not kept pace.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same cost asymmetry appears as pure extraction (snare) to trapped junior employees, mixed coordination-extraction (tangled rope) to constrained mid-level managers and analytical observers, beneficial coordination (rope) to senior leadership, temporary problem with sunset (scaffold) to whistleblower protection advocates, and degraded theater (piton) to corporate ethics departments. The gap is not about disagreement over facts — all agents can observe the cost differential — but about structural position. Junior employees experience maximum extraction because they have no exit and bear full intervention costs. Senior leadership experiences coordination because they benefit from the stability the asymmetry provides. The analytical observer sees both functions as real: the role structure genuinely enables coordination while simultaneously extracting moral agency. The perspectival gap reveals that 'is this extraction or coordination?' is not a question with a single answer — it depends on where you stand in the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Junior employees are victims with trapped exit options — the derivation chain produces high d (approaching 0.95) because they bear maximum extraction with no escape. Mid-level managers are victims with constrained exit — they experience extraction but have some agency, producing moderate-high d (around 0.65). Senior leadership are beneficiaries with arbitrage exit — they benefit from the stability mechanism and can move to equivalent positions elsewhere, producing low d (around 0.10). Organized actors (unions, ethics boards, whistleblower advocates) have mobile exit and mixed beneficiary/victim status — they benefit from professional role structures while challenging the cost asymmetry, producing moderate d (around 0.50). The analytical observer uses the canonical analytical d (0.73) because they are measuring the structure rather than experiencing it. No directionality overrides are needed — the structural derivation from beneficiary/victim declarations and exit options accurately captures each agent's relationship to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that tangled_rope is the correct analytical classification when a structure exhibits both genuine coordination and asymmetric extraction. The cost asymmetry is not pure extraction (snare) because role differentiation does enable collective action and organizational function — flat structures face real coordination costs. But it is also not pure coordination (rope) because the cost differential systematically suppresses intervention and enables complicity — the asymmetry extracts moral agency beyond what coordination requires. The tangled_rope classification captures this duality: the constraint coordinates (role structures enable efficient decision-making) AND extracts (cost asymmetries suppress moral agency). The mandatrophy question 'is this necessary coordination overhead or extractive rent-seeking?' has the answer 'both' — and the framework's job is to measure how much of each, from which perspectives, rather than forcing a binary choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_injury_quantification,
    'How do we quantify the long-term psychological cost of complicity (moral injury, identity damage) relative to the immediate economic cost of intervention?',
    'Longitudinal psychological studies of employees who intervened vs those who remained complicit; measurement of moral injury symptoms, career satisfaction, and identity coherence over biographical timescales',
    'If moral injury costs are high and persistent: the cost asymmetry is illusory — complicity is expensive, just delayed. If moral injury costs are low or recoverable: the asymmetry is real and the extraction mechanism is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_injury_quantification, empirical, 'Whether delayed moral injury costs offset immediate intervention costs').

omega_variable(
    retaliation_base_rate,
    'What is the actual base rate of retaliation against whistleblowers vs the perceived rate, and how does this gap affect intervention decisions?',
    'Systematic tracking of whistleblower outcomes across industries; comparison of actual retaliation rates with employee perception surveys; identification of availability bias and organizational narrative effects',
    'If perceived retaliation rate significantly exceeds actual rate: the suppression mechanism is partly cognitive (fear-based) rather than purely structural, suggesting identity_locked dynamics. If rates match: suppression is structural and the cost asymmetry is accurately perceived.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retaliation_base_rate, empirical, 'Gap between perceived and actual retaliation risk').

omega_variable(
    alternative_structure_viability,
    'Can organizations with equalized intervention costs (flat hierarchies, collective decision-making, rotating leadership) achieve comparable operational efficiency to hierarchical structures?',
    'Comparative organizational performance studies; measurement of decision speed, coordination costs, and operational outcomes across governance structures; identification of scale and domain dependencies',
    'If alternative structures are viable at scale: the cost asymmetry is extractive rather than necessary coordination overhead. If alternatives fail at scale: some degree of cost asymmetry may be inherent to complex coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_structure_viability, empirical, 'Whether alternative structures can match hierarchical efficiency').

omega_variable(
    legal_protection_effectiveness,
    'Do existing whistleblower protection laws actually reduce intervention costs, or do they merely add procedural theater without changing retaliation dynamics?',
    'Analysis of intervention rates and outcomes before/after whistleblower protection legislation; tracking of legal protection invocation vs actual protection received; identification of enforcement gaps',
    'If protections are effective: scaffold perspective confirmed — legal frameworks are building a genuine sunset. If protections are theatrical: the scaffold is aspirational and the piton perspective applies to the legal framework itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_protection_effectiveness, empirical, 'Whether legal protections reduce intervention costs in practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(role_capture_through_cost_asymmetry, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(role_cap_tr_t0, role_capture_through_cost_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(role_cap_tr_t10, role_capture_through_cost_asymmetry, theater_ratio, 10, 0.48).
narrative_ontology:measurement(role_cap_tr_t20, role_capture_through_cost_asymmetry, theater_ratio, 20, 0.58).
narrative_ontology:measurement(role_cap_tr_t30, role_capture_through_cost_asymmetry, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(role_cap_be_t0, role_capture_through_cost_asymmetry, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(role_cap_be_t10, role_capture_through_cost_asymmetry, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(role_cap_be_t20, role_capture_through_cost_asymmetry, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(role_cap_be_t30, role_capture_through_cost_asymmetry, base_extractiveness, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(role_capture_through_cost_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(role_capture_through_cost_asymmetry, institutional_capture_of_regulators).
narrative_ontology:affects_constraint(role_capture_through_cost_asymmetry, professional_identity_lock).
narrative_ontology:affects_constraint(role_capture_through_cost_asymmetry, organizational_omerta).

% DUAL FORMULATION NOTE:
% Role capture through cost asymmetry is a general mechanism that appears across multiple institutional contexts. It affects regulatory capture (regulators face career costs for challenging industry), professional identity lock (professionals internalize role-based cost structures as identity), and organizational omerta (collective silence norms emerge from individual cost asymmetries). Each of these is a distinct constraint with its own epsilon value, but all are downstream of or coupled to the cost asymmetry mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
