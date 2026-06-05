% ============================================================================
% CONSTRAINT STORY: responsibility_without_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_responsibility_without_power, []).

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
 *   constraint_id: responsibility_without_power
 *   human_readable: The Scapegoat Architecture: Responsibility Without Power
 *   domain: organizational/legal/socio-economic
 *
 * SUMMARY:
 *   The scapegoat architecture describes a structural pattern where legal,
 *   moral, or professional responsibility for an outcome is assigned to an
 *   agent who lacks the actual power or tools to control that outcome. This
 *   constraint appears in: middle-management accountability for executive
 *   strategy failures; compliance officers held liable for systemic design
 *   flaws; algorithm operators responsible for model biases embedded by
 *   engineers; supply chain workers accountable for buyer-imposed margins;
 *   junior traders blamed for systemic risk inherent in bank structure. The
 *   constraint functions as a 'Rope' coordination mechanism (it enables
 *   upstream decision-makers to act decisively without diffuse
 *   accountability) while simultaneously operating as a 'Snare' extraction
 *   mechanism (it transfers liability cost from those who control outcomes to
 *   those who implement them). The core tension: the same structure that
 *   enables efficient decision-making at the top creates catastrophic
 *   accountability asymmetries at the implementation layer. The
 *   extractiveness has increased over the measurement interval (0.35 → 0.52)
 *   as organizations have become more complex and power has concentrated
 *   further from implementation, while theater (performative compliance) has
 *   risen (0.45 → 0.65) as formal accountability mechanisms increasingly
 *   substitute ritual for real control.
 *
 * KEY AGENTS:
 *   - Designated Responsible Agent: Primary victim (powerless/trapped) — bears formal liability for outcomes outside their control; faces termination, prosecution, or career destruction
 *   - Upstream Decision Maker: Primary beneficiary (institutional/arbitrage) — retains control and decision-making power while liability is transferred downward; captures efficiency gains
 *   - Regulatory/Legal System: Secondary beneficiary (institutional/constrained) — serves coordination function but also extracts rent through compliance costs, litigation, settlement overhead
 *   - System Accountability Commons: Secondary victim (powerless/trapped) — abstract collective good cannot organize; bears cost of false attribution that obscures real system failures
 *   - Compliance Theater Infrastructure: Institutional degraded actor (institutional/constrained) — maintains performative mechanisms (training, audits, certifications) that signal accountability but rarely prevent failures (Piton)
 *   - Distributed Accountability Movement: Organized reform agent (organized/mobile) — labor unions, algorithmic auditing, supply chain transparency, workplace power-building create alternative pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(responsibility_without_power, 0.52).
domain_priors:suppression_score(responsibility_without_power, 0.68).
domain_priors:theater_ratio(responsibility_without_power, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(responsibility_without_power, extractiveness, 0.52).
narrative_ontology:constraint_metric(responsibility_without_power, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(responsibility_without_power, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(responsibility_without_power, tangled_rope).
narrative_ontology:human_readable(responsibility_without_power, "The Scapegoat Architecture: Responsibility Without Power").
narrative_ontology:topic_domain(responsibility_without_power, "organizational/legal/socio-economic").

domain_priors:requires_active_enforcement(responsibility_without_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(responsibility_without_power, upstream_decision_makers).
narrative_ontology:constraint_beneficiary(responsibility_without_power, liability_shifting_institutions).
narrative_ontology:constraint_victim(responsibility_without_power, designated_responsible_agent).
narrative_ontology:constraint_victim(responsibility_without_power, system_accountability_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DESIGNATED RESPONSIBLE AGENT (SNARE) — The middle manager, compliance officer, or algorithmic system operator bears legal/moral responsibility for outcomes they cannot control. Trapped by employment contract, regulatory mandate, or architectural role. Cannot exit without catastrophic career/legal consequence. High suppression: formal enforcement mechanisms (liability, termination, prosecution) ensure compliance regardless of actual control. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(responsibility_without_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UPSTREAM DECISION MAKER (ROPE) — C-suite executives, algorithm designers, or board members retain actual control over system design and decision-making but face minimal personal liability. The responsibility transfer constraint functions for them as pure coordination: it enables efficient decision-making by shifting accountability to implementers. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(responsibility_without_power, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY/LEGAL SYSTEM (TANGLED ROPE) — Courts, regulatory agencies, and legal frameworks serve a genuine coordination function (defining responsibility boundaries, enabling liability claims) while simultaneously extracting rent through: high litigation costs, settlement premiums, regulatory compliance overhead, and institutional power consolidation. The system both clarifies accountability AND obscures who actually controls outcomes. d≈0.55, f(d)≈0.72, σ=1.0 → χ≈0.38.
constraint_indexing:constraint_classification(responsibility_without_power, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SYSTEM ACCOUNTABILITY COMMONS (SNARE) — The abstract collective interest in true accountability and system reliability bears the full cost of scapegoating architecture. When responsibility is assigned to powerless agents, actual causes (structural design failures, resource constraints, systemic incentives) are obscured rather than fixed. The accountability commons cannot organize, cannot exit, and absorbs the cost of false attribution. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.74.
constraint_indexing:constraint_classification(responsibility_without_power, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPLIANCE THEATER INFRASTRUCTURE (PITON) — Training programs, audit requirements, certification systems, and internal compliance mechanisms persist largely as performative ritual. They signal accountability to stakeholders while rarely preventing actual failures or identifying true causes. theater_ratio=0.65 reflects that compliance overhead is substantial but the constraint's degraded function (catching actual system failures vs. performing organizational virtue) is masked by institutional inertia. The infrastructure is maintained because alternatives haven't replaced it, not because it works.
constraint_indexing:constraint_classification(responsibility_without_power, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DISTRIBUTED ACCOUNTABILITY MOVEMENT (SCAFFOLD) — Emerging frameworks (algorithmic auditing, supply chain transparency, workplace power-building) attempt to redistribute responsibility to those with actual control. Labor organizing, open-source accountability, and regulatory reform create exit pathways from scapegoat architecture. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.18. Low effective extraction because organized agents have agency and visibility of alternatives.
constraint_indexing:constraint_classification(responsibility_without_power, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(responsibility_without_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(responsibility_without_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(responsibility_without_power, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(responsibility_without_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(responsibility_without_power, TR),
    TR >= 0.70.

:- end_tests(responsibility_without_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant value from the designated responsible agent through career risk, legal liability exposure, and constant pressure to compensate for structural failures beyond their control. However, extraction is not maximal (≥0.66) because the agent typically receives some compensatory authority within their delegated sphere, and some coordination benefits (faster decision-making, clearer accountability boundaries) are genuine. The increase from 0.35 to 0.52 over the interval reflects rising organizational complexity (more layers between decision and implementation) and power concentration (fewer executives controlling more outcomes). Suppression (0.68): High. Formal enforcement mechanisms (employment law, liability standards, regulatory penalties, criminal prosecution) ensure compliance regardless of whether the agent can actually control the outcome. The agent cannot negotiate the responsibility assignment and faces severe consequences for non-compliance or whistleblowing. Theater ratio (0.65): Moderate-high. Compliance infrastructure (training, audits, certifications, risk assessments) occupies substantial institutional resources but often functions performatively: it signals accountability and risk mitigation to external stakeholders while failing to prevent actual failures or identify true causes. The increase from 0.45 to 0.65 reflects growing compliance bureaucracy without corresponding improvement in actual accountability.
 *
 * PERSPECTIVAL GAP:
 *   The designated responsible agent (Snare) experiences the constraint as pure extraction: they bear consequences for outcomes controlled by others, with no exit and no mitigation. The upstream decision-maker (Rope) experiences it as pure coordination: the constraint enables efficient decision-making by centralizing power while distributing accountability. The regulatory system (Tangled Rope) genuinely serves both functions simultaneously: it clarifies responsibility boundaries (coordination) while extracting rent through complexity and cost (extraction). The compliance infrastructure (Piton) maintains the appearance of accountability (ritual) while the actual function (preventing failures) has atrophied — the infrastructure persists through institutional inertia. The accountability commons (Snare) bears the full cost of scapegoating: when responsibility is assigned to powerless agents, actual causes are obscured rather than fixed, degrading system reliability. The distributed accountability movement (Scaffold) sees a temporary problem with a sunset: alternative frameworks (worker power-building, algorithmic transparency, supply chain auditing) are creating exit pathways from scapegoat architecture.
 *
 * DIRECTIONALITY LOGIC:
 *   Designated responsible agent: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — agent cannot exit without catastrophic consequence. Upstream decision-maker: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary — retains control and captures efficiency gains. Regulatory/legal system: Victim + constrained (moderate position within enforcement) → d≈0.55, f(d)≈0.72. Mixed experience: enforces coordination function but also creates extraction via complexity and cost. System accountability commons: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — cannot organize or exit; absorbs cost of false attribution. Distributed accountability movement: Organized + mobile → d≈0.35, f(d)≈0.35. Low effective extraction — organized agents have agency and see alternative pathways.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the scapegoat architecture IS a legitimate coordination mechanism (it enables decision-making) WHILE BEING an extraction mechanism (it transfers liability downward). The resolution is not 'which is it?' but 'what coordination benefits could be retained while eliminating the extraction?'. Empirical investigation (omega 1: causal attribution boundary) determines whether responsibility assignments are legitimate accountability or pure scapegoating. For assignments where the responsible agent controlled >70% of relevant veto points, the responsibility is legitimate and the constraint is primarily Tangled Rope (mixed coordination + extraction). For assignments where the agent controlled <30%, the responsibility is pure scapegoating and the constraint is primarily Snare. Reform pathway viability (omega 3) determines whether alternatives can replace scapegoat architecture without collapsing coordination. If viable: the scaffold perspective's sunset is realistic. If not: the architecture is baked into complex organizations and can only be resolved through deeper system redesign. Intentionality analysis (omega 2) determines whether the structure was deliberately designed or emerged from institutional drift — deliberate design suggests architectural extraction; drift suggests Piton classification (degraded Rope). The analytical observer risks naturalizing scapegoat architecture as inherent to organizational hierarchies (false mountain). The structural data reveals it as a contingent institutional choice: it could be reformed by matching responsibility to control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_attribution_boundary,
    'At what point does assigning responsibility to a powerless agent cease to be a legitimate accountability mechanism and become pure extraction?',
    'Causal analysis framework: identify the decision tree and veto points where the designated agent could have prevented the outcome. If agent controlled >70% of relevant veto points, responsibility assignment is legitimate. If <30%, it is scapegoating.',
    'If most assignments cross the scapegoat threshold: constraint is primarily Snare for victims. If most assignments retain legitimate accountability: constraint is primarily Tangled Rope (mixed coordination + extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_boundary, empirical, 'Causal attribution boundary between legitimate accountability and scapegoating').

omega_variable(
    power_asymmetry_intentionality,
    'Is the responsibility-without-power structure deliberately designed as an extraction mechanism, or does it emerge from institutional complexity and unintended consequences?',
    'Institutional archaeology: trace the historical development of responsibility rules. Were they designed with explicit power transfers or did power drift upward while responsibility remained fixed? Interview institutional actors about intentionality.',
    'If deliberately designed: constraint is architecturally a Snare. If emergent: constraint is a Rope that degraded into scapegoat function over time (Piton). Classification impact: deliberate design lowers confidence in ''natural law'' framing; emergent drift suggests reform tractability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(power_asymmetry_intentionality, conceptual, 'Whether responsibility-without-power structure is deliberate design or institutional drift').

omega_variable(
    reform_pathway_viability,
    'Can responsibility assignments be restructured to match actual control without collapsing the coordination function that justifies the constraint?',
    'Comparative case analysis: examine organizations that have successfully redistributed responsibility. Identify: (a) what coordination benefits were retained, (b) what extraction mechanisms were eliminated, (c) what new implementation costs emerged.',
    'If viable: scaffold perspective is realistic and sunset is achievable. If not viable: scapegoat structure is baked into the coordination mechanism and can only be resolved through system redesign, not reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_pathway_viability, empirical, 'Viability of restructuring responsibility to match actual control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(responsibility_without_power, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(resp_power_tr_t0, responsibility_without_power, theater_ratio, 0, 0.45).
narrative_ontology:measurement(resp_power_tr_t5, responsibility_without_power, theater_ratio, 5, 0.55).
narrative_ontology:measurement(resp_power_tr_t10, responsibility_without_power, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(resp_power_be_t0, responsibility_without_power, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(resp_power_be_t5, responsibility_without_power, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(resp_power_be_t10, responsibility_without_power, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(responsibility_without_power, enforcement_mechanism).
narrative_ontology:affects_constraint(responsibility_without_power, principal_agent_moral_hazard).
narrative_ontology:affects_constraint(responsibility_without_power, organizational_power_concentration).
narrative_ontology:affects_constraint(responsibility_without_power, compliance_theater_degradation).

% DUAL FORMULATION NOTE:
% The scapegoat architecture is downstream of two distinct structural constraints: (1) organizational power concentration (upstream decision-makers accumulating control), and (2) enforcement mechanism design (formal liability rules that allocate responsibility). These have different ε values and structural properties. The scapegoat architecture emerges from their interaction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(responsibility_without_power, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
