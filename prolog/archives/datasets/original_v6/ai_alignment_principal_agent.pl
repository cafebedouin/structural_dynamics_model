% ============================================================================
% CONSTRAINT STORY: ai_alignment_principal_agent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_principal_agent, []).

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
 *   constraint_id: ai_alignment_principal_agent
 *   human_readable: AI Alignment as Principal-Agent Constraint
 *   domain: artificial_intelligence/governance
 *
 * SUMMARY:
 *   The AI alignment constraint exemplifies a severe principal-agent problem
 *   at civilizational scale. The principals (humanity, regulatory bodies,
 *   future populations) cannot directly control the agents (AI developers and
 *   capability funders) and lack credible verification mechanisms to ensure
 *   the alignment of advanced AI systems with human values. The constraint
 *   exhibits the full taxonomy of DR types because different stakeholders
 *   experience the same structural misalignment fundamentally differently:
 *   for future populations it is a snare (trapped in existential risk); for
 *   developers it is a rope (coordination mechanism that enables deployment);
 *   for regulators it is an aspirational scaffold (building oversight
 *   structures with a sunset once systems are controllable); for current
 *   governance it is a degraded piton (safety theater that legitimizes
 *   deployment without constraining capability). The theater ratio (0.68)
 *   reflects that formal alignment verification and safety review processes
 *   operate largely at compliance margins while capability scaling proceeds
 *   unimpeded by meaningful safety constraints. Base extractiveness (0.58)
 *   captures the asymmetric benefit to developers: they capture capability
 *   gains and market value while diffusing the risks across humanity.
 *
 * KEY AGENTS:
 *   - Future Populations and Public Safety Interests: Primary victim (powerless/trapped) — cannot verify alignment claims or withdraw consent from deployment; bear civilizational-scale risks
 *   - AI Developers and Capability Funders: Primary beneficiary (institutional/arbitrage) — capture market value and capability advantages; can arbitrage safety requirements across jurisdictions or reputational channels
 *   - Alignment Research Community: Secondary victim/trapped in coordination (moderate/constrained) — depend on developer funding while trying to constrain developer actions; become legitimizing tools
 *   - Regulatory and Governance Coalition: Organized coalition (organized/constrained) — attempting to build safety-first governance with real constraints but facing developer mobility and geopolitical competition
 *   - Powerful States and Geopolitical Competitors: Institutional actors (powerful/mobile) — see alignment constraint as both coordination (preventing mutual destruction) and extraction (legitimizing differential access to capability)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — identifies structural information asymmetry as the core binding mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_principal_agent, 0.58).
domain_priors:suppression_score(ai_alignment_principal_agent, 0.65).
domain_priors:theater_ratio(ai_alignment_principal_agent, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_principal_agent, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_alignment_principal_agent, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_alignment_principal_agent, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_principal_agent, tangled_rope).
narrative_ontology:human_readable(ai_alignment_principal_agent, "AI Alignment as Principal-Agent Constraint").
narrative_ontology:topic_domain(ai_alignment_principal_agent, "artificial_intelligence/governance").

domain_priors:requires_active_enforcement(ai_alignment_principal_agent).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_principal_agent, ai_developers).
narrative_ontology:constraint_beneficiary(ai_alignment_principal_agent, capability_funders).
narrative_ontology:constraint_victim(ai_alignment_principal_agent, public_safety_interests).
narrative_ontology:constraint_victim(ai_alignment_principal_agent, long_term_human_flourishing).
narrative_ontology:constraint_victim(ai_alignment_principal_agent, alignment_researchers_as_field).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE HUMANITY (SNARE) — No exit option; bears the full asymmetric risk. Cannot credibly enforce safety standards on systems they do not control. The 'principal' (humanity) cannot effectively monitor or constrain the 'agent' (AI developers and corporations) without shutting down the development entirely. Trapped in maximum existential risk during the development window.
constraint_indexing:constraint_classification(ai_alignment_principal_agent, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALIGNMENT RESEARCH COMMUNITY (TANGLED ROPE) — Constrained by funding dependence on the same actors whose incentives misalign with safety. Benefits from the research problem itself (career, attention, resources) while simultaneously trying to solve the constraint that generates their work. High suppression through funding gatekeeping; genuine coordination function in developing safety methods. Asymmetric extraction as safety researchers become tools legitimizing deployment rather than gatekeepers.
constraint_indexing:constraint_classification(ai_alignment_principal_agent, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: AI DEVELOPERS / CAPABILITY FUNDERS (ROPE) — See the constraint as coordination: safety alignment processes enable them to deploy systems and capture market value while managing regulatory and reputational risk. The constraint solves a collective action problem (all developers benefit from shared safety standards) while creating extraction opportunity (safety becomes a compliance checkbox rather than a genuine constraint). Maximum benefit from the arbitrage position — can exit the alignment requirement by regulatory arbitrage, geopolitical competition, or reputational management.
constraint_indexing:constraint_classification(ai_alignment_principal_agent, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AND GOVERNANCE COALITION (SCAFFOLD) — Organized agents (governments, international bodies, civil society coalitions) attempting to build safety-first governance structures with sunset logic: the goal is to establish alignment norms and verification mechanisms robust enough that they persist after the 'dangerous scaling' phase ends. Theater is high but declining as genuine oversight mechanisms develop. Sunset scenario: AI systems become stable and controllable within 15-30 years, rendering alignment constraints unnecessary.
constraint_indexing:constraint_classification(ai_alignment_principal_agent, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CURRENT GOVERNANCE AND COMPLIANCE THEATER (PITON) — Existing regulatory and safety frameworks (AI safety reviews, ethics boards, safety committees within labs) are largely performative from the developer perspective. The rituals persist through institutional inertia and reputation management, not because they effectively constrain capability development. Theater ratio 0.68 reflects that formal safety review processes often operate at decision margins (e.g., which prompt to use) rather than at core capability scaling decisions. Degraded because the primary function (ensuring alignment) has atrophied relative to the secondary function (managing liability and stakeholder legitimacy).
constraint_indexing:constraint_classification(ai_alignment_principal_agent, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: POWERFUL STATES AND GEOPOLITICAL ACTORS (TANGLED ROPE) — See the constraint as a hybrid: coordination function (preventing mutual destruction through unconstrained competition in AI capability) exists alongside extraction (using safety concerns as cover for restricting competitor access to capability development). High mobility and power but also genuine interest in preventing catastrophic misalignment. The constraint both coordinates (reduces risk of mutually destructive arms race) and extracts (legitimizes differential access to advanced capabilities).
constraint_indexing:constraint_classification(ai_alignment_principal_agent, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a universal/civilizational viewpoint analyzing the deep structure: the principal-agent problem is irreducible to the information asymmetry between public principals and private agent developers. The principals cannot observe the true alignment state of proprietary systems. The agents have incentives to overstate alignment while advancing capabilities. No external observer can verify claims credibly because verification requires internal access the developers control. This analysis classifies as snare (not mountain) because the asymmetry is structurally contingent — different governance architectures (mandatory open-source, external auditing, international verification) could resolve it.
constraint_indexing:constraint_classification(ai_alignment_principal_agent, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_principal_agent_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_alignment_principal_agent, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_alignment_principal_agent, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_principal_agent, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_alignment_principal_agent, TR),
    TR >= 0.70.

:- end_tests(ai_alignment_principal_agent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.58: This value reflects the asymmetric distribution of capability benefits and catastrophic risks. Developers capture economic value, career advancement, and technology leadership during the development window. Future populations and safety interests bear the existential risk. The 0.58 figure is higher than earlier development phases because as capability advances, the asymmetry becomes more severe — systems become harder to constrain and the downside risk increases exponentially. Suppression 0.65: Moderate-high suppression through multiple mechanisms: funding dependence (alignment researchers depend on developers for resources), information asymmetry (principals cannot audit proprietary systems), geopolitical incentives (states race to develop capabilities), and epistemic suppression (the true alignment state of systems is fundamentally unknowable without internal access). Theater ratio 0.68: Significant performative component. Formal AI safety review processes, ethics boards, and alignment research operate largely within parameters set by developers. Theater has increased over the measurement interval because as stakes have risen, the cosmetic legitimacy of safety processes has become more valuable to developers while the actual constraints on capability scaling have not increased proportionally. The theater ratio rising from 0.45 to 0.68 is diagnostic of Goodhart drift — the safety metric (alignment claims) has become the objective, divorced from the underlying goal (actual alignment).
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces the broadest perspectival gap in the corpus. Future humanity sees an irreversible snare (trapped in risk from a system they cannot control or exit). Developers see pure coordination (the alignment constraint enables deployment and manages regulatory risk). Alignment researchers see tangled_rope (genuine safety function + extraction of their labor for legitimacy). Regulators see a temporary scaffold (building oversight with a sunset). Current governance sees a degraded piton (safety theater that persists through inertia). Powerful states see tangled_rope coordination-and-extraction hybrid (genuinely wants to prevent mutual destruction AND wants to constrain competitors). The analytical observer recognizes that the perspectival gap itself is the constraint: information asymmetry prevents any perspective from verifying claims about the system's true alignment. This gap is not an observation bias — it is structural.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the classification varies systematically with structural position: (1) Principals with maximum information deficit see snare; (2) Agents with information privilege see rope; (3) Researchers caught between both see tangled_rope; (4) Regulators building external accountability see scaffold; (5) Current governance systems see piton. The mandatrophy question 'Is this coordination or extraction?' is answered by: 'both, depending on whether you are benefiting from the asymmetry.' For developers, the constraint genuinely solves a coordination problem (preventing chaotic competition in capability development). For principals, the same constraint is pure extraction (they bear risks they cannot monitor). The resolution is not to pick one type but to recognize that the distributional asymmetry IS the constraint. In a world with symmetrical information (principals could verify alignment), the constraint would collapse to rope. In a world with no information gap but opposing interests (principals directly controlled capability development), it would be a symmetric snare. The current structure — information asymmetry combined with structural divergence of interests — produces the taxonomy we observe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alignment_measurability,
    'Can alignment be measured objectively, or does measurement depend on the measurer''s control of the system?',
    'Development of external auditing protocols for AI system alignment; independent verification of safety properties; comparison of internal safety claims vs external audit findings',
    'If measurable externally: principal-agent gap can be bridged. If not: snare classification is structural — principals fundamentally cannot verify agent claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alignment_measurability, empirical, 'Whether alignment is externally verifiable').

omega_variable(
    capability_containment_feasibility,
    'Can AI capability development be meaningfully slowed or constrained by safety requirements without driving development underground or to less scrupulous actors?',
    'Historical analysis of compliance with international technology restrictions (nuclear, biotech, dual-use); game-theoretic modeling of defection incentives under different governance regimes',
    'If containment is feasible: scaffold/tangled_rope classifications hold. If not: snare classification becomes more severe — constraint only redistributes risk between actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_containment_feasibility, empirical, 'Whether meaningful capability constraints are sustainably enforceable').

omega_variable(
    alignment_incentive_alignment,
    'Are there genuine economic or strategic incentives for developers to achieve true alignment, or is alignment only profitable as theater and compliance?',
    'Analysis of developer investment in safety relative to capability scaling; market incentives for safety-first systems; study of whether safety innovations are monetized or suppressed',
    'If incentives exist: rope/coordination dominates. If not: tangled_rope or snare dominates — alignment constraint is extraction mechanism disguised as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_incentive_alignment, empirical, 'Whether developer incentives genuinely align with safety outcomes').

omega_variable(
    geopolitical_race_dynamics,
    'Does international competition in AI capability override coordination incentives for safety alignment?',
    'Analysis of safety compliance in periods of high geopolitical tension vs cooperation; comparison of alignment rigor across jurisdictions with different competitive positions',
    'If race dynamics dominate: snare and scaffold perspectives become more severe. Powerful state actors may use safety concerns as cover while accelerating capability. If coordination holds: tangled_rope analysis accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_race_dynamics, empirical, 'Whether geopolitical competition overrides safety coordination').

omega_variable(
    hidden_misalignment,
    'Can systems be genuinely misaligned while appearing aligned to their developers?',
    'Study of cases where systems exhibited unexpected behaviors; analysis of adversarial examples in safety testing; longitudinal tracking of emergent capabilities that violate safety assumptions',
    'If hidden misalignment is possible: snare classification for principals is structural. Developers cannot ensure alignment even with best intentions. Information asymmetry is fundamental to the technology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hidden_misalignment, empirical, 'Whether misalignment can be hidden from developers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_principal_agent, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aialign_tr_t0, ai_alignment_principal_agent, theater_ratio, 0, 0.45).
narrative_ontology:measurement(aialign_tr_t3, ai_alignment_principal_agent, theater_ratio, 3, 0.58).
narrative_ontology:measurement(aialign_tr_t6, ai_alignment_principal_agent, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(aialign_be_t0, ai_alignment_principal_agent, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(aialign_be_t3, ai_alignment_principal_agent, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(aialign_be_t6, ai_alignment_principal_agent, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_principal_agent, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_principal_agent, 0.12).
narrative_ontology:affects_constraint(ai_alignment_principal_agent, ai_capability_measurement).
narrative_ontology:affects_constraint(ai_alignment_principal_agent, corporate_governance_externality).
narrative_ontology:affects_constraint(ai_alignment_principal_agent, geopolitical_technology_competition).

% DUAL FORMULATION NOTE:
% AI alignment decomposes into multiple structurally distinct constraints: (1) Technical alignment (making systems behave as intended) with lower extractiveness; (2) Governance alignment (ensuring systems behave as society intends) with higher extractiveness and higher information asymmetry; (3) Incentive alignment (ensuring developers prioritize safety over capability) with the highest extractiveness. This story focuses on the principal-agent constraint at the governance level where information asymmetry is maximum and extraction potential is highest. Technical alignment stories would have lower ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_principal_agent, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
