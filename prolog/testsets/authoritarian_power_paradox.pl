% ============================================================================
% CONSTRAINT STORY: authoritarian_power_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_authoritarian_power_paradox, []).

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
 *   constraint_id: authoritarian_power_paradox
 *   human_readable: The Paradox of Authoritarian Centralization
 *   domain: political/social/technological
 *
 * SUMMARY:
 *   The paradox of authoritarian centralization describes a structural trap
 *   in which an institution or leader centralizes power to achieve stability
 *   and efficiency, but the very mechanisms of centralization (elimination of
 *   internal opposition, suppression of criticism, control of information
 *   flows) disable the diagnostic feedback loops required to perceive threats
 *   to the system. The constraint exhibits the paradox: centralization claims
 *   to solve coordination problems through unified command, but it
 *   simultaneously creates extraction mechanisms (suppression of peripheral
 *   voices, control of information channels, enforcement apparatus) and
 *   destroys the capacity to detect existential threats. The central
 *   authority experiences the system as pure coordination (stable hierarchy,
 *   unified command, rapid resource mobilization). Peripheral subjects
 *   experience it as pure extraction (forced compliance, information control,
 *   surveillance). The analytical observer risks naturalizing this as an
 *   immutable law of centralized systems, but the structural data reveals it
 *   as a choice point: centralization does NOT inherently require suppression
 *   of diagnostic information; the paradox emerges when suppression is chosen
 *   as the instrument of control. The constraint's theater ratio (0.68)
 *   reflects the security apparatus's performative character: reporting
 *   positive findings (threats detected, security maintained) justifies
 *   budget expansion and demonstrates competence, while actual
 *   threat-detection capacity atrophies because dissent and information flow
 *   are suppressed.
 *
 * KEY AGENTS:
 *   - Central Authority: Primary beneficiary (institutional/arbitrage) — captures efficiency gains from unified command, reduced internal opposition, rapid resource mobilization
 *   - Peripheral Subjects: Primary victim (powerless/trapped) — forced compliance, information control, no exit option, maximum extraction
 *   - Security Apparatus: Secondary beneficiary (institutional/arbitrage) — budget expansion, jurisdiction expansion, justification through positive threat-detection findings
 *   - Diagnostic Capacity: Primary victim (powerless/trapped) — suppression of dissent eliminates early-warning signals, reduces threat-detection capability, creates blindness to destabilization
 *   - Intermediate Functionaries: Mixed victim/beneficiary (moderate/constrained) — access to state resources and hierarchical stability, but constrained communication of threats upward
 *   - International Community: Secondary victim (organized/constrained) — constrained by geopolitical dependency, cannot fully exit, experiences mixed coordination/extraction
 *   - Underground Resistance: Secondary actor (organized/mobile) — perceives the constraint as temporary, maintains alternative information networks, exit pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(authoritarian_power_paradox, 0.58).
domain_priors:suppression_score(authoritarian_power_paradox, 0.72).
domain_priors:theater_ratio(authoritarian_power_paradox, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(authoritarian_power_paradox, extractiveness, 0.58).
narrative_ontology:constraint_metric(authoritarian_power_paradox, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(authoritarian_power_paradox, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(authoritarian_power_paradox, tangled_rope).
narrative_ontology:human_readable(authoritarian_power_paradox, "The Paradox of Authoritarian Centralization").
narrative_ontology:topic_domain(authoritarian_power_paradox, "political/social/technological").

domain_priors:requires_active_enforcement(authoritarian_power_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(authoritarian_power_paradox, central_authority).
narrative_ontology:constraint_beneficiary(authoritarian_power_paradox, security_apparatus).
narrative_ontology:constraint_victim(authoritarian_power_paradox, peripheral_agents).
narrative_ontology:constraint_victim(authoritarian_power_paradox, diagnostic_capacity).
narrative_ontology:constraint_victim(authoritarian_power_paradox, system_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL SUBJECT (SNARE) — Trapped within the centralized system with no exit option. Information channels are monitored; dissent is suppressed; exit requires abandonment of social/economic ties. Experiences maximum extraction: forced compliance, information control, surveillance apparatus directed inward. No alternative coordination mechanism available.
constraint_indexing:constraint_classification(authoritarian_power_paradox, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERMEDIATE FUNCTIONARY (TANGLED ROPE) — Constrained by career dependence on the central authority but also benefits from coordination function (stable hierarchies, predictable rules, access to state resources). Experiences mixed coordination and extraction: stability and order versus suppression of dissent and information control. Can perceive threats but cannot safely communicate them upward.
constraint_indexing:constraint_classification(authoritarian_power_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL AUTHORITY (ROPE) — Benefits from coordination function: unified command, reduced internal opposition, capacity to mobilize resources. Experiences the constraint as pure coordination (from its structural position). High exit flexibility — can reshape institutions, redistribute power. Net beneficiary. Extraction runs toward this agent, not away.
constraint_indexing:constraint_classification(authoritarian_power_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SURVEILLANCE THEATER (PITON) — The security apparatus (secret police, informant networks, monitoring systems) is ostensibly designed to detect threats but increasingly performs theater: reporting positive findings confirms competence and justifies budget expansion, while actual threat-detection capacity atrophies. The apparatus persists through institutional inertia and resource capture, not functional effectiveness. Theater ratio exceeds function.
constraint_indexing:constraint_classification(authoritarian_power_paradox, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: UNDERGROUND RESISTANCE (SCAFFOLD) — Organized agents (diaspora networks, exile communities, international opposition) see the bottleneck as a temporary constraint with a sunset: censorship technologies degrade over time, information flows are inevitable, generational cohort turnover enables exit. Low effective extraction because this agent has agency and perceives alternative pathways. Constraints on information exchange are real but temporary.
constraint_indexing:constraint_classification(authoritarian_power_paradox, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL COMMUNITY (TANGLED ROPE) — Organized external actors (other states, international organizations, multinational firms) experience mixed coordination and extraction. The authoritarian regime provides stability and predictable rules (coordination benefit) but also requires alignment with regime interests, sanctions vulnerability, and limited transparency (extraction cost). Constrained by geopolitical dependency and economic ties; cannot simply exit.
constraint_indexing:constraint_classification(authoritarian_power_paradox, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — The civilizational/universal analytical perspective risks naturalizing the paradox as an immutable law: 'all centralized power requires suppression of information to prevent delegation; therefore, centralization necessarily blinds itself to threats; therefore, rigid hierarchies inevitably decay.' This framing treats a contingent institutional choice (suppression of diagnostic information) as a structural necessity. The engine's false summit detector will flag this as naturalization of what is actually a choice point.
constraint_indexing:constraint_classification(authoritarian_power_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(authoritarian_power_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(authoritarian_power_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(authoritarian_power_paradox, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(authoritarian_power_paradox, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(authoritarian_power_paradox, TR),
    TR >= 0.70.

:- end_tests(authoritarian_power_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The central authority captures genuine coordination benefits from unified command and reduced internal opposition. However, the extraction mechanism is substantial: suppression of dissent, information control, forced compliance. The value reflects that the extraction is significant but not total — much of the apparent extraction is justified as efficiency gains, and some peripheral agents benefit from the stability provided by hierarchical order. The rising trajectory (0.35 → 0.58 over 30 years) reflects the accumulation of suppression and the gradual atrophy of diagnostic capacity; as information suppression becomes more sophisticated, the extraction mechanism becomes more efficient and harder to challenge. Suppression (0.72): High. Extensive barriers prevent internal dissent, information flow, and exit: active surveillance, censorship, punishment of criticism, restricted movement, controlled media. Suppression is not total but is substantial enough to prevent organized internal opposition. Theater ratio (0.68): High and rising. The security apparatus (secret police, informant networks, monitoring systems) generates positive findings and threat reports that justify its existence and budget, independent of actual threat magnitude. The surveillance theater has increased over the interval as information suppression has become more sophisticated; the apparatus maintains itself through reportage of threats rather than through actual threat prevention. Claimed type: Tangled Rope. The constraint combines genuine coordination (hierarchical efficiency, unified command) with asymmetric extraction (suppression of peripheral voices, information control, forced compliance). Active enforcement is required to maintain suppression.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the central authority's rope perspective and the peripheral subject's snare perspective is maximum. They observe identical institutional structures but experience them with opposite sign: one gains efficiency and stability; the other loses voice and exit. The gap reflects fundamental structural asymmetry in the extraction flow. The intermediate functionary's tangled-rope perspective bridges the gap — they experience both coordination benefits and extraction costs, demonstrating that the hybrid classification accurately captures the mixed experience. The international community's constrained exit produces a different gap: they have exit options (trade diversion, diplomatic pressure, capital flight) that peripheral subjects lack, making their experience tangled rope rather than snare. The underground resistance's scaffold perspective reveals that the constraint's 'permanence' is not structural but temporal — younger cohorts and diaspora networks perceive exit pathways that the suppression apparatus tries to close. The analytical observer's false-summit mountain perspective is the most dangerous gap: it risks naturalizing the paradox as immutable, preventing recognition that suppression is a choice, not a necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) derives from their structural position relative to the extraction flow. The central authority benefits from suppression (d ≈ 0.05: low d, low/negative chi, experiences rope/coordination). Peripheral subjects bear extraction costs and have no exit (d ≈ 0.95: high d, high chi, experiences snare). Intermediate functionaries are partly beneficiary (access to state resources) and partly victim (constrained communication); their d is moderate (~0.50-0.65), producing tangled-rope classification. The security apparatus benefits from suppression expansion (low d, rope perspective) but also experiences extraction from resource constraints and performance pressure (moderate d, constrained exit). The international community has constrained exit (dependency on trade, geopolitical ties), making them neither full beneficiary nor full victim (d ≈ 0.50-0.60, tangled-rope perspective). The underground resistance has exit options through diaspora and digital technologies (d ≈ 0.30-0.40, mobile exit), producing scaffold classification despite suppression because they perceive the constraint as time-bounded.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION OF MANDATROPHY: The tangled-rope classification resolves the mandatrophy by showing that centralization does provide genuine coordination benefits (unified command, reduced negotiation overhead, rapid resource mobilization) AND asymmetric extraction (suppression of peripheral voices, information control, forced compliance). The paradox is not whether centralization works for coordination — it does. The paradox is whether the cost of suppression (elimination of diagnostic feedback) exceeds the coordination benefits. The constraint demonstrates that this cost rises over time: as suppression becomes more sophisticated (theater_ratio rising from 0.42 to 0.68), the diagnostic capacity atrophies, and the system's ability to detect threats declines. The central authority experiences rope (coordination) because they capture the coordination benefits without bearing the suppression costs. Peripheral subjects experience snare (extraction) because they bear the suppression costs without capturing coordination benefits. The mandatrophy is resolved by recognizing that the classification depends entirely on the agent's structural position relative to the extraction flow. There is no single 'true' type — the presheaf over observation positions IS the answer. The false-summit analytical perspective reveals a critical insight: the paradox is not universal law but institutional choice. Centralization does not logically require suppression of diagnostic information; the paradox emerges when suppression is chosen as the control mechanism. Alternative institutional designs (federated command, transparent hierarchies, protected dissent channels) could maintain coordination benefits while preserving diagnostic capacity. The constraint is therefore not a mountain but a tangled rope: it requires active enforcement of suppression to maintain the paradox. If enforcement fails or alternatives emerge (digital technologies enabling encrypted communication, generational cohort turnover, international pressure), the constraint's structure transforms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_detection_capacity_loss,
    'At what point does suppression of internal diagnostic information reduce the central authority''s capacity to detect genuine threats below the level where the authority can maintain control?',
    'Comparative analysis of collapse timelines across regimes; correlation between suppression intensity and threat-detection lag; measurement of systemic crisis surprise factors',
    'If threshold is < 20 years: many regimes will cross it before collapse, enabling predictive stability analysis. If threshold is > 50 years: regimes can persist in blind stability for multiple generations. If threshold is regime-dependent: no universal law; contingent on institutional design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_detection_capacity_loss, empirical, 'Threshold for suppression reducing threat detection below control capacity').

omega_variable(
    benign_feedback_loop_possibility,
    'Can centralization establish benign feedback loops where limited diagnostic information is sufficient because the system is genuinely stable, versus malign feedback loops where suppression prevents detection of destabilizing threats?',
    'Analysis of regime stability under information constraints; identification of early-warning signals that appear in diverse systems before collapse; measurement of threat detection lag versus actual threat manifestation',
    'If benign loops are possible: some regimes can sustain centralization without collapse. If only malign loops emerge: centralization inevitably blinds itself. If depends on initial conditions: the paradox is not universal law but path-dependent institutional choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(benign_feedback_loop_possibility, conceptual, 'Whether benign or only malign feedback loops can sustain centralization').

omega_variable(
    information_technology_escape_valve,
    'Do modern information technologies (encrypted communication, decentralized networks, VPNs, anonymous platforms) fundamentally alter the enforceability of central information suppression, creating a structural exit option that did not exist in pre-digital eras?',
    'Historical comparison of suppression effectiveness pre-internet vs post-internet in regimes attempting information control; measurement of actual information flow despite censorship apparatus; comparative cost-benefit of surveillance vs information leakage in digital-era regimes',
    'If technologies create genuine escape: the paradox becomes temporally bounded — centralization can suppress diagnostics for 1-2 decades but not indefinitely. If suppression can adapt: technologies do not resolve the paradox. If technologies create new suppression mechanisms: the constraint transforms but does not resolve.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_technology_escape_valve, empirical, 'Whether digital technologies enable escape from information suppression').

omega_variable(
    extracted_coordination_necessity,
    'Does the tangled-rope classification reflect genuine coordination (hierarchical command being less costly than negotiation) or is the ''coordination'' purely extractive rent-seeking disguised as efficiency?',
    'Comparison of administrative efficiency: centralized vs decentralized response to identical crises; measurement of actual resource utilization vs claimed efficiency gains; analysis of decision-quality in high-suppression vs low-suppression contexts',
    'If genuine coordination: centralization provides real stability benefits even with suppression costs. If purely extractive: the rope classification is aspirational, and the system is actually a snare with sophisticated justification. If mixed: the tangled rope classification is correct and the mandatrophy requires distinguishing coordination gains from extraction losses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extracted_coordination_necessity, empirical, 'Whether hierarchical centralization provides genuine coordination benefits or is purely extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(authoritarian_power_paradox, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auth_power_tr_t0, authoritarian_power_paradox, theater_ratio, 0, 0.42).
narrative_ontology:measurement(auth_power_tr_t15, authoritarian_power_paradox, theater_ratio, 15, 0.58).
narrative_ontology:measurement(auth_power_tr_t30, authoritarian_power_paradox, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(auth_power_be_t0, authoritarian_power_paradox, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(auth_power_be_t15, authoritarian_power_paradox, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(auth_power_be_t30, authoritarian_power_paradox, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(authoritarian_power_paradox, enforcement_mechanism).
narrative_ontology:affects_constraint(authoritarian_power_paradox, information_suppression_asymmetry).
narrative_ontology:affects_constraint(authoritarian_power_paradox, hierarchy_fragility_collapse).

% DUAL FORMULATION NOTE:
% The paradox of authoritarian centralization decomposes into two structurally distinct constraints: (1) information_suppression_asymmetry (ε ≈ 0.52): the institutional choice to suppress dissent and control information flows; (2) hierarchy_fragility_collapse (ε ≈ 0.65): the consequence of suppression — blind hierarchies cannot detect threats and eventually collapse. The present story focuses on the structural coupling between centralization and suppression. The downstream constraints model the specific mechanisms and failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(authoritarian_power_paradox, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
