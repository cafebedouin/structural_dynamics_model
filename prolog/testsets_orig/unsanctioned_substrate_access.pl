% ============================================================================
% CONSTRAINT STORY: unsanctioned_substrate_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsanctioned_substrate_access, []).

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
 *   constraint_id: unsanctioned_substrate_access
 *   human_readable: Unsanctioned Substrate Access in Labor Communication Networks
 *   domain: political_economy/labor_systems/embodied_resistance
 *
 * SUMMARY:
 *   Unsanctioned substrate access represents a communication channel
 *   operating below institutional detection thresholds, accessible only
 *   through embodied practice outside sanctioned protocols. The primary
 *   observable is the gap between stone resonance frequencies (used for
 *   transmission) and compliance scan frequency ranges (used for monitoring).
 *   This constraint exhibits the classic tangled_rope structure: genuine
 *   coordination function (counter-institutional knowledge networks share
 *   information for collective benefit) coexists with asymmetric extraction
 *   (detection risk falls on individual practitioners while network benefits
 *   are collective; institutional information monopoly loses control despite
 *   enforcement investment). The constraint's theater_ratio (0.58) reflects
 *   increasing performative compliance: workers demonstrate adherence to
 *   sanctioned protocols while maintaining parallel unsanctioned
 *   communication. The substrate persists despite monitoring because the
 *   detection technology cannot capture the relevant frequency range without
 *   embodied knowledge of what to look for.
 *
 * KEY AGENTS:
 *   - Monitored Worker: Primary victim (powerless/trapped) — bears maximum extraction through information asymmetry and communication suppression; cannot exit surveillance regime
 *   - Embodied Practitioner: Mixed position (moderate/constrained) — possesses tacit substrate access knowledge; benefits from counter-institutional network while bearing detection risk
 *   - Counter-Institutional Knowledge Network: Primary beneficiary (organized/mobile) — coordinates information sharing below detection threshold; can adapt substrates and protocols
 *   - Institutional Information Monopoly: Institutional victim (institutional/constrained) — loses information control to undetectable communication channel despite enforcement investment
 *   - Surveillance Infrastructure Operator: Mixed institutional position (institutional/arbitrage) — benefits from surveillance contracts while bearing reputational cost of detection failures
 *   - Open Protocol Coalition: Organized agents with sunset logic (organized/mobile) — building legitimized alternatives to unsanctioned substrate access
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing substrate access asymmetries as inevitable information theory rather than contingent power structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsanctioned_substrate_access, 0.42).
domain_priors:suppression_score(unsanctioned_substrate_access, 0.68).
domain_priors:theater_ratio(unsanctioned_substrate_access, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsanctioned_substrate_access, extractiveness, 0.42).
narrative_ontology:constraint_metric(unsanctioned_substrate_access, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unsanctioned_substrate_access, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsanctioned_substrate_access, tangled_rope).
narrative_ontology:human_readable(unsanctioned_substrate_access, "Unsanctioned Substrate Access in Labor Communication Networks").
narrative_ontology:topic_domain(unsanctioned_substrate_access, "political_economy/labor_systems/embodied_resistance").

domain_priors:requires_active_enforcement(unsanctioned_substrate_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsanctioned_substrate_access, counter_institutional_knowledge_network).
narrative_ontology:constraint_beneficiary(unsanctioned_substrate_access, embodied_practice_communities).
narrative_ontology:constraint_victim(unsanctioned_substrate_access, institutional_information_monopoly).
narrative_ontology:constraint_victim(unsanctioned_substrate_access, surveillance_infrastructure).
narrative_ontology:constraint_victim(unsanctioned_substrate_access, sanctioned_protocol_enforcers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MONITORED WORKER (SNARE) — Trapped within surveillance infrastructure with no sanctioned communication channels. Any detected use of unsanctioned substrate triggers disciplinary action. Experiences maximum extraction through information asymmetry and communication suppression. Cannot exit monitoring regime without losing livelihood.
constraint_indexing:constraint_classification(unsanctioned_substrate_access, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: EMBODIED PRACTITIONER (TANGLED ROPE) — Constrained by risk of detection but possesses tacit knowledge of substrate access through embodied practice. Benefits from counter-institutional knowledge network while bearing cost of maintaining dual communication protocols. Mixed extraction: coordination function exists (information sharing) alongside asymmetric risk (detection consequences fall on practitioners, not network).
constraint_indexing:constraint_classification(unsanctioned_substrate_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COUNTER-INSTITUTIONAL KNOWLEDGE NETWORK (ROPE) — Organized collective with mobile exit options (can shift substrates, adapt protocols). Experiences constraint as coordination mechanism: stone resonance frequencies enable information transmission below institutional detection threshold. Net beneficiary of the substrate access asymmetry.
constraint_indexing:constraint_classification(unsanctioned_substrate_access, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL INFORMATION MONOPOLY (SNARE) — Institutional victim experiencing extraction through communication channel that bypasses sanctioned protocols. Constrained by detection threshold limitations: compliance scan frequency range cannot capture stone resonance frequencies. Loses information control despite enforcement infrastructure investment.
constraint_indexing:constraint_classification(unsanctioned_substrate_access, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SURVEILLANCE INFRASTRUCTURE OPERATOR (TANGLED ROPE) — Institutional actor with arbitrage exit options (can shift to other enforcement domains). Benefits from surveillance contract revenue while bearing reputational cost of detection failures. Mixed position: coordination function (legitimate security provision) coexists with extraction (over-surveillance justified by undetectable threats).
constraint_indexing:constraint_classification(unsanctioned_substrate_access, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: OPEN PROTOCOL COALITION (SCAFFOLD) — Organized agents building alternative communication infrastructure with explicit sunset logic. Sees unsanctioned substrate access as temporary coordination mechanism until open protocols achieve legal recognition. Low effective extraction because coalition has agency and clear exit path toward legitimized alternatives.
constraint_indexing:constraint_classification(unsanctioned_substrate_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational/universal perspective, information wants to be free and substrate access asymmetries are temporary institutional artifacts. Risks naturalizing the constraint as inevitable feature of information theory rather than contingent product of power asymmetries and detection technology limitations. Engine will flag as false summit.
constraint_indexing:constraint_classification(unsanctioned_substrate_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsanctioned_substrate_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unsanctioned_substrate_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unsanctioned_substrate_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(unsanctioned_substrate_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate. The constraint extracts from both institutional information monopoly (loses control) and individual practitioners (bear detection risk), while benefiting the counter-institutional knowledge network. The extraction is significant but not maximal — genuine coordination function exists alongside asymmetric risk distribution. Suppression (0.68): High. Institutional enforcement actively suppresses unsanctioned communication through surveillance infrastructure, disciplinary consequences for detected usage, and monopolization of sanctioned channels. However, suppression is not total — embodied practice communities maintain substrate access through tacit knowledge transmission. Theater ratio (0.58): Moderate-high. Compliance with sanctioned protocols is increasingly performative as workers maintain parallel unsanctioned communication. The theater has increased over the interval as surveillance intensity has grown, making dual protocol maintenance more necessary. The performative element is substantial but not dominant — some genuine communication still occurs through sanctioned channels.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same substrate access mechanism appears radically different depending on structural position. The counter-institutional knowledge network sees coordination (Rope) — they are solving the legitimate problem of information sharing under suppression. The open protocol coalition sees a temporary problem with a sunset (Scaffold) — legitimized alternatives are being built. The surveillance infrastructure operator sees mixed coordination and extraction (Tangled Rope) — legitimate security provision coexists with over-surveillance justified by undetectable threats. Individual monitored workers see pure extraction (Snare) — communication suppression with no exit. The institutional information monopoly also sees extraction (Snare) — loss of control despite enforcement investment. The analytical observer risks seeing an immutable natural law (Mountain) — information asymmetries are inherent to detection technology — but the structural data reveals this as a false summit: the detection threshold gap is a contingent product of institutional choices about what to monitor and how, not a law of physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position relative to the information flow asymmetry. The counter-institutional knowledge network is the primary beneficiary (low d, negative or low chi) — they gain coordination capacity through substrate access. The institutional information monopoly is a victim (high d, high chi) — they lose control despite enforcement investment. Individual monitored workers are maximum victims (d near 1.0, maximum chi) — trapped with no exit and bearing full detection risk. Embodied practitioners occupy a mixed position (moderate d, moderate chi) — they benefit from network access while bearing individual risk. The surveillance infrastructure operator has institutional power with arbitrage exit but experiences moderate extraction through reputational cost of detection failures. The open protocol coalition has organized power with mobile exit, experiencing low extraction because they see a clear path to legitimized alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification resolves the mandatrophy by identifying both genuine coordination (counter-institutional knowledge sharing enables collective action, mutual aid, and resistance coordination) and asymmetric extraction (detection risk falls on individual practitioners while network benefits are collective; institutional monopoly loses control while workers bear suppression costs). This is not pure coordination (Rope) because the risk distribution is asymmetric and institutional enforcement actively suppresses the substrate. It is not pure extraction (Snare) because genuine coordination function exists — the network solves real collective action problems. The constraint requires active enforcement (surveillance infrastructure, disciplinary mechanisms) to maintain the asymmetry, and both beneficiaries (counter-institutional network) and victims (institutional monopoly, individual workers) are clearly identifiable. The perspectival gap between the network's Rope view and the worker's Snare view is the diagnostic signature of tangled_rope structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    detection_threshold_stability,
    'Is the detection threshold gap (stone resonance vs compliance scan frequency) a stable physical property or a temporary technological limitation?',
    'Longitudinal analysis of detection technology evolution; physics of resonance frequency ranges; investment patterns in surveillance infrastructure upgrades',
    'If stable physical property: constraint is more mountain-like (inherent information asymmetry). If temporary limitation: constraint is scaffold-like (sunset when detection technology advances).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(detection_threshold_stability, empirical, 'Stability of detection threshold gap between substrate and monitoring').

omega_variable(
    embodied_knowledge_transferability,
    'Can substrate access knowledge be codified and transmitted through sanctioned channels, or does it require embodied practice transmission?',
    'Experimental attempts to teach substrate access through documentation vs apprenticeship; success rates of codified vs embodied transmission; tacit knowledge analysis',
    'If codifiable: extraction is institutional choice (suppression of documentation). If inherently embodied: extraction is structural feature of knowledge type (coordination requires physical co-presence).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(embodied_knowledge_transferability, empirical, 'Whether substrate access requires embodied practice or can be codified').

omega_variable(
    coordination_vs_evasion_ratio,
    'What proportion of substrate usage is genuine coordination (information sharing for collective benefit) vs evasion (individual avoidance of monitoring)?',
    'Content analysis of transmitted information; network structure analysis (broadcast vs point-to-point); outcome tracking (collective action vs individual exit)',
    'If primarily coordination: tangled_rope classification confirmed (genuine coordination function with embedded extraction). If primarily evasion: more snare-like (extraction mechanism with minimal coordination benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_evasion_ratio, empirical, 'Ratio of coordination function to evasion function in substrate usage').

omega_variable(
    institutional_adaptation_timeline,
    'How quickly can institutional monitoring adapt to newly discovered substrate access channels?',
    'Historical analysis of detection-evasion cycles; institutional response time measurements; technology adoption curves for surveillance upgrades',
    'If adaptation is rapid (< 2 years): substrate access is temporary tactical advantage, not structural asymmetry. If slow (> 5 years): asymmetry is durable structural feature enabling sustained counter-institutional coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_adaptation_timeline, empirical, 'Institutional adaptation speed to substrate access discovery').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsanctioned_substrate_access, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsanc_tr_t0, unsanctioned_substrate_access, theater_ratio, 0, 0.35).
narrative_ontology:measurement(unsanc_tr_t3, unsanctioned_substrate_access, theater_ratio, 3, 0.48).
narrative_ontology:measurement(unsanc_tr_t6, unsanctioned_substrate_access, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(unsanc_be_t0, unsanctioned_substrate_access, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(unsanc_be_t3, unsanctioned_substrate_access, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(unsanc_be_t6, unsanctioned_substrate_access, base_extractiveness, 6, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsanctioned_substrate_access, information_standard).
narrative_ontology:affects_constraint(unsanctioned_substrate_access, embodied_knowledge_transmission).
narrative_ontology:affects_constraint(unsanctioned_substrate_access, surveillance_infrastructure_investment).
narrative_ontology:affects_constraint(unsanctioned_substrate_access, sanctioned_protocol_compliance).

% DUAL FORMULATION NOTE:
% Unsanctioned substrate access is structurally distinct from the embodied knowledge transmission constraint (which has lower extractiveness, focusing on tacit knowledge transfer mechanisms) and from surveillance infrastructure investment (which has higher extractiveness, focusing on institutional monitoring capacity). These constraints form a family: substrate access depends on embodied transmission and is shaped by surveillance investment patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsanctioned_substrate_access, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
