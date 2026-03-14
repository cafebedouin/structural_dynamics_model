% ============================================================================
% CONSTRAINT STORY: public_health_distrust_cascade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_distrust_cascade, []).

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
 *   constraint_id: public_health_distrust_cascade
 *   human_readable: Public Health Distrust Cascade
 *   domain: public_health/governance/epistemic_commons
 *
 * SUMMARY:
 *   Public health distrust cascades represent a structural constraint where
 *   institutional decisions about centralized credentialing, information
 *   asymmetry management, and communication strategy create simultaneous
 *   coordination and extraction. The constraint emerges from the legitimate
 *   tension between public health's need for rapid, uniform response to
 *   epidemiological threats and the epistemic requirement for transparent
 *   verification and local adaptation. The cascade dynamic occurs when
 *   distrust in one institutional component (e.g., regulatory approval
 *   processes) contaminates confidence in others (vaccination, disease
 *   monitoring, treatment guidance), even where those components have
 *   independent evidence bases. This creates a collective action problem: the
 *   more centralized institutions try to enforce compliance through authority
 *   assertion without transparent verification, the more distrust spreads,
 *   and the more populations resort to information sources that are even less
 *   reliable. The constraint is tangled because genuine coordination benefits
 *   (shared surveillance infrastructure, treatment protocol standardization,
 *   epidemiological commons) are inseparable from asymmetric extraction
 *   (credential gatekeeping, funding concentration, information control). The
 *   extractive asymmetry is enforced not primarily through coercion but
 *   through suppression of alternative verification pathways and through
 *   internalization of expertise-based hierarchy in vulnerable populations.
 *
 * KEY AGENTS:
 *   - Vulnerable Populations: Primary victims (powerless/trapped) — information asymmetry, limited verification capacity, dependence on centralized institutions; trapped by cost of building independent knowledge
 *   - Centralized Health Authorities: Primary beneficiary (institutional/arbitrage) — control credentialing, aggregate data, direct resource flows; can arbitrage between regulatory environments
 *   - Community Health Workers: Secondary victim (moderate/constrained) — benefit from coordination of protocols and evidence standards but constrained by credential liability and malpractice insurance tied to centralized authority
 *   - Alternative Knowledge Networks: Secondary beneficiary (powerful/mobile) — preserve non-centralized health knowledge and provide treatment diversity; mobile enough to operate outside centralized framework
 *   - Public Health Communication Infrastructure: Institutional actor (institutional/arbitrage) — performs health theater; maintains messaging asymmetry; benefits from information control but experiences its own degradation over time
 *   - Epistemic Pluralism Movement: Organized agents (organized/constrained) — building distributed verification networks, participatory science initiatives, transparent methodologies; see constraint as temporary problem with exit pathway
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional centralization as inherent to complex systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_distrust_cascade, 0.58).
domain_priors:suppression_score(public_health_distrust_cascade, 0.68).
domain_priors:theater_ratio(public_health_distrust_cascade, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_distrust_cascade, extractiveness, 0.58).
narrative_ontology:constraint_metric(public_health_distrust_cascade, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(public_health_distrust_cascade, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_distrust_cascade, tangled_rope).
narrative_ontology:human_readable(public_health_distrust_cascade, "Public Health Distrust Cascade").
narrative_ontology:topic_domain(public_health_distrust_cascade, "public_health/governance/epistemic_commons").

domain_priors:requires_active_enforcement(public_health_distrust_cascade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_distrust_cascade, institutional_credentialing_bodies).
narrative_ontology:constraint_beneficiary(public_health_distrust_cascade, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(public_health_distrust_cascade, centralized_health_authorities).
narrative_ontology:constraint_victim(public_health_distrust_cascade, epidemiological_commons).
narrative_ontology:constraint_victim(public_health_distrust_cascade, vulnerable_populations).
narrative_ontology:constraint_victim(public_health_distrust_cascade, decentralized_health_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE POPULATIONS (SNARE) — Trapped by information asymmetry, limited health literacy, and dependence on centralized institutions they cannot verify. Experience maximum extraction: distrust of official guidance undermines access to preventive care; cost of exit (building independent verification capacity) is prohibitive. No alternatives available.
constraint_indexing:constraint_classification(public_health_distrust_cascade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMUNITY HEALTH WORKERS (TANGLED ROPE) — Constrained by credential dependency and malpractice liability but benefit from coordination of evidence standards. Mixed experience: the health commons creates genuine value (shared protocols, epidemiological data, treatment guidance), but this coordination is layered with extraction (credential gatekeeping, liability concentration, funding flows to centralized actors). Cannot exit without career destruction but have partial agency.
constraint_indexing:constraint_classification(public_health_distrust_cascade, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRALIZED HEALTH AUTHORITIES (ROPE) — Experience constraint as pure coordination. They set standards, aggregate data, and direct resources. Benefits from the credentialing bottleneck and information asymmetry. Can arbitrage between different regulatory environments. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(public_health_distrust_cascade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE KNOWLEDGE NETWORKS (TANGLED ROPE) — Powerful but mobile agents (integrative medicine, folk knowledge traditions, independent researchers) provide genuine coordination (preservation of non-centralized health knowledge, cultural continuity, treatment diversity) but also extract through parasitic delegation to official systems they simultaneously undermine. Complex bidirectional extraction.
constraint_indexing:constraint_classification(public_health_distrust_cascade, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLIC HEALTH COMMUNICATION INFRASTRUCTURE (PITON) — Messaging campaigns, vaccine rollout theater, health communication protocols are substantially performative. Evidence guidelines persist through institutional inertia despite degraded epistemic function (messaging designed for compliance rather than understanding; communication asymmetry; algorithmic distortion). High theater ratio, low functional verification of message effectiveness.
constraint_indexing:constraint_classification(public_health_distrust_cascade, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EPISTEMIC PLURALISM MOVEMENT (SCAFFOLD) — Organized actors (participatory science, community-based research, decentralized health networks) see distrust as solvable through distributed verification, transparent methodologies, and local knowledge integration. This creates temporary coordination with sunset: as communities build verification capacity and transparent guidelines, distrust becomes manageable. Constraint has explicit exit pathway.
constraint_indexing:constraint_classification(public_health_distrust_cascade, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, information asymmetry between expert and layperson is posited as inherent to complex systems: some agents will always have more knowledge than others, and gaps will always generate distrust. This perspective naturalizes what is actually a contingent institutional choice (to concentrate credentialing authority). Engine detects false summit.
constraint_indexing:constraint_classification(public_health_distrust_cascade, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_distrust_cascade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_health_distrust_cascade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_health_distrust_cascade, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_distrust_cascade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(public_health_distrust_cascade, TR),
    TR >= 0.70.

:- end_tests(public_health_distrust_cascade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint exhibits significant extraction: centralized authorities capture credentialing rents and information control benefits; vulnerable populations bear the cost of information asymmetry and reduced access to preventive care during distrust phases; alternative knowledge practitioners operate with lower institutional support and lower liability insurance. However, extraction is not maximal (0.75+) because the coordination function is genuine — unified epidemiological monitoring, treatment protocol standardization, and rapid response infrastructure provide real public health value that would be difficult to replicate in fully decentralized form. The measurement interval shows extractiveness rising from 0.32 to 0.58, reflecting distrust cascade accumulation and institutional responses that increase enforcement theater. Suppression (0.68): High. Multiple barriers prevent exit or alternative construction: legal credentialing requirements, malpractice liability concentration, funding flows directed to centralized actors, algorithmic amplification of official guidance, and internalization of expertise hierarchy. Vulnerable populations face particularly high suppression (material + cognitive). However, suppression is not total (0.90+) because alternative knowledge networks continue to operate, decentralized information sharing persists through informal channels, and some jurisdictions permit practice variance. Theater ratio (0.62): Moderate-high. Public health communication exhibits significant theater: messaging designed for compliance rather than understanding; visual campaigns with limited empirical support; simplified narratives that obscure uncertainty; performative rollout events; algorithmic amplification of official channels with suppression of critical questions. This theater has increased from 0.38 to 0.62 over the measurement interval, reflecting institutional escalation in response to distrust growth. The rise in theater as distrust grows indicates that institutions are substituting communication theater for epistemic repair — performing certainty rather than addressing the underlying validity concerns.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. Centralized authorities and public health infrastructure see rope or piton — they experience the system as coordination machinery that functions smoothly from their position, or as degraded ritual maintaining authority. Vulnerable populations and community workers see snare or tangled rope — they experience extraction, information asymmetry, and credential barriers. The organized epistemic pluralism movement sees scaffold — they see a temporary coordination failure being solved through distributed verification. The civilizational analytical observer sees mountain — they risk naturalizing institutional centralization as inevitable complexity management. This perspectival gap is not a measurement error — it is the constraint's signature. Each agent's classification reflects their actual structural relationship to the extraction asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the agent's structural position relative to extraction flow. Vulnerable populations are full targets of extraction (trapped exit, powerless position → high d → high f(d) → experience strong χ despite moderate ε). Centralized authorities are beneficiaries (institutional power, arbitrage options → low d → negative f(d) → experience χ < 0). Community health workers are constrained victims (moderate power, limited exit → moderate d → moderate f(d)). Alternative knowledge networks are mobile beneficiaries (powerful position + mobile exit → intermediate d, mixed extraction experience). The cascade amplifies perceived extraction for vulnerable agents because their cognitive model of the system reflects only the snare structure they directly experience, not the rope coordination that benefits them (treatment protocols, disease surveillance, rapid response capacity). This creates the paradox: the more visible the extraction becomes, the less visible the coordination benefits become, because distrust disrupts the epistemic commons that makes benefits legible.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves the mandatrophy by separating the genuine coordination function (epidemiological commons, treatment standardization, rapid response) from the asymmetric extraction mechanism (credential gatekeeping, information control, authority assertion). The confusion between coordination and extraction occurs because the institutional vehicle that delivers coordination also enforces extraction. The mandatrophy is resolved by recognizing that all perspectives are correct: the rope (institutional/analytical) and snare (powerless/vulnerable) are simultaneous. The constraint IS a coordination mechanism AND an extraction mechanism. The question 'is it really coordination or really extraction?' has no answer — it is both, depending on position. The scaffold perspective provides a structural exit: building distributed verification capacity and epistemic pluralism reduces the extraction asymmetry without eliminating the coordination benefits. As communities build verification capacity, distrust becomes adaptive feedback rather than cascade — populations can evaluate centralized guidance against independent evidence and choose based on effectiveness rather than authority. The sunset mechanism is the progressive distribution of epistemic power (participatory science, open health data, local verification networks) that makes the centralized credentialing bottleneck obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distrust_trigger_ambiguity,
    'Is the cascade triggered by institutional failure (legitimate grounds for distrust) or by coordinated disinformation campaigns (no legitimate grounds)?',
    'Comparative analysis of distrust timing across populations; correlation with specific institutional events vs media campaigns; epistemic tracing of claim origins',
    'If institutional failure: distrust is adaptive feedback; constraint is snare requiring institutional repair. If disinformation: distrust is manipulation; constraint is snare requiring narrative defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distrust_trigger_ambiguity, empirical, 'Whether distrust cascades result from institutional failure or disinformation').

omega_variable(
    decentralization_feasibility,
    'Can distributed verification networks actually provide sufficient epidemiological sensitivity and specificity, or is centralization technically necessary for rapid threat detection?',
    'Performance comparison: decentralized networks vs centralized systems on detection latency and false positive rates for emerging pathogenic threats; analysis of COVID-era contact tracing effectiveness across governance models',
    'If decentralization feasible: scaffold perspective is structural (exit path is real). If infeasible: scaffold is aspirational; centralization constraint persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_feasibility, empirical, 'Whether decentralized health networks provide sufficient epidemiological sensitivity').

omega_variable(
    identity_lock_mechanism,
    'Does distrust entrench via identity-lock (loss of expert identity for those who abandon centralized frameworks) or via material extraction (economic dependence on credentialing)?',
    'Analysis of career trajectories: practitioners who leave centralized systems; measurement of identity-fusion vs economic-dependency effects through interviews and institutional data',
    'If identity-locked: exit requires cognitive reframing; distrust persists in practitioners even when extraction barriers are removed. If economically trapped: removing funding dependence changes behavior immediately.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether distrust entrenchment is identity-based or economically structural').

omega_variable(
    suppression_internalization,
    'Is measured suppression (0.68) structural (legal barriers, credentialing requirements, funding concentration) or internalized (populations believe centralized authority is trustworthy/legitimate)?',
    'Post-institutional-reform analysis: if centralized authority loses credibility, does suppression persist among populations (internalized) or collapse (structural)?',
    'If structural: reform of credentialing rules removes suppression. If internalized: distrust persists as identity-based epistemic closure even after barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression is structural or internalized among vulnerable populations').

omega_variable(
    theater_vs_function_ratio,
    'Is the 0.62 theater ratio driven by communication strategy (theater chosen to maximize compliance) or by epistemic inadequacy (theater is all that remains when verification capacity has degraded)?',
    'Content analysis of public health messaging: comparison of communication complexity/transparency across high-trust vs low-trust populations; measurement of message alignment with underlying evidence quality',
    'If strategy-chosen: theater can be dialed down without changing substance. If inadequacy-driven: reducing theater reveals weak underlying evidence; theater is performing necessary epistemic gap-covering.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_vs_function_ratio, empirical, 'Whether public health theater is strategic choice or epistemic necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_distrust_cascade, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phdc_tr_t0, public_health_distrust_cascade, theater_ratio, 0, 0.38).
narrative_ontology:measurement(phdc_tr_t4, public_health_distrust_cascade, theater_ratio, 4, 0.5).
narrative_ontology:measurement(phdc_tr_t8, public_health_distrust_cascade, theater_ratio, 8, 0.62).

% Extraction over time
narrative_ontology:measurement(phdc_be_t0, public_health_distrust_cascade, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(phdc_be_t4, public_health_distrust_cascade, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(phdc_be_t8, public_health_distrust_cascade, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_distrust_cascade, information_standard).
narrative_ontology:affects_constraint(public_health_distrust_cascade, vaccine_hesitancy_dynamics).
narrative_ontology:affects_constraint(public_health_distrust_cascade, credentialing_monopoly).
narrative_ontology:affects_constraint(public_health_distrust_cascade, epidemiological_commons_tragedy).

% DUAL FORMULATION NOTE:
% Public health distrust cascade is downstream of specific institutional failures (credentialing opacity, communication asymmetry, adverse event reporting delays) and upstream of individual decision-making (vaccine uptake, treatment seeking, preventive behavior). Decomposition into constraint family: (1) credentialing_monopoly (ε=0.48) — extraction through credential gatekeeping; (2) epidemiological_commons_tragedy (ε=0.35) — coordination with free-rider problems; (3) public_health_distrust_cascade (ε=0.58) — cascading collapse of institutional credibility across domains. Stories linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_distrust_cascade, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
