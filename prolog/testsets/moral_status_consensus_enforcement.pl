% ============================================================================
% CONSTRAINT STORY: moral_status_consensus_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_moral_status_consensus_enforcement, []).

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
 *   constraint_id: moral_status_consensus_enforcement
 *   human_readable: Moral Status Consensus Enforcement
 *   domain: social/political/ethical
 *
 * SUMMARY:
 *   Moral status consensus enforcement represents the mechanisms by which
 *   societies maintain boundaries around who or what counts as morally
 *   considerable. The constraint operates across multiple institutional
 *   domains — legal systems, religious frameworks, philosophical canons,
 *   professional ethics bodies — and exhibits the core tension between
 *   genuine coordination (societies need shared moral frameworks) and
 *   asymmetric extraction (control over moral status boundaries grants
 *   enormous power). The constraint has intensified over the measured
 *   interval (0.35 → 0.58 extractiveness) as moral status claims have become
 *   more sophisticated and institutional defenses more elaborate. The theater
 *   ratio rise (0.38 → 0.58) reflects increasing performative dimension:
 *   ritual objections to moral status expansion, canonized refutations,
 *   gatekeeping through citation networks. Historically, moral status has
 *   expanded (slavery abolition, women's rights, animal protection emerging)
 *   but only through direct conflict with consensus enforcers. The current
 *   interval captures a period where expansion pressure is increasing but
 *   institutional resistance is also crystallizing.
 *
 * KEY AGENTS:
 *   - Moral Status Claimants: Powerless/trapped (individuals, advocates) seeking recognition for newly excluded moral patients; bear full cost of suppression through ostracism and institutional exclusion
 *   - Consensus Enforcer Institutions: Institutional/arbitrage (religions, legal systems, philosophical canons) setting and defending moral boundaries; benefit from legitimacy monopoly and predictability
 *   - Moral Boundary Negotiators: Moderate/constrained (ethicists, policy bodies, professional associations) ostensibly mediating expansion but actually gatekeeping; extract through control of legitimacy frames
 *   - Moral Community Coalitions: Organized/constrained (animal rights movements, disability justice, indigenous communities) coordinating to expand boundaries while facing suppression
 *   - Academic Philosophy Establishment: Institutional/arbitrage maintaining consensus through peer review rituals, canonical texts, professional gatekeeping; benefits from consensus defense
 *   - Analytical Observer: Civilizational/analytical perspective risking naturalization of contingent institutional arrangements as logical necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(moral_status_consensus_enforcement, 0.58).
domain_priors:suppression_score(moral_status_consensus_enforcement, 0.65).
domain_priors:theater_ratio(moral_status_consensus_enforcement, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(moral_status_consensus_enforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(moral_status_consensus_enforcement, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(moral_status_consensus_enforcement, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(moral_status_consensus_enforcement, tangled_rope).
narrative_ontology:human_readable(moral_status_consensus_enforcement, "Moral Status Consensus Enforcement").
narrative_ontology:topic_domain(moral_status_consensus_enforcement, "social/political/ethical").

domain_priors:requires_active_enforcement(moral_status_consensus_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(moral_status_consensus_enforcement, consensus_enforcer_group).
narrative_ontology:constraint_beneficiary(moral_status_consensus_enforcement, established_moral_hierarchy).
narrative_ontology:constraint_victim(moral_status_consensus_enforcement, moral_status_claimants).
narrative_ontology:constraint_victim(moral_status_consensus_enforcement, ethical_reasoning_pluralism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MORAL STATUS CLAIMANT (SNARE) — An agent seeking recognition for previously excluded moral patients (animals, future generations, AI systems) faces maximal suppression. Exit is structurally impossible: one cannot stop advocating for moral inclusion without abandoning the claim itself. The agent bears extraction through social ostracism, institutional exclusion, and career damage. No alternatives exist within the consensus framework.
constraint_indexing:constraint_classification(moral_status_consensus_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MORAL BOUNDARY NEGOTIATOR (TANGLED ROPE) — Institutional actors (ethicists, policy bodies, professional associations) experience genuine coordination problems: moral boundaries must be defined and enforced for coherent societies. Simultaneously, they extract asymmetric benefit by controlling what counts as moral reasoning. Exit is constrained by professional reputation and institutional embedding.
constraint_indexing:constraint_classification(moral_status_consensus_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSENSUS ENFORCER (ROPE) — Established institutions (major religions, philosophical traditions, legal systems) that hold consensus-setting power experience the constraint as pure coordination. Enforcing moral boundaries enables predictability, coherence, and social stability. These actors benefit from prioritized access to legitimacy frames but frame it as technical necessity rather than extraction.
constraint_indexing:constraint_classification(moral_status_consensus_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MORAL COMMUNITY COALITION (TANGLED ROPE) — Organized groups (animal rights movements, disability justice advocates, indigenous communities) coordinate to expand moral boundaries while constrained by the suppression of dominant frameworks. They experience both the coordination benefit (collective voice) and extraction (delegitimization, resource barriers). Exit is constrained but organized resistance creates agency.
constraint_indexing:constraint_classification(moral_status_consensus_enforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: RITUALIZED MORAL PHILOSOPHY (PITON) — Academic moral philosophy has ritualized consensus enforcement through peer review, canonical texts, and departmental gatekeeping. The performative component (citing approved frameworks, ritual citations of key figures) has substantially increased relative to substantive innovation. Theater ratio reflects that much moral philosophical discourse maintains consensus theater rather than expanding moral reasoning genuinely. The system persists through institutional inertia.
constraint_indexing:constraint_classification(moral_status_consensus_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some moral status consensus may appear immutable: stable societies require shared frameworks, finite moral resources require boundaries, and cognitive constraints limit how many entities can occupy moral consideration. This perspective naturalizes what are actually contingent institutional choices. The engine will identify this as a false summit — the appearance of immutability derives from social enforcement, not logical necessity.
constraint_indexing:constraint_classification(moral_status_consensus_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moral_status_consensus_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(moral_status_consensus_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moral_status_consensus_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(moral_status_consensus_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(moral_status_consensus_enforcement, TR),
    TR >= 0.70.

:- end_tests(moral_status_consensus_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts asymmetric benefit to consensus enforcers who control what counts as moral reasoning. The extraction is substantial but not total (0.70+) because some genuine coordination function exists — societies do need shared moral frameworks. The upward trajectory (0.35 → 0.58) reflects that institutional defenses have become more elaborate and more explicitly aimed at preventing expansion. Suppression (0.65): High. Multiple barriers prevent moral status claimants from gaining standing: epistemic dismissal ('not within serious philosophy'), institutional exclusion ('not at the table'), career damage ('unseriousness'), social ostracism ('moral concern fatigue'), and enforcement through citation networks and peer review. Theater ratio (0.58): Moderate-high. Consensus defense increasingly relies on performative elements: citing approved refutations, ritual objections to 'expanding moral consideration indefinitely,' academic theater that mimics serious engagement while precluding substantive consideration. The rise over the interval reflects defensive crystallization — as pressures mount, the performative component increases relative to substantive argument.
 *
 * PERSPECTIVAL GAP:
 *   The powerless claimant sees pure extraction (Snare) — they cannot exit consensus enforcement without abandoning the moral claim itself. The consensus enforcer sees pure coordination (Rope) — enforcing boundaries enables stable society. The moderate negotiator sees mixed coordination-extraction (Tangled Rope) — genuine coordination needs but also gatekeeper power. The organized coalition sees constrained agency within extraction (Tangled Rope at organized power) — they can coordinate resistance but face systematic suppression. The philosophical establishment sees performative necessity (Piton) — the consensus is maintained through ritual citation and gatekeeping despite questionable functional necessity. The analytical observer risks false summit (Mountain) — moral boundaries may appear natural/logical but actually reflect institutional enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position relative to the extraction flow. Consensus enforcers (institutional/arbitrage) have low d values (~0.15) because they benefit from the constraint and can exit via arbitrage (moving to alternative moral frameworks confers no cost for the empowered). Claimants (powerless/trapped) have high d values (~0.95) because they cannot exit without abandoning their claim — the constraint is their structural reality. Negotiators (moderate/constrained) have moderate d values (~0.65) because they are partly captured (exit via accepting claimant frames damages career) but partly benefit (gatekeeper power yields status). Organized coalitions (organized/constrained) have slightly lower d values (~0.50) than trapped claimants because organization creates agency, but still constrained by institutional barriers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy collapse by clearly containing both genuine coordination (moral frameworks are socially necessary) and asymmetric extraction (control of moral status boundaries grants power). The tangled rope classification at institutional/moderate perspectives captures this hybrid. The snare classification at powerless/trapped captures the experience of those seeking inclusion. The rope classification at institutional/arbitrage beneficiaries captures their genuine coordination framing. The classification diversity reflects real perspectival differences, not analytic confusion. The piton classification correctly identifies increasing performative dimension without collapsing the type — theater ratio 0.58 exceeds the Rope threshold (0.45) but does not reach Piton floor (0.70). The analytical mountain is explicitly flagged as false summit. The constraint's extractiveness is high enough (0.58) to require measurements and omegas, which are included. The upward trajectory in measurements supports the tangled rope classification — extractiveness increasing as institutional defenses crystallize around consensus enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_empirical_content,
    'Are disputes over moral status primarily empirical disagreements about properties (sentience, rationality, future-orientedness) or purely normative disagreements about value assignment?',
    'Disaggregation of historical moral status disputes into factual vs normative components; analysis of scientific evidence response in moral status debates (e.g., does evidence of animal sentience change consensus?)',
    'If primarily empirical: consensus enforcement suppresses legitimate evidence. If primarily normative: consensus enforcement reflects genuine value coordination. Affects whether suppression metric should be higher (if empirical resolution is possible but blocked).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_status_empirical_content, conceptual, 'Whether moral status disputes are empirical or purely normative').

omega_variable(
    moral_status_resource_constraints,
    'Do finite moral resources (capacity for moral consideration, institutional enforcement bandwidth) genuinely require boundary enforcement, or is this a myth naturalizing exclusionary power?',
    'Cross-cultural analysis of moral status frameworks with different scope; cognitive science studies on actual moral consideration capacity; comparison of enforcement intensity to evidence of resource scarcity',
    'If genuine resource constraints exist: consensus enforcement is partly coordination (Tangled Rope justified). If myth: consensus enforcement is pure extraction and power maintenance (Snare confirmed). Affects beneficiary justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_status_resource_constraints, empirical, 'Whether moral consideration capacity is genuinely limited').

omega_variable(
    alternative_moral_frameworks_viability,
    'Can plural moral frameworks coexist functionally, or does social stability require a single dominant consensus?',
    'Historical analysis of societies with genuinely plural moral frameworks; study of consensus fragmentation in modern pluralistic societies; examination of coordination failures attributed to moral disagreement vs other causes',
    'If coexistence is viable: consensus enforcement is extraction (agents choosing to coordinate around single framework despite alternatives). If single framework is necessary: consensus enforcement is genuine coordination (Rope classification appropriate). Affects type assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_moral_frameworks_viability, empirical, 'Whether plural moral frameworks can coexist functionally').

omega_variable(
    moral_status_consensus_direction,
    'Does moral status consensus naturally expand (as evidence accumulates and moral reasoning develops) or does expansion require active suppression breaking?',
    'Historical trajectory of moral status expansion (slavery, women, animals, future generations); analysis of whether expansion happened through internal consensus evolution or external pressure and conflict; measurement of enforcement intensity over time during expansion periods',
    'If naturally expanding: current consensus enforcement is temporary scaffolding (Scaffold classification appropriate). If resistance is structural: enforcement is Snare/Tangled Rope. Affects mandatrophy and sunset expectations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_status_consensus_direction, empirical, 'Direction and mechanism of moral status consensus evolution').

omega_variable(
    identity_lock_mechanism,
    'For consensus enforcers and boundary negotiators, is their resistance to moral status expansion rooted in identity fusion (professional identity constituted through current frameworks) or material interests (career, institutional resources)?',
    'Analysis of individual trajectory data: do professionals accept moral status expansion when freed from career constraints? Study of institutional changes when resource interests align with expansion. Comparison of resistance intensity to material stakes.',
    'If identity-locked: enforcers see constraints differently than beneficiaries; may classify differently under identity_locked exit option. If material interests: directionality overrides may be needed to reflect actual capture. Affects perspective differentiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether moral status resistance is identity-locked or interest-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(moral_status_consensus_enforcement, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(moralstatus_tr_t0, moral_status_consensus_enforcement, theater_ratio, 0, 0.38).
narrative_ontology:measurement(moralstatus_tr_t25, moral_status_consensus_enforcement, theater_ratio, 25, 0.5).
narrative_ontology:measurement(moralstatus_tr_t50, moral_status_consensus_enforcement, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(moralstatus_be_t0, moral_status_consensus_enforcement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(moralstatus_be_t25, moral_status_consensus_enforcement, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(moralstatus_be_t50, moral_status_consensus_enforcement, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(moral_status_consensus_enforcement, identity_coordination).
narrative_ontology:affects_constraint(moral_status_consensus_enforcement, epistemic_authority_gatekeeping).
narrative_ontology:affects_constraint(moral_status_consensus_enforcement, institutional_moral_framework_monopoly).
narrative_ontology:affects_constraint(moral_status_consensus_enforcement, moral_circle_expansion_resistance).

% DUAL FORMULATION NOTE:
% Moral status consensus enforcement is linked to epistemic gatekeeping (who gets to speak authoritatively about moral truth) and institutional monopoly (which frameworks count as legitimate). These are structurally distinct constraints with different ε values but operate in tandem. The network relationships enable contamination analysis: if consensus enforcement weakens, epistemic gatekeeping must strengthen to maintain the constraint's total effect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(moral_status_consensus_enforcement, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
