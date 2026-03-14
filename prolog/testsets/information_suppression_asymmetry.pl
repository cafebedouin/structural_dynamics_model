% ============================================================================
% CONSTRAINT STORY: information_suppression_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_suppression_asymmetry, []).

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
 *   constraint_id: information_suppression_asymmetry
 *   human_readable: Information Suppression Asymmetry
 *   domain: epistemology/institutional/political
 *
 * SUMMARY:
 *   Information suppression asymmetry is a structural constraint operating
 *   across political, institutional, and epistemological domains. It exists
 *   when one set of agents (information controllers: governments, media
 *   corporations, organizational hierarchies) can selectively suppress,
 *   delay, or reframe information relevant to the decisions of other agents
 *   (information seekers: citizens, employees, researchers), while
 *   information controllers retain access to suppressed information. The
 *   asymmetry is not mere inequality of access—it is a **structural advantage
 *   enabling extraction**. Controllers use suppressed information to make
 *   strategic decisions while dependents operate under information scarcity,
 *   paying extraction costs (bad decisions, misallocated resources, identity
 *   confusion). The constraint exhibits mixed coordination and extraction:
 *   controllers coordinate internally through information control, while
 *   simultaneously extracting from dependents who bear asymmetric information
 *   costs. Theater ratio is moderate-high (0.61) because verification of
 *   suppressed information is systemically difficult—the very fact of
 *   suppression means verification mechanisms are compromised. Over the
 *   40-year measurement interval, both extractiveness and theater decline as
 *   decentralized information networks proliferate, but the fundamental
 *   asymmetry persists because credibility assessment and cognitive capture
 *   mechanisms partially substitute for institutional monopoly.
 *
 * KEY AGENTS:
 *   - Information Controllers: Institutional beneficiary (institutional/arbitrage) — governments, media monopolies, corporate leadership, security agencies. Benefit from suppression asymmetry through strategic advantage, narrative control, and extraction of decision-making advantage.
 *   - Information Seekers (Powerless): Primary victim (powerless/trapped) — populations dependent on controlled channels, employees without access to management information, citizens in information-scarce environments. Bear full cost of asymmetry through bad decisions and identity capture.
 *   - Intellectual Class: Secondary victim (moderate/constrained) — credentialed knowledge workers, academics, professional communicators. Can access suppressed information but at professional cost; also benefit from expert gatekeeping status.
 *   - Decentralized Networks: Organized agents (organized/constrained) — whistleblower networks, citizen journalists, distributed fact-checking, open-source intelligence communities. Building alternative pathways that reduce but do not eliminate suppression asymmetry.
 *   - Legacy Gatekeeping Institutions: Institutional actor (institutional/arbitrage) — academic journals, broadcast media, official channels. Maintain suppression through performative verification; facing competitive erosion from decentralized alternatives.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional suppression as inherent information asymmetry of complex societies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_suppression_asymmetry, 0.58).
domain_priors:suppression_score(information_suppression_asymmetry, 0.68).
domain_priors:theater_ratio(information_suppression_asymmetry, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_suppression_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(information_suppression_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(information_suppression_asymmetry, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_suppression_asymmetry, tangled_rope).
narrative_ontology:human_readable(information_suppression_asymmetry, "Information Suppression Asymmetry").
narrative_ontology:topic_domain(information_suppression_asymmetry, "epistemology/institutional/political").

domain_priors:requires_active_enforcement(information_suppression_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_suppression_asymmetry, information_controllers).
narrative_ontology:constraint_beneficiary(information_suppression_asymmetry, agenda_setters).
narrative_ontology:constraint_victim(information_suppression_asymmetry, information_seekers).
narrative_ontology:constraint_victim(information_suppression_asymmetry, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINFORMED POPULATION (SNARE) — Trapped agents with no credible information channels. Cannot exit the constraint without access to suppressed information. Maximum extraction: depend on agenda-setters for knowledge, pay asymmetric cost for counterinformation. No escape except through emergence of parallel information ecosystems.
constraint_indexing:constraint_classification(information_suppression_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTELLECTUAL CLASS (TANGLED ROPE) — Constrained by professional gatekeeping and verification requirements. Also benefit from information asymmetry through credentialing advantages and expert status. Can access suppressed information at professional cost, but career and social integration depend on conforming to approved narratives. Mixed extraction and coordination.
constraint_indexing:constraint_classification(information_suppression_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INFORMATION CONTROLLERS (ROPE) — Institutional beneficiaries (governments, media monopolies, corporate communications) experience the suppression asymmetry as pure coordination: efficiently managing information flow to maintain strategic advantage and organizational coherence. Can arbitrage between suppressed and revealed information, or pivot information strategy. Net beneficiary with low friction.
constraint_indexing:constraint_classification(information_suppression_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZED INFORMATION NETWORKS (SCAFFOLD) — Organized agents (citizen journalism, distributed fact-checking, whistleblower networks, open-source intelligence) are building alternative information pathways with sunset logic. These networks have no sunset clause as written, but represent a genuine reduction in suppression asymmetry over generational timescales. Theater_ratio declining as verification mechanisms move from centralized gatekeepers to distributed consensus.
constraint_indexing:constraint_classification(information_suppression_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY GATEKEEPING INSTITUTIONS (PITON) — Traditional information institutions (academic journals, broadcast media, official channels) maintain suppression through performative verification and institutional ritual. The theatrical component is high: peer review and editorial standards create perceived legitimacy and centralized authority despite declining functional monopoly on information quality. Maintained by institutional inertia as decentralized alternatives proliferate.
constraint_indexing:constraint_classification(information_suppression_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information suppression appears as a fundamental law: agents always have incentive to suppress information that undermines their position; verification always lags claim; perfect symmetry is logically impossible. However, this naturalizes what is actually a contingent institutional arrangement. The constraint's suppressibility through decentralized networks reveals it as a Piton or Scaffold, not a Mountain.
constraint_indexing:constraint_classification(information_suppression_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_suppression_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(information_suppression_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_suppression_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(information_suppression_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(information_suppression_asymmetry, TR),
    TR >= 0.70.

:- end_tests(information_suppression_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Information asymmetry creates systematic extraction through compounded disadvantage: information seekers make worse decisions, accumulate worse outcomes, and often cannot recognize the extraction mechanism because information about the suppression itself is suppressed. The constraint is not as severe as pure Snare (0.72) because decentralized networks provide partial alternative pathways, and some information eventually circulates. Initial measurement (0.72) reflects pre-internet institutional monopoly; current (0.58) reflects partial market competition in information channels. Suppression (0.68): High. Suppression mechanisms include legal barriers (classification, trade secrets), technical barriers (encryption, access control), institutional barriers (credentialing, gatekeeping), and cognitive barriers (narrative framing, narrative immunity). Suppression is enforced not by singular agent but by system of aligned incentives across institutions—controllers benefit, intermediaries (journalists, academics) face career costs for revealing, and dependents lack resources to verify. Theater ratio (0.61): Moderate-high. Institutional verification mechanisms (peer review, editorial standards, official channels) create perception of legitimacy despite functional compromise. The theater persists because verification of suppressed information is epistemically impossible—cannot verify what is hidden. Theater ratio declines over interval as distributed verification (citizen fact-checking, open-source intelligence) becomes visible, reducing institutional verification monopoly.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. Information controllers (Rope) experience suppression as pure coordination—efficiently managing information to maintain organizational coherence. Trapped information seekers (Snare) experience it as pure extraction with no escape. Moderate intellectual class (Tangled Rope) experience mixed coordination (credentialing benefit) and extraction (professional conformity costs). Decentralized networks (Scaffold) experience it as a temporary coordination problem being solved through alternative pathways. Legacy institutions (Piton) experience it as their own degraded ritual—maintaining suppression through inertia after functional necessity has passed. The civilizational analytical view (Mountain) risks treating institutional suppression as inevitable information asymmetry. This perspectival range reflects that information suppression is not a single structural phenomenon but a system of aligned incentives that produces dramatically different experiences for different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by each agent's structural position relative to the suppression flow. Information controllers who benefit from asymmetry and have full exit options (arbitrage) derive low d → low χ → experience Rope. Trapped agents dependent on controlled channels derive high d → high χ → experience Snare. Moderate agents with partial exit (constrained) derive middle d → moderate χ → experience Tangled Rope. Organized alternative networks with exit paths (constrained but organized) derive moderate d with coalition power → reduced χ → experience Scaffold. Legacy institutions maintaining suppression by inertia derive low direct extraction but high theater → Piton. The beneficiary/victim declarations establish the extraction direction: information controllers are beneficiaries (low d), information seekers are victims (high d). The gap between institutional and powerless perspectives reveals that suppression asymmetry is not an impersonal coordination mechanism but an asymmetric power structure maintained by aligned institutional incentives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that information suppression is genuinely hybrid: it coordinates within the controlling group (shared strategic advantage, narrative coherence) while extracting from dependents (information scarcity, decision costs). The tangled_rope classification avoids two errors: (1) treating suppression as pure coordination (Rope), which ignores extraction from information seekers and treats the constraint as beneficial to all; (2) treating it as pure Snare, which ignores that controllers have genuine coordination function (strategic advantage, narrative control) that Snare classification erases. The theater_ratio (0.61) is moderate because institutional verification mechanisms have partially functional role in detecting institutional errors, even if they fail to detect suppression itself. The declining theater over the interval (0.78 → 0.61) reflects that distributed verification mechanisms are breaking the institutional monopoly on credibility assessment, though institutional theater persists through inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression primarily structural (external barriers to information access) or internalized (cognitive capture of the audience)?',
    'Post-suppression removal analysis: if suppression persists after barrier removal, reclassify as internalized. Measure information-seeking behavior when suppressed information becomes freely available.',
    'If primarily structural: constraint can be broken by institutional transparency. If primarily internalized: constraint requires cognitive reframing and persists even after information release.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    decentralized_network_coordination_limit,
    'Can distributed information networks achieve consensus-building at scale comparable to centralized gatekeeping, or do they plateau at smaller group sizes?',
    'Comparative analysis of distributed vs centralized fact-checking coverage, error rates, and convergence speed on contested claims across domains.',
    'If scalable: Scaffold classification confirmed and suppression asymmetry has genuine sunset. If limited: decentralized networks remain niche channels, suppression asymmetry persists as primary constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_network_coordination_limit, empirical, 'Scalability limit of decentralized information networks').

omega_variable(
    false_symmetry_illusion,
    'Does proliferation of information channels create perceived symmetry (all claims equally available) that masks persistent asymmetry (credibility differentials, algorithmic amplification bias)?',
    'Information access audit: measure distribution of information source authority and reach across populations. Distinguish between information availability and information credibility.',
    'If false symmetry: constraint has shifted from access suppression to credibility suppression. Classification remains Snare/Tangled Rope but mechanism changes. Requires different intervention strategies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_symmetry_illusion, conceptual, 'Whether information proliferation masks credibility asymmetry').

omega_variable(
    agenda_setter_identity_lock,
    'Do institutional beneficiaries remain locked into information suppression strategies by organizational identity and path dependence, even when suppression becomes suboptimal?',
    'Longitudinal analysis of institutional transparency initiatives and their adoption patterns. Measure divergence between stated transparency goals and actual information release.',
    'If identity-locked: beneficiaries continue suppression despite declining strategic value. Constraint persists through institutional inertia (Piton). If rational calculation: beneficiaries shift strategies when suppression becomes costly, reducing extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agenda_setter_identity_lock, empirical, 'Whether information controllers remain locked in suppression strategies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_suppression_asymmetry, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infsup_tr_t0, information_suppression_asymmetry, theater_ratio, 0, 0.78).
narrative_ontology:measurement(infsup_tr_t20, information_suppression_asymmetry, theater_ratio, 20, 0.7).
narrative_ontology:measurement(infsup_tr_t40, information_suppression_asymmetry, theater_ratio, 40, 0.61).
narrative_ontology:measurement(infsup_tr_t10, information_suppression_asymmetry, theater_ratio, 10, 0.74).

% Extraction over time
narrative_ontology:measurement(infsup_be_t0, information_suppression_asymmetry, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(infsup_be_t20, information_suppression_asymmetry, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(infsup_be_t40, information_suppression_asymmetry, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(infsup_be_t10, information_suppression_asymmetry, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_suppression_asymmetry, information_standard).
narrative_ontology:affects_constraint(information_suppression_asymmetry, epistemic_inequality).
narrative_ontology:affects_constraint(information_suppression_asymmetry, institutional_narrative_capture).
narrative_ontology:affects_constraint(information_suppression_asymmetry, credibility_asymmetry).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(information_suppression_asymmetry, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
