% ============================================================================
% CONSTRAINT STORY: alternative_institution_clustering
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alternative_institution_clustering, []).

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
 *   constraint_id: alternative_institution_clustering
 *   human_readable: Alternative Institution Clustering and Institutional Bifurcation
 *   domain: institutional_structure/governance/organizational_formation
 *
 * SUMMARY:
 *   Alternative institution clustering describes the structural tendency for
 *   genuinely novel institutional forms (non-hierarchical governance,
 *   commons-based resource allocation, horizontally-organized production,
 *   mutual aid networks) to be systematically marginalized, excluded, or
 *   co-opted by incumbent institutional frameworks (corporations, state
 *   agencies, professional associations). The constraint operates through
 *   regulatory capture, resource monopolization, legal prohibition of
 *   non-standard templates, and rhetorical appropriation of innovation
 *   narratives. It exhibits all six DR types across different perspectives.
 *   The increasing theater ratio (0.35 → 0.73) reflects the rising
 *   performative content: incumbents adopt 'diversity,' 'sustainability,' and
 *   'innovation' language while deepening regulatory moats and resource
 *   gatekeeping. This constraint determines whether institutional pluralism
 *   is possible or whether path-dependent institutional clustering creates a
 *   universal template trap.
 *
 * KEY AGENTS:
 *   - Alternative Institution Founders: Primary victims (powerless/trapped) — individual or small collective attempting to establish new institutional forms; face regulatory barriers, resource scarcity, and legitimacy exclusion
 *   - Coalition of Alternative Practitioners: Organized victims (organized/constrained) — networks of cooperatives, commons, mutual aid structures with genuine coordination function but facing suppression and resource barriers
 *   - Incumbent Institutional Frameworks: Primary beneficiaries (institutional/arbitrage) — large existing institutions that benefit from clustering, possess regulatory capture power, and face minimal exit costs
 *   - Regulatory Gatekeeper: Secondary beneficiary (institutional/constrained) — standard-setting bodies and regulators that coordinate legitimate functions but also enforce incumbent-advantaging templates and face constraints in deregulation
 *   - Technology-Enabled Decentralization: Organized actor with mobile exit (powerful/mobile) — blockchain, peer-to-peer, open-source governance tools creating temporary scaffolding around incumbent dominance
 *   - Diversity-and-Inclusion Theater Maintainers: Institutional actors (institutional/arbitrage) — incumbent institutions using progressive rhetoric while deepening suppression
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices as inevitable laws of organizational ecology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alternative_institution_clustering, 0.58).
domain_priors:suppression_score(alternative_institution_clustering, 0.65).
domain_priors:theater_ratio(alternative_institution_clustering, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alternative_institution_clustering, extractiveness, 0.58).
narrative_ontology:constraint_metric(alternative_institution_clustering, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(alternative_institution_clustering, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alternative_institution_clustering, tangled_rope).
narrative_ontology:human_readable(alternative_institution_clustering, "Alternative Institution Clustering and Institutional Bifurcation").
narrative_ontology:topic_domain(alternative_institution_clustering, "institutional_structure/governance/organizational_formation").

domain_priors:requires_active_enforcement(alternative_institution_clustering).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alternative_institution_clustering, incumbent_institutional_frameworks).
narrative_ontology:constraint_beneficiary(alternative_institution_clustering, regulatory_gatekeepers).
narrative_ontology:constraint_victim(alternative_institution_clustering, alternative_institution_founders).
narrative_ontology:constraint_victim(alternative_institution_clustering, institutional_innovation_seekers).
narrative_ontology:constraint_victim(alternative_institution_clustering, distributed_coordination_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE INSTITUTION FOUNDER (SNARE) — Individual or small collective attempting to establish genuinely new institutional forms faces overwhelming barriers: regulatory capture, inability to access resources or legitimacy without conforming to incumbent templates, legal prohibition of non-standard governance structures, and de facto exclusion from markets or funding. Founder is trapped — cannot exit without abandoning the attempt to innovate. Experiences maximum extraction: bears all development costs and risk while incumbent frameworks extract through regulatory advantage and resource monopoly.
constraint_indexing:constraint_classification(alternative_institution_clustering, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COALITION OF ALTERNATIVE PRACTITIONERS (TANGLED ROPE) — Organized groups (cooperatives, commons networks, mutual aid structures) benefit from genuine coordination function: shared knowledge, legitimacy through numbers, collective bargaining power against incumbents. But also experience extraction: regulatory harassment, market exclusion, forced compliance with templates designed for profit-maximizing firms, and suppression of alternative success stories. Exit constrained by resource requirements and sunk capital in alternative structures. Moderate extraction with real coordination benefits.
constraint_indexing:constraint_classification(alternative_institution_clustering, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT INSTITUTIONAL FRAMEWORK (ROPE) — Large existing institutions (corporations, state agencies, professional associations) experience the constraint as coordination mechanism. Their dominance is itself a coordination solution: standardized templates, familiar procedures, predictable liability structures, access to legitimacy and capital. They have arbitrage options (can shift resources, form subsidiaries, adapt rules in their favor). From their view, the constraint is primarily coordination — the clustering of alternatives into marginal spaces rather than mainstream creates a clear market boundary they control. Low or negative experienced extraction.
constraint_indexing:constraint_classification(alternative_institution_clustering, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TECHNOLOGY-ENABLED DECENTRALIZATION (SCAFFOLD) — Blockchain, peer-to-peer networks, open-source governance models, and distributed coordination tools are creating temporary pathways around incumbent institutional clustering. These tools lower barriers to alternative institution formation by reducing dependence on centralized intermediaries. However, the scaffold has a sunset clause: as technologies mature and incumbent institutions co-opt decentralization rhetoric (corporate blockchain, regulated stablecoins, open-source enterprise licensing), the technological advantage diminishes. The decentralization wave is temporary support, not permanent liberation.
constraint_indexing:constraint_classification(alternative_institution_clustering, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DIVERSITY-AND-INCLUSION THEATER (PITON) — Incumbent institutions maintain rhetorical commitment to 'supporting alternative models,' 'enabling social entrepreneurship,' and 'fostering ecosystem diversity' while materially suppressing alternatives through regulatory gatekeeping and resource hoarding. The diversity narrative is performative: it allows incumbents to claim alignment with innovation culture while extracting the legitimacy from alternatives without ceding structural power. Theater ratio (0.68) reflects the gap between stated commitment and actual resource allocation. The constraint persists through institutional inertia and the legitimacy value of appearing progressive.
constraint_indexing:constraint_classification(alternative_institution_clustering, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY GATEKEEPER (TANGLED ROPE) — Regulatory bodies and standard-setting organizations (SEC, professional licensing boards, corporate law frameworks) genuinely coordinate market function and consumer protection. But they also extract through regulatory capture: they enforce templates that benefit incumbents, they suppress alternative governance forms as inherently risky despite evidence of safety, and they create compliance barriers that only large actors can navigate. Exit constrained — regulators cannot simply deregulate without real coordination costs. But they possess agency and benefit from the current clustering, so their extraction is less severe than snare-level.
constraint_indexing:constraint_classification(alternative_institution_clustering, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, institutional clustering around successful templates may appear as an irreducible feature of large-scale coordination: larger institutions are more reliable, predictable, and resilient; alternative forms will always be marginal because they lack these properties. This perspective sees the clustering as a natural law of institutional ecology. However, this represents naturalization of what are actually contingent institutional choices: the 'success' of incumbents partly derives from their regulatory advantage, not purely from superior coordination. The mountain classification is a false summit, masking extractive mechanisms as natural necessity.
constraint_indexing:constraint_classification(alternative_institution_clustering, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alternative_institution_clustering_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alternative_institution_clustering, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alternative_institution_clustering, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(alternative_institution_clustering, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(alternative_institution_clustering, TR),
    TR >= 0.70.

:- end_tests(alternative_institution_clustering_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. Founders face near-total resource extraction (unable to access capital, talent, legitimacy without conforming); organized coalitions face moderate extraction through regulatory barriers and market exclusion. The average extractiveness reflects both the severe case (founder = snare-level) and moderate cases (organized coalitions = tangled rope). The 15-year trajectory shows extractiveness increasing as regulatory frameworks adapt to suppress alternatives more precisely. Suppression (0.65): High. Multiple suppression mechanisms: legal prohibition of non-standard governance (forcing alternatives to adopt incumbent templates or remain unlegalized), resource monopolization (venture capital, bank lending, institutional investment flow almost exclusively to incumbent-template startups), legitimacy exclusion (alternatives dismissed as 'unproven' while incumbents benefit from halo effects), and knowledge suppression (successful alternative cases are not publicized to maintain narrative of inevitable incumbent dominance). Theater ratio (0.68): Moderately high and rising. Incumbent institutions increasingly adopt rhetoric of supporting alternatives ('ecosystem enablement,' 'supporting founders,' 'celebrating innovation') while maintaining material gatekeeping. This gap between stated support and structural barriers creates performative content. The rise from 0.35 to 0.73 reflects deepening sophistication of co-optation narratives.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates stark perspectival divergence. Founders see a pure snare: complete institutional capture with no path to alternatives. Organized coalitions see mixed coordination and extraction (tangled rope): the shared infrastructure and legitimacy they build coordinates real work but is actively suppressed. Incumbents see coordination (rope): the clustering solves their genuine need for predictable templates and access to trusted capital flows. Regulators see tangled rope: they coordinate legitimate consumer protection but also enforce incumbent advantage through capture. Decentralization technologists see a temporary problem with a sunset (scaffold): blockchain and peer-to-peer models bypass incumbent gatekeeping but are being co-opted. Incumbent diversity narratives see piton: the institutional rhetoric persists through inertia even as material gatekeeping deepens. The analytical observer risks seeing a mountain: inevitable institutional concentration reflecting genuine coordination advantages. The perspectival gap reveals that what appears as natural institutional concentration from the analytical view is actually extractive suppression from the powerless view.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values reflect the agent's structural position within the clustering extraction flow. Founders with no exit options are trapped victims: d ≈ 0.95, yielding high f(d) and experienced extraction chi. Organized coalitions have constrained exit (can shift to informal structures but at organizational cost): d ≈ 0.60, moderate f(d). Incumbents with arbitrage options (can adapt, relocate, reshape regulations): d ≈ 0.10, low f(d), low or negative chi. Regulators are constrained (cannot deregulate without coordination costs) but benefit: d ≈ 0.35, moderate f(d). The analytical observer at civilizational scope risks d ≈ 0.5 (symmetric cost-benefit), which masks the asymmetry actually visible at shorter time horizons where founders bear concentrated costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that institutional clustering is neither pure coordination nor pure extraction but structurally hybrid (tangled rope) with different classification outcomes depending on the observer's position. The mandate — 'support institutional diversity' — is simultaneously truthful and deceptive: incumbents genuinely coordinate legitimate functions (the rope part), but they simultaneously extract from would-be alternatives through gatekeeping (the snare part). The mandatrophy is resolved by observing that institutional clustering solves a real coordination problem (standardization reduces transaction costs) while simultaneously enabling extractive moats (incumbent advantage is greater than would occur in an open market). The false summit (mountain) classification from the analytical view is the signal: when civilization-scale observers naturalize what are actually contingent regulatory choices as inevitable institutional laws, the analytical frame itself has been captured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_gatekeeping_boundary,
    'At what point does legitimate regulatory coordination become illegitimate gatekeeping of alternative institutional forms?',
    'Comparative analysis of safety outcomes: do alternative governance structures actually produce worse consumer protection or worker safety than incumbents? Cross-national analysis of jurisdictions with different regulatory stances toward alternatives. Longitudinal tracking of failure rates.',
    'If alternatives fail at higher rates: suppression may be justified as coordination cost. If failure rates are comparable: suppression is revealed as pure extraction. Current evidence is suppressed by regulatory gatekeepers themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_gatekeeping_boundary, empirical, 'Boundary between legitimate coordination and illegitimate gatekeeping').

omega_variable(
    technology_enablement_permanence,
    'Are blockchain, peer-to-peer, and decentralized coordination tools genuinely creating permanent alternatives or are they temporary scaffolds that will be co-opted by incumbents?',
    '10-year trajectory analysis: do alternative institutions built on decentralized tech maintain independence or do incumbents integrate/regulate them into standard frameworks? Measurement of power concentration on supposedly decentralized platforms over time.',
    'If permanent: scaffold classification is aspirational. If co-opted: scaffold sunset is real, and alternatives face renewed clustering after technology integration. This determines whether generational exit is possible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technology_enablement_permanence, empirical, 'Whether decentralized technology enables permanent institutional alternatives').

omega_variable(
    suppression_mechanism_origin,
    'Is alternative institution clustering driven primarily by incumbent active suppression or by genuine coordination advantages of scale?',
    'Counterfactual analysis: measure growth rates of alternatives in jurisdictions with lighter regulatory barriers vs jurisdictions with active suppression. Natural experiments where regulatory stance shifts. Measurement of alternative institution viability in resource-rich vs resource-poor startup environments.',
    'If primarily suppression: Snare classification from powerless perspective is accurate. If primarily coordination advantage: clustering reflects genuine efficiency, and alternatives face inherent limits rather than extractive barriers. This determines whether the constraint is mutable through policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_origin, empirical, 'Whether clustering is driven by suppression or genuine coordination advantages').

omega_variable(
    institutional_capture_of_alternatives,
    'Do incumbent institutions systematically co-opt successful alternative institutional models, integrating them as subsidiaries or regulated variants?',
    'Historical case analysis: track which alternative institutional innovations (corporate cooperatives, B-corporations, benefit LLCs, platform cooperatives) were successfully co-opted vs maintained independence. Measurement of legitimacy extraction: do alternatives that are legalized/integrated continue to function as originally designed?',
    'If systematic co-optation: this is an additional extraction mechanism beyond resource gatekeeping. The snare-level extraction for founders reflects not just market barriers but active institutional predation. This adds a layer to the tangled_rope perspective for organized coalitions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_of_alternatives, empirical, 'Systematic co-optation of alternative institutional innovations by incumbents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alternative_institution_clustering, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(altinst_tr_t0, alternative_institution_clustering, theater_ratio, 0, 0.35).
narrative_ontology:measurement(altinst_tr_t5, alternative_institution_clustering, theater_ratio, 5, 0.52).
narrative_ontology:measurement(altinst_tr_t10, alternative_institution_clustering, theater_ratio, 10, 0.68).
narrative_ontology:measurement(altinst_tr_t15, alternative_institution_clustering, theater_ratio, 15, 0.73).

% Extraction over time
narrative_ontology:measurement(altinst_be_t0, alternative_institution_clustering, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(altinst_be_t5, alternative_institution_clustering, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(altinst_be_t10, alternative_institution_clustering, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(altinst_be_t15, alternative_institution_clustering, base_extractiveness, 15, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alternative_institution_clustering, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(alternative_institution_clustering, 0.25).
narrative_ontology:affects_constraint(alternative_institution_clustering, regulatory_capture_dynamics).
narrative_ontology:affects_constraint(alternative_institution_clustering, path_dependent_institutional_selection).
narrative_ontology:affects_constraint(alternative_institution_clustering, commons_tragedy_and_coordination_failure).

% DUAL FORMULATION NOTE:
% Alternative institution clustering is downstream of specific regulatory regimes (which have their own extractiveness values) and specific technological enablement contexts (which have different extractiveness values). This story represents the meta-constraint that binds these specific constraints together. Decomposition by domain (alternative finance vs alternative labor vs alternative governance) would yield separate constraint stories with different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(alternative_institution_clustering, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
