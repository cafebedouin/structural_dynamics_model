% ============================================================================
% CONSTRAINT STORY: direct_democracy__swiss_referendum_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_direct_democracy__swiss_referendum_system, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: direct_democracy__swiss_referendum_system
 *   human_readable: Swiss Direct Democracy: Initiative and Referendum as Standing Fourth Branch
 *   domain: political/comparative_governance
 *
 * SUMMARY:
 *   The Swiss direct democracy system — featuring initiative and referendum
 *   as standing mechanisms available to circumvent, approve, or block any law
 *   — is one institutionalized reading of how direct democracy operates. This
 *   constraint models the Swiss system as a structured mechanism of
 *   collective decision-making that preserves legislator agency while
 *   subjecting that agency to standing popular review. The system is
 *   presented as functional: initiatives and referenda allow the mobilizable
 *   publics to serve as a fourth branch, initiating legislation directly (8%
 *   signature threshold) or forcing popular votes on enacted laws (50% +
 *   canton support). The beneficiaries of this arrangement are the
 *   mobilizable publics (who gain standing veto power) and consensus-politics
 *   operators (who benefit from laws that survive public scrutiny). The
 *   victims are swift parliamentary action and executive prerogative — any
 *   law may face referendum, and legislatures must pre-position to
 *   anticipated popular sentiment. The constraint exhibits the full
 *   perspectival range: legislators experience it as a snare (trapped in
 *   perpetual conditional authority); the institutional apparatus sees pure
 *   coordination (rope); organized initiative committees and voter publics
 *   experience tangled extraction-coordination hybrids; the cantonal
 *   ballot-submission ritual has become performative (piton); and the
 *   analytical observer risks naturalizing this contingent institutional
 *   choice as an immutable feature of democratic legitimacy (false summit
 *   mountain). The measurement trajectory shows extractiveness and
 *   suppression rising gradually (from 0.22 and 0.42 in early 20th century to
 *   0.38 and 0.48 in 2020s), reflecting accumulating pressure as politics
 *   polarized, campaign costs rose, and deliberative quality declined.
 *   Theater_ratio remains low (0.35 in 2020s), indicating that despite
 *   polarization, the referendum mechanism retains genuine deliberative
 *   function — voters are not simply ratifying predetermined outcomes. This
 *   constraint is ONE READING of the contested 'direct_democracy' kernel. Two
 *   sibling readings — 'plebiscitary_capture' and 'town_meeting_tradition' —
 *   model alternative interpretations of direct democratic legitimacy.
 *
 * KEY AGENTS:
 *   - Mobilizable Publics / Voters (organized/mobile): Primary beneficiary — gain standing fourth-branch veto; also partially victimized (constrained by complexity and ballot fatigue)
 *   - Legislatures / Parliament (institutional/arbitrage): Primary victim — all laws subject to referendum; also partially beneficiary (referendum legitimacy enhances law stability)
 *   - Initiative Committees / Organized Minorities (organized/mobile): Secondary beneficiary — can initiate legislation directly; also extraction vector (resource asymmetry in signature collection and campaign costs)
 *   - Cantonal Authorities (institutional/arbitrage): Enforcer of the referendum apparatus; maintains procedural legitimacy
 *   - Federal Council / Executive (institutional/arbitrage): Subject to standing review; constrained but also gains legitimacy through submission to popular review
 *   - Analytical Observer (analytical/analytical): Risks naturalizing the Swiss design as the universal model rather than recognizing it as one contingent reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(direct_democracy__swiss_referendum_system, 0.38).
domain_priors:suppression_score(direct_democracy__swiss_referendum_system, 0.48).
domain_priors:theater_ratio(direct_democracy__swiss_referendum_system, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(direct_democracy__swiss_referendum_system, extractiveness, 0.38).
narrative_ontology:constraint_metric(direct_democracy__swiss_referendum_system, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(direct_democracy__swiss_referendum_system, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(direct_democracy__swiss_referendum_system, tangled_rope).
narrative_ontology:human_readable(direct_democracy__swiss_referendum_system, "Swiss Direct Democracy: Initiative and Referendum as Standing Fourth Branch").
narrative_ontology:topic_domain(direct_democracy__swiss_referendum_system, "political/comparative_governance").

domain_priors:requires_active_enforcement(direct_democracy__swiss_referendum_system).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(direct_democracy__swiss_referendum_system, '80298e9c-6dc5-412d-b052-35aba8915b6b').
narrative_ontology:cs_kernel_codification('80298e9c-6dc5-412d-b052-35aba8915b6b', formalized).
narrative_ontology:cs_authority_grounding('80298e9c-6dc5-412d-b052-35aba8915b6b', lineage).
narrative_ontology:cs_interpretation_layer_present('80298e9c-6dc5-412d-b052-35aba8915b6b').
narrative_ontology:cs_reading_relation('80298e9c-6dc5-412d-b052-35aba8915b6b', direct_democracy__plebiscitary_capture, coexists_with).
narrative_ontology:cs_reading_relation('80298e9c-6dc5-412d-b052-35aba8915b6b', direct_democracy__town_meeting_tradition, influences).
narrative_ontology:cs_axiom('80298e9c-6dc5-412d-b052-35aba8915b6b', foundational, standing_institutionalized_veto_preserves_coordination).
narrative_ontology:cs_axiom_status(standing_institutionalized_veto_preserves_coordination, holdable).
narrative_ontology:cs_axiom_grounding('80298e9c-6dc5-412d-b052-35aba8915b6b', standing_institutionalized_veto_preserves_coordination, instrumental).
narrative_ontology:cs_axiom('80298e9c-6dc5-412d-b052-35aba8915b6b', secondary, federal_scale_consensus_norms_prevent_plebiscitary_capture).
narrative_ontology:cs_axiom_status(federal_scale_consensus_norms_prevent_plebiscitary_capture, holdable).
narrative_ontology:cs_axiom_grounding('80298e9c-6dc5-412d-b052-35aba8915b6b', federal_scale_consensus_norms_prevent_plebiscitary_capture, empirically_contingent).
narrative_ontology:cs_reference_frame('80298e9c-6dc5-412d-b052-35aba8915b6b', federal_democratic_legitimacy_through_popular_standing_veto).
narrative_ontology:cs_drift_state('80298e9c-6dc5-412d-b052-35aba8915b6b', contemporary_polarized_politics, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('80298e9c-6dc5-412d-b052-35aba8915b6b', '').
narrative_ontology:cs_kernel_id(direct_democracy__swiss_referendum_system, direct_democracy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(direct_democracy__swiss_referendum_system, mobilizable_publics).
narrative_ontology:constraint_beneficiary(direct_democracy__swiss_referendum_system, consensus_politics_operators).
narrative_ontology:constraint_victim(direct_democracy__swiss_referendum_system, swift_parliamentary_action).
narrative_ontology:constraint_victim(direct_democracy__swiss_referendum_system, executive_prerogative).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGISLATOR FACING REFERENDUM (SNARE) — Parliament enacts law; faces mandatory submission to referendum or vulnerability to initiative challenge. No exit: every law carries the risk of popular veto. Trapped in perpetual conditional authority — legislative action is extractively burdened by the standing threat of recall. High experienced extraction chi; low agency.
constraint_indexing:constraint_classification(direct_democracy__swiss_referendum_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONALIZED REFERENDUM APPARATUS (ROPE) — Parliament, cantons, initiative machinery as a coordinated system. The referendum is functional: it ensures laws survive popular scrutiny, channels dissent into formal petition rather than street action, distributes power across branches without zero-sum extraction. Net beneficiary through systemic legitimacy. The apparatus experiences the constraint as pure coordination — a solved collective action problem.
constraint_indexing:constraint_classification(direct_democracy__swiss_referendum_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZED PUBLICS / INITIATIVE COMMITTEES (TANGLED ROPE) — Non-state actors that can initiate legislation directly (8% signature threshold). Genuine power to reshape law, but extraction emerges: initiative campaigns require resource mobilization; media access is asymmetric; small organized minorities can trigger expensive referendum cycles on low-salience issues. Mixed: real coordination function (populace has agency) plus real extraction (resource-asymmetric gatekeeping).
constraint_indexing:constraint_classification(direct_democracy__swiss_referendum_system, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: MOBILIZABLE PUBLICS / VOTERS (TANGLED ROPE) — Citizens as the standing fourth branch. Coordinate legitimate outcomes through voting; also extracted: most never participate in initiative campaigns, yet bear costs (time spent on frequent votes, polarization during campaigns, complex technical issues reduced to binary ballots). Moderate extraction — constrained by complexity and civic fatigue even with access rights.
constraint_indexing:constraint_classification(direct_democracy__swiss_referendum_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CANTONAL REFERENDUM RITUAL (PITON) — Many cantonal laws face mandatory referendum despite high passage likelihood (>80% in routine cases). The ritual persists through institutional inertia; the ballot submission is largely performative. Theater_ratio high because the outcome is usually predetermined, yet the formal machinery must cycle. Maintains legitimacy claim through participation ritual rather than genuine deliberation.
constraint_indexing:constraint_classification(direct_democracy__swiss_referendum_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational perspective, the constraint appears to be an immutable feature of democratic legitimacy: any government action requires the consent of the governed, which can only be tested through holding the governed's standing option to say no. This perspective risks naturalizing what is actually a contingent institutional choice about WHERE to place the veto point.
constraint_indexing:constraint_classification(direct_democracy__swiss_referendum_system, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(direct_democracy__swiss_referendum_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(direct_democracy__swiss_referendum_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(direct_democracy__swiss_referendum_system, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(direct_democracy__swiss_referendum_system, TR),
    TR >= 0.70.

:- end_tests(direct_democracy__swiss_referendum_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Swiss system is fundamentally a coordination mechanism — it solves the collective action problem of ensuring laws align with popular sentiment — but extraction emerges in multiple forms: (1) legislator freedom is constrained by anticipation of referendum veto (they lose the option of bold action); (2) voter attention is extracted through frequent ballot participation (8-10 referenda per year per Swiss adult); (3) initiative signature collection creates resource barriers (organized minorities can trigger expensive campaigns, diffuse majorities cannot). The measurement trajectory shows rising extractiveness over time (0.22 → 0.38), reflecting that as campaign costs rose and politics polarized, the extraction components intensified while the pure coordination remained stable. Suppression (0.48): Moderate-high. Multiple suppression mechanisms: (1) complex technical law cannot be reduced to binary ballot questions, suppressing nuanced policy options; (2) veto threat suppresses legislative experimentation (risk-averse legislatures pre-position); (3) information asymmetries between campaign-resourced organized interests and diffuse voters suppress participation quality. Theater ratio (0.35): Low-moderate. The referendum retains genuine deliberative function — outcomes are not predetermined, voters engage with substantive arguments, turnout is meaningful (>50% typical). However, minor theater emerges in cantonal mandatory referenda on routine laws (>80% passage rate despite full participation ritual). The low theater reflects that Switzerland's referendum system has preserved deliberation better than many other direct-democracy implementations. Claimed type is Tangled Rope: genuine coordination function (laws must survive public scrutiny) + genuine asymmetric extraction (legislator prerogative and voter attention).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal across the six perspectives. Legislatures experience the constraint as a snare (trapped in perpetual conditional authority); the institutional apparatus experiences pure rope (elegant coordination solution); mobilizable publics experience tangled rope (gain real power but also real burden); organized minorities experience leverage (arbitrage-style extraction of attention and resources); cantonal ritual keepers experience piton (performing legitimacy through ballot submission); the analytical observer risks mountain (naturalizing as immutable democratic requirement). The gap reveals that 'direct democracy' is not a unified phenomenon but a configuration that maps differently depending on the actor's structural position relative to the veto power. The legislator sees constraint; the system designer sees solution; the voter sees burden; the organized minority sees opportunity; the ritual performer sees theater; the philosopher sees law. All six perspectives are structurally valid readings of the same institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation follows from each agent's structural position. Legislatures: beneficiary of legitimacy → d=0.15, but victims of veto threat → override to d=0.65 (actual structural relationship as victim of suppressed action). Mobilizable publics: beneficiaries of fourth-branch power → d=0.20, but partially victims of complexity burden → override to d=0.55 (moderate extraction experience). Initiative committees: beneficiaries of direct agenda access → d=0.10, but extractors through resource asymmetry → override to d=0.40 (asymmetric benefit). Institutional apparatus: beneficiary of legitimate coordination mechanism → d=0.05 (net beneficiary, low extraction). The canonical fallback (institutional → d=0.00, f(d)≈-0.12) applies only when no structural data exists; here, all agents have explicit victim/beneficiary declarations, so derivation chain uses those inputs and produces the effective directionality for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the Swiss reading instantiates a genuine tangled_rope: coordination function is real (laws must survive public scrutiny; referendum mechanism channels dissent into formal petition; legitimacy is enhanced by popular submission) AND extraction is real (legislator prerogative is suppressed; voter attention is extracted; organized minorities can weaponize the initiative mechanism). The false summit (analytical/mountain) is a diagnostic signal: the 'direct democracy is immutable democratic law' framing naturalizes what is actually a contingent institutional choice about WHERE to place the veto point. The system could be reconfigured (raise signature thresholds, reduce referendum frequency, tier the mechanism by issue salience) — it is not a law of nature. The mandatrophy is resolved when all perspectives are held together: the system is functionally robust (rope aspects stabilize through legitimacy and consensus-seeking) yet structurally extractive (tangled_rope aspects emerge through resource asymmetry and attention burden). Neither reading alone is correct; the presheaf over the observation site is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    referendum_frequency_extraction_threshold,
    'At what referendum frequency does the coordination function degrade into fatigue-driven voter apathy and extraction of civic attention?',
    'Comparative analysis of turnout rates, issue comprehension, and legislative stability across cantons and countries with varying referendum frequencies (Switzerland 2-4 per year vs California 8-15 per election cycle vs others <1 per year)',
    'If threshold < 2 per year: Swiss frequency may already exceed optimal; classification shifts toward snare for voters. If threshold > 4 per year: current Swiss frequency remains within coordination zone; tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(referendum_frequency_extraction_threshold, empirical, 'Referendum frequency threshold for coordination function degradation').

omega_variable(
    initiative_signature_collection_asymmetry,
    'Does the 8% signature threshold create a structural filtering mechanism that favors organized minorities over diffuse majorities, or does it effectively democratize agenda-setting?',
    'Historical analysis of initiatives that failed to reach signature threshold vs those that succeeded; comparison of signature collection costs and campaign resource requirements across issue types; identification of which actor types routinely achieve threshold',
    'If filtering favors organized minorities: extractiveness increases toward 0.50+ (resource asymmetry is real). If threshold truly opens agenda: extractiveness decreases toward 0.25 (coordination dominated).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(initiative_signature_collection_asymmetry, empirical, 'Whether initiative signature threshold democratizes or restricts agenda-setting').

omega_variable(
    reading_kernel_contest_plebiscitary_capture,
    'Can the Swiss referendum system be captured by a unified executive wielding the initiative power as a plebiscitary tool, converting standing fourth-branch review into autocratic acclamation?',
    'Historical case analysis: identification of executives (federal council members, canton governors) using initiative/referendum strategically to bypass legislative deliberation; measurement of initiative proposals from executive sources and their passage rates; comparison to opposition-initiated referenda',
    'If capture pathway is viable and has occurred: the swiss_referendum_system reading does not foreclose plebiscitary_capture — they coexist as rival interpretations depending on executive restraint. If capture proves structurally blocked: readings are in direct logical contest (foreclose relation applies).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest_plebiscitary_capture, empirical, 'Whether Swiss referendum can be captured as plebiscitary tool (kernel contest with plebiscitary_capture reading)').

omega_variable(
    scale_dependency_town_meeting_vs_referendum,
    'Do the coordination benefits attributed to the Swiss referendum derive from institutional design or from Switzerland''s specific scale (8.6M population), cultural homogeneity, and literate electorate? Would the same mechanism preserve coordination at larger scales or in more diverse polities?',
    'Comparative analysis: referendum outcomes in larger direct-democracy systems (California: 39M people, higher diversity) vs Swiss cantons; measurement of comprehension rates, deliberative quality, and extraction outcomes as function of scale; identification of breakpoint where coordination degrades',
    'If scale-dependent: the swiss reading describes a contingent coordination arrangement that cannot generalize (affects validity of reading for other contexts). If scale-invariant: reading claims universal coordination principle. Directly relevant to kernel contest with town_meeting_tradition (which assumes small scale as necessary condition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_dependency_town_meeting_vs_referendum, empirical, 'Scale-dependency of Swiss referendum coordination benefits').

omega_variable(
    legislative_pre_positioning_circumvention,
    'Do Swiss legislatures structurally pre-position laws to avoid referendum challenges by anticipating popular sentiment, thereby capturing direct democracy as a hidden extraction mechanism (legislatures extract the benefit of knowing what voters will accept, at the cost of bold action)?',
    'Comparison of legislative boldness (distance from median voter preference) in Swiss vs non-referendum parliamentary systems; analysis of rejected initiatives and referenda to identify whether legislative pre-positioning correctly predicted outcomes; measurement of policy space foreclosure attributable to anticipated referenda',
    'If pre-positioning is significant: extractiveness of parliamentary freedom increases (victim set is swift action); classification may shift toward higher-extraction tangled_rope or snare depending on degree. If negligible: coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_pre_positioning_circumvention, empirical, 'Whether legislative pre-positioning constitutes hidden extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(direct_democracy__swiss_referendum_system, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dd_swiss_theater_t0_1900s, direct_democracy__swiss_referendum_system, theater_ratio, 0, 0.28).
narrative_ontology:measurement(dd_swiss_theater_t1_1970s, direct_democracy__swiss_referendum_system, theater_ratio, 3, 0.32).
narrative_ontology:measurement(dd_swiss_theater_t2_2020s, direct_democracy__swiss_referendum_system, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(dd_swiss_ext_t0_1900s, direct_democracy__swiss_referendum_system, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(dd_swiss_ext_t1_1970s, direct_democracy__swiss_referendum_system, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(dd_swiss_ext_t2_2020s, direct_democracy__swiss_referendum_system, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dd_swiss_supp_t0_1900s, direct_democracy__swiss_referendum_system, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(dd_swiss_supp_t1_1970s, direct_democracy__swiss_referendum_system, suppression_requirement, 3, 0.45).
narrative_ontology:measurement(dd_swiss_supp_t2_2020s, direct_democracy__swiss_referendum_system, suppression_requirement, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(direct_democracy__swiss_referendum_system, enforcement_mechanism).
narrative_ontology:affects_constraint(direct_democracy__swiss_referendum_system, direct_democracy__plebiscitary_capture).
narrative_ontology:affects_constraint(direct_democracy__swiss_referendum_system, direct_democracy__town_meeting_tradition).

% DUAL FORMULATION NOTE:
% The direct_democracy kernel decomposes into three structurally distinct constraint stories: swiss_referendum_system (institutionalized standing mechanism, ε=0.38), plebiscitary_capture (executive weaponization of direct democracy, ε=0.72), and town_meeting_tradition (communal deliberation at assembly scale, ε=0.15). Each reading instantiates a different extractiveness profile and classification. The Swiss reading serves as the institutional coordination path; the plebiscitary reading shows how the same mechanisms could be captured; the town meeting reading shows the underlying deliberative logic at its native scale. All three link via network.affects_constraints to reflect their kernel family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(direct_democracy__swiss_referendum_system, powerless, 0.65).
constraint_indexing:directionality_override(direct_democracy__swiss_referendum_system, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
