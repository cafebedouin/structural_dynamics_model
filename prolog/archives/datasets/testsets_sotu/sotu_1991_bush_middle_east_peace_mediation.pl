% ============================================================================
% CONSTRAINT STORY: sotu_1991_bush_middle_east_peace_mediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1991_bush_middle_east_peace_mediation, []).

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
 *   constraint_id: sotu_1991_bush_middle_east_peace_mediation
 *   human_readable: U.S. Mediation Framework for Middle East Peace (UN Resolutions 242/338)
 *   domain: foreign_policy/conflict_resolution
 *
 * SUMMARY:
 *   The U.S.-mediated peace process for the Middle East, formalized in the
 *   Madrid Conference (1991) and anchored to UN Security Council Resolutions
 *   242 (land-for-peace principle) and 338 (ceasefire framework), establishes
 *   a diplomatic constraint that simultaneously enables coordination and
 *   extracts from non-dominant actors. The framework positions the United
 *   States as the neutral third-party broker tasked with reconciling Israeli
 *   security demands (requiring territorial control and demilitarized zones)
 *   with Palestinian political recognition and Arab state normalization. The
 *   constraint benefits U.S. regional influence, moderate Arab states seeking
 *   stability, and Israel's security interests through formalized Arab
 *   recognition; it costs Palestinian political autonomy (locked into
 *   dependent status), Israeli settlement advocates (constrained from
 *   territorial expansion), and Arab rejectionists (excluded from negotiation
 *   and forced to accept mainstream leadership's commitment to the process).
 *   The theater ratio (0.58) reflects that while the framework invokes UN
 *   resolutions as legitimacy, the actual negotiation mechanism operates
 *   outside UN enforcement, making the resolutions substantially performative
 *   — invoked for legitimacy but not binding on implementation. Base
 *   extractiveness (0.52) indicates moderate extraction: genuine coordination
 *   problems are being solved (preventing war, creating bilateral frameworks)
 *   alongside asymmetric burden distribution, particularly on Palestinian
 *   agents who must concede territorial maximalism without receiving
 *   equivalent guarantees of political autonomy.
 *
 * KEY AGENTS:
 *   - United States: Primary beneficiary (institutional/arbitrage) — captures regional influence, positions as conflict resolution authority, prevents Soviet/Russian counter-positioning, demonstrates great-power mediation capacity. Low perceived cost.
 *   - Israeli Security Establishment: Constrained beneficiary (powerful/constrained) — receives formalized Arab recognition and security guarantees but must accept constraints on settlement expansion and acknowledge Palestinian political entity. Mixed coordination-extraction experience.
 *   - Palestinian Authority: Primary victim (powerless/trapped) — trapped within framework requiring territorial concessions in exchange for politically conditioned autonomy. High suppression: lacks independent leverage, faces security dependence on Israeli/U.S. actors, exits would forfeit all recognition.
 *   - Arab Moderate States (Egypt, Jordan): Constrained victims (moderate/trapped) — trapped between U.S. pressure to normalize and domestic rejectionist opposition. High suppression: vulnerable to U.S. sanctions and internal destabilization.
 *   - Israeli Settlement Movement: Rejectionist victim (powerful/constrained) — must coordinate within constraint (cannot prevent negotiations) while extracting from it (using negotiation as cover for consolidation). High suppression through political marginalization.
 *   - Arab Rejectionists: Excluded victim (organized/trapped) — forced to accept mainstream leadership's commitment to mediation without formal voice. High suppression: cannot prevent process, face internal isolation within Arab politics.
 *   - International Law Architecture (UN): Institutional actor (institutional/constrained) — provides nominal legitimacy through resolutions but has no enforcement role. Piton classification reflects degraded function maintained through performance.
 *   - Civil Peace Movements: Organized agents with agency (organized/constrained) — see framework as temporary support with genuine sunset logic (organic normalization). Lower effective extraction because of visible alternative pathways and organizing capacity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1991_bush_middle_east_peace_mediation, 0.52).
domain_priors:suppression_score(sotu_1991_bush_middle_east_peace_mediation, 0.68).
domain_priors:theater_ratio(sotu_1991_bush_middle_east_peace_mediation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1991_bush_middle_east_peace_mediation, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1991_bush_middle_east_peace_mediation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sotu_1991_bush_middle_east_peace_mediation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1991_bush_middle_east_peace_mediation, tangled_rope).
narrative_ontology:human_readable(sotu_1991_bush_middle_east_peace_mediation, "U.S. Mediation Framework for Middle East Peace (UN Resolutions 242/338)").
narrative_ontology:topic_domain(sotu_1991_bush_middle_east_peace_mediation, "foreign_policy/conflict_resolution").

domain_priors:requires_active_enforcement(sotu_1991_bush_middle_east_peace_mediation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1991_bush_middle_east_peace_mediation, united_states_regional_influence).
narrative_ontology:constraint_beneficiary(sotu_1991_bush_middle_east_peace_mediation, moderate_regional_actors).
narrative_ontology:constraint_beneficiary(sotu_1991_bush_middle_east_peace_mediation, international_conflict_resolution_framework).
narrative_ontology:constraint_victim(sotu_1991_bush_middle_east_peace_mediation, palestinian_political_autonomy).
narrative_ontology:constraint_victim(sotu_1991_bush_middle_east_peace_mediation, israeli_settlement_advocates).
narrative_ontology:constraint_victim(sotu_1991_bush_middle_east_peace_mediation, arab_rejectionists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN POLITICAL AUTHORITY (SNARE) — Trapped within U.S.-mediated framework that requires conceding territorial maximalism in exchange for recognition that remains conditioned on security guarantees controlled by Israel and the U.S. High suppression: lack of independent leverage, asymmetric security arrangements, threat of negotiation collapse. Cannot exit without forfeiting all political recognition. Maximum extraction experienced.
constraint_indexing:constraint_classification(sotu_1991_bush_middle_east_peace_mediation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ISRAELI SECURITY ESTABLISHMENT (TANGLED ROPE) — Constrained by U.S. strategic commitment and international legitimacy requirements, but also benefits from the framework: U.S. security guarantees, formalized Arab recognition of Israeli statehood, regional stability reducing terror/rocket threat. Experiences genuine coordination (shared interest in preventing conflict escalation) alongside extraction (must cede settlement expansion, accept Palestinian political entity). Significant agency but genuine cost.
constraint_indexing:constraint_classification(sotu_1991_bush_middle_east_peace_mediation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UNITED STATES DIPLOMATIC AUTHORITY (ROPE) — Primary beneficiary. Benefits from positioning as neutral arbiter, regional influence through mediation, prevention of Soviet/Russian counter-positioning, demonstration of conflict resolution capacity. Extraction runs toward this agent. Low perceived cost — the U.S. is not a territorial claimant. Can arbitrage between competing regional actors and international legitimacy.
constraint_indexing:constraint_classification(sotu_1991_bush_middle_east_peace_mediation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ARAB MODERATE STATES (SNARE) — Trapped between U.S. pressure to normalize Israeli relations and domestic political pressure from rejectionist movements and Palestinian constituencies. High suppression: vulnerable to both U.S. sanctions and internal destabilization. Cannot credibly exit without catastrophic regional conflict or U.S. abandonment. Benefits from framework (reduced war risk) are unequally distributed against domestic costs (legitimacy damage, opposition organizing).
constraint_indexing:constraint_classification(sotu_1991_bush_middle_east_peace_mediation, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL LAW / UN ARCHITECTURE (PITON) — UN Resolutions 242/338 provide the nominal framework, but the actual mechanism bypasses UN enforcement entirely. Theater ratio high: the resolutions are invoked as legitimacy cover for U.S./Israeli/Arab negotiation; the UN Security Council has no enforcement role in the mediation process. International law appears as functional but is substantially performative — territorial interpretation, security guarantees, and implementation are determined outside UN mechanisms. Piton classification reflects degraded function maintained through institutional inertia (invoking UN resolutions as legitimacy ritual despite their non-binding status in actual negotiations).
constraint_indexing:constraint_classification(sotu_1991_bush_middle_east_peace_mediation, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CIVIL PEACE MOVEMENT (SCAFFOLD) — Organized agents (Israeli Peace Now, Palestinian civil society, Jordanian/Egyptian peace constituencies) see the mediation framework as temporary support structure with potential sunset: grassroots normalization, economic interdependence, shared civil institutions could eventually supersede top-down negotiation. Low effective extraction because these agents have agency (organizing capacity, legitimacy for alternative peace-building) and visible exit path (movement from mediated to organic peace mechanisms). Theater acceptable because the sunset logic is visible.
constraint_indexing:constraint_classification(sotu_1991_bush_middle_east_peace_mediation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: REJECTIONISTS ON BOTH SIDES (TANGLED ROPE) — Palestinian rejectionists and Israeli settlement advocates are forced to coordinate through the constraint (cannot prevent negotiations, must acknowledge the framework as binding on mainstream leadership) while simultaneously extracting from it (using negotiation process as cover for territorial consolidation, delegitimation of opposite side, movement organizing). High suppression: excluded from formal mediation, constrained by their mainstream leadership's commitment to the process. Mixed experience: constrained by framework rules but also using framework rules to advance maximalist position.
constraint_indexing:constraint_classification(sotu_1991_bush_middle_east_peace_mediation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (NATURAL LAW VIEW) (MOUNTAIN) — From a civilizational perspective, some form of third-party mediation appears inevitable in asymmetric territorial conflicts: the parties' maximalist positions are logically irreconcilable, and a neutral arbiter with enforcement capacity is a structural necessity for any negotiated resolution. This perspective risks naturalizing what is actually a specific institutional choice (U.S. mediation) as inevitable architectural requirement. The engine will likely detect this as a false summit.
constraint_indexing:constraint_classification(sotu_1991_bush_middle_east_peace_mediation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1991_bush_middle_east_peace_mediation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1991_bush_middle_east_peace_mediation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1991_bush_middle_east_peace_mediation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1991_bush_middle_east_peace_mediation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1991_bush_middle_east_peace_mediation, TR),
    TR >= 0.70.

:- end_tests(sotu_1991_bush_middle_east_peace_mediation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The framework solves genuine coordination problems (preventing war escalation, creating bilateral recognition, establishing negotiation channels) but distributes costs asymmetrically. Palestinians bear maximum costs (territorial concession, autonomy dependence, security subordination) in exchange for recognition that remains conditioned on Israeli security satisfaction. Suppression rises over time (0.38→0.52) as settlement expansion continues despite framework constraints, suggesting the framework provides cover for consolidation rather than actual constraint. Suppression (0.68): High. Structural barriers prevent exit: Palestinians cannot leave without forfeiting all political recognition; Arab states cannot exit without U.S. sanctions; even rejectionist movements are structurally forced to operate within the framework's rules (whether they participate or resist). Theater ratio (0.58): Moderate-high, rising initially then stabilizing. The framework invokes UN resolutions as legitimacy performance, but actual enforcement occurs outside UN mechanisms through bilateral Israeli-Palestinian agreements and U.S.-brokered arrangements. Theater increases 1991-1997 as negotiation becomes more complex and performative (Camp David talks, multiple failed frameworks), then stabilizes as post-Camp David reality sets in. The measurements show theater rising faster than extractiveness initially (0.42→0.54 for theater vs 0.38→0.48 for extractiveness), indicating increasing performative content relative to coordination function — a signature of degradation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across power levels. The United States sees Rope: pure coordination problem (preventing war escalation, establishing bilateral recognition frameworks). The Israeli security establishment sees Tangled Rope: genuine coordination benefits (formalized Arab recognition, security guarantees, reduced terror risk) alongside constraints on settlement expansion and requirement to acknowledge Palestinian entity. Palestinian Authority sees Snare: pure extraction (forced territorial concession in exchange for politically dependent autonomy with no guarantee of independence or genuine sovereignty). Arab moderate states see Snare: trapped between U.S. pressure and domestic opposition, receiving stability benefits unequally distributed against legitimacy costs. Rejectionists see Tangled Rope: forced to coordinate within constraint mechanism (cannot prevent negotiations) while extracting from it (using negotiation failure as recruitment tool). Civil peace movements see Scaffold: temporary support structure with visible sunset logic (organic normalization pathways superseding top-down mediation). The analytical observer risks seeing Mountain (territorial conflict requiring neutral mediation is inevitable) but structural data reveals this as false summit: U.S. mediation is a specific institutional choice with identifiable beneficiaries, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status, power level, and exit options. United States experiences low d (0.15): institutional power + arbitrage exit + net beneficiary status → negative f(d) → low experienced extraction. Israel experiences moderate d (0.45): powerful agent but constrained by framework, benefits from recognition but costs from settlement constraints. Palestinian Authority experiences high d (0.92): powerless agent + trapped exit + victim status → high f(d) → maximum experienced extraction. Arab moderate states experience high d (0.88): moderate power but trapped by U.S. dependency, benefiting from stability but costing from legitimacy damage. Rejectionists experience d ≈ 0.75: organized power but constrained by exclusion from formal negotiation, forced to coordinate within framework constraints. Civil peace movements experience lower d (0.35): organized power + constrained exit + mixed beneficiary/victim status (benefit from normalization pathways, cost from political dependence) → moderate experienced extraction. The perspectival gap between institutional beneficiaries (U.S., moderate Arab states receiving security cooperation) and victim agents (Palestinians, rejectionists) is substantial: the same framework appears as pure Rope (coordination) to the U.S. and as Snare (pure extraction) to Palestinians.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint shows genuine coordination function (preventing war escalation, establishing diplomatic channels, creating bilateral frameworks) but systematically distributes extraction toward weaker parties. Classification as Tangled Rope is correct: χ (0.52 × 1.35 × 1.1 scope modifier) ≈ 0.77 places it in the Tangled Rope range (0.40-0.90). BUT the distribution of extraction is so asymmetric that a Palestinian observer would classify this as Snare. The mandatrophy is not resolvable by tweaking metrics — it reveals a fundamental tension: the constraint solves a real coordination problem (bilateral war prevention) by imposing a solution that extracts primarily from weaker parties (Palestinians must accept dependence). This is endemic to third-party mediation in asymmetric power conflicts. Either: (A) the coordination function is genuine and asymmetric extraction is a legitimate cost of preventing war (accept Tangled Rope and acknowledge the perspectival gap), or (B) the coordination function is secondary and the constraint is primarily an extraction mechanism (reclassify to Snare from analytical perspective). The omega variables on U.S. neutrality and settlement expansion enforcement will resolve this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_guarantees_enforcement,
    'Are U.S./Israeli security guarantees for Palestinian political entity structurally credible or dependent on continuous U.S. political will?',
    'Historical tracking of guarantee enforcement when challenged; comparison of stated vs actual implementation when Palestinian authority perceived to threaten Israeli security; U.S. political change analysis (turnover in administrations, shifts in strategic priorities)',
    'If credible: framework is genuine Tangled Rope for all parties (real coordination with asymmetric extraction). If politically contingent: framework is extractive Snare for Palestinians (guarantees evaporate when U.S. interest shifts or Israeli security narrative changes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_guarantees_enforcement, empirical, 'Credibility of U.S. security guarantees for Palestinian political entity').

omega_variable(
    settlement_expansion_as_extraction,
    'Does the negotiation framework actually constrain Israeli settlement expansion or does it provide cover for accelerated consolidation?',
    'Quantitative measurement of settlement area, housing units, and population growth during mediation intervals; comparison of expansion rate before/during/after negotiation phases; analysis of whether freeze agreements are enforced or violated without consequences',
    'If framework constrains expansion: genuine Tangled Rope (mixed coordination-extraction). If expansion accelerates during mediation: framework is Snare (provides legitimacy cover for territorial extraction masked as negotiation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settlement_expansion_as_extraction, empirical, 'Whether negotiation framework actually constrains Israeli settlement expansion').

omega_variable(
    palestinian_political_entity_type,
    'Is the framework designed to produce genuine Palestinian political autonomy (state or viable quasi-state) or administrative dependence on Israeli/U.S. security control?',
    'Text analysis of actual agreements produced; measurement of Palestinian control over: territory, security forces, taxation, foreign relations, resource allocation. Comparison with minimal threshold for recognized statehood or viable autonomy.',
    'If genuine autonomy: framework could resolve to pure Tangled Rope (mixed coordination-extraction with path to parity). If administrative dependence: framework resolves to Snare for Palestinians (extraction masked by autonomy theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_political_entity_type, conceptual, 'Type of political entity framework is designed to produce (genuine autonomy vs. administrative dependence)').

omega_variable(
    arab_state_alternative_leverage,
    'Do Arab moderate states retain genuine leverage to exit the mediation if terms become unacceptable, or does U.S. economic/security dependency make exit prohibitive?',
    'Analysis of economic sanctions, military aid, intelligence sharing, and regional positioning capabilities for states attempting exit or renegotiation; historical cases of state attempts to withdraw from framework and consequences (Egypt Anwar Sadat case, Jordan King Abdullah cases)',
    'If exit viable: Snare classification is overstated; should be Tangled Rope. If exit prohibitive: Snare confirmed; suppression is structural rather than strategic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_state_alternative_leverage, empirical, 'Whether Arab moderate states retain genuine leverage to exit or renegotiate').

omega_variable(
    rejectionist_underground_coordination,
    'Does the mediation framework suppress rejectionist movements or does it provide organizational cover that accelerates their organizing and recruitment?',
    'Comparative analysis of rejectionist movement size, funding, attack capability before/during/after periods of active mediation; examination of whether negotiation failures provide recruitment narratives (argument that negotiation is futile, only struggle works)',
    'If suppression effective: framework is constraining extraction for rejectionist agents. If underground organizing accelerates: framework is extraction mechanism masquerading as peace-building.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rejectionist_underground_coordination, empirical, 'Whether mediation framework suppresses or enables rejectionist movement organizing').

omega_variable(
    civil_peace_movement_genuine_alternatives,
    'Are the civil peace mechanisms (economic cooperation, cultural exchange, joint institutions) producing genuine alternative coordination paths or are they dependent on top-down political agreement?',
    'Historical tracking of civil peace projects that survived political breakdown (e.g., Israeli-Palestinian joint ventures, educational exchange programs). Measurement of organic normalization proceeds during negotiation breakdown periods.',
    'If genuine alternatives: Scaffold sunset logic is realistic; framework could transition to organic peace. If dependent on top-down agreement: Scaffold is aspirational theater; civil movements are secondary to political mediation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_peace_movement_genuine_alternatives, empirical, 'Whether civil peace mechanisms provide genuine alternative to top-down mediation').

omega_variable(
    us_mediation_neutrality,
    'Is U.S. mediation position genuinely neutral or is it structured to favor Israeli security interests and Arab state accommodation?',
    'Analysis of U.S. pressure campaigns, aid conditionality, and diplomatic recognition timing against each party''s concessions; comparative measurement of enforcement burden (who is asked to concede more and who is accommodated more); tracking of U.S. public statements and intelligence sharing patterns',
    'If genuinely neutral: U.S. is pure Rope (coordination broker). If structurally favoring Israel: U.S. is net extractor from Palestinian and rejectionist positions (Tangled Rope with asymmetric directionality).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(us_mediation_neutrality, empirical, 'Whether U.S. mediation is genuinely neutral or structurally favors particular parties').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1991_bush_middle_east_peace_mediation, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mepp_theater_1991, sotu_1991_bush_middle_east_peace_mediation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mepp_theater_1994, sotu_1991_bush_middle_east_peace_mediation, theater_ratio, 3, 0.54).
narrative_ontology:measurement(mepp_theater_1997, sotu_1991_bush_middle_east_peace_mediation, theater_ratio, 6, 0.62).
narrative_ontology:measurement(mepp_theater_2000, sotu_1991_bush_middle_east_peace_mediation, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(mepp_extract_1991, sotu_1991_bush_middle_east_peace_mediation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mepp_extract_1994, sotu_1991_bush_middle_east_peace_mediation, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(mepp_extract_1997, sotu_1991_bush_middle_east_peace_mediation, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(mepp_extract_2000, sotu_1991_bush_middle_east_peace_mediation, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1991_bush_middle_east_peace_mediation, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1991_bush_middle_east_peace_mediation, israeli_security_doctrine).
narrative_ontology:affects_constraint(sotu_1991_bush_middle_east_peace_mediation, palestinian_political_representation).
narrative_ontology:affects_constraint(sotu_1991_bush_middle_east_peace_mediation, arab_state_foreign_policy).
narrative_ontology:affects_constraint(sotu_1991_bush_middle_east_peace_mediation, us_middle_east_hegemony).

% DUAL FORMULATION NOTE:
% This constraint is part of a larger institutional ecosystem: it depends on Israeli security doctrine (upstream) and enables/constrains Palestinian political representation and Arab state foreign policy (downstream). The U.S. mediation framework is the coordinating mechanism connecting these structurally distinct constraints. Decomposition is not required (all use the same observable: the Madrid Conference framework and subsequent bilateral agreements), but the network links reveal how perturbations in one constraint propagate through the system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1991_bush_middle_east_peace_mediation, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
