% ============================================================================
% CONSTRAINT STORY: political_attention_scarcity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_political_attention_scarcity, []).

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
 *   constraint_id: political_attention_scarcity
 *   human_readable: Political Attention Scarcity
 *   domain: political_economy/governance
 *
 * SUMMARY:
 *   Political attention scarcity is a structural constraint on democratic
 *   discourse where the finite time, media slots, and mental bandwidth of
 *   voters and policymakers create bottlenecks that concentrate political
 *   power. This constraint exhibits the full range of Deferential Realism
 *   classifications from different structural positions: it appears as an
 *   immutable cognitive law (mountain) from a civilizational analytical view,
 *   but decomposes into produced institutional mechanisms (tangled rope,
 *   scaffold, piton) when observed from the positions of actual political
 *   agents. The constraint generates both genuine coordination function
 *   (incumbent parties use limited attention to maintain coalition
 *   discipline, media gatekeepers filter information) and substantial
 *   extraction (marginalized constituencies remain invisible, advocacy
 *   organizations must align with funder preferences, long-tail policy
 *   domains receive no electoral attention). The trajectory shows increasing
 *   theater ratio (media gatekeeping becoming more performative as audience
 *   fragmentation accelerates) and increasing extractiveness (attention
 *   concentration worsening as digital platforms consolidate, contrary to
 *   initial expectations that internet would democratize access). The digital
 *   coalition perspective offers a real alternative pathway with sunset
 *   logic: decentralized communication tools are building capacity to bypass
 *   gatekeeping, but this process is generational, not immediate.
 *
 * KEY AGENTS:
 *   - Marginalized Constituencies: Primary victims (powerless/trapped) — low-income voters, rural communities, immigrants, racial minorities; face maximum extraction through invisibility to political decision-making
 *   - Mid-Tier Advocacy Organizations: Secondary victims (moderate/constrained) — environmental, labor, consumer, and civil rights groups; experience mixed extraction (donor capture) and coordination (policy progress on secondary issues)
 *   - Incumbent Political Parties: Primary beneficiaries (institutional/arbitrage) — Democratic and Republican parties in US context; benefit from ability to control salient issue set and maintain coalition discipline
 *   - Wealthy Interest Groups: Secondary beneficiaries (institutional/arbitrage) — fossil fuel, pharmaceutical, financial sector groups; monopolize attention in their regulatory domains
 *   - Digital Coalition: Organized agents (organized/constrained) — social media platforms, grassroots digital organizing networks, blockchain-based governance experiments; building alternative attention pathways
 *   - Broadcast News Apparatus: Institutional actor (institutional/arbitrage) — traditional television and legacy print journalism; maintains gatekeeping role through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional concentration as cognitive immutability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_attention_scarcity, 0.58).
domain_priors:suppression_score(political_attention_scarcity, 0.65).
domain_priors:theater_ratio(political_attention_scarcity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_attention_scarcity, extractiveness, 0.58).
narrative_ontology:constraint_metric(political_attention_scarcity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(political_attention_scarcity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(political_attention_scarcity, tangled_rope).
narrative_ontology:human_readable(political_attention_scarcity, "Political Attention Scarcity").
narrative_ontology:topic_domain(political_attention_scarcity, "political_economy/governance").

domain_priors:requires_active_enforcement(political_attention_scarcity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(political_attention_scarcity, incumbent_political_parties).
narrative_ontology:constraint_beneficiary(political_attention_scarcity, wealthy_interest_groups).
narrative_ontology:constraint_beneficiary(political_attention_scarcity, media_gatekeepers).
narrative_ontology:constraint_victim(political_attention_scarcity, marginalized_constituencies).
narrative_ontology:constraint_victim(political_attention_scarcity, long_tail_policy_domains).
narrative_ontology:constraint_victim(political_attention_scarcity, democratic_discourse_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED CONSTITUENCY (SNARE) — Trapped populations (low-income voters, rural communities, immigrants, minorities) cannot reallocate political attention to their core concerns. Media filters, advertising costs, and incumbent party gatekeeping ensure that their issues remain invisible to decision-makers. No exit option — cannot form separate political space or redirect media attention without existing capital. Maximum extraction, minimum coordination benefit.
constraint_indexing:constraint_classification(political_attention_scarcity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-TIER ADVOCACY ORGANIZATION (TANGLED ROPE) — Constrained by resource requirements (fundraising, staff, media production capacity) but genuinely coordinates collective action on secondary policy domains (environmental regulation, labor standards, consumer protection). Experience mixed extraction (must align messaging with donor preferences) and real coordination (enables policy progress on non-salient issues). High barrier to entry but some agency within constraints.
constraint_indexing:constraint_classification(political_attention_scarcity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT POLITICAL PARTY (ROPE) — Primary beneficiary. Experiences attention scarcity as pure coordination mechanism: limited attention bandwidth enables party discipline, message control, and agenda-setting power. Party leadership can focus voter attention on 2-3 salient issues per cycle, marginalizing issues that threaten coalition. Net beneficiary with high arbitrage — can move attention allocation between election cycles, can trade attention for campaign funding.
constraint_indexing:constraint_classification(political_attention_scarcity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WEALTHY INTEREST GROUP (ROPE) — Secondary beneficiary. High-resource groups (fossil fuel companies, financial sector, pharmaceutical manufacturers) use concentrated capital to monopolize attention in their policy domain. Attention scarcity blocks competing narratives and keeps regulatory action off the agenda. Arbitrage exit — can move funding between parties/candidates. Perceives constraint as enabling coordination of favorable policy outcomes.
constraint_indexing:constraint_classification(political_attention_scarcity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DIGITAL COALITION (SCAFFOLD) — Organized agents (social media platforms, blockchain-based governance experiments, digital organizing networks) see attention scarcity as a temporary bottleneck being bypassed through decentralized communication. Low-cost digital coordination enables grassroots movements to direct attention without gatekeepers (Arab Spring, #MeToo, Black Lives Matter). Theater remains high but sunset is real — traditional media gatekeeping power decays as information distribution cost approaches zero. Has sunset clause: as digital native generation ages into full electoral participation, gatekeeping mechanisms lose force.
constraint_indexing:constraint_classification(political_attention_scarcity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: BROADCAST NEWS APPARATUS (PITON) — Traditional broadcast journalism maintains gatekeeping role through institutional inertia despite declining functional value. News production processes (editorial meetings, fact-checking, access negotiations with political figures) are substantially performative in a world where citizen-produced content on social media reaches equivalent or larger audiences. The apparatus persists because alternative credibility certification mechanisms haven't fully replaced it, not because broadcast journalism's verification adds measurable epistemic value. Theater ratio ≥ 0.70 drives piton classification.
constraint_indexing:constraint_classification(political_attention_scarcity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, attention scarcity appears as an immutable constraint of cognition: voters have finite mental bandwidth, media channels have finite content slots, legislatures have finite session days. The scarcity is inherent to the structure of human attention itself. However, this classification masks a contingent institutional arrangement — scarcity is produced by concentration (few media channels, few political parties, few funding sources), not by absolute cognitive limits. The engine's false summit detector will identify this as naturalization.
constraint_indexing:constraint_classification(political_attention_scarcity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_attention_scarcity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(political_attention_scarcity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(political_attention_scarcity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(political_attention_scarcity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(political_attention_scarcity, TR),
    TR >= 0.70.

:- end_tests(political_attention_scarcity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Political attention is a finite resource, and its concentration produces measurable extraction for marginalized groups (policy invisibility, electoral marginalization) and for advocacy organizations (donor preference capture). The value reflects that attention scarcity creates real asymmetric costs: a wealthy interest group can purchase attention for their preferred regulatory outcome; a marginalized constituency cannot. But the extraction is not maximal (0.80+) because: (a) some attention does reach non-salient constituencies, (b) digital tools are creating low-cost alternatives to traditional gatekeeping, and (c) periodically (during crisis, scandals, elections) attention allocation breaks from patterns. Suppression (0.65): High. Substantial barriers exist to reallocating attention: high cost of media access, editorial gatekeeping filters, cognitive limits on voter attention, structural bias toward incumbent narratives. But suppression is not total (0.80+) because digital platforms have reduced distribution cost below traditional media thresholds. Theater ratio (0.58): Moderate-high and increasing. Traditional broadcast journalism performs significant theater (editorial meetings, fact-checking rituals, access negotiations with political figures) that has declining functional verification value in an environment where citizen-produced content, leaked documents, and social media discussion reach equivalent audiences. Theater has increased over the 30-year measurement interval as editorial gatekeeping has become less functionally necessary but more institutionally entrenched.
 *
 * PERSPECTIVAL GAP:
 *   Primary gap between beneficiary (incumbent party rope perception) and victim (marginalized constituency snare perception) stems from opposite directionality values derived from opposite structural positions. Incumbent party perceives attention scarcity as enabling coordination: it allows leadership to focus voter attention on 2-3 salient issues, maintain coalition discipline, and prevent attention allocation to issues that would fracture the coalition. Marginalized constituency perceives attention scarcity as extractive: the same mechanism that benefits the incumbent prevents their issues from reaching the attention threshold required for electoral salience. The analytical observer's false summit (mountain/natural law) is particularly dangerous because it justifies the institutional arrangements that produce scarcity (two-party system, media oligopoly, donor funding concentration) as immutable facts of nature rather than changeable political choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position — beneficiary status (negative d), victim status (positive d), and exit options. Incumbent parties with arbitrage exit (can trade attention allocation between candidates/elections) experience low d → negative χ (beneficiaries). Wealthy interest groups with concentrated capital get similar d (beneficiaries). Marginalized constituencies with no exit (trapped) experience high d → high χ (maximum extraction). Mid-tier advocacy with constrained exit (high cost but possible) experience moderate d. Digital coalition with constrained exit but real alternatives (social media reach, decentralized networks) experience lower d than powerless agents. The piton classification derives from theater_ratio ≥ 0.70, indicating that broadcast journalism's institutional persistence is performative rather than functionally necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival decomposition. The mandatrophy question is: 'Is attention scarcity a coordination mechanism (rope) or an extraction mechanism (snare or tangled rope)?' The answer is: both, depending on structural position. For incumbent parties, it is genuine coordination (rope) — enables message discipline and coalition maintenance. For marginalized constituencies, it is genuine extraction (snare) — produces systematic invisibility. For mid-tier advocacy organizations, it is mixed (tangled rope) — genuine coordination on secondary issues alongside extraction via donor preference capture. The piton classification (broadcast journalism) reveals that much of the apparent coordination function is performative — the gatekeeping theater has declined in functional value but persists institutionally. The digital coalition's scaffold perspective reveals that the constraint has a sunset clause — decentralized communication is building alternative attention pathways that will eventually bypass traditional gatekeeping. The mountain classification at the analytical level is false — the engine should flag this as naturalization of a produced institutional structure, not as an immutable cognitive law. Mandatrophy is resolved by showing that all classifications are structurally correct from their respective positions; the constraint is not one type universally, but a presheaf of types determined by observational position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scarcity_production_vs_inherence,
    'Is political attention scarcity an inherent property of cognition or a produced effect of institutional concentration?',
    'Cross-national comparison of attention distribution (multiparty vs two-party systems, public broadcasting vs private media, distributed funding vs donor concentration); analysis of attention deficit in systems with alternative institutional structures; historical trajectory of attention concentration over time',
    'If inherent: mountain classification is correct, constraint is immutable. If produced: classification should be tangled rope, extraction mechanism is institutional design choice, constraint is changeable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scarcity_production_vs_inherence, empirical, 'Whether attention scarcity is cognitive limit or institutional production').

omega_variable(
    coalition_formation_threshold,
    'What population size threshold enables marginalized constituencies to form competing political attention sources without institutional gatekeeping?',
    'Historical analysis of successful grassroots movements; measurement of attention-formation capacity (social media reach, organizational fundraising, earned media) by constituency size; identification of tipping points where decentralized organization overcomes gatekeeping barriers',
    'If threshold is low: marginalized groups can escape snare via digital tools (scaffold sunset is real). If threshold is high: digital tools insufficient, institutional reform required, snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_formation_threshold, empirical, 'Population threshold for effective independent political attention source').

omega_variable(
    broadcast_credibility_replacement,
    'Do alternative credibility certification mechanisms (distributed fact-checking, reputation systems, peer review of journalism) provide epistemic value comparable to traditional broadcast journalism''s editorial filtering?',
    'Comparative accuracy analysis of stories vetted through traditional news channels vs decentralized verification; measurement of misinformation persistence by initial source; longitudinal tracking of false claims in broadcast vs digital-native media',
    'If alternatives effective: piton classification confirmed, broadcast apparatus persists through inertia only. If alternatives insufficient: broadcast journalism retains functional role, piton misclassified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broadcast_credibility_replacement, empirical, 'Credibility comparison: broadcast journalism vs decentralized verification').

omega_variable(
    donor_capture_extraction_proportion,
    'What proportion of the extraction experienced by mid-tier advocacy organizations is attributable to donor preference alignment vs structural scarcity itself?',
    'Counterfactual analysis: comparison of advocacy output when external funding is controlled vs distributed; measurement of issue prioritization correlation with funder preferences; analysis of advocacy effectiveness when funding is diverse vs concentrated',
    'If donor preference is dominant: extraction mechanism is separable from scarcity constraint (write two stories). If scarcity is dominant: tangled rope classification is correct for this perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(donor_capture_extraction_proportion, empirical, 'Donor preference capture vs structural scarcity in advocacy extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(political_attention_scarcity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(patts_tr_t0, political_attention_scarcity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(patts_tr_t10, political_attention_scarcity, theater_ratio, 10, 0.55).
narrative_ontology:measurement(patts_tr_t20, political_attention_scarcity, theater_ratio, 20, 0.58).
narrative_ontology:measurement(patts_tr_t30, political_attention_scarcity, theater_ratio, 30, 0.6).

% Extraction over time
narrative_ontology:measurement(patts_be_t0, political_attention_scarcity, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(patts_be_t10, political_attention_scarcity, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(patts_be_t20, political_attention_scarcity, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(patts_be_t30, political_attention_scarcity, base_extractiveness, 30, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(political_attention_scarcity, information_standard).
narrative_ontology:affects_constraint(political_attention_scarcity, regulatory_capture).
narrative_ontology:affects_constraint(political_attention_scarcity, campaign_finance_asymmetry).
narrative_ontology:affects_constraint(political_attention_scarcity, media_gatekeeping_power).
narrative_ontology:affects_constraint(political_attention_scarcity, voter_information_deficit).

% DUAL FORMULATION NOTE:
% Political attention scarcity is a superordinate constraint that structures how regulatory capture, campaign finance concentration, media power, and voter information deficits all operate. Decomposition: write separate stories for attention-scarcity-as-institutional-production (tangled rope, ε ≈ 0.58) vs attention-scarcity-as-cognitive-limit (mountain, ε ≤ 0.25) if analysis requires treating immutable cognitive constraint separately from produced institutional constraint. Current story treats scarcity as primarily produced (ε=0.58) because measurement evidence shows that attention concentration correlates with institutional concentration, not with cognitive capacity limits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(political_attention_scarcity, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
