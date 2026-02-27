% ============================================================================
% CONSTRAINT STORY: ulysses_chp07
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp07, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ulysses_chp07
 *   human_readable: The Rhetorical Press (Aeolus) — Freeman's Journal Advertisement Coordination
 *   domain: technological/social/political
 *
 * SUMMARY:
 *   Leopold Bloom enters the Freeman's Journal office attempting to renew an
 *   advertisement for his client (Plumtree's Potted Meat). He encounters a
 *   newspaper operation dominated by editorial theater: the staff perform
 *   rhetorical set pieces, discuss politics and literature, and engage in
 *   conversational grandstanding while the actual business of advertising
 *   coordination happens almost accidentally amid the chaos. The constraint
 *   is the gap between Bloom's simple coordination need (placing an ad) and
 *   the institutional machinery he must navigate to accomplish it. The
 *   Freeman's Journal holds a local monopoly on commercial advertising
 *   distribution, enabling them to extract through delay, uncertainty, and
 *   the cognitive burden of navigating their performative culture. Yet the
 *   constraint is not pure extraction — the newspaper genuinely coordinates
 *   the flow of advertisements to readers, and this coordination function is
 *   real. The staff's rhetoric, while excessive, also serves (or has served)
 *   a genuine editorial mission. The constraint exhibits all six DR types
 *   depending on the observer's structural position: pure extraction from
 *   Bloom's perspective (snare), mixed coordination-extraction from the
 *   advertiser network's perspective (tangled_rope), pure coordination from
 *   the editorial staff's perspective (rope), a temporary problem being
 *   solved by telegraph and rival publishers (scaffold), a degraded
 *   institutional remnant maintained by inertia (piton), or an immutable
 *   feature of information asymmetry (mountain). The theater_ratio increases
 *   from 0.65 to 0.81 over the interval, indicating that editorial
 *   performance is becoming increasingly disconnected from the actual
 *   business of advertisement coordination — the constraint is degrading from
 *   coordination-with-overhead toward pure performance with administrative
 *   accident.
 *
 * KEY AGENTS:
 *   - Leopold Bloom: Primary victim (powerless/trapped) — advertiser requiring newspaper reach with no alternative exit and no control over timing or prominence
 *   - Advertising Client (Plumtree's Potted Meat): Secondary victim (moderate/constrained) — depends on Freeman's Journal reach but could theoretically use alternative media at higher cost or lower efficiency
 *   - Freeman's Journal Editorial Staff: Primary beneficiary (institutional/arbitrage) — monopolizes commercial reach, controls ad placement timing/prominence, extracts through gatekeeper authority
 *   - Competing Publications & Telegraph Operators: Alternative agents (organized/constrained) — represent sunset pathway for newspaper's monopoly power over local commercial visibility
 *   - Print Media Industry: Institutional observer (institutional/arbitrage) — maintains the advertising coordination system through accumulated institutional practice
 *   - Information Market Analyst: Civilizational observer (analytical/analytical) — risks naturalizing the monopoly gatekeeper function as inherent to market structure rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp07, 0.52).
domain_priors:suppression_score(ulysses_chp07, 0.68).
domain_priors:theater_ratio(ulysses_chp07, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp07, extractiveness, 0.52).
narrative_ontology:constraint_metric(ulysses_chp07, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ulysses_chp07, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp07, tangled_rope).
narrative_ontology:human_readable(ulysses_chp07, "The Rhetorical Press (Aeolus) — Freeman's Journal Advertisement Coordination").
narrative_ontology:topic_domain(ulysses_chp07, "technological/social/political").

domain_priors:requires_active_enforcement(ulysses_chp07).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp07, newspaper_editorial_staff).
narrative_ontology:constraint_beneficiary(ulysses_chp07, advertising_proprietors).
narrative_ontology:constraint_victim(ulysses_chp07, routine_advertisers).
narrative_ontology:constraint_victim(ulysses_chp07, commercial_communication_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ROUTINE ADVERTISER (SNARE) — Trapped within the newspaper office's rhetorical machinery. Bloom requires the Freeman's Journal's distribution reach to place his client's advertisement, but the office's performative culture (grand rhetorical displays, editorial grandstanding, conversational tangents) delays a simple administrative transaction. No exit: the newspaper monopolizes local commercial reach. Suppression is severe — Bloom cannot bypass editorial theater to accomplish his task. Experienced extractiveness is maximum: time cost, cognitive load of navigating incomprehensible office dynamics, and uncertainty about whether the ad will actually be placed.
constraint_indexing:constraint_classification(ulysses_chp07, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ADVERTISING CLIENT NETWORK (TANGLED ROPE) — Constrained but not trapped. Advertisers depend on newspaper reach for commercial visibility, but alternative distribution exists (handbills, word-of-mouth, rival publications). The Freeman's Journal coordination function is genuine — it aggregates ads efficiently for readers — but the constraint extracts through delay, uncertainty, and cultural hazing. Moderate effective extraction: clients benefit from the newspaper's reach but bear costs of rhetorical theater and unpredictability.
constraint_indexing:constraint_classification(ulysses_chp07, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NEWSPAPER EDITORIAL STAFF (ROPE) — Institutional beneficiary with arbitrage options. The staff coordinates the flow of advertisements into the paper's commercial pages and profits from their placement. They experience the constraint as pure coordination: they are solving the problem of matching advertisers with readers. Their rhetorical performances (the editorial bombast, the conversational grandstanding) are performative, but they extract value through monopoly on distribution channels and control of ad placement timing and prominence. They can arbitrage between advertisers, favoring high-profile clients. Net beneficiary — extraction flows toward them.
constraint_indexing:constraint_classification(ulysses_chp07, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: TELEGRAPH & PRINTING ALTERNATIVES (SCAFFOLD) — Organized agents (telegraph operators, rival publishers, printing technology innovators) represent a sunset path for the newspaper's monopoly. As telegraph communication spreads and rival publications emerge, the Freeman's Journal's gatekeeper power diminishes. The constraint appears as a temporary coordination failure — the newspaper's centralized authority over local commercial visibility will be distributed among multiple channels. Low suppression from this perspective: the scaffold agents see pathways to bypass the bottleneck within a generational timeframe.
constraint_indexing:constraint_classification(ulysses_chp07, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PRINT MEDIA INSTITUTION (PITON) — From a civilizational view, the newspaper's advertising coordination is a degraded remnant of an earlier function. The constraint persists through institutional inertia: the Freeman's Journal maintains its editorial grandstanding and rhetorical theater as if it were still the sole source of market information, even as competing channels emerge. Theater ratio is very high (0.81): the editorial staff's conversational displays, their rhetorical performances, and their control of the ad placement process are substantially performative rather than functionally necessary. The institution itself recognizes its degradation — the office is chaotic, the staff is distracted by politics and wordplay, and the actual business of advertising coordination happens almost by accident amid the theatrical display.
constraint_indexing:constraint_classification(ulysses_chp07, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, information asymmetry between advertiser and newspaper is an immutable feature of the communication market: the newspaper knows its circulation, the advertiser does not. This knowledge gap creates structural leverage that the paper can extract through theater and delay. This perspective risks naturalizing the Freeman's Journal's rhetorical machinery as inherent to the advertising market itself, rather than recognizing it as a contingent institutional arrangement. The mountain classification signals a false summit — the engine's analysis reveals that the 'inherent asymmetry' framing disguises what is actually extractive institutional behavior.
constraint_indexing:constraint_classification(ulysses_chp07, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp07_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp07, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp07, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp07, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp07, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp07_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The Freeman's Journal extracts significant value from advertisers through their monopoly control of local commercial reach and their ability to delay, gate, or feature advertisements based on editorial discretion. But the extraction is not as severe as a pure snare (which would require 0.66+) because the coordination function is genuine — the newspaper does deliver advertisements to readers who want them. The extracted value comes from gatekeeper power, not from making the coordination impossible. Suppression (0.68): High. Advertisers face severe barriers to exit: no viable alternative that matches the Freeman's Journal's circulation reach, high switching costs (multiple publications or alternative media), and cultural/commercial pressure to be in 'the' newspaper. The newspaper staff actively uses their rhetorical theater as a suppression mechanism — the bureaucratic chaos makes it harder for advertisers like Bloom to navigate quickly or predictably. Theater ratio (0.81): Very high. The Freeman's Journal office is dominated by editorial performance: rhetorical set pieces, political debate, conversational grandstanding, and performative intellectual display. The actual business of ad coordination (accepting copy, scheduling placement, managing layout) occurs almost accidentally within this theatrical framework. The theater has increased over time (from 0.65 to 0.81) as the editorial staff have become increasingly focused on their intellectual/political mission and less on the administrative coordination they theoretically serve. This trajectory indicates the constraint is degrading from 'coordination with overhead' toward 'performance with administrative accident' — a classic piton progression.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence: Bloom sees snare, the advertiser network sees tangled_rope, the staff sees rope, the telegraph operators see scaffold, the print industry sees piton, and the analytical observer risks seeing mountain. This divergence is not measurement ambiguity — it is structural. Each perspective occupies a genuinely different position relative to the extraction flow, and each position generates a different classification. The perspectival gap is the constraint's defining feature: it is a hybrid that contains all six types simultaneously, depending on where you stand.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position relative to extraction flow. Bloom (beneficiary: false, victim: true, exit: trapped) occupies the position of maximum target — he benefits zero from the coordination, bears full cost of theater and delay, and has no escape option. Derived d ≈ 0.95 (full target), producing maximum f(d) ≈ 1.42 and experienced extraction. The editorial staff (beneficiary: true, victim: false, exit: arbitrage) occupy the position of maximum beneficiary — they capture gatekeeper rents and can exit the constraint entirely by choosing not to publish. Derived d ≈ 0.05 (full beneficiary), producing f(d) ≈ -0.12 (negative, indicating subsidy). The advertiser network (beneficiary: partial, victim: partial, exit: constrained) occupy the middle position — they benefit from coordination but bear costs of theater and extraction. Derived d ≈ 0.55, producing f(d) ≈ 0.75 (moderate). The emerging telegraph operators (beneficiary: false currently, victim: false, exit: mobile/creative) occupy an observer position that is moving toward beneficiary as their technology matures — d ≈ 0.35 initially, trending downward as the constraint approaches sunset. No directionality overrides are necessary — the structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: The Rhetorical Press resolves mandatrophy by showing that the tangled_rope classification is NOT a mislabeling of pure extraction (snare) or pure coordination (rope). The constraint genuinely has both functions: the Freeman's Journal coordinates advertisements to readers (real coordination), AND it extracts gatekeeper rents from advertisers (real extraction). The staff's rhetorical theater serves a secondary editorial mission (content that attracts readers and justifies the ads), but it also functions as a suppression mechanism (making it harder for advertisers to navigate and forcing them to depend on staff discretion for placement). The mandatrophy is resolved by the structural data: multiple agents experience multiple classification types, none of which reduce to the others. Bloom's snare experience does not invalidate the staff's rope experience — they are both real, occurring simultaneously in the same constraint. The theater_ratio trajectory (0.65 → 0.81) indicates that the constraint is degrading over time: the coordination function is becoming secondary to the performative function. If this trajectory continues, the constraint will eventually degrade into a pure piton (performance with no coordination) or potentially collapse as telegraph and rival media replace the newspaper's monopoly altogether.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    editorial_intention_signaling,
    'Is the Freeman''s Journal staff''s rhetorical performance deliberate gatekeeper extraction, or unintentional performative spillover from their editorial mission?',
    'Historical analysis of staff correspondence, advertising pricing records, comparative office culture across Dublin publications, correlation between editorial grandstanding and ad placement delays',
    'If deliberate: snare classification from advertiser perspective is confirmed (extraction is intentional gatekeeping). If performative spillover: classification shifts toward rope (coordination with collateral theater costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(editorial_intention_signaling, empirical, 'Whether editorial theater represents deliberate gatekeeping or unintentional performance').

omega_variable(
    advertiser_exit_realism,
    'Do rival publications and alternative media offer genuine functional equivalence to Freeman''s Journal advertising, or is the newspaper''s reach sufficiently unique that advertiser exit is illusory?',
    'Market analysis of circulation numbers, reader demographics by publication, advertiser distribution across outlets, cost-benefit analysis of multi-publication strategies vs Freeman''s Journal exclusive',
    'If genuine equivalence: advertiser exit_options should be ''mobile'' (not trapped). Classification shifts from snare toward tangled_rope. If unique reach: exit_options remain ''trapped'' and snare classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(advertiser_exit_realism, empirical, 'Whether rival publications provide genuine advertising reach equivalence').

omega_variable(
    theater_functional_necessity,
    'Does the Freeman''s Journal''s editorial grandstanding serve a genuine coordination function (e.g., enhancing reader engagement and thus advertiser value) or is it purely performative theater disconnected from advertising delivery?',
    'Comparative analysis of editorial intensity vs ad placement efficiency, reader engagement metrics vs circulation figures, profitability trends correlating with editorial vs advertising revenue',
    'If functional necessity: theater_ratio should be lower (~0.45-0.55), tangled_rope classification justified. If purely performative: theater_ratio (0.81) confirms high piton-slope toward institutional degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_functional_necessity, conceptual, 'Whether editorial theater serves genuine coordination or is purely performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp07, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aeolus_tr_t0, ulysses_chp07, theater_ratio, 0, 0.65).
narrative_ontology:measurement(aeolus_tr_t5, ulysses_chp07, theater_ratio, 5, 0.74).
narrative_ontology:measurement(aeolus_tr_t10, ulysses_chp07, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(aeolus_be_t0, ulysses_chp07, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(aeolus_be_t5, ulysses_chp07, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(aeolus_be_t10, ulysses_chp07, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp07, information_standard).
narrative_ontology:affects_constraint(ulysses_chp07, dublin_newspaper_circulation).
narrative_ontology:affects_constraint(ulysses_chp07, victorian_advertising_market).

% DUAL FORMULATION NOTE:
% The Rhetorical Press (Freeman's Journal advertising) is downstream of the broader Victorian advertising market constraint and lateral to the broader Dublin newspaper competition constraint. Each constraint has distinct extractiveness values reflecting different structural levels: the broad market constraint reflects general gatekeeper power (lower ε), while the Freeman's Journal specifically reflects institutional degradation and theatrical overhead (higher ε due to piton-slope theater_ratio).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
