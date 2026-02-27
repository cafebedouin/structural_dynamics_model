% ============================================================================
% CONSTRAINT STORY: bangladesh_july_national_charter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bangladesh_july_national_charter, []).

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
 *   constraint_id: bangladesh_july_national_charter
 *   human_readable: The July National Charter Referendum (Bangladesh)
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The July National Charter Referendum represents a constitutional bundling
 *   strategy in which the Muhammad Yunus-led interim government proposed 80
 *   reforms as a binary Yes/No choice, tethered to the February 2026 general
 *   elections. The constraint exhibits the full range of DR classification
 *   because different actors have radically different structural
 *   relationships to the referendum mechanism. For the interim government
 *   coalition, it is a coordination solution to gridlock — bundling prevents
 *   sequential amendment veto. For civil society and reform advocates, it is
 *   a mixed benefit (genuine progress on women's rights and anti-corruption)
 *   coupled with forced acceptance of contested executive authority
 *   provisions (tangled rope). For the parliamentary opposition, it is a pure
 *   extraction mechanism: forced to accept the package whole or forfeit
 *   constitutional input entirely (snare). For the democratic deliberation
 *   commons, the binary choice suppresses granular deliberation on 80
 *   discrete items (snare). For structural veto points (courts, military,
 *   regional power brokers), the referendum is a legitimacy mechanism that
 *   simultaneously constrains their veto authority (tangled rope). The
 *   constraint's theater ratio increased from 0.45 to 0.68 over the campaign
 *   period as the interim government's messaging emphasized democratic
 *   legitimacy while the structural suppression of deliberation and
 *   sequencing control remained constant. Extractiveness rose from 0.38 to
 *   0.58 as the referendum date approached and opposition exit options
 *   narrowed. The core mandatrophy tension is whether the bundling is a
 *   functional necessity (coordination function dominates) or a strategic
 *   choice (extraction function dominates). The evidence is mixed: veto
 *   points in Bangladesh's institutional structure are real, but interim
 *   government control of the amendment agenda was discretionary.
 *
 * KEY AGENTS:
 *   - Muhammad Yunus Interim Government Coalition: Primary beneficiary (institutional/arbitrage) — designed referendum structure, benefits from bundling mechanism, maintains exit option through electoral authority
 *   - Parliamentary Opposition: Primary victim (powerless/trapped) — excluded from amendment process, forced binary choice, no exit option except to boycott/marginalize
 *   - Reform Coalition & Civil Society: Mixed beneficiary/victim (moderate/constrained) — benefits from anti-corruption and women's rights provisions; constrained by forced bundling of contested executive authority items
 *   - Structural Veto Points (Courts, Military, Regional Power Brokers): Organized constraint bearer (organized/constrained) — experience legitimacy gain (popular referendum ratification) coupled with veto authority constraint (if certain provisions take effect)
 *   - Democratic Deliberation Commons: Abstract victim (powerless/trapped) — voters cannot deliberate 80 items selectively; theater apparatus obscures information asymmetry
 *   - Electoral Commission & Referendum Machinery: Institutional intermediary (institutional/arbitrage) — maintains neutral administration appearance while implementing interim government's structural design
 *   - Elected Parliament Post-Feb 12: Future institutional actor (organized/mobile) — will have amendment capacity under standard legislative process, but referendum outcomes may function as de facto ceiling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bangladesh_july_national_charter, 0.58).
domain_priors:suppression_score(bangladesh_july_national_charter, 0.62).
domain_priors:theater_ratio(bangladesh_july_national_charter, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bangladesh_july_national_charter, extractiveness, 0.58).
narrative_ontology:constraint_metric(bangladesh_july_national_charter, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bangladesh_july_national_charter, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bangladesh_july_national_charter, tangled_rope).
narrative_ontology:human_readable(bangladesh_july_national_charter, "The July National Charter Referendum (Bangladesh)").
narrative_ontology:topic_domain(bangladesh_july_national_charter, "political/constitutional").

domain_priors:requires_active_enforcement(bangladesh_july_national_charter).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bangladesh_july_national_charter, interim_government_coalition).
narrative_ontology:constraint_beneficiary(bangladesh_july_national_charter, reform_coalition_civil_society).
narrative_ontology:constraint_victim(bangladesh_july_national_charter, parliamentary_opposition).
narrative_ontology:constraint_victim(bangladesh_july_national_charter, structural_vetoes).
narrative_ontology:constraint_victim(bangladesh_july_national_charter, democratic_deliberation_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARLIAMENTARY OPPOSITION (SNARE) — Faced a binary referendum without advance debate opportunity. Prevented from proposing alternative amendments. No institutional venue to offer counterproposals. Exit option is to boycott or be marginalized. Structurally trapped with maximum experienced extraction: lose constitutional input entirely or accept the package whole. Career risk in opposition; no arbitrage available. Maximum suppression.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL SOCIETY & REFORM COALITION (TANGLED ROPE) — Some reforms (women's representation, minority protections, anticorruption provisions) genuinely benefit this constituency; coordinating benefits from the reform package. Simultaneously, the binary structure forces acceptance of provisions they might contest (executive authority, interpretation scope). Constrained exit — can lobby for amendments or vote no, but either path carries political cost. Mixed extraction and coordination.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERIM GOVERNMENT COALITION (ROPE) — Primary beneficiary. Designed the referendum structure. Captures institutional gains from constitutional reform (executive authority, administrative capacity). Experiences the constraint as coordination: bundling reforms solves the problem of sequential amendment gridlock. Arbitrage available — can dissolve referendum and call fresh elections if needed. Low effective extraction from their own constraint structure.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STRUCTURAL VETO POINTS (TANGLED ROPE) — Courts, military, bureaucracy, and regional power brokers can block or challenge implementation. The referendum is ostensibly a democratic authorization, but actual constitutional authority depends on institutional acceptance. These actors benefit from some reforms (institutional legitimacy, clarified authority) but also experience constraint on their own veto power if certain provisions take effect. Organized but constrained exit: can litigate or slow implementation. Mixed extraction and coordination.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEMOCRATIC DELIBERATION COMMONS (SNARE) — Abstract collective good. Voters presented with 80-item package without granular deliberation. No mechanism for selective approval or modification. Information asymmetry (beneficiaries have months of drafting; voters have weeks of campaign messaging). Theater masquerades as consultation. Cannot exit or organize. Bears full cost of rushed constitutional reform without deliberative process. Trapped.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: ELECTORAL COMMISSION & REFERENDUM MACHINERY (PITON) — The institutional apparatus (ballot design, voter registration, counting procedures) is substantial and performative. Much of the 'referendum' legitimacy rests on procedural propriety — neutral administration, transparent counting. The machinery has real coordination function (aggregating votes) but also theater function (legitimizing a predetermined outcome). Theater ratio elevated because the commission is simultaneously neutral arbiter and instrument of the interim government. Degraded institutional role: maintains legitimacy appearance while structured constraints limit genuine deliberative choice.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: POST-ELECTION AMENDMENT PATHWAY (SCAFFOLD) — The bundle structure is temporary. Once the general elections occur (Feb 12, 2026), elected parliament can propose further amendments via standard legislative process. The referendum becomes one checkpoint, not the final word. This perspective sees the constraint as a coordination mechanism with built-in sunset: binary referendum accelerates baseline reform; subsequent parliaments have mobile exit (can amend). Theater ratio is lower from this view — the referendum is scaffolding for institutional transition, not permanent constitutional settlement.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / CONSTITUTIONALISM VIEW (MOUNTAIN) — From civilizational scope, constitutional change via referendum may be framed as an immutable structural feature: democratic legitimacy requires popular ratification; bundling is unavoidable in mass democracies; citizens cannot deliberate 80 items individually — this perspective naturalizes the referendum structure as inherent to constitutionalism. However, the structural data contradicts the mountain classification. The binary choice, bundling, and suppression of deliberation are institutional design choices, not laws of nature. False summit.
constraint_indexing:constraint_classification(bangladesh_july_national_charter, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bangladesh_july_national_charter_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bangladesh_july_national_charter, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bangladesh_july_national_charter, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bangladesh_july_national_charter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bangladesh_july_national_charter, TR),
    TR >= 0.70.

:- end_tests(bangladesh_july_national_charter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The interim government captures significant constitutional authority gains through the bundling mechanism. By controlling amendment agenda and binary framing, they extract concessions from opposition (acceptance of executive authority provisions, inability to propose alternatives). However, extractiveness is not maximal (0.80+) because the reforms include genuine public goods (anti-corruption, women's representation) that justify some extraction as coordination cost, and the interim status is temporary with defined electoral endpoint. Suppression (0.62): Moderate-high. Multiple blocking mechanisms: opposition excluded from amendment drafting, binary referendum structure prevents granular deliberation, campaign timeline constrains public information processing, structural veto points (courts) retain implicit veto on implementation. But suppression is not total (0.90+) because opposition can campaign for 'No' vote, civil society can mobilize, and post-election parliament retains amendment authority. Theater ratio (0.68): Elevated. The interim government conducts widespread 'consultation' meetings, but actual amendment power was concentrated. Referendum procedural legitimacy (ballot administration, counting transparency) is substantial, creating appearance of democratic ratification while structural choices (bundling, binary framing) were made prior to public input. Theater has increased over time as campaign messaging emphasized democratic legitimacy while core structural constraints remained fixed.
 *
 * PERSPECTIVAL GAP:
 *   All eight perspectives are justified by the structural data. The key gaps: (1) Interim government sees Rope (coordination), opposition sees Snare (extraction) — directionality differs by ~0.80 on the d scale. (2) Civil society's Tangled Rope reflects genuine mixed experience — not a classification error but a true structural mixture of coordination (anti-corruption benefits) and extraction (forced bundling). (3) The Scaffold perspective (post-election amendment pathway) is legitimate only if the omega on post-election amendment behavior resolves affirmatively. (4) The Mountain perspective is a false summit — constitutionalism does not require bundling or binary framing; these are design choices. The perspectival spread (Snare to Rope to Mountain) indicates the constraint operates across multiple structural regimes simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality from agent power, exit options, and beneficiary/victim declarations. The interim government (institutional + arbitrage) is a declared beneficiary with mobile exit (can dissolve and call fresh elections) — derives low d (≈0.15), producing negative/low χ. The parliamentary opposition (powerless + trapped) is a declared victim with no exit — derives high d (≈0.95), producing maximum χ via sigmoid f(d). Civil society (moderate + constrained) is declared both beneficiary (genuine reform gains) and victim (forced bundling) — derives mixed d (≈0.50-0.60) per their constrained exit, producing moderate χ. Structural veto points (organized + constrained) are declared victims of veto-power constraint but benefit from legitimacy — derive medium-high d (≈0.50-0.60) reflecting their constrained but agency-rich position. The deliberation commons (powerless + trapped) is a victim with zero exit — derives high d, producing maximum experienced extraction. The electoral machinery (institutional + arbitrage) derives low d as beneficiary of the process they administer. Post-election parliament (organized + mobile) would derive low-medium d if assessed at future time point — their mobile exit option (can amend) reduces their experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY. The constraint exhibits high extractiveness (0.58) but the core question remains: is the bundling a functional necessity (legitimate coordination cost) or a strategic choice (pure extraction)? If bundling is necessary to overcome veto gridlock (Tangled Rope primary), the constraint is partially justified and moderate-to-high χ is appropriate compensation for solving a collective action problem. If bundling is strategic (to maximize interim government authority), the constraint is closer to Snare and χ should be interpreted as pure loss. The three key ambiguities: (a) Could sequential amendment have succeeded without bundling? (b) Will post-election parliament actually amend outcomes, or is the referendum de facto constitutional ceiling? (c) Did opposition have genuine capacity to coordinate a counter-proposal during the interim period? Resolution requires tracking post-2026 election amendment behavior and forensic analysis of interim government's institutional constraints. For now, the claimed_type (Tangled Rope) reflects the prima facie case for bundling as mixed coordination-extraction, but this will be reassessed once post-election parliamentary behavior provides data. The Snare and Mountain perspectives are minority readings — the opposition's experienced reality is Snare, but the structural justification for bundling leans toward Tangled Rope if veto gridlock is genuine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberative_capacity_threshold,
    'How many constitutional items can a citizenry deliberate and decide upon in a referendum without significant comprehension collapse?',
    'Exit polls, cognitive testing of voter understanding, comparative analysis of other multi-item referenda (EU, California ballot measures). Threshold determination via information-processing capacity studies.',
    'If threshold ≥ 80 items: binary bundling is coordination mechanism (Rope/Scaffold). If threshold ≤ 20 items: bundling is extraction mechanism (Snare/Tangled Rope). If 20-50 items: mixed threshold behavior, depending on time available for campaigns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_capacity_threshold, empirical, 'Threshold for citizen comprehension of multi-item referenda').

omega_variable(
    amendment_trajectory_post_election,
    'Will the elected parliament (post-February 12) actually exercise its amendment power, or will the referendum outcomes become de facto constitutional ceiling?',
    'Historical tracking of constitutional amendment rates in Bangladesh post-2026 elections. Measurement of political willingness to revisit referendum items. Legislative voting patterns on follow-up amendments.',
    'If parliament amends freely: scaffold perspective confirmed, sunset is real. If referendum items are treated as untouchable: sunset is illusory, constraint becomes permanent (tangled rope, not scaffold). If selective amendment (some items revisited, others locked): mixed pathway.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_trajectory_post_election, empirical, 'Whether elected parliament will actually amend post-referendum outcomes').

omega_variable(
    opposition_coordination_capacity,
    'Could parliamentary opposition have coordinated a counter-proposal during the interim government''s period, or was veto power genuinely suppressed by interim status?',
    'Archival review of interim government constraints on parliamentary activity, opposition organizing capacity, institutional barriers to alternative amendments. Comparison with other interim governments'' amendment processes.',
    'If opposition could have coordinated: suppression derives from interim status design, not from structural necessity. If opposition was genuinely blocked: suppression reflects deeper institutional asymmetry. Affects directionality derivation for opposition agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opposition_coordination_capacity, empirical, 'Whether opposition had capacity to coordinate counter-proposal').

omega_variable(
    referendum_bundling_necessity,
    'Was bundling 80 reforms into a binary referendum a functional necessity (sequential amendment would be blocked by veto points), or a strategic choice to maximize approval?',
    'Comparative institutional analysis: other democracies'' amendment timelines, veto point modeling, counterfactual analysis of sequential amendment scenarios. Expert testimony on blocking coalitions.',
    'If bundling necessary: constraint is Tangled Rope (mixed coordination and extraction). If bundling strategic: constraint leans toward Snare (pure extraction via forced choice). Affects claimed_type reassessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(referendum_bundling_necessity, conceptual, 'Whether bundling was necessity or strategic choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bangladesh_july_national_charter, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bngla_charter_tr_t0, bangladesh_july_national_charter, theater_ratio, 0, 0.45).
narrative_ontology:measurement(bngla_charter_tr_t2, bangladesh_july_national_charter, theater_ratio, 2, 0.6).
narrative_ontology:measurement(bngla_charter_tr_t4, bangladesh_july_national_charter, theater_ratio, 4, 0.68).

% Extraction over time
narrative_ontology:measurement(bngla_charter_be_t0, bangladesh_july_national_charter, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bngla_charter_be_t2, bangladesh_july_national_charter, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(bngla_charter_be_t4, bangladesh_july_national_charter, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bangladesh_july_national_charter, enforcement_mechanism).
narrative_ontology:affects_constraint(bangladesh_july_national_charter, bangladesh_electoral_gridlock).
narrative_ontology:affects_constraint(bangladesh_july_national_charter, veto_points_institutional_capacity).
narrative_ontology:affects_constraint(bangladesh_july_national_charter, civil_society_organizational_capacity).

% DUAL FORMULATION NOTE:
% The referendum bundles 80 discrete constitutional claims, but the constraint as a structural phenomenon is the binary framing and bundling mechanism itself (extractiveness 0.58), not the individual provisions. A sibling constraint story could decompose specific reform provisions (e.g., women's representation, executive authority scope) as separate claims with their own ε values. The current story treats the bundling as a unified extraction mechanism. Downstream constraints include parliamentary gridlock resolution (if bundling succeeds) and post-election amendment pathway (if bundling becomes permanent ceiling).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
