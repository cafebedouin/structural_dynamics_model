% ============================================================================
% CONSTRAINT STORY: exclusionary_base__imperial_tribute_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exclusionary_base__imperial_tribute_reading, []).

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
 *   constraint_id: exclusionary_base__imperial_tribute_reading
 *   human_readable: Imperial Tribute as Foundation of Athenian Democracy (Tribute Reading)
 *   domain: political/historical
 *
 * SUMMARY:
 *   This constraint instantiates the IMPERIAL TRIBUTE READING of the
 *   exclusionary_base kernel. The reading positions the Athenian democracy as
 *   fundamentally dependent on imperial extraction: allied tribute financed
 *   the fleet (war), the festivals (politics/religion), and the assembly pay
 *   (participation). The constraint is not that democracy existed; it is that
 *   THIS PARTICULAR democracy — with its festivals, its pay-for-jury-service,
 *   its sea-power focus — was structurally enabled by collecting taxes from
 *   subject cities. The reading frames the exclusionary character of Athenian
 *   democracy not as a restriction of who could be a citizen
 *   (citizen_privilege_reading) or as dependence on enslaved labor
 *   (slave_economy_reading) but as dependence on external imperial
 *   extraction. The demos became a collective rentier class, extracting
 *   tribute from allies to fund its own domestic institutions. The constraint
 *   exhibits suppression (revolts besieged by Athenian navy), beneficiary
 *   (the Athenian demos), and victim (tributary cities and their autonomy).
 *   It is presented as a SNARE from the perspective of trapped cities and as
 *   a ROPE/TANGLED ROPE from the perspective of the beneficiary and nominal
 *   allies. The measurement trajectory shows extractiveness rising and
 *   theater declining — the league's nominal coordination function
 *   (antipersan alliance) is overtaken by straightforward extraction
 *   (tributary collection), while the formal alliance structures persist as
 *   performance.
 *
 * KEY AGENTS:
 *   - Athenian Demos: Collective beneficiary (institutional/arbitrage) — receives tribute, funds assembly pay, fleet, festivals; primary agent authorized to extract
 *   - Tributary Cities (Chios, Lesbos, Thasos, etc.): Primary victims (powerless/trapped) — bear tribute burden, suppress revolts at military cost, lose autonomy
 *   - Delian League Assembly: Nominal ally/secondary beneficiary (organized/constrained) — formally deliberates but Athens controls decisions; some cities extract minor benefits (trade priority, naval protection) while bearing heavier cost
 *   - Individual Allied Merchants/Craftsmen: Secondary victims (moderate/constrained) — personal wealth reduced by tributary extraction, limited mobility due to kinship/property ties
 *   - Athenian State Treasury: Institutional mechanism (institutional/arbitrage) — accumulates and deploys tribute for dole, festivals, fleet; the constraint is embedded in budget structure
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing hegemonic extraction as inevitable law of imperial organization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exclusionary_base__imperial_tribute_reading, 0.62).
domain_priors:suppression_score(exclusionary_base__imperial_tribute_reading, 0.72).
domain_priors:theater_ratio(exclusionary_base__imperial_tribute_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exclusionary_base__imperial_tribute_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(exclusionary_base__imperial_tribute_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(exclusionary_base__imperial_tribute_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exclusionary_base__imperial_tribute_reading, snare).
narrative_ontology:human_readable(exclusionary_base__imperial_tribute_reading, "Imperial Tribute as Foundation of Athenian Democracy (Tribute Reading)").
narrative_ontology:topic_domain(exclusionary_base__imperial_tribute_reading, "political/historical").

domain_priors:requires_active_enforcement(exclusionary_base__imperial_tribute_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exclusionary_base__imperial_tribute_reading, 'ff476c71-5f61-4cd3-87ce-f60d2142a475').
narrative_ontology:cs_kernel_codification('ff476c71-5f61-4cd3-87ce-f60d2142a475', fixed_text).
narrative_ontology:cs_authority_grounding('ff476c71-5f61-4cd3-87ce-f60d2142a475', lineage).
narrative_ontology:cs_interpretation_layer_present('ff476c71-5f61-4cd3-87ce-f60d2142a475').
narrative_ontology:cs_reading_relation('ff476c71-5f61-4cd3-87ce-f60d2142a475', exclusionary_base__citizen_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff476c71-5f61-4cd3-87ce-f60d2142a475', exclusionary_base__slave_economy_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('ff476c71-5f61-4cd3-87ce-f60d2142a475', foundational, imperial_extraction_primary_funding_source).
narrative_ontology:cs_axiom_status(imperial_extraction_primary_funding_source, holdable).
narrative_ontology:cs_axiom_grounding('ff476c71-5f61-4cd3-87ce-f60d2142a475', imperial_extraction_primary_funding_source, empirically_contingent).
narrative_ontology:cs_axiom('ff476c71-5f61-4cd3-87ce-f60d2142a475', secondary, tribute_justification_antipersan_coordination).
narrative_ontology:cs_axiom_status(tribute_justification_antipersan_coordination, overridden).
narrative_ontology:cs_axiom_grounding('ff476c71-5f61-4cd3-87ce-f60d2142a475', tribute_justification_antipersan_coordination, empirically_contingent).
narrative_ontology:cs_reference_frame('ff476c71-5f61-4cd3-87ce-f60d2142a475', tributary_league_as_coordination).
narrative_ontology:cs_drift_state('ff476c71-5f61-4cd3-87ce-f60d2142a475', contemporary_modern_analysis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ff476c71-5f61-4cd3-87ce-f60d2142a475', '').
narrative_ontology:cs_kernel_id(exclusionary_base__imperial_tribute_reading, exclusionary_base).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exclusionary_base__imperial_tribute_reading, athenian_demos).
narrative_ontology:constraint_victim(exclusionary_base__imperial_tribute_reading, tributary_cities).
narrative_ontology:constraint_victim(exclusionary_base__imperial_tribute_reading, allied_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% TRIBUTARY CITY PERSPECTIVE (SNARE): Structurally trapped. Revolts are besieged; exit is military defeat. The tribute flows inexorably to Athens. The alternative to payment is naval blockade, hunger, and conquest. No meaningful exit option exists within biographical time. Maximum suppression, high extraction, low coordination benefit. The constraint appears as pure force.
constraint_indexing:constraint_classification(exclusionary_base__imperial_tribute_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% LEAGUE ASSEMBLY PERSPECTIVE (TANGLED ROPE): Organized actors nominally coordinate shared defense against Persian threat. Genuine coordination function exists — the league navy does provide security. But Athens holds hegemonic power and appropriates surplus. Exit is costly (naval reprisal) but theoretically possible via coalition formation. The constraint is mixed: real security coordination plus asymmetric extraction. Theater is moderate — the league assembly maintains forms of deliberation, but Athens controls outcomes.
constraint_indexing:constraint_classification(exclusionary_base__imperial_tribute_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% ATHENIAN STATE PERSPECTIVE (ROPE): Pure coordination from the beneficiary's structural position. The tribute finances fleet, festivals, pay for participation — the institutions of democracy itself. Athens experiences the constraint as solving a genuine collective-action problem: how to fund a navy and maintain a demos? The extraction from allies is not experienced as extraction but as the solution to a shared problem. High arbitrage capacity — Athens can walk away anytime by abandoning hegemony.
constraint_indexing:constraint_classification(exclusionary_base__imperial_tribute_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% ALLIED CITIZEN (MODERATE/CONSTRAINED): Moderate power, generational horizon. The tribute extracts wealth from allied cities, reducing local prosperity and local control over public funds. Exit is theoretically available (emigration, relocation) but costly — breaking kinship networks, abandoning property, losing social status. The constraint is experienced as snare because the suppression is structural (naval superiority) and the extraction is visible (tribute fleets), but exit is constrained rather than impossible.
constraint_indexing:constraint_classification(exclusionary_base__imperial_tribute_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% IDEALIZED LEAGUE MEMORY (PITON): From a civilizational perspective, the Delian League begins as a genuine anti-Persian coalition (rope — shared coordination) but degrades into a tribute extraction mechanism (snare). The theater persists: league assemblies continue, nominal collective deliberation persists, allies retain formal voice. But the primary function (shared security against Persia) has been replaced by structural extraction (Athens' hegemony). The constraint is piton: the form of alliance persists through institutional inertia long after the substance has become imperial extraction. Theater ratio is moderate (0.45) because the nominal alliance structure still functions, not purely performatively.
constraint_indexing:constraint_classification(exclusionary_base__imperial_tribute_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% ANALYTICAL OBSERVER / HEGEMONIC INEVITABILITY VIEW (MOUNTAIN): From a universal/analytical perspective, the constraint appears as an immutable feature of how large-scale political organization works: any city with naval superiority will extract from weaker cities; hegemonic tribute is the natural outcome of power asymmetry. This perspective risks naturalizing what this reading exposes as a specific historical arrangement. The analytical observer may frame the tribute as 'inevitable given the military technology and geopolitical conditions.' The engine will flag this as a false summit: the suppression and extraction metrics do not support a mountain classification. What appears as law of nature is actually a contingent institutional arrangement (the political decision to maintain the league as a tribute system rather than dissolve it or reconstitute it as egalitarian federation).
constraint_indexing:constraint_classification(exclusionary_base__imperial_tribute_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exclusionary_base__imperial_tribute_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exclusionary_base__imperial_tribute_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exclusionary_base__imperial_tribute_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exclusionary_base__imperial_tribute_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(exclusionary_base__imperial_tribute_reading, TR),
    TR >= 0.70.

:- end_tests(exclusionary_base__imperial_tribute_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The tribute stream is substantial and flows directly from cities to Athens. The base measure reflects that the extraction is real, visible, and economically significant — it funds infrastructure that would otherwise require internal taxation or sacrifice. This reading does NOT claim that the tribute is the ONLY source of Athenian revenue (domestic taxation, silver mines, private wealth all contribute) but that the tribute is the ENABLING mechanism for the particular scale and character of democratic institutions. The value rises from 0.48 to 0.68 over the league's 40-year interval, reflecting the hardening of extraction: early period has more nominal negotiation and coordination rhetoric; later period sees pure tributary collection with minimal performative deference. Suppression (0.72): High. The Athenian navy enforces tribute collection through blockade, siege, and reprisal. Revolts (Thassos 465, Lesbos 427) are crushed by military force. The alternatives to payment are: (1) accept subordination, (2) attempt exit via revolt (military defeat is probable), (3) emigrate (costly, breaks social ties). No city has real exit capacity. Suppression is structural (naval asymmetry), not merely ideological. Theater ratio (0.45): Moderate-low. The league maintains nominal assemblies and collective deliberation (theater), but the practical function of these meetings is to ratify Athenian decisions rather than genuinely coordinate shared policy. Theater is not high (the Assembly is not purely ritual, some deliberation occurs) but is notably lower than founding period (0.55) when the anti-Persian coordination was more genuine. By the later period (0.40), the assembly is mostly theater — the real mechanism is tributary collection and naval enforcement. The declining theater trajectory is diagnostic: as the constraint hardens from mixed coordination-extraction to pure extraction, the theater component (the nominal alliance form) becomes less necessary and less maintained.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the Athenian state (Rope from institutional perspective) and the tributary cities (Snare from powerless perspective) is maximal. Athens experiences the constraint as coordination — solving the problem 'how do we fund a navy and maintain assembly participation?' The tribute is the solution to a shared-defense problem. From the city of Chios, the constraint is pure force: pay or be blockaded. The League Assembly (organized/constrained) sees a mixed constraint: genuine anti-Persian coordination benefits exist (shared fleet, military security), but the extraction is asymmetric (Athens captures surplus, sets terms). The piton perspective (civilizational view of the degraded alliance) captures the historical arc: the league begins with higher theater (0.55) and lower extraction (0.48) when Persian threat is immediate and coordination is genuine; it hardens into extraction (0.68) with minimal theater (0.40) as the Persian threat fades and Athens consolidates control. The analytical observer risks a false-summit classification (Mountain: 'hegemonic extraction is inevitable given power asymmetry'), which the structural data contradicts — the extractiveness and suppression metrics do not support an immutable law; they show a contingent institutional arrangement that could have been constituted differently (genuine federation, tribute redistribution, mandatory settlement of disputes, etc.).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position (beneficiary/victim status, exit capacity, power level). The Athenian demos holds arbitrage capacity — they can walk away from hegemony at any time by dissolving the league (loss of tribute income, but no military penalty). This positions them with low d (beneficiary + arbitrage → d ≈ 0.05-0.15). The tributary city is trapped with no exit option (revolt fails, departure is military defeat, payment is mandatory). This positions them with high d (victim + trapped → d ≈ 0.95). The League Assembly as organized body has constrained exit (coalition revolt is theoretically possible but requires coordination among many cities and risks naval reprisal). This positions them with moderate-high d (victims/mixed beneficiaries + constrained → d ≈ 0.55-0.65). The engine applies the sigmoid f(d) to convert d into experienced extractiveness chi. The analytical observer (d ≈ 0.72, roughly observer-neutral) sees the structure clearly but risks framing it as natural law rather than contingent choice.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by showing that the constraint is genuinely a SNARE from the tributary perspective (high extractiveness, high suppression, no coordination benefit) while being experienced as ROPE from the beneficiary perspective (coordination function is real: the league navy does provide security; extraction is not experienced as extraction but as solution to a collective-action problem). The perspectival gap is not a failure of classification but a structural fact about how the constraint operates: the same arrangement is coordination for the beneficiary and extraction for the victim. The piton classification captures the institutional degradation: the league form persists (theater 0.40-0.55) even as its functional coordination purpose (anti-Persian defense) is replaced by extraction. The false-summit mountain classification is a risk the analytical observer must avoid: framing hegemonic extraction as an immutable law of political organization naturalizes a contingent choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Where does the Delian League transition from genuine anti-Persian coordination (rope) to pure extraction mechanism (snare)? Is there a structural inflection point, or is the extraction present from the founding?',
    'Historical analysis of early vs late league structure: compare charter documents, tribute demand curves over time, recorded league deliberations, frequency and scale of revolts as proxy for legitimacy collapse.',
    'If extraction is founding feature: classify constraint as snare from inception; frames the league as instrumentally constructed hegemony. If transition is late (post-Persian Wars): frames early period as genuine coordination that corrupted; suggests empire was a contingent choice by Athens, not inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Inflection point between coordination and extraction in league evolution').

omega_variable(
    reading_contest__tribute_vs_citizen_privilege,
    'Does this reading (tribute as foundational) logically foreclose the citizen_privilege_reading (citizenship as guarded estate), or do both readings coexist in describing the same exclusionary structure from different angles?',
    'Logical analysis: does restricting citizenship to double descent necessarily imply dependence on tribute? Or can a polity restrict citizenship AND fund democracy through internal means? Identify the dependency structure: does citizen privilege require tribute income, or only enable it?',
    'If foreclose: the two readings are incompatible framings of one structural fact (democracy excluded non-citizens and funded itself through empire). If coexist: democracy''s exclusions operate on multiple axes simultaneously (citizenship law, imperial extraction, slave labor) without one determining the others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest__tribute_vs_citizen_privilege, conceptual, 'Logical relationship between tribute reading and citizen privilege reading').

omega_variable(
    reading_contest__tribute_vs_slave_dependency,
    'Does tributary extraction coexist with slave-labor extraction, or do the readings describe the same structural phenomenon from different measurement perspectives (federal vs domestic economy)?',
    'Decomposition of budget sources: estimate the economic magnitude of tributary income vs slave-labor productive output. Trace the flow of tribute into the state budget (treasury, military, dole pay) vs the flow of slave-produced goods into household and craft economies. Determine whether they address the same ''pay the demos'' problem or separate problems.',
    'If separate mechanisms (tributary + slave labor both fund democracy): both readings are live, affecting constraint distinct mechanisms. If same problem viewed through different lenses: readings should classify identically or one should be subordinated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest__tribute_vs_slave_dependency, empirical, 'Whether tribute and slave economy are separate extraction mechanisms or the same constraint viewed differently').

omega_variable(
    suppression_mechanism_revolt_cycle,
    'Is the measured suppression (0.72) driven by the permanent naval threat (structural coercion), the frequency of revolts and their military suppression (performative example-making), or the normalization of hegemony as inevitable (cognitive capture of allied elites)?',
    'Historical record of revolts: magnitude, frequency, outcomes. Cross-examine whether each revolt represents a genuine breakdown of legitimacy (suggesting suppression is not fully internalized) or ritualized script that allies expect and plan around (suggesting high cognitive normalization). Trace whether suppression intensity rises or falls over the league''s history.',
    'If primarily structural coercion: suppression metric should remain high. If primarily cognitive normalization: metric should drift lower over time as allies internalize hegemonic order; divergence between structural and experienced suppression. If performative: analysis of spectacle effectiveness for deterrence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_revolt_cycle, empirical, 'Source of suppression mechanism: structural coercion, cognitive normalization, or performative deterrence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exclusionary_base__imperial_tribute_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(excl_tr_t0, exclusionary_base__imperial_tribute_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(excl_tr_t20, exclusionary_base__imperial_tribute_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(excl_tr_t40, exclusionary_base__imperial_tribute_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(excl_be_t0, exclusionary_base__imperial_tribute_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(excl_be_t20, exclusionary_base__imperial_tribute_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(excl_be_t40, exclusionary_base__imperial_tribute_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(excl_su_t0, exclusionary_base__imperial_tribute_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(excl_su_t20, exclusionary_base__imperial_tribute_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(excl_su_t40, exclusionary_base__imperial_tribute_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exclusionary_base__imperial_tribute_reading, resource_allocation).
narrative_ontology:affects_constraint(exclusionary_base__imperial_tribute_reading, exclusionary_base__citizen_privilege_reading).
narrative_ontology:affects_constraint(exclusionary_base__imperial_tribute_reading, exclusionary_base__slave_economy_dependency_reading).

% DUAL FORMULATION NOTE:
% The exclusionary_base kernel decomposes into three structurally distinct constraints, each with its own ε and beneficiary/victim structure. The imperial_tribute_reading (this story) isolates the extraction of external resources (tribute) as the primary funding mechanism. The citizen_privilege_reading isolates the legal restriction of the citizen body as the primary exclusion mechanism. The slave_economy_reading isolates dependence on enslaved labor as the primary extraction mechanism. Each reading describes a true mechanism; the readings are not contradictory but complementary. They are linked via network.affects_constraints to enable contamination analysis: a breakdown in one exclusionary mechanism may shift pressure to the others (e.g., if tributary income collapsed, dependence on slave labor or citizen-privilege restrictions might intensify).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
