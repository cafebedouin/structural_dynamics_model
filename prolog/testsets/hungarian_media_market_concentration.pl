% ============================================================================
% CONSTRAINT STORY: hungarian_media_market_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hungarian_media_market_concentration, []).

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
 *   constraint_id: hungarian_media_market_concentration
 *   human_readable: Hungarian Media Market Concentration and Editorial Control
 *   domain: political_economy/media_control
 *
 * SUMMARY:
 *   Hungary's media market exhibits textbook regulatory capture combined with
 *   capital concentration, creating a system that suppresses independent
 *   journalism while maintaining performative compliance with EU media
 *   freedom standards. The constraint evolved from fragmented post-communist
 *   media (1990s) through gradual oligarchic concentration (2000s) to
 *   state-aligned editorial control (2010s-present). The extractiveness
 *   trajectory (0.35→0.52→0.68) reflects accumulation of control mechanisms:
 *   advertising flows to state-aligned outlets, independent outlets face
 *   regulatory harassment and economic pressure, opposition parties lose
 *   access to broadcast media, and journalists internalize self-censorship
 *   norms. The theater ratio (0.28→0.45→0.58) reflects increasing
 *   performativity: formal regulatory compliance (broadcasting codes,
 *   ownership limits) coexists with substantive capture (regulatory
 *   exemptions for friendly oligarchs, state budget advertising concentrated
 *   in aligned outlets, defamation suits against critical reporting). From
 *   the victim perspective (independent journalists, opposition parties),
 *   this is a Snare — no meaningful exit option exists within Hungarian media
 *   ecosystem. From the beneficiary perspective (government coalition, media
 *   oligarchs), this is Rope — coordination of political messaging and
 *   cultural narratives. The EU oversight framework (Perspective 6) appears
 *   as Piton — formal rules exist but enforcement is weak. The natural law
 *   view (Perspective 7) risks naturalizing the constraint as inevitable
 *   market concentration, obscuring contingent policy choices around media
 *   ownership and broadcasting licensing.
 *
 * KEY AGENTS:
 *   - Independent Journalists: Primary victim (powerless/trapped) — face career barriers, legal harassment, advertiser pressure; minimal exit options within national ecosystem
 *   - Opposition Political Parties: Secondary victim (moderate/constrained) — limited access to major broadcast media; fragmented reach through online platforms and newspapers
 *   - Orbán Political Coalition (Fidesz and allied parties): Primary beneficiary (institutional/arbitrage) — maintains political messaging dominance; uses state media for agenda-setting
 *   - Government-Aligned Oligarchs (Mészáros, Szíjjártó holdings, others): Primary beneficiary (institutional/arbitrage) — controls TV2, RTL advertising networks, regional papers; gains regulatory favor and business advantage through alignment
 *   - State Broadcasting Authority (MTVA): Captured institutional actor (institutional/constrained) — nominally public but increasingly executive-controlled; provides legitimacy for state messaging
 *   - International Media Freedom Organizations: Organized observer (organized/constrained) — Reporters Without Borders, Committee to Protect Journalists, EU bodies; limited enforcement power
 *   - Hungarian General Public: Broad victim (powerless/constrained) — information ecosystem biased toward government narratives; alternative sources fragmented and difficult to access
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hungarian_media_market_concentration, 0.68).
domain_priors:suppression_score(hungarian_media_market_concentration, 0.72).
domain_priors:theater_ratio(hungarian_media_market_concentration, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hungarian_media_market_concentration, extractiveness, 0.68).
narrative_ontology:constraint_metric(hungarian_media_market_concentration, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hungarian_media_market_concentration, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hungarian_media_market_concentration, snare).
narrative_ontology:human_readable(hungarian_media_market_concentration, "Hungarian Media Market Concentration and Editorial Control").
narrative_ontology:topic_domain(hungarian_media_market_concentration, "political_economy/media_control").

domain_priors:requires_active_enforcement(hungarian_media_market_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hungarian_media_market_concentration, orban_political_coalition).
narrative_ontology:constraint_beneficiary(hungarian_media_market_concentration, friendly_oligarchs).
narrative_ontology:constraint_victim(hungarian_media_market_concentration, independent_journalists).
narrative_ontology:constraint_victim(hungarian_media_market_concentration, opposition_political_parties).
narrative_ontology:constraint_victim(hungarian_media_market_concentration, general_public_information_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT JOURNALISTS (SNARE) — Trapped within the Hungarian media ecosystem with minimal exit options. Career advancement requires access to major media outlets now controlled by government-friendly oligarchs. Publishing critical content risks loss of access, professional isolation, advertiser pressure on employers, and legal harassment through defamation suits. No meaningful alternative employment for specialized political journalists. Maximum extraction experienced — independence becomes unaffordable.
constraint_indexing:constraint_classification(hungarian_media_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSITION POLITICAL PARTIES (SNARE) — Constrained but not fully trapped. Can access some media outlets (Index, 444, independent online platforms), but reach is fragmented. Major broadcast media (TV, radio) largely inaccessible without purchasing expensive advertising. Campaign messaging dominated by state-aligned media framing. Exit cost (emigration, overseas media operations) is high but theoretically possible. High extraction — limited voice in setting political agenda.
constraint_indexing:constraint_classification(hungarian_media_market_concentration, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GOVERNMENT-ALIGNED OLIGARCHS (ROPE) — Primary beneficiary. Controls largest broadcast outlets (TV2, RTL Klub ownership structures). Experiences constraint as coordination mechanism: state cooperation enables regulatory favor, advertiser incentives, and preferential licensing. Extraction runs toward this agent. Has arbitrage options (can switch business sectors, maintain political access through alternative means) but chooses media ownership because it provides political leverage. Net beneficiary.
constraint_indexing:constraint_classification(hungarian_media_market_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE BROADCASTING AUTHORITY (TANGLED ROPE) — Constrained by its public mandate to serve broad audience interests, but increasingly captured by executive control. Provides some genuine coordination function (emergency broadcasting, public information) alongside asymmetric extraction (suppressing critical coverage, amplifying government messaging). Active enforcement required through budget control and personnel appointments. Both coordination and extraction present in the same institution.
constraint_indexing:constraint_classification(hungarian_media_market_concentration, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL CIVIL SOCIETY (TANGLED ROPE) — Organized actors (Reporters Without Borders, media freedom advocacy, EU oversight bodies) see mixed coordination and extraction. The constraint coordinates media into a political control system (asymmetric benefit to governing coalition). Has some exit options (EU pressure mechanisms, international sanctions) but constrained by sovereignty concerns and limited enforcement power. Generational horizon reveals that erosion of press freedom becomes path-dependent — increasingly difficult to reverse once concentrated ownership is entrenched.
constraint_indexing:constraint_classification(hungarian_media_market_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: EU MEDIA FREEDOM FRAMEWORK (PITON) — EU rules (Article 10 ECHR, Media Freedom Act, digital services regulation) exist but enforcement is largely theatrical. Hungary maintains nominal compliance through regulatory workarounds while substantive freedom erodes. EU procedures (infringement actions, Article 7 hearings) provide ritualistic oversight without mechanism to restore competitive media landscape. Persists through institutional inertia despite low effectiveness.
constraint_indexing:constraint_classification(hungarian_media_market_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational timescale, media concentration appears as an inevitable consequence of capital concentration under weak regulation — a natural law of market dynamics. Larger outlets always tend to dominate smaller competitors absent sustained antitrust enforcement. But this naturalizes a contingent policy choice (decades of weak media ownership regulation, privatization without diversification requirements). The engine's false summit detector will flag this as naturalization rather than genuine irreducibility.
constraint_indexing:constraint_classification(hungarian_media_market_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hungarian_media_market_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hungarian_media_market_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hungarian_media_market_concentration, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hungarian_media_market_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hungarian_media_market_concentration, TR),
    TR >= 0.70.

:- end_tests(hungarian_media_market_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The state-aligned coalition extracts significant political benefit — editorial control over narrative, suppression of opposition messaging, normalization of corruption narratives favorable to oligarchy. The extraction is not total (some independent outlets survive, internet reduces barriers to information) but substantial across major broadcast media. The trajectory (0.35→0.68 over 16 years) reflects accumulation through: ownership concentration (major outlets consolidated under friendly oligarchs), regulatory capture (selective enforcement against critical outlets), and normalization (journalists self-censor to preserve career viability). Suppression (0.72): Very high. Multiple barriers prevent independent journalism: ownership concentration eliminates job prospects, defamation liability threatens independent outlets, advertiser withdrawal pressures skeptical coverage, professional isolation punishes deviation from consensus. The suppression is not absolute (internet access, some remaining independent outlets, diaspora journalism) but covers most high-reach channels. Theater ratio (0.58): Moderate-high. The system maintains formal compliance with EU media directives while substantively capturing editorial output. Regulatory exemptions (TV2 and RTL ownership structures that nominally comply with concentration limits), formal licensing procedures (that always approve friendly applicants), and press councils (that rarely sanction state outlets) provide theatrical legitimacy. The theater has increased as international pressure mounted — formal procedures now more elaborate to mask substantive capture.
 *
 * PERSPECTIVAL GAP:
 *   This constraint manifests maximum perspectival gap. The beneficiary's Rope perspective (coordination of political communication and oligarchic stability) is genuinely their lived experience — the constraint does solve their collective action problem. The victim's Snare perspective (no exit, maximum suppression, pure extraction) is equally real from their structural position. The captured state broadcaster sits in the middle, experiencing genuine coordination (public service mandate) fused with extraction (executive direction). International observers from outside the system (EU bodies, media freedom organizations) see performance and theater because they lack power to change outcomes. The natural law observer (civilizational timescale) risks seeing the system as inevitable rather than contingent. Each perspective is structurally grounded in the agent's actual position — the gap reveals that the constraint's 'type' depends entirely on where you stand within it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from the agent's relationship to the extraction. Government beneficiaries have low d (0.05-0.15 range) — they are net recipients, experiencing negative effective extraction. Captured institutional actors (state media) have moderate d (0.35-0.45) — they provide some coordination function (emergency broadcasting, public information) alongside capture. Independent journalists have very high d (0.92-0.98) — they bear maximum extraction with no exit. Opposition parties have high d (0.80-0.88) — they are primary targets of suppression. The beneficiary group (oligarchs, political coalition) derives d from arbitrage exit options + beneficiary status → low f(d) → extraction runs toward them. The victim groups derive d from trapped/constrained exit options + victim status → high f(d) → they bear the extraction. Captured institutional actors have mixed derivation: they appear as beneficiaries (get continued operating authority) but are constrained from exit (cannot refuse executive direction without losing institutional position) → moderate d.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED through perspectival analysis: The constraint simultaneously satisfies the definitions of Snare (from victim perspective: ε=0.68, suppression=0.72, χ≥0.66, minimal coordination benefit), Tangled Rope (from state broadcaster perspective: genuine coordination function + asymmetric extraction + active enforcement), Rope (from beneficiary perspective: solves coordination problem of political alignment), and Piton (from EU observer perspective: formal rules + performative enforcement + degraded function). The resolution is that ALL these are correct descriptions of different agents' structural experiences. The apparent mandatrophy dissolves when recognizing that constraint classification is inherently indexical — it depends on the position of the agent being described. This is not a 'which type is really correct' problem but rather a confirmation that the Deferential Realism framework correctly captures that the same structural phenomenon (media market concentration + state control) simultaneously appears as coordination to beneficiaries, extraction to victims, and performance to powerless observers. The high extractiveness (0.68) confirms that this is not a pure coordination problem being misunderstood — genuine asymmetric benefit exists. The mandatrophy is resolved by recognizing that Snare is the primary classification (the constraint's essential structural feature is extraction), but perspectives from other positions reveal how the extraction is enabled and legitimized through coordination and theater narratives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_vs_ownership_capture,
    'Is the constraint primarily regulatory capture (state apparatus directing content through legal/budget control) or market capture (ownership concentration reducing competitive alternatives)?',
    'Counterfactual analysis: what would happen if executive control ended? If media diversity returns rapidly, constraint was primarily regulatory. If concentration persists due to ownership structure, constraint is primarily market-based.',
    'If regulatory: Scaffold perspective (sunset through regime change) becomes primary. If market-based: Snare perspective (extraction embedded in capital structure) becomes more durable. Classification severity and timeline for resolution depend critically on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_vs_ownership_capture, empirical, 'Relative weight of regulatory vs market capture mechanisms').

omega_variable(
    audience_belief_lock,
    'To what extent is the suppression mechanism maintained by audience internalization of state-aligned narratives (identity_locked dynamic) versus structural barriers to accessing alternatives (trapped or constrained dynamic)?',
    'Survey data on news consumption patterns and source diversity; analysis of audience belief formation; measurement of narrative persistence after policy changes; longitudinal tracking of audiences exposed to alternative information sources.',
    'If primarily internalized: audiences are identity_locked and will resist alternatives even if available (suppression persists beyond structural removal). If primarily structural: removal of ownership concentration enables rapid audience reorientation. This determines whether lifting the constraint requires institutional change only or sustained counter-narrative work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audience_belief_lock, empirical, 'Internalized vs structural suppression mechanisms in audience belief formation').

omega_variable(
    international_alignment_necessity,
    'Is media control in Hungary functionally tied to NATO/EU alignment (coordination function) or purely extractive for domestic political benefit?',
    'Analysis of content patterns comparing EU-favorable vs domestic-critical coverage; examination of state media behavior during EU policy negotiations; counterfactual assessment of whether state would maintain media control absent geopolitical positioning.',
    'If tied to alignment: constraint might classify as Tangled Rope (genuine coordination function of maintaining Western orientation alongside extraction). If purely extractive: Snare classification is confirmed. Timeline for EU intervention effectiveness depends on this.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_alignment_necessity, conceptual, 'Whether media control serves geopolitical coordination or pure domestic extraction').

omega_variable(
    regulatory_enforcement_efficacy,
    'Can EU regulatory mechanisms (Media Freedom Act, Digital Services Act, competition law) actually restore competitive media landscape, or are they structurally incapable of reversing entrenched ownership?',
    'Historical analysis of media concentration reversals in Europe following regulatory intervention; technical examination of EU enforcement mechanisms for media ownership cases; assessment of member state capacity to resist enforcement.',
    'If efficacious: Scaffold perspective correct (EU framework provides sunset mechanism). If ineffective: constraint is Snare or Piton (extraction persists regardless of regulatory theater). Determines whether international pressure is viable intervention or merely performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_efficacy, empirical, 'Whether EU regulatory mechanisms can reverse entrenched media concentration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hungarian_media_market_concentration, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hunmedia_tr_t0, hungarian_media_market_concentration, theater_ratio, 0, 0.28).
narrative_ontology:measurement(hunmedia_tr_t8, hungarian_media_market_concentration, theater_ratio, 8, 0.45).
narrative_ontology:measurement(hunmedia_tr_t16, hungarian_media_market_concentration, theater_ratio, 16, 0.58).

% Extraction over time
narrative_ontology:measurement(hunmedia_be_t0, hungarian_media_market_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hunmedia_be_t8, hungarian_media_market_concentration, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(hunmedia_be_t16, hungarian_media_market_concentration, base_extractiveness, 16, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hungarian_media_market_concentration, information_standard).
narrative_ontology:affects_constraint(hungarian_media_market_concentration, hungarian_rule_of_law_degradation).
narrative_ontology:affects_constraint(hungarian_media_market_concentration, eu_democratic_backsliding).

% DUAL FORMULATION NOTE:
% Media concentration in Hungary is structurally linked to broader rule of law degradation (judicial capture, constitutional amendments favoring executive) and EU-level democratic backsliding (contagion effects to Poland, Slovakia, others). The constraint family includes: hungarian_rule_of_law_degradation (ε=0.60, Tangled Rope) as upstream cause/enabler, hungarian_media_market_concentration (ε=0.68, Snare) as manifestation, and eu_democratic_backsliding (ε=0.52, Snare) as downstream consequence and contagion vector.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hungarian_media_market_concentration, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
