% ============================================================================
% CONSTRAINT STORY: platform_content_moderation_snare
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_content_moderation_snare, []).

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
 *   constraint_id: platform_content_moderation_snare
 *   human_readable: Platform Content Moderation as Structural Snare
 *   domain: digital_platforms/content_moderation
 *
 * SUMMARY:
 *   Platform content moderation creates a structural snare: users depend
 *   entirely on centralized platforms for audience reach and discourse
 *   participation, platforms wield unilateral suspension authority with
 *   minimal meaningful recourse, and the mechanisms of suppression
 *   (algorithmic opacity, asymmetric appeals, network lock-in, data
 *   dependency) prevent organized exit or negotiation. The snare operates
 *   across multiple dimensions simultaneously: economic (creators depend on
 *   platform ad revenue and algorithm distribution), social (audience and
 *   followers are platform-mediated assets), technological (data portability
 *   and federation remain underdeveloped), and political (regulatory
 *   frameworks have historically privileged platform liability immunity). The
 *   constraint exhibits rising extraction (0.42 → 0.68) as platforms have
 *   consolidated market position and faced regulatory pressure to increase
 *   moderation intensity. Theater ratio rising (0.35 → 0.58) reflects
 *   deployment of formal appeals processes, transparency reports, and policy
 *   statements as performative compliance rituals that do not meaningfully
 *   constrain suspension authority. Suppression requirement rising (0.58 →
 *   0.72) indicates intensifying mechanisms: algorithmic content filtering
 *   with minimal human review, asymmetric appeals processes, and deliberate
 *   opacity about policy application.
 *
 * KEY AGENTS:
 *   - Suspended Users: Primary victims (powerless/trapped) — depend entirely on platform for reach; suspension destroys livelihood and social participation; minimal recourse; cannot organize collective response.
 *   - Marginal Creators: Secondary victims (moderate/constrained) — constrained by audience concentration on dominant platforms; self-censorship from algorithmic penalty fear; extraction includes ad revenue share and behavioral data harvesting.
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture data value, behavioral control, content leverage; full exit capacity; treat moderation as coordination problem justifying asymmetric authority.
 *   - Organized Resistance (Civil Society, Regulators): (organized/mobile) — developing interoperability standards, federated alternatives, regulatory requirements (DMA, Online Safety Bills); have exit pathways and leverage.
 *   - Content Moderation Theater: Institutional performance (institutional/arbitrage) — formal appeals, transparency reports, policy statements function as compliance ritual; actual mechanism (algorithmic opacity, unilateral control) persists.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent platform business model as inherent to scale; treats moderation bottleneck as immutable natural law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_content_moderation_snare, 0.68).
domain_priors:suppression_score(platform_content_moderation_snare, 0.72).
domain_priors:theater_ratio(platform_content_moderation_snare, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_content_moderation_snare, extractiveness, 0.68).
narrative_ontology:constraint_metric(platform_content_moderation_snare, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(platform_content_moderation_snare, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_content_moderation_snare, snare).
narrative_ontology:human_readable(platform_content_moderation_snare, "Platform Content Moderation as Structural Snare").
narrative_ontology:topic_domain(platform_content_moderation_snare, "digital_platforms/content_moderation").

domain_priors:requires_active_enforcement(platform_content_moderation_snare).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_content_moderation_snare, platform_operators).
narrative_ontology:constraint_victim(platform_content_moderation_snare, suspended_users).
narrative_ontology:constraint_victim(platform_content_moderation_snare, marginal_creators).
narrative_ontology:constraint_victim(platform_content_moderation_snare, discourse_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUSPENDED USER (SNARE) — Faces total audience loss with minimal recourse. User depends entirely on platform for reach; suspension destroys livelihood and social participation simultaneously. No alternative distribution channel provides comparable reach. Appeals process is opaque and rarely successful. The user cannot organize collective response because the platform's suppression (account termination, de-indexing) prevents coordination.
constraint_indexing:constraint_classification(platform_content_moderation_snare, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINAL CREATOR (SNARE) — Constrained by audience concentration on dominant platforms. Creator has built audience base on platform; leaving means starting from zero. Self-censorship emerges from fear of algorithmic penalty or moderation action. High suppression because constraints operate through algorithmic opacity and content policy ambiguity, not explicit threat. Extraction includes ad revenue share, data harvesting, and behavioral nudging.
constraint_indexing:constraint_classification(platform_content_moderation_snare, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences moderation as coordination mechanism: removing illegal content, preventing harassment, and maintaining platform functionality enable network effects and user retention. Operator has full exit capacity (can choose moderation standards, appeal processes, transparency levels). From this position, the constraint solves a genuine collective action problem (many users benefit from safe space). Extraction to operators (data harvesting, behavior control, content leverage) is treated as necessary cost of service provision.
constraint_indexing:constraint_classification(platform_content_moderation_snare, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZED RESISTANCE (TANGLED ROPE) — Civil society organizations, advocacy groups, and regulatory bodies have developed exit pathways: interoperability standards, federated platforms (Mastodon, Bluesky), decentralized alternatives. These organized actors see genuine coordination function (content safety) but recognize asymmetric extraction (data harvesting, behavioral manipulation, speech control). Classified as tangled rope rather than snare because organized actors have leverage, regulatory visibility, and alternative platforms developing. Sunset pressure from DMA/Online Safety Bill creates structural change.
constraint_indexing:constraint_classification(platform_content_moderation_snare, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MODERATION THEATER (PITON) — The formal appeals process, public policy statements, and transparency reports function largely as performative compliance rituals. The actual moderation mechanism (algorithmic content filtering, user reporting queues, ML classifiers) operates with minimal human review and substantial error rates. Operators maintain the theater (appeals, policies, reports) because regulatory pressure demands it, not because it effectively addresses the underlying extraction. Theater ratio is moderate (0.58) because some genuine safety coordination exists, but the primary function (opacity + unilateral control) persists.
constraint_indexing:constraint_classification(platform_content_moderation_snare, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some centralized moderation is inherent to any at-scale communication system: perfect decentralization enables harassment, illegal content, and manipulation at scale; perfect transparency about moderation enables evasion. This perspective treats the moderation bottleneck as an immutable feature of network coordination. However, the structural data (identifiable beneficiaries, unilateral authority, algorithmic opacity, trapped users) triggers false summit detection. The 'necessity' framing naturalizes what is actually a contingent business model choice.
constraint_indexing:constraint_classification(platform_content_moderation_snare, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_content_moderation_snare_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_content_moderation_snare, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_content_moderation_snare, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_content_moderation_snare, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_content_moderation_snare, TR),
    TR >= 0.70.

:- end_tests(platform_content_moderation_snare_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Platform operators capture substantial value through data harvesting, behavioral manipulation, and audience control. Users cannot access their own audiences or data without platform mediation. Career and livelihood depend on algorithmic distribution. The extraction is particularly severe because it operates simultaneously through economic (ad revenue control), social (audience lock-in), technological (data portability barriers), and political (regulatory immunity) mechanisms. Suppression (0.72): High. Barriers to exit are substantial and multifaceted: network effects concentrate audience on dominant platforms; alternative platforms lack equivalent reach; formal appeals processes have minimal success rates; users lack transparency about why content was removed or what policy violation occurred; algorithmic opacity prevents users from understanding moderation logic; account termination prevents coordination to challenge decisions. The constraint mechanism relies on suppression rather than consent — users remain on platforms despite recognition of extraction because alternatives are worse. Theater ratio (0.58): Moderate-high. Platforms maintain formal moderation governance structures (appeals, transparency reports, policy statements) as performative compliance, particularly in response to regulatory pressure. These theaters do not constrain the actual moderation mechanism (unilateral algorithmic authority). However, theater ratio is not higher because some genuine safety coordination exists — content policy against illegal material and harassment addresses real coordination problems, creating legitimacy cover for asymmetric extraction. The rising trajectory reflects platforms deploying more elaborate theater (appeals processes, public policy work) while maintaining algorithmic opacity and unilateral authority.
 *
 * PERSPECTIVAL GAP:
 *   The suspended user and platform operator experience the same constraint mechanism as entirely different types: rope (coordination benefit) vs. snare (extraction trap). The gap reveals the asymmetry of power, exit capacity, and information access. The operator can choose moderation standards, observe aggregate suspension patterns, and adjust policies. The user cannot see why they were suspended, has no meaningful appeal, and cannot organize with other suspended users (who are, by definition, silenced). The organized resistance perspective occupies the middle: recognizing both the genuine coordination problem (platforms do need to prevent harassment and illegal content) and the asymmetric extraction (unilateral authority, data harvesting, behavioral manipulation). The tangled rope classification reflects this middle position — genuine coordination function but asymmetric extraction and active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's effective extractiveness is computed from base extractiveness (0.68) scaled by directionality f(d) and scope modifier. Platform operators with arbitrage exit and beneficiary status derive d ≈ 0.05, producing negative effective extraction (the platform solves their problem). Suspended users with trapped exit and victim status derive d ≈ 0.92, producing maximum effective extraction. Marginal creators with constrained exit and victim status derive d ≈ 0.85, producing high effective extraction. Organized resistance with mobile exit and mixed beneficiary/victim status derive d ≈ 0.60, producing moderate effective extraction. The directionality derivation from beneficiary/victim declarations and exit options produces the perspectival classification distribution: the same base structure appears as rope to beneficiaries, snare to powerless victims, tangled rope to organized actors with developing exit options, and piton to the formal appeals theater.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare classification is confirmed by the structural data: (1) primary victims with trapped exit options experiencing the constraint as unilateral authority with minimal recourse; (2) suppression (0.72) exceeds the snare threshold (0.60), indicating coercion-dominant mechanism; (3) beneficiaries exist (platform operators, shareholders) whose interests are structurally opposed to victim interests; (4) alternatives exist but are suppressed by network effects, data lock-in, and regulatory immunity rather than technical inability; (5) the constraint persists through asymmetric information (operators understand moderation logic; users do not) and asymmetric power (operators control data, distribution, appeals; users have no leverage). The mandatrophy is resolved by recognizing that what appears to platforms as coordination (content safety, network effects) manifests to users as extraction (unilateral authority, data harvesting, behavioral control). The false summit perspective (mountain/natural law) is identified: the 'necessity' framing treats contingent business model choices (unilateral moderation authority, algorithmic opacity, data lock-in) as inherent features of platform scale. The organized resistance perspective (tangled rope with sunset pressure) identifies structural dissolution paths: regulatory requirements (DMA, Online Safety Bills), technical development (federation, interoperability), and platform alternatives (Threads, Bluesky) are creating exit options and constraining extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_versus_design_choice,
    'Is centralized unilateral moderation authority a necessary feature of platform scale, or a contingent design choice that could be replaced by alternative governance models?',
    'Comparative analysis of decentralized moderation (Mastodon''s instance federation, community standards, inter-instance blocking). Measurement of moderation effectiveness (harassment prevention, illegal content removal) across centralized vs. federated platforms at comparable scale.',
    'If necessary: mountain classification holds; constraint is immutable. If contingent: false summit confirmed; constraint reclassifies to snare or tangled rope depending on deployment of alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_versus_design_choice, empirical, 'Whether unilateral moderation authority is structurally necessary or contingent').

omega_variable(
    algorithmic_opacity_function,
    'Does algorithmic opacity serve a genuine security purpose (preventing evasion of content policies) or primarily serve platform control?',
    'Audit of moderation algorithm design: comparison between platforms with high transparency (Mastodon''s open-source moderation) vs. proprietary opacity; measurement of evasion rates and accuracy metrics; analysis of whether transparency would materially degrade moderation effectiveness.',
    'If security purpose: suppression (0.72) is legitimate coordination cost. If control purpose: suppression is pure extraction mechanism, supporting snare classification and raising extraction estimates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_opacity_function, empirical, 'Whether algorithmic opacity serves security or control function').

omega_variable(
    appeals_process_effectiveness,
    'Do formal appeals processes (Twitter''s bots appeals, YouTube''s manual review) meaningfully reduce false positives and restore suspended users?',
    'Data from internal platform audits and external research: success rates of appeals by violation category, time to reinstatement, distribution of successful appeals by user demographic.',
    'If effective (>40% appeal success): suppression understated; users have meaningful exit option within platform. If ineffective (<10%): theater ratio confirmed as high; appeals are performative ritual, not functional recourse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appeals_process_effectiveness, empirical, 'Actual effectiveness of formal appeals processes').

omega_variable(
    network_effects_substitutability,
    'Can alternative platforms (Bluesky, Threads, federated networks) provide substitutable reach and network effects, or is platform stickiness due to lock-in rather than genuine utility advantage?',
    'Analysis of user migration patterns post-suspension; measurement of creator revenue and audience overlap across platforms; comparative engagement metrics (time spent, interaction rates) controlling for content type and creator investment.',
    'If substitutable: exit_options for users upgrade from trapped to constrained or mobile; extraction estimates decline. If lock-in dominant: trapped classification confirmed; snare severity increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_substitutability, empirical, 'Substitutability of alternative platforms for reach and network effects').

omega_variable(
    suspension_targeting_pattern,
    'Are suspensions distributed randomly relative to policy violation, or concentrated on marginal voices and dissenting speech?',
    'Large-scale audit of suspension decisions: comparison of suspension rates by content category, political orientation, user follower count, and geography. Analysis of policy violations in suspended vs. unsuspended similar content.',
    'If random: snare classification requires revision; constraint appears as outcome of scale rather than extraction. If targeted: confirms asymmetric extraction; snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_targeting_pattern, empirical, 'Whether suspension targeting follows policy or reflects extraction bias').

omega_variable(
    interoperability_technical_feasibility,
    'Are technical barriers (data portability, protocol standardization, federation) the primary obstacles to meaningful interoperability, or do business model incentives prevent implementation?',
    'Technical feasibility studies (IETF protocols, data export standards); analysis of platform resistance to interoperability requirements (DMA compliance); timeline for viable federated alternatives.',
    'If technical barriers primary: snare may be transient; technological development dissolves constraint. If business model barriers: snare is active institutional choice; organized resistance perspective is accurate and sunset timeline applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_technical_feasibility, empirical, 'Whether interoperability barriers are technical or institutional').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_content_moderation_snare, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pcms_tr_t0, platform_content_moderation_snare, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pcms_tr_t5, platform_content_moderation_snare, theater_ratio, 5, 0.48).
narrative_ontology:measurement(pcms_tr_t10, platform_content_moderation_snare, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(pcms_be_t0, platform_content_moderation_snare, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pcms_be_t5, platform_content_moderation_snare, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(pcms_be_t10, platform_content_moderation_snare, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pcms_su_t0, platform_content_moderation_snare, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(pcms_su_t5, platform_content_moderation_snare, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(pcms_su_t10, platform_content_moderation_snare, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_content_moderation_snare, enforcement_mechanism).
narrative_ontology:affects_constraint(platform_content_moderation_snare, algorithmic_ranking_opacity).
narrative_ontology:affects_constraint(platform_content_moderation_snare, data_portability_barriers).
narrative_ontology:affects_constraint(platform_content_moderation_snare, network_effects_lock_in).

% DUAL FORMULATION NOTE:
% Platform content moderation is a single constraint covering the full moderation system (authority, enforcement, appeals, transparency). Sibling constraints in the platform constraint family include algorithmic ranking (a separate extraction mechanism using similar opacity and beneficiary advantage), data portability barriers (institutional choice preventing user exit), and network effects lock-in (structural dependency amplifying snare severity). All three affect moderation's extractiveness: removing any one would reduce snare severity to tangled rope or scaffold.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_content_moderation_snare, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
