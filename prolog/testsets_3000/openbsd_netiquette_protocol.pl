% ============================================================================
% CONSTRAINT STORY: openbsd_netiquette_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openbsd_netiquette_protocol, []).

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
 *   constraint_id: openbsd_netiquette_protocol
 *   human_readable: OpenBSD Mailing List Netiquette Protocol
 *   domain: technological/social
 *
 * SUMMARY:
 *   The OpenBSD mailing list netiquette protocol represents a constraint that
 *   enforces strict communication standards — proper formatting,
 *   comprehensive problem descriptions, specific questions, reproducible test
 *   cases, no HTML or attachments, careful English grammar — to maintain
 *   discussion quality and prevent noise. This constraint exhibits tension
 *   between coordination (enabling technical focus) and extraction (excluding
 *   contributors who cannot meet rigid standards). The same structural
 *   phenomenon — strict communication norms enforced by core developers —
 *   appears as a necessary coordination mechanism (rope), an extractive
 *   barrier to entry (snare), a temporary structure being supplanted by
 *   alternative platforms (scaffold), a performative ritual (piton), a mixed
 *   mechanism (tangled rope), or an immutable aspect of large-group
 *   collaboration (mountain), depending on the observer's structural
 *   position. The protocol's theater ratio (0.48) reflects that enforcement
 *   balances functionality with performative demonstration of project values
 *   — public corrections serve both to improve future messages and to perform
 *   the project's commitment to rigor.
 *
 * KEY AGENTS:
 *   - Core OpenBSD Developers: Primary beneficiary (institutional/arbitrage) — experience constraint as enabling; can bypass standards or use private channels; benefit from filtered discussion
 *   - Technical Discussion Quality: Primary beneficiary (institutional/arbitrage) — abstract collective good that benefits from reduced noise and focused problem-solving
 *   - Novice Contributors: Primary victim (powerless/trapped) — face message rejection, thread exclusion, and barrier to entry; cannot exit without abandoning project participation
 *   - Casual Users: Secondary victim (moderate/constrained) — can lurk without posting; attempt to contribute triggers enforcement; partial exit option (read-only participation)
 *   - Non-English Speakers: Secondary victim (moderate/constrained) — grammar and formatting requirements implicitly filter for native speakers; constrained exit (can post in native language but may face rejection)
 *   - Alternative Platform Ecosystem: Organized competitor (organized/mobile) — Mastodon, Matrix, Discord, GitHub discussions offer alternative coordination channels with lower entry barriers; mobile exit available
 *   - Email Infrastructure: Institutional actor (institutional/arbitrage) — mailing list persists through inertia despite degraded UX; performs project identity more than function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openbsd_netiquette_protocol, 0.38).
domain_priors:suppression_score(openbsd_netiquette_protocol, 0.52).
domain_priors:theater_ratio(openbsd_netiquette_protocol, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openbsd_netiquette_protocol, extractiveness, 0.38).
narrative_ontology:constraint_metric(openbsd_netiquette_protocol, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(openbsd_netiquette_protocol, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openbsd_netiquette_protocol, tangled_rope).
narrative_ontology:human_readable(openbsd_netiquette_protocol, "OpenBSD Mailing List Netiquette Protocol").
narrative_ontology:topic_domain(openbsd_netiquette_protocol, "technological/social").

domain_priors:requires_active_enforcement(openbsd_netiquette_protocol).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openbsd_netiquette_protocol, core_openbsd_developers).
narrative_ontology:constraint_beneficiary(openbsd_netiquette_protocol, technical_discussion_quality).
narrative_ontology:constraint_victim(openbsd_netiquette_protocol, novice_contributors).
narrative_ontology:constraint_victim(openbsd_netiquette_protocol, casual_users).
narrative_ontology:constraint_victim(openbsd_netiquette_protocol, non_english_speakers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASPIRING CONTRIBUTOR (SNARE) — Cannot exit the communication standards without sacrificing project participation. Trapped between desire to contribute and rigid enforcement of formatting, tone, and preparation requirements. Moderation can result in message rejection, thread exclusion, or public shaming. Maximum experienced extraction due to zero degrees of freedom.
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PERIPHERAL DEVELOPER (TANGLED ROPE) — Experiences both coordination benefit and extraction burden. The standards enforce discussion quality that benefits all (enables focused technical problem-solving). But enforcement is punitive and discretionary — public corrections, thread exclusions, and informal reputation penalties. Constrained exits: can post to forums or blogs instead, but mailing list participation carries higher technical weight in the project.
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CORE DEVELOPMENT TEAM (ROPE) — Benefits from strict netiquette as a coordination mechanism. Standards filter noise, reduce moderation burden, and accelerate decision-making. Team members experience the constraint as enabling: clear rules allow them to skip off-topic threads and focus on technical content. Arbitrage exit: can use private channels or direct communication if needed. Net beneficiary.
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE ECOSYSTEM (SCAFFOLD) — Sees the protocol as a temporary coordinating structure with an implicit sunset. As communication tools mature (Mastodon, Matrix, Discord, collaborative wikis), the mailing list's role as the sole coordination medium diminishes. The strictness serves a purpose during the list-era but is becoming redundant. Mobile exit: developers can migrate to other platforms without losing project voice. Theater ratio low for this perspective because the protocol is purely functional, not performative.
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: EMAIL PROTOCOL INFRASTRUCTURE (PITON) — The mailing list format and enforcement are maintained through institutional inertia despite better alternatives existing. Email threading is degraded for large discussions; rich formatting is deliberately suppressed; mobile-friendly composition is difficult. The constraint persists because OpenBSD's identity is built on 'no bullshit' culture — the performance of strictness matters as much as its function. Theater ratio high: the enforcement ritual (public corrections, canonical thread formats) is performative — it demonstrates project values more than it improves outcomes.
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some standardization of discourse is inherent to collective technical problem-solving: large groups always require clarity norms, and enforcing preparation gates (test cases, reproducible reports, specific questions) is a structural feature of effective debugging collaboration. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the 'inherent to technical collaboration' framing naturalizes what is actually a contingent institutional choice about enforcement severity.
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openbsd_netiquette_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openbsd_netiquette_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openbsd_netiquette_protocol, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(openbsd_netiquette_protocol, TR),
    TR >= 0.70.

:- end_tests(openbsd_netiquette_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The protocol does enforce participation barriers — message rejection, public shaming, thread exclusion — that extract from those unable or unwilling to meet standards. But the extraction is not as severe as a snare (0.66+) because legitimate alternatives exist (read-only participation, alternative platforms) and the standards do serve a real coordination function. The value reflects that the constraint contains both genuine coordination and genuine exclusion. Suppression (0.52): Moderate-high. Significant barriers to participation include formatting requirements, English proficiency expectations, pressure to do advance research before posting, and discretionary moderation. But suppression is not total — messages can be reposted with corrections, and lurking remains accessible. Theater ratio (0.48): Moderate. The enforcement ritual (public corrections, canonical thread formats, 'no top-posting' policing) serves partly to maintain discussion quality (functional) and partly to perform the project's identity as rigorous and uncompromising (performative). The ratio is not low (pure functionality) or high (pure performance) — it's genuinely mixed.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a clear perspectival gap between institutional beneficiaries and powerless victims. The core development team sees coordination (Rope) — strict standards enable focused problem-solving. Aspiring contributors see extraction (Snare) — rigid barriers prevent participation. The open source ecosystem sees a temporary structure (Scaffold) — alternative platforms are reducing the mailing list's monopoly on project voice. The mailing list infrastructure itself sees a degraded ritual (Piton) — email threading is worse than modern alternatives, yet persists. Peripheral developers experience the mixed reality (Tangled Rope) — the standards do enable quality discussion, but enforcement is punitive. The civilizational analytical observer risks a false summit (Mountain) — 'large groups always need communication standards' — when the structural data reveals a contingent choice about enforcement severity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to the constraint. Core developers benefit from filtering (low d) and have escape routes (arbitrage exit). Aspiring contributors bear costs and cannot escape (high d). The constraint extracts from those trying to enter. Peripheral developers experience mixed costs and benefits (moderate d). The open source ecosystem sees the constraint as temporary (mobile exit, lower experienced d). The analytical observer risks naturalizing a contingent institutional choice (high false d). The directionality derivation reflects that beneficiaries are institutional, have arbitrage options, and experience low effective extraction; victims are powerless or moderate, have trapped/constrained options, and experience high effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC: This constraint resolves the mandatrophy by exposing how the same netiquette mechanism can be read as pure coordination (rope) from the core developer's perspective or pure extraction (snare) from the aspiring contributor's perspective. The mandate is not 'what should netiquette be?' but 'who benefits from this particular implementation of netiquette standards?' The resolution shows that the coordination function (filtering noise) is real AND the extraction function (excluding non-conformists) is real. Both readings are structurally correct from their respective positions. The constraint is not mislabeled coordination; it is genuine hybrid (tangled rope) because it simultaneously enables and excludes. The theater ratio (0.48) prevents piton misclassification — the enforcement is not purely performative. The moderate extractiveness (0.38) prevents snare misclassification — the coordination function is real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_severity_threshold,
    'What level of netiquette enforcement (warnings vs. message rejection vs. thread exclusion) distinguishes necessary coordination from punitive extraction?',
    'Comparative analysis of list quality metrics (signal-to-noise ratio, resolution time, code quality) across enforcement regimes; survey of contributor experience before/after policy changes',
    'If threshold is high (low enforcement): contributors participate freely but discussion quality degrades. If threshold is low (high enforcement): discussion quality maintained but contributor base shrinks and becomes homogeneous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_severity_threshold, empirical, 'Enforcement severity threshold for coordination vs. extraction').

omega_variable(
    alternative_platform_migration,
    'If OpenBSD moved development discussion to structured platforms (GitHub discussions, GitLab issues, Discourse forums), would discussion quality improve, degrade, or remain equivalent?',
    'Historical analysis of other projects'' migrations (Debian, PostgreSQL); comparison of signal-to-noise ratios, resolution times, and contributor diversity across platforms',
    'If quality improves: netiquette strictness is not necessary for coordination (snare classification strengthened). If quality degrades: strictness is functional (rope classification strengthened). If equivalent: choice of platform is orthogonal to underlying coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_migration, empirical, 'Whether alternative platforms would support equivalent discussion quality').

omega_variable(
    implicit_english_proficiency_gate,
    'To what extent do netiquette standards (complex formatting rules, cultural assumptions about tone, English grammar expectations) implicitly filter for native English speakers and exclude non-native contributors?',
    'Analysis of contributor linguistic diversity before/after policy changes; correlation between English fluency and message rejection rates; survey of non-native speakers on barriers to participation',
    'If high barrier: netiquette is extracting from linguistic minorities (snare classification strengthened, victim group clarified). If low barrier: standards are culturally inclusive (rope classification strengthened).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_english_proficiency_gate, empirical, 'Language proficiency gate implicit in netiquette standards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openbsd_netiquette_protocol, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(obsd_net_tr_t0, openbsd_netiquette_protocol, theater_ratio, 0, 0.35).
narrative_ontology:measurement(obsd_net_tr_t7, openbsd_netiquette_protocol, theater_ratio, 7, 0.45).
narrative_ontology:measurement(obsd_net_tr_t15, openbsd_netiquette_protocol, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(obsd_net_be_t0, openbsd_netiquette_protocol, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(obsd_net_be_t7, openbsd_netiquette_protocol, base_extractiveness, 7, 0.33).
narrative_ontology:measurement(obsd_net_be_t15, openbsd_netiquette_protocol, base_extractiveness, 15, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openbsd_netiquette_protocol, information_standard).
narrative_ontology:affects_constraint(openbsd_netiquette_protocol, open_source_contributor_retention).
narrative_ontology:affects_constraint(openbsd_netiquette_protocol, technical_documentation_quality).
narrative_ontology:affects_constraint(openbsd_netiquette_protocol, project_governance_legitimacy).

% DUAL FORMULATION NOTE:
% The netiquette protocol is downstream of OpenBSD's broader culture of rigor and skepticism. Separate constraints track specific effects: contributor retention (snare from victims' perspective), documentation quality (rope from beneficiaries' perspective), and governance legitimacy (piton as culture performance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(openbsd_netiquette_protocol, powerless, 0.92).
constraint_indexing:directionality_override(openbsd_netiquette_protocol, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
