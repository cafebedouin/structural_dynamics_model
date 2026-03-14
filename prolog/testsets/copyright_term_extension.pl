% ============================================================================
% CONSTRAINT STORY: copyright_term_extension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_term_extension, []).

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
 *   constraint_id: copyright_term_extension
 *   human_readable: Copyright Term Extension Regime
 *   domain: intellectual_property/cultural_commons
 *
 * SUMMARY:
 *   Copyright term extension represents a structural arrangement where legal
 *   monopoly periods for creative works are repeatedly extended (most notably
 *   in the US through the Sonny Bono Copyright Term Extension Act of 1998,
 *   which added 20 years). The constraint exhibits tangled rope
 *   characteristics: it coordinates efficient licensing and cross-border
 *   enforcement (genuine coordination function) while simultaneously
 *   extracting from derivative creators, researchers, and the public domain
 *   (asymmetric extraction). The core tension is between justifying term
 *   extension as incentive for original creation versus its actual function
 *   as rent extraction from works already created. The theater_ratio has
 *   increased over the measurement interval as digital distribution has made
 *   enforcement increasingly theatrical — the mechanism persists through
 *   legal threat and international treaty rather than economic necessity. The
 *   constraint's extractiveness has grown as works that were expected to
 *   enter public domain remain locked through legislative renewal.
 *
 * KEY AGENTS:
 *   - Derivative Creators: Primary victims (powerless/trapped) — unable to adapt, remix, or build on copyrighted works; face absolute barriers unless licensed
 *   - Academic Researchers: Secondary victims (moderate/constrained) — constrained access to copyrighted materials for scholarly purposes; fair use provides limited exit but is litigated and uncertain
 *   - Public Domain Creators: Diffuse victims (powerless/trapped) — potential future creators of works that would build on public domain material; abstract collective unable to organize
 *   - Content Rights Holders: Primary beneficiaries (institutional/arbitrage) — publishers, studios, music labels capturing extended monopoly rents; have arbitrage options through licensing and enforcement
 *   - Legacy Copyright Owners: Secondary beneficiaries (institutional/arbitrage) — estates and assignees of deceased creators maintaining copyright control; benefit from term extensions even though original creator incentives are moot
 *   - Open Culture Movement: Organized agents (organized/mobile) — Creative Commons, Internet Archive, Library of Congress building alternative pathways with explicit public domain dedication; see term extension as temporary institutional arrangement being superseded by open licensing
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy choice as incentive law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_term_extension, 0.58).
domain_priors:suppression_score(copyright_term_extension, 0.65).
domain_priors:theater_ratio(copyright_term_extension, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_term_extension, extractiveness, 0.58).
narrative_ontology:constraint_metric(copyright_term_extension, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(copyright_term_extension, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_term_extension, tangled_rope).
narrative_ontology:human_readable(copyright_term_extension, "Copyright Term Extension Regime").
narrative_ontology:topic_domain(copyright_term_extension, "intellectual_property/cultural_commons").

domain_priors:requires_active_enforcement(copyright_term_extension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_term_extension, content_rights_holders).
narrative_ontology:constraint_beneficiary(copyright_term_extension, legacy_copyright_owners).
narrative_ontology:constraint_victim(copyright_term_extension, public_domain_creators).
narrative_ontology:constraint_victim(copyright_term_extension, derivative_artists).
narrative_ontology:constraint_victim(copyright_term_extension, cultural_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DERIVATIVE CREATOR (SNARE) — Artists, filmmakers, and writers who wish to build on existing copyrighted works face near-absolute barriers. Works they intended to adapt or sample remain locked behind copyright for their entire professional life. No exit option exists short of abandoning the creative project or obtaining expensive licenses. Maximum experienced extraction — the constraint prevents creative reuse and maintains monopoly control over cultural material.
constraint_indexing:constraint_classification(copyright_term_extension, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACADEMIC RESEARCHER (TANGLED ROPE) — Scholars benefit from digitization and preservation infrastructure (coordination function) but face constrained access to copyrighted materials for quotation, analysis, and compilation. Fair use doctrine provides some exit, but fair use is litigated and uncertain. Research in literary history, media studies, and cultural analysis bears extraction costs (licensing fees, legal uncertainty) alongside coordination benefits (shared digital archives, scholarly commons).
constraint_indexing:constraint_classification(copyright_term_extension, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COPYRIGHT HOLDER (ROPE) — Publishers, film studios, and music labels experience the constraint as pure coordination: term extension synchronizes global copyright regimes, enabling efficient licensing and enforcement across jurisdictions. They benefit from extended monopoly periods and have arbitrage options (licensing, enforcement, territorial pricing). Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(copyright_term_extension, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN CULTURE MOVEMENT (SCAFFOLD) — Organized agents (Creative Commons, Internet Archive, Library of Congress) view term extension as a temporary coordination failure with a visible sunset. Open-licensing alternatives, digital preservation, and public domain farming are creating parallel pathways. The constraint has decreasing force as works enter public domain through alternative licenses or deliberate abandonment. Estimated sunset: as digital creation tools lower barriers to original content creation, cultural dependency on extended copyright terms declines.
constraint_indexing:constraint_classification(copyright_term_extension, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY PUBLISHING REGIME (PITON) — Traditional copyright enforcement and term extension mechanisms are largely theatrical in the digital era. Digital circumvention is trivial; enforcement relies on legal threat rather than technical or economic barriers. The regime persists through institutional inertia and international treaty obligation (Berne Convention, TRIPS) rather than functional necessity. Publishers maintain the term-extension fiction even as digital distribution makes the underlying economics obsolete.
constraint_indexing:constraint_classification(copyright_term_extension, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, copyright term extension could be framed as a natural incentive law: creators require exclusive control periods to capture return on creative investment. The constraint appears immutable — some form of exclusive control is inherent to knowledge production incentives. However, the structural data contradicts mountain classification. Term extension is a contingent policy choice, not a natural law. The engine identifies this as a false summit: the 'inherent to innovation' framing naturalizes what is actually a legal-political arrangement subject to reform.
constraint_indexing:constraint_classification(copyright_term_extension, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_term_extension_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(copyright_term_extension, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(copyright_term_extension, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_term_extension, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(copyright_term_extension, TR),
    TR >= 0.70.

:- end_tests(copyright_term_extension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting dual character of the constraint. Base extractiveness starts at 0.35 (coordination function for efficient licensing) and rises to 0.58 as works fail to enter public domain and digital barriers make enforcement increasingly theatrical rather than functional. The increase reflects rent extraction exceeding legitimate incentive compensation. Suppression (0.65): High. Barriers to accessing copyrighted material are institutional (licensing requirements, fair use litigation, international treaty enforcement) and technical (DRM, takedown notices). The constraint creates absolute barriers for some uses (adaptation without license, republication) and high-cost alternatives for others (fair use litigation, licensing fees). Theater ratio (0.68): Moderately high and increasing. Digital copyright enforcement is substantially performative — the technical ease of copying and the ubiquity of circumvention tools mean enforcement relies primarily on legal threat and institutional pressure rather than functional control. The constraint persists because institutions enforce it, not because the underlying economic scarcity it allegedly protects is real.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap is between those who can afford licensing and those who cannot. Copyright holders frame the constraint as coordination (efficient licensing, global synchronization). Derivative creators frame it as extraction (prevention of reuse). Researchers frame it as mixed (access barriers + scholarly commons). The organized open culture movement frames it as temporary (sunset through open licensing and public domain farming). The legacy publishing regime sees its own constraint as degraded ritual (piton). The false mountain classification occurs when term extension is naturalized as necessary for incentives — the analytical observer risks treating a policy choice as a law of knowledge production.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from power and exit options. Copyright holders (institutional/arbitrage) experience low or negative d — they are net beneficiaries with exit options through licensing and enforcement; f(d) is negative or near-zero. Derivative creators (powerless/trapped) experience maximum d approaching 1.0 — they have no exit short of abandoning projects; f(d) is maximum. Academic researchers (moderate/constrained) experience mid-range d around 0.65 — they face high but surmountable costs (fair use litigation, licensing); f(d) is moderate. The pipeline correctly identifies that term extension is not experienced symmetrically: beneficiaries see coordination while victims see extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   COPYRIGHT TERM EXTENSION RESOLVES MANDATROPHY BY REVEALING INSTITUTIONAL LAYERING: The constraint appears to serve incentive coordination (justifying rope classification) but simultaneously operates as rent extraction (justifying snare classification). The mandatrophy is resolved by recognizing that the original incentive function (compensating living creators) is separate from the extraction function (maintaining monopoly on already-created works and preventing public domain catalysis). These are structurally distinct constraints that have been conflated under one legal regime. DECOMPOSITION: The incentive constraint (justifying limited exclusive control for living creators) and the monopoly constraint (maintaining control over dead creators' works and preventing derivative works) are separate stories with different ε values. The incentive story (ε ≈ 0.20, Rope) has genuine coordination content. The monopoly extension story (ε ≈ 0.68, Snare) has minimal coordination and maximum extraction. By separating them, the tangled rope classification is preserved for the actual constraint (which mixes both functions) while identifying that the mixture is decomposable into coordinated and extractive components. The theater ratio's increase reflects that the performative content grows as works age — enforcement for 200-year-old works cannot appeal to incentivizing the original creator, so the constraint's justification becomes purely theatrical institutional maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_term_length,
    'What copyright term length optimizes total creative output: incentivizing original creation while maximizing cultural commons?',
    'Empirical analysis of publication rates, creative output, derivative works, and public domain utilization across jurisdictions with different term lengths (e.g., EU 70 years, US pre-1998 56 years, Australia 70 years post-2006). Cross-jurisdictional comparison of creative productivity.',
    'If optimal term < 20 years: current extensions are extraction without incentive justification. If optimal term > 50 years: term extension is functional for incentives and constraint classification shifts toward rope. If non-monotonic (inverted-U): current extensions are past optimal, confirming extraction dominates incentive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_term_length, empirical, 'Empirical optimum for copyright term length relative to creative output').

omega_variable(
    digital_distribution_dependency,
    'Do rights holders genuinely require extended copyright terms in the digital era, or do network effects, DRM, and continuous updates create de facto perpetual monopoly independent of legal term length?',
    'Analysis of digital platform economics: correlation between copyright term and pricing power; comparison of monopoly rent extraction via platform control vs copyright enforcement; measurement of ''effective copyright length'' through network lock-in and digital rights management.',
    'If network effects dominate: copyright term extension is redundant and pure extraction. If copyright enforcement materially affects pricing: term extension has functional content. If intermediate: tangled rope classification is robust; if dominated by network effects, classification shifts toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_distribution_dependency, empirical, 'Whether copyright term matters relative to platform/network monopoly in digital distribution').

omega_variable(
    public_domain_creative_catalysis,
    'Does entry into public domain materially increase derivative works, adaptations, and cultural reuse, or do works remain economically dormant regardless of copyright status?',
    'Historical analysis of derivative works produced after entry into public domain (e.g., post-Disney Snow White, post-Sherlock Holmes court cases); comparison of adaptation rates before and after public domain entry for equivalent works; measurement of open-access utilization for academic works.',
    'If derivative catalysis is strong: public domain produces measurable cultural output gain; scaffold perspective confirmed and extraction cost is material. If weak: public domain entry has minimal effect; victims'' welfare is unaffected and classification shifts toward rope (pure coordination). If intermediate: tangled rope holds with measurable extraction cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_domain_creative_catalysis, empirical, 'Whether public domain entry catalyzes derivative creative works').

omega_variable(
    corporate_vs_individual_beneficiary_asymmetry,
    'Are copyright term extensions equally distributed across individual creators, small publishers, and corporate rights holders, or concentrated in corporate hands?',
    'Institutional analysis of who holds copyright at term-end for major cultural works; correlation between corporate ownership, term-extension lobbying, and enforcement intensity; measurement of royalty distribution from extended-term licensing.',
    'If concentrated in corporate hands: term extension is pure extraction by intermediaries, not incentive for creators. If distributed: term extension has coordination content. If highly asymmetric: classification remains tangled rope but with directionality overrides reflecting corporate capture of nominal creator incentives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corporate_vs_individual_beneficiary_asymmetry, empirical, 'Distribution of copyright term extension benefits across individual vs corporate holders').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_term_extension, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copyterm_tr_t0, copyright_term_extension, theater_ratio, 0, 0.52).
narrative_ontology:measurement(copyterm_tr_t15, copyright_term_extension, theater_ratio, 15, 0.62).
narrative_ontology:measurement(copyterm_tr_t30, copyright_term_extension, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(copyterm_be_t0, copyright_term_extension, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(copyterm_be_t15, copyright_term_extension, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(copyterm_be_t30, copyright_term_extension, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_term_extension, resource_allocation).
narrative_ontology:affects_constraint(copyright_term_extension, trademark_renewal_perpetuity).
narrative_ontology:affects_constraint(copyright_term_extension, patent_extension_lobbying).
narrative_ontology:affects_constraint(copyright_term_extension, digital_rights_management_circumvention).

% DUAL FORMULATION NOTE:
% Copyright term extension decomposes into two structurally distinct constraints: (1) incentive_copyright_initial_term (ε ≈ 0.20, Rope) — justifiable as creation incentive for living authors; (2) copyright_monopoly_extension (ε ≈ 0.68, Snare) — maintaining control over dead creators' works and preventing derivative works. The tangled rope classification reflects mixture of both functions in current law. Network edges link to related intellectual property extraction mechanisms (trademark perpetuation, patent extension lobbying, DRM circumvention barriers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_term_extension, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
