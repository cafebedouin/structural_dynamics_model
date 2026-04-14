% ============================================================================
% CONSTRAINT STORY: platform_copyright_liability_shift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_copyright_liability_shift, []).

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
 *   constraint_id: platform_copyright_liability_shift
 *   human_readable: Platform Copyright Liability Shift
 *   domain: intellectual_property/platform_governance
 *
 * SUMMARY:
 *   The platform copyright liability shift represents a structural extraction
 *   mechanism whereby large digital platforms externalize copyright
 *   enforcement costs onto creators while capturing distribution value and
 *   algorithmic control. Beginning with the DMCA safe harbor (1998) and
 *   evolving through Article 17 of the EU Digital Services Directive and
 *   proposed US legislative frameworks, this constraint exhibits the core
 *   tension between coordination (platforms enable global distribution) and
 *   extraction (platforms suppress creator compensation mechanisms). The
 *   constraint's extractiveness has increased from 0.35 to 0.58 over the
 *   measured interval as platforms have systematized content appropriation
 *   while maintaining the appearance of copyright compliance through
 *   performative takedown processes. Theater ratio (0.55) reflects that
 *   copyright enforcement mechanisms (DMCA notices, Content ID flagging)
 *   exist but are substantially theatrical — they create compliance theater
 *   without systematically redirecting value to creators. The liability shift
 *   mechanism works by: (1) legally insulating platforms from liability for
 *   user-generated copyright infringement through safe harbors, (2) placing
 *   enforcement burden on individual copyright holders (expensive litigation,
 *   cease-and-desist letters), (3) algorithmic redistribution that continues
 *   regardless of takedown notices, and (4) compensation structures (ad
 *   revenue sharing, creator funds) that systematically undervalue
 *   copyrighted content relative to platform value extraction.
 *
 * KEY AGENTS:
 *   - Individual Creators: Primary victims (powerless/trapped) — work is distributed without consent or compensation; no exit options (network effects, legal barriers)
 *   - Small Publishers: Secondary victims (moderate/constrained) — dependent on platform distribution but face extraction via algorithmic suppression of paywalled content
 *   - Copyright Holders: Tertiary victims (institutional/constrained) — hold legal rights but enforcement costs prohibitive; licensing negotiations asymmetric
 *   - Large Platforms: Primary beneficiary (institutional/arbitrage) — capture distribution value, suppress creator compensation, negotiate copyright selectively
 *   - Content Aggregators: Secondary beneficiary (institutional/arbitrage) — scrape and redistribute content with minimal licensing cost
 *   - Regulatory Coalition: Organized alternative builders (organized/constrained) — EU DSM Directive, UK Online Safety Bill, US copyright reform proposals create statutory licensing and rebalanced liability
 *   - Copyright Enforcement Regime: Institutional actor (institutional/arbitrage) — maintains performative takedown process; sees own mechanisms as degraded (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_copyright_liability_shift, 0.58).
domain_priors:suppression_score(platform_copyright_liability_shift, 0.68).
domain_priors:theater_ratio(platform_copyright_liability_shift, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_copyright_liability_shift, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_copyright_liability_shift, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(platform_copyright_liability_shift, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_copyright_liability_shift, tangled_rope).
narrative_ontology:human_readable(platform_copyright_liability_shift, "Platform Copyright Liability Shift").
narrative_ontology:topic_domain(platform_copyright_liability_shift, "intellectual_property/platform_governance").

domain_priors:requires_active_enforcement(platform_copyright_liability_shift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_copyright_liability_shift, large_platforms).
narrative_ontology:constraint_beneficiary(platform_copyright_liability_shift, content_aggregators).
narrative_ontology:constraint_victim(platform_copyright_liability_shift, individual_creators).
narrative_ontology:constraint_victim(platform_copyright_liability_shift, small_publishers).
narrative_ontology:constraint_victim(platform_copyright_liability_shift, copyright_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL CREATOR (SNARE) — Creators have no viable exit. Their work is syndicated, algorithmically redistributed, and scraped without consent or compensation. They cannot prevent redistribution (legal barriers), cannot migrate to alternative platforms (network effects), and cannot sue (cost prohibitive). Maximum extraction with minimal coordination benefit. The constraint exists precisely to suppress their exit options and enforce asymmetric appropriation of creative value.
constraint_indexing:constraint_classification(platform_copyright_liability_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT PUBLISHER (TANGLED ROPE) — Constrained exit: leaving platform distribution means losing audience reach, but remaining means accepting algorithmic redistribution and reduced compensation. These agents experience genuine coordination benefits (platform distribution) alongside extraction (liability shift, algorithmic suppression of paywalled content). Active enforcement required — the platform must continuously extract work and suppress payment mechanisms.
constraint_indexing:constraint_classification(platform_copyright_liability_shift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE PLATFORM (ROPE) — Experiences the liability shift as pure coordination: it solves the platform's problem of managing user-generated content without legal exposure. The constraint enables distribution at scale while externalizing copyright enforcement costs to creators. Beneficiary with full arbitrage options — can negotiate copyright terms selectively or exit copyright-heavy jurisdictions entirely.
constraint_indexing:constraint_classification(platform_copyright_liability_shift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Organized agents (EU DSM Directive, UK Online Safety Bill, proposed US reforms) see liability shift as a temporary institutional arrangement with a sunset clause. Regulations creating statutory licensing, mandatory content ID systems, and platform liability for infringement are building alternative pathways. Low effective extraction because this perspective has agency and sees an exit path through legislative reform. Has sunset clause rationale: regulatory frameworks requiring notice-and-takedown reform, mandatory licensing negotiations, and platform liability rebalancing are gradually reducing the asymmetry that enabled the original extraction.
constraint_indexing:constraint_classification(platform_copyright_liability_shift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COPYRIGHT ENFORCEMENT REGIME (PITON) — Traditional copyright law is substantially performative when applied to platforms. The DMCA takedown regime exists, but platforms ignore enforcement selectively. Cease-and-desist letters create theatrical compliance — platforms remove specific URLs while continuing algorithmic distribution. The regime persists through institutional inertia (copyright offices, licensing bodies) despite low functional enforcement. Theater ratio reflects that rights holders send thousands of takedown notices annually but extraction continues via algorithmic redistribution and metadata stripping.
constraint_indexing:constraint_classification(platform_copyright_liability_shift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NETWORK TOPOLOGY VIEW (MOUNTAIN) — From a universal perspective, some copyright enforcement gap is inherent to network topology: at planetary scale with billions of pieces of content, individual verification is computationally infeasible. This perspective sees the liability shift as an immutable consequence of distributed information architecture. However, the structural data contradicts this — the 'inherent to networks' framing naturalizes what is actually a policy choice (platforms COULD implement mandatory licensing, distributed royalty tracking, or content verification systems). The engine will compute this as a false summit.
constraint_indexing:constraint_classification(platform_copyright_liability_shift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_copyright_liability_shift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_copyright_liability_shift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_copyright_liability_shift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_copyright_liability_shift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_copyright_liability_shift, TR),
    TR >= 0.70.

:- end_tests(platform_copyright_liability_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The platform captures significant distribution value while systematically suppressing creator compensation. The increase from 0.35 to 0.58 over the interval reflects platform maturation of extraction mechanisms: early platforms (YouTube 2005-2010) claimed educational purpose and creator revenue sharing; mature platforms (TikTok, TikTok's short-form dominance 2015-2025) optimized for creator dependency while suppressing monetization paths. The extraction is not maximal (not 0.72+) because some creator pathways exist (Patreon, direct sponsorship), and regulatory pressure creates genuine uncertainty about enforcement duration. Suppression (0.68): High. Structural barriers include safe harbor legal architecture, cost of enforcement, network effects that prevent exit, algorithmic suppression of monetized content, and lack of transparency in compensation formulas. Creators cannot organize easily (distributed, powerless, competing) and cannot exit (distribution requires platform scale). Theater ratio (0.55): Moderate. Copyright enforcement exists (Content ID, takedown processes) but functions theatrically — platforms implement compliance performance without systematic value redistribution. The theater has increased over the interval as platforms have invested in visible compliance infrastructure (Content ID, creator funds) while actual creator compensation has declined as percentage of platform value.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates divergent classification across power and exit positions. Powerless creators see pure extraction (Snare) — no coordination benefit, maximum burden. Moderate publishers see mixed coordination and extraction (Tangled Rope) — distribution enables reach, but extraction suppresses compensation. Large platforms see pure coordination (Rope) — liability shift solves their distribution problem elegantly. Regulatory bodies see a solvable problem (Scaffold) — statutory licensing frameworks build an alternative pathway with sunset logic. The copyright regime sees its own enforcement as degraded (Piton) — the takedown process exists but is systemically theatrical. The analytical observer risks naturalizing this as an inevitable feature of digital scale (Mountain) — too much content to verify individually. However, the structural data (platforms deliberately suppress Content ID for creators, suppress transparency in algorithms, actively lobby against licensing reform) reveals this as a policy choice, not a law of nature. The gap between powerless and institutional perspectives is maximal (Snare vs Rope) — the same constraint appears as opposite types depending on whose extraction is measured.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation for each perspective derives from structural position and exit options. Individual creators (powerless, trapped, victim) experience full d = 0.95, producing maximum f(d) ≈ 1.42 and high chi. Large platforms (institutional, arbitrage, beneficiary) experience low d ≈ 0.05, producing negative f(d) ≈ -0.12 and negative chi (net benefit). Independent publishers (moderate, constrained, both victim and beneficiary) experience moderate d ≈ 0.55, producing f(d) ≈ 0.75 and moderate chi — they benefit from distribution but are extracted from via suppression of monetization. Regulatory coalition (organized, constrained, alternative builder) experiences moderate-low d ≈ 0.40, producing f(d) ≈ 0.40 and low chi — organized agents see an exit path through regulation, reducing experienced extraction. Copyright regime (institutional, arbitrage) captures the piton perspective — maintains enforcement theater while actual extraction persists.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through perspectival decomposition and recognition of legitimate vs extractive mechanisms. Coordination function (Rope aspect): Platforms genuinely enable global distribution at scale that individual creators cannot achieve. This is a coordination benefit that justifies some platform capture. Extraction function (Snare aspect): The safe harbor structure + lack of transparency + suppression of creator monetization + network lock-in constitute pure extraction orthogonal to coordination. The tangled rope classification requires ALL THREE gates: (1) beneficiaries (platforms) + (2) victims (creators) + (3) active enforcement (platforms enforce creator dependency while suppressing compensation). All three are present. The constraint is NOT pure coordination (that would be Rope) because the extraction component is asymmetric and enforced. The constraint is NOT pure extraction (that would be Snare) because genuine coordination benefits exist and platforms could theoretically provide them with lower suppression. Mandatrophy is resolved by recognizing that the classification is context-dependent: from the powerless creator's perspective, it is a Snare (they experience no coordination benefit, only extraction); from the analytical perspective with regulatory alternative visible, it is a Scaffold (the sunset is real through legislative action); from the beneficiary's perspective, it is a Rope (they experience the constraint as solving coordination). The engine's job is to report all six, not to choose one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    platform_technical_capacity,
    'Do large platforms lack genuine technical capacity to implement copyright enforcement, or do they suppress it as economically profitable?',
    'Comparative analysis of platform investment in content ID (YouTube Content ID, TikTok Rights Manager) vs platform investment in suppressing creator monetization. Audit of platform code for deliberately-disabled copyright verification.',
    'If technical barrier: classification shifts toward mountain/rope (system-wide coordination problem). If economic choice: classification confirms snare/tangled_rope (extractive suppression of enforcement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_technical_capacity, empirical, 'Whether copyright enforcement gap reflects technical impossibility or deliberate suppression').

omega_variable(
    statutory_licensing_feasibility,
    'Can statutory licensing (compulsory licensing with per-use payments tracked algorithmically) scale to platform-wide distribution without recreating the transaction cost problem it solves?',
    'Implementation data from EU licensing directives (Article 17 DSM), UK Online Safety Bill licensing provisions, and proposed US digital music legislation. Measurement of royalty distribution speed and accuracy.',
    'If feasible: scaffold sunset is real — regulatory alternatives can reduce extraction. If transaction costs resurface: scaffold is aspirational, and the liability shift persists despite regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_licensing_feasibility, empirical, 'Whether statutory licensing can scale without recreating transaction costs').

omega_variable(
    creator_coalition_threshold,
    'At what scale of organized creator action do platforms'' liability and revenue extraction costs exceed benefits, triggering settlement?',
    'Historical analysis of creator strikes, organized holdouts (music licensing standoffs, author collective actions), and resulting platform concessions. Threshold identification via cost-benefit modeling.',
    'If threshold low: organized creator power (perspective 2 upgraded from moderate to organized) can change classification. If threshold high: powerless creators remain trapped even in coalition (perspective 1 unaffected).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_coalition_threshold, empirical, 'Critical mass threshold for organized creator power to force platform concessions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_copyright_liability_shift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pccls_tr_t0, platform_copyright_liability_shift, theater_ratio, 0, 0.4).
narrative_ontology:measurement(pccls_tr_t5, platform_copyright_liability_shift, theater_ratio, 5, 0.48).
narrative_ontology:measurement(pccls_tr_t10, platform_copyright_liability_shift, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(pccls_be_t0, platform_copyright_liability_shift, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pccls_be_t5, platform_copyright_liability_shift, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(pccls_be_t10, platform_copyright_liability_shift, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_copyright_liability_shift, resource_allocation).
narrative_ontology:affects_constraint(platform_copyright_liability_shift, algorithmic_content_suppression).
narrative_ontology:affects_constraint(platform_copyright_liability_shift, creator_labor_commodification).
narrative_ontology:affects_constraint(platform_copyright_liability_shift, safe_harbor_liability_doctrine).

% DUAL FORMULATION NOTE:
% The copyright liability shift is downstream of safe harbor legal architecture (safe_harbor_liability_doctrine, ε=0.08, Mountain — the legal framework is a structural given) but represents a distinct extractive mechanism. The algorithmic suppression of monetization links to creator_labor_commodification (creators internalize undercompensation as 'exposure' value).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_copyright_liability_shift, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
