% ============================================================================
% CONSTRAINT STORY: copyright_attribution_erasure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_attribution_erasure, []).

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
 *   constraint_id: copyright_attribution_erasure
 *   human_readable: Copyright Attribution Erasure
 *   domain: intellectual_property/creative_work
 *
 * SUMMARY:
 *   Copyright attribution erasure describes the structural problem of
 *   original creator attribution being lost or obscured as content passes
 *   through digital aggregation, curation, and re-use pipelines. The
 *   constraint operates across social media platforms, content aggregators,
 *   educational repositories, and derivative works ecosystems. It is not
 *   identical to copyright infringement — technically, the work may be shared
 *   with permission, licensed under open terms, or even lawfully
 *   re-distributed. Yet the erasure of authorship creates extraction:
 *   platforms and secondary users gain visibility/utility without
 *   proportional attribution cost; original creators lose provenance,
 *   attribution-based earnings, and control over their narrative identity.
 *   The constraint exhibits a perspectival gap: aggregators see a
 *   coordination problem (how to organize vast content flows); creators see
 *   pure extraction (their work benefits others without reciprocal
 *   attribution). The theater_ratio (0.55) reflects that copyright law still
 *   mandates attribution in some contexts, but enforcement against
 *   metadata-level erasure is performative — takedown mechanisms target
 *   infringing copies but not systematic erasure of author identity through
 *   re-aggregation.
 *
 * KEY AGENTS:
 *   - Original Creators: Primary victims (powerless/trapped) — cannot prevent copying or verify attribution across distributed systems; experience maximum extraction with no exit
 *   - Aggregator Platforms: Primary beneficiaries (institutional/arbitrage) — gain user engagement, content volume, and monetization through low-friction content aggregation; experience constraint as neutral coordination infrastructure
 *   - Attribution Integrity: Epistemic victim (powerless/trapped) — abstract collective good representing knowable provenance; bears maximum cost as attribution chains dissolve
 *   - Secondary Users/Derivative Creators: Mixed actors (moderate/constrained) — benefit from easy content access but also face attribution risks in their own work; constrained by legal uncertainty
 *   - Creative Commons and Open Attribution Movements: Organized advocates (organized/constrained) — develop coordination solutions (metadata standards, licensing tools) but constrained by platform adoption barriers
 *   - Copyright Law Enforcement System: Institutional actor (institutional/arbitrage) — maintains performative attribution protections (DMCA, takedown procedures) with degraded function at scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_attribution_erasure, 0.58).
domain_priors:suppression_score(copyright_attribution_erasure, 0.72).
domain_priors:theater_ratio(copyright_attribution_erasure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_attribution_erasure, extractiveness, 0.58).
narrative_ontology:constraint_metric(copyright_attribution_erasure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(copyright_attribution_erasure, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_attribution_erasure, snare).
narrative_ontology:human_readable(copyright_attribution_erasure, "Copyright Attribution Erasure").
narrative_ontology:topic_domain(copyright_attribution_erasure, "intellectual_property/creative_work").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_attribution_erasure, aggregators_and_platforms).
narrative_ontology:constraint_beneficiary(copyright_attribution_erasure, derivative_users).
narrative_ontology:constraint_victim(copyright_attribution_erasure, original_creators).
narrative_ontology:constraint_victim(copyright_attribution_erasure, attribution_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORIGINAL CREATOR (SNARE) — The creator bears full extraction cost. Their work is reused, republished, or aggregated without attribution. Exit is structurally impossible: the work exists in digital ecosystems where copies proliferate beyond control. No legal recourse is effective at scale. Maximum suppression: the creator cannot prevent copying, cannot reliably detect unauthorized use, cannot enforce attribution retroactively. The constraint is extraction with minimal coordination benefit — the original creator receives no value from the system that erases their attribution.
constraint_indexing:constraint_classification(copyright_attribution_erasure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AGGREGATOR PLATFORM (ROPE) — Experiences the constraint as pure coordination: platforms aggregate content to solve a real collective action problem (discovery, organization, accessibility). From their perspective, attribution is a technical detail, often handled through metadata or linked sources. Suppression is low from this view — they can implement attribution mechanisms if incentivized. Exit is available (they could switch attribution protocols). The constraint appears as neutral coordination infrastructure rather than extraction.
constraint_indexing:constraint_classification(copyright_attribution_erasure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SECONDARY USER (TANGLED ROPE) — Uses aggregated content to create derivative works, educational materials, or compilations. Benefits from easy access and low friction re-use (coordination function). But also bears costs: their own work's attribution may be erased in the same system; they face legal uncertainty (copyright liability, fair use questions). Constrained by legal risk and platform terms of service. Experiences mixed coordination and extraction — the system enables their work but also threatens it.
constraint_indexing:constraint_classification(copyright_attribution_erasure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ATTRIBUTION INTEGRITY / EPISTEMIC COMMONS (SNARE) — The abstract collective good of knowable provenance and reliable attribution. Cannot exit, cannot organize, cannot defend itself. Bears maximum cost: false attribution, obscured origins, lost historical record, inability to credit original contributors. Suppression is total — no mechanism can recover attribution once erased at scale in digital systems. The epistemic commons has no advocate and experiences pure extraction.
constraint_indexing:constraint_classification(copyright_attribution_erasure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN ATTRIBUTION MOVEMENTS (TANGLED ROPE) — Organized agents (Creative Commons, linked data communities, archivalists) see the constraint as a hybrid: genuine coordination need (how to track provenance at scale) plus extractive erasure (platforms profiting from attribution-free re-use). These movements provide coordination solutions (CC licenses, metadata standards, persistent identifiers) but are constrained by platform incentives against adoption. They experience both the coordination function they provide and the extraction they resist.
constraint_indexing:constraint_classification(copyright_attribution_erasure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COPYRIGHT LAW SYSTEM (PITON) — Legal attribution protections (copyright law, moral rights, DMCA takedowns) persist as institutional theater with degraded function. The law still requires attribution in some jurisdictions, but enforcement is prohibitively expensive for individual creators. Automated takedowns target content but not metadata-level attribution erasure. The legal system's primary function (protecting creator attribution) has atrophied while its performative aspects (cease-and-desist letters, platform compliance procedures) remain. Piton classification derives from high theater (0.55) and low effective enforcement.
constraint_indexing:constraint_classification(copyright_attribution_erasure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — At civilizational scale, digital information's replicability might appear as a natural law: once content is digitized, its origin becomes expensive to track and impossible to control. Copies multiply; metadata detaches; attribution dissolves into information entropy. From this view, erasure appears inevitable and natural. However, this classification is a false summit — the structural data (beneficiaries gaining value, victims losing attribution, platform incentives against tracking) reveals contingent institutional choices, not immutable physical constraints. The apparent 'law of digital replication' naturalizes what is actually the result of platform design decisions and copyright enforcement gaps.
constraint_indexing:constraint_classification(copyright_attribution_erasure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_attribution_erasure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(copyright_attribution_erasure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(copyright_attribution_erasure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_attribution_erasure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(copyright_attribution_erasure, TR),
    TR >= 0.70.

:- end_tests(copyright_attribution_erasure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint demonstrates sustained growth in extraction severity over the interval (0.35 → 0.58), driven by increasing platform scale, algorithmic content curation, and cultural normalization of re-sharing without attribution. The aggregation pipeline extracts value from creators (attention, discoverability, secondary revenue) while attribution becomes optional. Suppression (0.72): High. Barriers to restoring attribution are substantial: creators lack technical tools to track re-use at scale; platform algorithms do not prioritize source metadata; digital copies detach from original context; legal remedies are prohibitively expensive. The suppression reflects both technical friction (copies propagate faster than metadata) and institutional incentives (platforms benefit from attribution-free re-aggregation). Theater ratio (0.55): Moderate. Copyright law and platform attribution policies still exist and are occasionally enforced, creating performative compliance theater. However, the theater is declining as automated re-aggregation reduces individual takedown effectiveness, explaining the slow increase over the interval.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon (content re-use without attribution) classifies as rope (platform's view), snare (creator's view), tangled rope (secondary user's view), and piton (law's view). The beneficiary sees coordination; the victim sees extraction; the organized advocates see a solvable hybrid; the legal system sees its own degraded function. The analytical observer risks seeing a natural law (digital information's replicability) but the structural data (beneficiary incentives, victim powerlessness, enforcement gaps) reveals institutional choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the attribution erasure flow. Original creators are trapped victims: they cannot exit the digital ecosystem where their work circulates, cannot prevent copying, cannot reliably detect or enforce attribution. Their d approaches 1.0 (full target). Aggregator platforms are beneficiaries with arbitrage options: they can adopt or ignore attribution standards, shift between platforms, or redesign their curation algorithm. Their d approaches 0.0 (full beneficiary). Secondary users are constrained victims-and-beneficiaries: they benefit from content access but face legal risk and attribution threats to their own work. Their d ≈ 0.55 (symmetric). The epistemic commons (attribution integrity) is a trapped victim with no agency: d ≈ 1.0. These directionality values feed into the sigmoid f(d) to compute experienced extractiveness (chi) for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy collapse by distinguishing genuine coordination (aggregation solves discovery problems) from asymmetric extraction (attribution erasure concentrates value on platforms). The snare classification is primary for victims; rope is legitimately the beneficiary's experience; the piton reflects institutional decay in enforcement. The false summit (mountain from civilizational/analytical view) is diagnosed by the structural data: replicability of digital content is a material fact, but attribution-erasure is a choice, not a consequence of physics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metadata_persistence_mechanism,
    'Can metadata-level attribution tracking (DOI, persistent identifiers, linked data) survive the copy-and-paste economy at scale?',
    'Empirical study of metadata preservation rates across content aggregation pipeline; tracking of URL permanence and identifier resolution across platforms over 5-10 year intervals',
    'If metadata persists reliably: attribution erasure is platform-design choice (snare downgrade to tangled rope as platforms adopt standards). If metadata detaches: structural constraint on digital provenance tracking is real (snare classification confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metadata_persistence_mechanism, empirical, 'Whether metadata-based attribution can persist through digital replication').

omega_variable(
    platform_incentive_alignment,
    'Would platforms adopt universal attribution standards if incentive structures were realigned (e.g., liability for unattributed content)?',
    'Policy experiments: jurisdictions that impose liability for attribution erasure vs those without; platform behavior under different legal regimes; cost-benefit analysis of attribution infrastructure investment',
    'If platforms adopt on incentive realignment: constraint is institutional, not structural (snare downgrades to tangled rope with remedial policy). If platforms resist despite incentives: extraction mechanism is deliberately maintained (snare confirmed with institutional agency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_incentive_alignment, empirical, 'Whether platform adoption of attribution standards is feasible under different incentive regimes').

omega_variable(
    automated_attribution_recovery,
    'Can machine learning and content fingerprinting reliably recover or verify original attribution after erasure?',
    'Technical pilot of reverse-image/reverse-text identification systems; accuracy rates on unattributed content; cost per successful attribution recovery',
    'If automation is effective and affordable: creators gain exit option (shift from trapped to constrained). If automation fails or is prohibitively expensive: trapped status is confirmed; exit remains unavailable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(automated_attribution_recovery, empirical, 'Technical feasibility of automated attribution recovery').

omega_variable(
    cultural_norm_shift_rate,
    'Is attribution omission driven by technical friction or by cultural norm erosion (younger users accept unattributed re-use as default)?',
    'Generational survey data on attribution expectations; longitudinal study of citation practices in educational and creative communities; analysis of platform norm-setting over time',
    'If technical friction dominates: solving the platform infrastructure solves the constraint. If norm erosion dominates: snare persists as internalized expectation (moves toward identity_locked dynamics for younger creators).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_norm_shift_rate, empirical, 'Whether attribution erasure is driven by technical or cultural factors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_attribution_erasure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cae_tr_t0, copyright_attribution_erasure, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cae_tr_t5, copyright_attribution_erasure, theater_ratio, 5, 0.5).
narrative_ontology:measurement(cae_tr_t10, copyright_attribution_erasure, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(cae_be_t0, copyright_attribution_erasure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cae_be_t5, copyright_attribution_erasure, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cae_be_t10, copyright_attribution_erasure, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_attribution_erasure, information_standard).
narrative_ontology:affects_constraint(copyright_attribution_erasure, open_licensing_adoption).
narrative_ontology:affects_constraint(copyright_attribution_erasure, content_fingerprinting_infrastructure).

% DUAL FORMULATION NOTE:
% Copyright attribution erasure decomposes into two structurally distinct constraints: (1) metadata-level erasure (platform design, technical friction) with ε ≈ 0.40 and (2) cultural norm shift (attribution as optional social practice) with ε ≈ 0.55. This story aggregates both; decomposition into separate stories would enable separate targeting of technical vs cultural remediation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_attribution_erasure, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
