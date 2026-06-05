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
 *   mechanism that evolved from the 1998 DMCA safe harbor (designed for
 *   passive ISPs) through algorithmic curation platforms to Article 17 of the
 *   EU Digital Services Directive and emerging US legislative proposals. The
 *   constraint exhibits genuine coordination value — platforms enable global
 *   creator distribution at unprecedented scale — while simultaneously
 *   externalizing copyright enforcement costs onto creators and rights
 *   holders. The tension is not between 'good coordination' and 'bad
 *   extraction,' but rather a hybrid structure where platforms capture
 *   distribution control and algorithmic gatekeeping while pushing copyright
 *   liability externalization outward. The constraint shows all six DR types
 *   from different structural positions: independent creators trapped by
 *   network effects experience snare; mid-scale rights holders experience
 *   tangled rope (mixed coordination benefit and enforcement burden); large
 *   platforms experience rope (coordination through liability safe harbor
 *   with massive arbitrage options); copyright intermediaries experience
 *   scaffold (building alternative licensing frameworks with clear sunset
 *   logic as standards mature); the DMCA framework itself appears as piton
 *   (performative fiction about platform neutrality); and the analytical
 *   observer risks naturalizing what is actually a distributive choice about
 *   cost allocation as a natural law of platform operation.
 *
 * KEY AGENTS:
 *   - Independent Creator: Primary victim (powerless/trapped) — depends on platform for distribution reach; faces algorithmic suppression, demonetization, and external copyright enforcement burden with no viable exit
 *   - Mid-Scale Copyright Holder: Secondary victim (moderate/constrained) — invests in monitoring and legal enforcement; also benefits from platform distribution; faces substantial but not absolute exit costs
 *   - Large Platform: Primary beneficiary (institutional/arbitrage) — captures algorithmic control, advertising revenue, user data; experiences safe harbor as coordination; can arbitrage to higher-compliance regimes by investing in filtering infrastructure
 *   - Performing Rights Organization / CMO: Organized intermediary (organized/constrained) — builds collective licensing frameworks; sees temporary market failure as business opportunity; perceives sunset as licensing standards mature
 *   - DMCA Safe Harbor Framework: Institutional constraint structure (institutional/arbitrage) — maintains legal fiction of platform neutrality despite algorithmic curation; persists through institutional inertia and political economy
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks treating cost externalization as natural law inherent to scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_copyright_liability_shift, 0.58).
domain_priors:suppression_score(platform_copyright_liability_shift, 0.62).
domain_priors:theater_ratio(platform_copyright_liability_shift, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_copyright_liability_shift, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_copyright_liability_shift, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(platform_copyright_liability_shift, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_copyright_liability_shift, tangled_rope).
narrative_ontology:human_readable(platform_copyright_liability_shift, "Platform Copyright Liability Shift").
narrative_ontology:topic_domain(platform_copyright_liability_shift, "intellectual_property/platform_governance").

domain_priors:requires_active_enforcement(platform_copyright_liability_shift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_copyright_liability_shift, large_platforms).
narrative_ontology:constraint_beneficiary(platform_copyright_liability_shift, algorithmic_distribution_gatekeepers).
narrative_ontology:constraint_victim(platform_copyright_liability_shift, independent_creators).
narrative_ontology:constraint_victim(platform_copyright_liability_shift, copyright_holders).
narrative_ontology:constraint_victim(platform_copyright_liability_shift, content_filtering_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT CREATOR (SNARE) — Trapped by platform dependency for distribution reach. Faces algorithmic suppression, demonetization without recourse, and liability for user uploads. No viable exit: alternative platforms lack reach; self-distribution is economically unviable for most creators. Bears maximum suppression and extraction through takedown costs, demonetization, and loss of control over algorithmic visibility.
constraint_indexing:constraint_classification(platform_copyright_liability_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-SCALE RIGHTS HOLDER (TANGLED ROPE) — Constrained by high enforcement costs (hiring monitoring services, legal action, repeat takedown burden) but also benefits from platform distribution reach and algorithmic amplification. Faces both extraction (enforcement burden externalized) and coordination (platform provides distribution infrastructure). Exit cost is substantial but not absolute — can pursue independent distribution or licensing intermediaries at significant economic penalty.
constraint_indexing:constraint_classification(platform_copyright_liability_shift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE PLATFORM (ROPE) — Experiences the constraint as coordination: liability safe harbor enables efficient content distribution at scale. Captures algorithmic control, advertising revenue, and user data while externalizing enforcement costs. Arbitrage exit available: can comply with stricter liability regimes by investing in filtering infrastructure (YouTube's Content ID) and passing costs to creators through revenue share reduction. Net beneficiary with low experienced extraction.
constraint_indexing:constraint_classification(platform_copyright_liability_shift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COPYRIGHT INTERMEDIARY ECOSYSTEM (SCAFFOLD) — Performing Rights Organizations, CMOs, and licensing intermediaries see the liability shift as a temporary market failure creating their business opportunity. Organized agents (PRS, ASCAP, Spotify's rights agreements) are building alternative licensing frameworks and collective management solutions. Constrained by regulatory uncertainty but see a sunset: as licensing standards mature and Article 17 enforcement evolves, direct creator-to-platform licensing (or forced platform licensing) will replace the current externalized enforcement regime. Low effective extraction because intermediaries have agency and perceive an exit path through standard maturation.
constraint_indexing:constraint_classification(platform_copyright_liability_shift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: DMCA SAFE HARBOR FRAMEWORK (PITON) — The 1998 safe harbor was designed for ISPs passively transmitting content; its application to algorithmic curation and recommendation platforms is largely theatrical. The legal fiction that platforms are 'neutral conduits' persists despite algorithmic content amplification. Platforms maintain this fiction to avoid liability tier elevation while extracting value through algorithmic control. The framework has lost functional justification but persists through institutional inertia and lobbying — a degraded constraint maintained for extractive purposes rather than any legitimate coordination function.
constraint_indexing:constraint_classification(platform_copyright_liability_shift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical stance, one might view copyright liability at scale as an intractable coordination problem: any platform enabling user-generated content at billion-scale must externalize some verification costs because perfect copyright detection is computationally infeasible (approaching natural law around verification cost). However, the structural data contradicts this — platforms invest heavily in algorithmic content ID and selective enforcement (YouTube's Content ID, TikTok's system), revealing that the 'impossibility' framing is a false summit naturalizing what is actually a distributive choice about who bears enforcement costs.
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
 *   Extractiveness (0.58): Moderate-high, rising over interval. At t=0 (1998-2005), platforms were genuinely neutral transmission infrastructure and the DMCA safe harbor was legitimate coordination mechanism — extractiveness was low (0.35) because the liability safe harbor actually solved a real coordination problem. As platforms shifted toward algorithmic curation (YouTube, TikTok, Instagram Reels), they captured value and control while maintaining the safe harbor fiction. By t=14 (2020-2026), platforms have matured algorithmic infrastructure (Content ID, automated takedowns) but externalized creator compliance burden instead of internalizing it, raising extractiveness to 0.58. The trajectory reflects platform choice: invest in filtering to internalize liability (as YouTube partially did) or externalize it (as most platforms prefer). Suppression (0.62): High, reflecting multiple barriers. Creators face demonetization policies, algorithmic suppression as punishment for repeat takedowns, geographic blocking for unlicensed content, platform Terms of Service enforcement with no appeal process, and economic dependence on single platform. Barriers to exit are severe — alternative platforms lack algorithmic reach, network effects lock creators into major platforms, and creator earnings are algorithmically determined and unauditable. Theater ratio (0.68): High, reflecting performative elements. Platform takedown systems (DMCA notices, automated flagging, copyright strikes) create the theatrical appearance of IP protection while the underlying copyright detection (Content ID, manual review) is applied selectively based on platform commercial interest. A copyright-infringing music track may persist for months on secondary creators' channels while being rapidly removed from major artists' channels — the enforcement pattern reveals extraction preference, not neutral IP protection.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits the diagnostic maximum perspectival gap: beneficiary experiences coordination (rope), victims experience extraction (snare), organized intermediaries perceive temporary problem (scaffold), institutional framework appears degraded (piton), and analytical view risks naturalizing contingent choice (mountain). The gap emerges from directionality: the safe harbor that appears as legitimate coordination enablement from the platform perspective appears as pure extraction from the creator perspective because platforms choose to externalize enforcement rather than internalize it. The choice is revealed by asymmetry: platforms invest heavily in Content ID for music and premium content (where revenue stakes are high) but apply minimal enforcement to UGC containing small amounts of copyrighted material (where creator revenue share is low). This selective enforcement proves the constraint is not about coordination efficiency — it is about who bears the costs. If platforms genuinely faced coordination problems in copyright detection, enforcement would be uniform across content types. Instead, enforcement scales with platform profit margin, confirming the extraction classification from creator perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure reveals how a nominally neutral coordination mechanism (safe harbor) becomes an extraction device through platform choice. The DMCA safe harbor (1998) genuinely solved a coordination problem: ISPs faced intractable liability for transient user content. The safe harbor enabled ISP growth by making liability exogenous. But when YouTube, TikTok, and algorithmic curation platforms emerged, platforms could (and partially did) internalize copyright compliance through filtering infrastructure. Most chose not to, instead maintaining the safe harbor fiction while externalizing costs to creators and rights holders. The constraint persists not because coordination is impossible but because platforms benefit from externalization. The beneficiary/victim structure makes this clear: platforms are beneficiaries (d ≈ 0.12 — they hold policy optionality); creators are victims (d ≈ 0.92 — they are trapped). The same technological infrastructure (Content ID algorithms, automated takedown systems) can internalize or externalize the enforcement burden — the choice is distribution policy, not technical necessity. This makes the constraint a canonical tangled rope: genuine coordination value (distribution infrastructure), genuine asymmetric extraction (externalized enforcement burden), and active enforcement requirement (DMCA notices, platform policies, takedown systems).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by tracking the hybrid structure across perspectives. From the platform's institutional perspective (immediate time horizon), the constraint is unambiguously rope — the safe harbor enables efficient coordination. From the creator's biographical perspective (trapped exit), it is unambiguously snare — enforcement burden is externalized with no recourse. From the copyright intermediary's generational perspective (constrained exit), it is unambiguously scaffold — alternative licensing frameworks are maturing with clear sunset logic as standards mature. The analytical observer at civilizational scale risks seeing mountain — copyright detection at platform scale approaches natural law (billions of potential infringements per day, verification cost infinity). But the false summit detector exposes this: platforms demonstrably reduce enforcement burden through selective application and Content ID investment, proving that the 'natural law' is actually a distributional choice. The constraint resolves mandatrophy by showing that type choice reflects observer position, not observer error. All six types are correct perspectival readings. The policy question is: which perspective should governance weight? EU Article 17 weights platform responsibility (forcing platforms toward snare classification from creator perspective by internalizing enforcement). US policy weights platform immunity (maintaining rope classification for platforms, snare for creators). The mandatrophy dissolves when policy choice is made explicit: governance picks which structural position to prioritize.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_curation_neutrality_fiction,
    'Is algorithmic recommendation fundamentally different from passive ISP transmission, or is the legal distinction a false construction?',
    'Comparative analysis of platform algorithmic contribution to distribution vs. passive conduit ISP role; measurement of algorithmic amplification effect on copyright-infringing content visibility vs. compliant content',
    'If algorithms are materially different: safe harbor framework is legitimate false summit (liability tier must reflect algorithmic responsibility). If distinction is fiction: classification shifts from piton (degraded framework) to snare (extractive legal construct). Platform extraction multiplies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_curation_neutrality_fiction, empirical, 'Whether algorithmic recommendation constitutes active content curation').

omega_variable(
    content_id_technical_feasibility,
    'Is Copyright ID-scale infrastructure technically feasible for platforms of all sizes, or only for trillion-dollar firms with monopoly margins?',
    'Technical audit of Content ID implementation; cost analysis per-platform per-user; feasibility study for small/medium platforms',
    'If universally feasible: externalization to creators is extractive choice (snare from creator perspective). If concentration-dependent: liability shift reflects genuine coordination problem masked by consolidation (tangled_rope from creator perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_id_technical_feasibility, empirical, 'Technical feasibility of Copyright ID infrastructure across platform sizes').

omega_variable(
    creator_exit_viability,
    'Are alternative distribution platforms (Patreon, Substack, Rumble, Odysee) genuinely viable exits, or do they lack algorithmic reach such that creators face a false choice?',
    'Comparative reach analysis: creator revenue on alternative platforms vs major platforms; network effect measurement; audience discovery feasibility on alternatives',
    'If alternatives are viable: creator exit is constrained but possible (tangled_rope from creator). If alternatives are non-viable: creator is trapped by network effects (snare classification confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_exit_viability, empirical, 'Viability of alternative creator distribution platforms').

omega_variable(
    article17_enforcement_asymmetry,
    'Does Article 17 enforcement in EU shift burden from platforms to creators/rights holders, or does it genuinely internalize copyright responsibility within platforms?',
    'Post-Article 17 measurement: platform enforcement costs vs creator/rights holder burden in EU vs US; compliance infrastructure comparison',
    'If burden remains externalized: Article 17 is theater (piton). If platforms genuinely internalize: European constraint is rope/scaffold (coordination-based). Impacts policy legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article17_enforcement_asymmetry, empirical, 'Whether Article 17 shifts enforcement burden or internalizes platform responsibility').

omega_variable(
    intermediary_licensing_scalability,
    'Can collective rights management and licensing intermediaries achieve sufficient scale and transparency to replace algorithmic gatekeeping, or will platforms maintain control through technical opacity?',
    'Licensing intermediary market growth; transparency of rights databases; creator access to licensing terms and revenue visibility',
    'If scalable: scaffold sunset is real (open-science style alternative pathway). If platforms resist transparency: intermediary model remains limited (constraint persists). Impacts constraint trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intermediary_licensing_scalability, empirical, 'Scalability of collective licensing intermediaries as alternative to platform gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_copyright_liability_shift, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pcls_tr_t0, platform_copyright_liability_shift, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pcls_tr_t7, platform_copyright_liability_shift, theater_ratio, 7, 0.62).
narrative_ontology:measurement(pcls_tr_t14, platform_copyright_liability_shift, theater_ratio, 14, 0.68).

% Extraction over time
narrative_ontology:measurement(pcls_be_t0, platform_copyright_liability_shift, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pcls_be_t7, platform_copyright_liability_shift, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(pcls_be_t14, platform_copyright_liability_shift, base_extractiveness, 14, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(pcls_su_t0, platform_copyright_liability_shift, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(pcls_su_t7, platform_copyright_liability_shift, suppression_requirement, 7, 0.56).
narrative_ontology:measurement(pcls_su_t14, platform_copyright_liability_shift, suppression_requirement, 14, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_copyright_liability_shift, resource_allocation).
narrative_ontology:affects_constraint(platform_copyright_liability_shift, creator_algorithmic_visibility_control).
narrative_ontology:affects_constraint(platform_copyright_liability_shift, platform_demonetization_opacity).
narrative_ontology:affects_constraint(platform_copyright_liability_shift, copyright_holder_enforcement_cost_externalization).

% DUAL FORMULATION NOTE:
% The platform copyright liability shift decomposes into three structurally distinct constraints: (1) algorithmic visibility control (ε ≈ 0.52, snare) — platforms control creator reach without transparency; (2) demonetization opacity (ε ≈ 0.61, snare) — revenue sharing is algorithmically determined and unauditable; (3) copyright enforcement externalization (ε ≈ 0.58, tangled_rope) — this story, focusing on liability distribution. Each exhibits the same beneficiary (platforms) but different victim structure and extraction mechanism. All three are linked via network relationships — they are components of the larger platform control architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_copyright_liability_shift, institutional, 0.08).
constraint_indexing:directionality_override(platform_copyright_liability_shift, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
