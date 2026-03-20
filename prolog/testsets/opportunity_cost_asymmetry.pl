% ============================================================================
% CONSTRAINT STORY: opportunity_cost_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_opportunity_cost_asymmetry, []).

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
 *   constraint_id: opportunity_cost_asymmetry
 *   human_readable: Opportunity Cost Asymmetry in Long-Form Creative Projects
 *   domain: writing_psychology/creative_labor_economics/professional_identity
 *
 * SUMMARY:
 *   The opportunity cost asymmetry in long-form creative projects creates a
 *   structural tension between deep work (books requiring years of sustained
 *   focus) and serial content (essays/posts maintaining audience contact and
 *   income flow). The Tim Urban case is diagnostic: 13 Wait But Why posts in
 *   2016, then 1 post across 2017-2018 during book project, followed by
 *   permanent platform decay. The constraint exhibits multiple DR types
 *   depending on the writer's structural position: established writers with
 *   platform flexibility experience it as pure coordination (Rope),
 *   mid-career writers under contract experience mixed coordination and
 *   extraction (Tangled Rope), identity-fused authors experience cognitive
 *   entrapment (Snare via identity_locked exit), publishers experience pure
 *   benefit (Rope from institutional position), and the emerging hybrid
 *   publishing ecosystem sees a sunset (Scaffold). The analytical observer
 *   risks naturalizing the trade-off as an immutable cognitive constraint
 *   (Mountain) when much of the asymmetry derives from contingent publishing
 *   economics rather than creative cognition limits.
 *
 * KEY AGENTS:
 *   - Established Writer with Options: Primary beneficiary when mobile (moderate/mobile) — can optimize between book and serial content strategically; experiences coordination
 *   - Mid-Career Writer Under Contract: Mixed position (moderate/constrained) — book advance funds focused work but creates lock-in; bears opportunity cost risk
 *   - Identity-Fused Book Author: Primary victim (powerless/identity_locked) — professional identity constituted through book-author role; cannot perceive serial content as legitimate; structurally mobile but cognitively trapped
 *   - Book Publisher: Primary beneficiary (institutional/arbitrage) — captures value from author time concentration; portfolio-diversified across many authors
 *   - Serial Content Audience: Secondary victim (powerless/trapped) — loses access to writer's output during book years; cannot exit or organize
 *   - Hybrid Publishing Ecosystem: Organized agents (organized/mobile) — Substack, Patreon, serialization platforms building alternative pathways with lower opportunity cost
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing publishing economics as cognitive limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(opportunity_cost_asymmetry, 0.22).
domain_priors:suppression_score(opportunity_cost_asymmetry, 0.18).
domain_priors:theater_ratio(opportunity_cost_asymmetry, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(opportunity_cost_asymmetry, extractiveness, 0.22).
narrative_ontology:constraint_metric(opportunity_cost_asymmetry, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(opportunity_cost_asymmetry, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(opportunity_cost_asymmetry, rope).
narrative_ontology:human_readable(opportunity_cost_asymmetry, "Opportunity Cost Asymmetry in Long-Form Creative Projects").
narrative_ontology:topic_domain(opportunity_cost_asymmetry, "writing_psychology/creative_labor_economics/professional_identity").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(opportunity_cost_asymmetry, book_publishers).
narrative_ontology:constraint_beneficiary(opportunity_cost_asymmetry, long_form_readers).
narrative_ontology:constraint_beneficiary(opportunity_cost_asymmetry, author_legacy_building).
narrative_ontology:constraint_victim(opportunity_cost_asymmetry, serial_content_audience).
narrative_ontology:constraint_victim(opportunity_cost_asymmetry, author_income_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ESTABLISHED WRITER (ROPE) — Has platform flexibility and can choose between book projects and serial content based on strategic goals. The time allocation is a coordination problem: how to balance deep work (books) with audience maintenance (posts/essays). Experiences the constraint as a legitimate trade-off with agency to optimize. Low extraction because the writer controls the choice and benefits from both pathways.
constraint_indexing:constraint_classification(opportunity_cost_asymmetry, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-CAREER WRITER UNDER CONTRACT (TANGLED ROPE) — Book advance creates financial lock-in; abandoning the project means returning money and damaging publisher relationships. The constraint coordinates deep work but extracts through opportunity cost: years of foregone essay income, audience atrophy, and platform decay. Genuine coordination function (the advance funds focused work time) coexists with asymmetric extraction (the writer bears all downside risk if the book underperforms or the audience fragments).
constraint_indexing:constraint_classification(opportunity_cost_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IDENTITY-FUSED AUTHOR (SNARE) — Writer whose professional identity is constituted through 'being a book author' rather than 'being a writer.' Cannot perceive serial content as legitimate output—essays feel like procrastination, posts feel like distraction. The book project extracts years of productive time while the writer's income collapses and audience disappears, but exit is unthinkable because it would require abandoning the identity frame. Structurally mobile (could pivot to essays) but cognitively trapped by identity fusion with the book-author role.
constraint_indexing:constraint_classification(opportunity_cost_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 4: BOOK PUBLISHER (ROPE) — Benefits from author time concentration. The constraint coordinates author effort toward a high-value single artifact (the book) that the publisher can monetize through multiple channels (hardcover, paperback, foreign rights, film options). Experiences minimal extraction—the publisher's downside is capped (advance is sunk cost) while upside is uncapped. Can arbitrage across many authors, so any single book's failure is portfolio-diversified.
constraint_indexing:constraint_classification(opportunity_cost_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HYBRID PUBLISHING ECOSYSTEM (SCAFFOLD) — Substack, Patreon, and serialized platforms are building alternative pathways that reduce opportunity cost asymmetry. Writers can now serialize book-length work while maintaining audience contact and income flow (e.g., Gwern's incremental essays, Scott Alexander's serialized sequences). The traditional book-or-nothing constraint has a sunset as hybrid models mature. Organized agents (platform builders, early adopters) see this as temporary coordination problem being solved by infrastructure.
constraint_indexing:constraint_classification(opportunity_cost_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COGNITIVE LOAD VIEW (MOUNTAIN) — From a universal perspective, deep creative work requires sustained attention that is genuinely incompatible with frequent context-switching to serial content. The opportunity cost asymmetry reflects an immutable cognitive constraint: you cannot hold a book-length argument structure in working memory while also optimizing for weekly essay production. This perspective naturalizes the trade-off as a law of creative cognition rather than a contingent feature of publishing economics.
constraint_indexing:constraint_classification(opportunity_cost_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(opportunity_cost_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(opportunity_cost_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(opportunity_cost_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(opportunity_cost_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low-moderate. The opportunity cost is real—years of foregone essay income, audience atrophy, platform decay—but much of this represents legitimate trade-offs in creative labor allocation rather than pure extraction. Writers with mobility can optimize; those under contract face genuine coordination (advance funding) alongside extraction (risk asymmetry). The value reflects that the asymmetry is partly structural (cognitive load limits) and partly contingent (publishing industry norms). Suppression (0.18): Low. Exit barriers are primarily psychological (identity fusion, sunk cost fallacy) and contractual (advance repayment) rather than structural. Most writers can pivot back to serial content, though at reputational and financial cost. The low suppression reflects that alternatives exist—hybrid models, serialization platforms, self-publishing—even if adoption requires overcoming identity frames. Theater ratio (0.15): Low. The constraint has minimal performative content. Book projects genuinely require sustained focus; the time allocation is a real coordination problem, not a ritual. The slight theater component reflects publishing industry signaling (book-as-credential) but the core function is legitimate deep work coordination.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon—time spent on book projects displacing serial content—appears as coordination, mixed coordination-extraction, or pure extraction depending on the observer's position. Established writers with options see Rope (strategic choice between deep and serial work). Mid-career writers under contract see Tangled Rope (advance funds focus but creates lock-in). Identity-fused authors see Snare (cognitive entrapment despite structural mobility). Publishers see Rope (pure benefit from author time concentration). The hybrid ecosystem sees Scaffold (temporary problem being solved by serialization platforms). The analytical observer risks seeing Mountain (naturalizing publishing economics as cognitive limits). The perspectival gap reveals that 'opportunity cost asymmetry' is not a single phenomenon but a presheaf over observation sites—the classification depends on who is measuring and from what structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and beneficiary/victim relationships. Publishers are clear beneficiaries with arbitrage exit—they capture value from author time concentration and diversify risk across portfolios, yielding low d and negative effective extraction. Established writers with mobile exit options are also beneficiaries when they choose book projects strategically—the constraint coordinates their deep work, yielding low d. Mid-career writers under contract occupy a mixed position: they benefit from advance funding (beneficiary aspect) but bear opportunity cost risk (victim aspect), with constrained exit due to contractual lock-in. This yields moderate d. Identity-fused authors are victims with identity_locked exit—their professional identity is constituted through the book-author role, making exit cognitively impossible despite structural mobility. This yields high d and high experienced extraction, producing the Snare classification. The serial content audience is a victim with trapped exit (cannot organize or exit), but as an abstract collective, their extraction is diffuse rather than concentrated. The hybrid publishing ecosystem (organized agents with mobile exit) sees low extraction because they are building the alternative pathways that reduce the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the Rope classification (claimed type) is legitimate from multiple perspectives but coexists with Tangled Rope and Snare classifications from other positions. The established writer's Rope is their genuine experience—they have agency and benefit from both pathways. The publisher's Rope is also genuine—they capture value with minimal extraction. But the mid-career writer's Tangled Rope is equally real—the advance coordinates but also extracts through risk asymmetry. And the identity-fused author's Snare is the most severe—cognitive entrapment produces maximum extraction despite structural mobility. The mandatrophy is not 'is this coordination or extraction?' but 'for whom is it coordination, and for whom is it extraction?' The low base extractiveness (0.22) reflects that the constraint is primarily coordination with extraction concentrated on specific subpopulations (contracted writers, identity-fused authors) rather than universal extraction. The Rope classification is correct for the modal case (writers with options) but incomplete without the Tangled Rope and Snare perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    serialization_equivalence,
    'Can book-length work be produced serially without quality loss, or does the book format require sustained uninterrupted focus that serial production cannot provide?',
    'Comparative quality analysis of serialized-then-compiled books vs. traditionally-written books; reader comprehension and argument coherence metrics; author self-reports of cognitive load differences',
    'If serialization is equivalent: the opportunity cost asymmetry is purely a publishing industry artifact (Snare from more perspectives). If serialization degrades quality: the asymmetry reflects genuine cognitive constraints (Mountain from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(serialization_equivalence, empirical, 'Whether book-quality work can be produced serially').

omega_variable(
    audience_decay_reversibility,
    'Is audience atrophy during multi-year book projects reversible, or does the loss represent permanent platform damage?',
    'Longitudinal tracking of writer audiences before/during/after book projects; engagement recovery timelines; comparison of writers who returned to serial content vs. those who did not',
    'If reversible: opportunity cost is lower than measured (coordination problem). If irreversible: opportunity cost is higher (extraction mechanism with permanent damage).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(audience_decay_reversibility, empirical, 'Whether audience loss during book projects is recoverable').

omega_variable(
    income_stability_threshold,
    'At what income level does book advance lock-in transition from coordination (funding focused work) to extraction (trapping writer in unprofitable project)?',
    'Analysis of advance-to-earnings ratios; writer financial outcomes for different advance sizes; correlation between advance magnitude and project completion rates',
    'If threshold is low (e.g., $10k advance): most book contracts are extractive. If threshold is high (e.g., $100k+ advance): most contracts are legitimate coordination with fair risk-sharing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(income_stability_threshold, empirical, 'Income threshold where advance becomes lock-in rather than support').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(opportunity_cost_asymmetry, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_initial, opportunity_cost_asymmetry, theater_ratio, 0, 0.1).
narrative_ontology:measurement(theater_mid, opportunity_cost_asymmetry, theater_ratio, 3, 0.12).
narrative_ontology:measurement(theater_final, opportunity_cost_asymmetry, theater_ratio, 6, 0.15).

% Extraction over time
narrative_ontology:measurement(extract_initial, opportunity_cost_asymmetry, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(extract_mid, opportunity_cost_asymmetry, base_extractiveness, 3, 0.18).
narrative_ontology:measurement(extract_final, opportunity_cost_asymmetry, base_extractiveness, 6, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(opportunity_cost_asymmetry, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is structurally independent but could be linked to broader creative labor constraints (platform dependency, audience capture, identity-based lock-in) in future corpus expansion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
