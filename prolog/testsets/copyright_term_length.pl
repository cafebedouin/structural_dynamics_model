% ============================================================================
% CONSTRAINT STORY: copyright_term_length
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_term_length, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: copyright_term_length
 *   human_readable: Copyright Term Length Extension Constraint
 *   domain: intellectual_property/cultural_policy
 *
 * SUMMARY:
 *   Copyright term length is a structural constraint on cultural access and
 *   creative production. The current regime (life of author + 70 years in
 *   most Western jurisdictions) creates extraction from powerless agents (the
 *   public, researchers, derivative creators) toward institutional
 *   beneficiaries (copyright-holding corporations and entertainment
 *   conglomerates). The constraint exhibits genuine coordination properties —
 *   copyright protection does incentivize creation and investment in creative
 *   works — but this coordination function is deeply intertwined with
 *   asymmetric extraction that locks cultural material away long after
 *   economic incentive justifications have expired. The theater ratio (0.68)
 *   reflects that much enforcement activity is performative: Digital Rights
 *   Management technologies prevent casual copying but not determined piracy;
 *   legal takedown notices target small infringers while industrial-scale
 *   copyright holders negotiate licensing; cultural preservation is blocked
 *   not by economic scarcity but by licensing bureaucracy. The extractiveness
 *   trajectory (0.35 → 0.58 over 100 years) shows increasing extraction as
 *   terms extend and enforcement mechanisms strengthen, while the theater
 *   ratio follows (0.52 → 0.71), indicating the enforcement apparatus becomes
 *   increasingly performative as digital distribution undermines
 *   reproducibility-for-profit models.
 *
 * KEY AGENTS:
 *   - Public Domain / Cultural Commons: Primary victim (powerless/trapped) — denied access to works that would be public domain under shorter terms; no exit option
 *   - Independent Creators & Archivists: Primary victim (powerless/trapped) — prevented from building on prior cultural works; cannot license efficiently
 *   - Derivative and Adaptive Creators: Secondary victim (moderate/constrained) — high licensing costs, long negotiation times, de facto barriers to transformative work
 *   - Major Entertainment Conglomerates: Primary beneficiary (institutional/arbitrage) — extract ongoing licensing revenue from backcatalogs; can arbitrage between jurisdictions and licensing regimes
 *   - Mid-Tier Publishers & Individual Authors: Mixed (moderate/constrained) — benefit from copyright protection but also constrained by major holder gatekeeping and licensing monopolies
 *   - Copyright Enforcement Apparatus: Institutional actor (institutional/arbitrage) — maintains performative enforcement machinery; sees own function as degraded
 *   - Open Culture Coalition: Organized actors (organized/constrained) — Creative Commons, digital archives, open-access advocates building alternative models with sunset trajectory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_term_length, 0.58).
domain_priors:suppression_score(copyright_term_length, 0.62).
domain_priors:theater_ratio(copyright_term_length, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_term_length, extractiveness, 0.58).
narrative_ontology:constraint_metric(copyright_term_length, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(copyright_term_length, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_term_length, tangled_rope).
narrative_ontology:human_readable(copyright_term_length, "Copyright Term Length Extension Constraint").
narrative_ontology:topic_domain(copyright_term_length, "intellectual_property/cultural_policy").

domain_priors:requires_active_enforcement(copyright_term_length).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_term_length, copyright_holders).
narrative_ontology:constraint_beneficiary(copyright_term_length, entertainment_conglomerates).
narrative_ontology:constraint_victim(copyright_term_length, public_domain_access).
narrative_ontology:constraint_victim(copyright_term_length, derivative_creators).
narrative_ontology:constraint_victim(copyright_term_length, archivists_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC DOMAIN ACCESS (SNARE) — Cannot exit or negotiate. Bears full cost of extended copyright terms: cultural works remain locked behind licensing requirements rather than entering public domain. No alternative, no exit, no coordination benefit to the powerless. Maximum extraction.
constraint_indexing:constraint_classification(copyright_term_length, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT CREATORS & ARCHIVISTS (SNARE) — Trapped by term extensions that prevent access to prior works for adaptation, sampling, or preservation. Career-level impact: derivative creators cannot build on 20th-century cultural works even after 70+ years. No negotiated exit, structural suppression.
constraint_indexing:constraint_classification(copyright_term_length, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR COPYRIGHT HOLDERS (ROPE) — Net beneficiaries (institutional/arbitrage). Experience the constraint as coordination: term length enables stable licensing agreements and revenue extraction. Can arbitrage between jurisdictions. Effective extraction runs toward this agent. Low experienced extraction because they choose the terms.
constraint_indexing:constraint_classification(copyright_term_length, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MID-TIER PUBLISHERS & CREATORS (TANGLED ROPE) — Constrained but mixed. Benefit from copyright protection (coordination function) but are also victims of major copyright holder gatekeeping and licensing monopolies. Some negotiation capacity but not full arbitrage. Moderate extraction.
constraint_indexing:constraint_classification(copyright_term_length, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COPYRIGHT ENFORCEMENT APPARATUS (PITON) — Largely performative. Term length enforcement requires institutional machinery (legal system, technical DRM, takedown notices) whose real function has decayed. Theater ratio high: enforcement performs protection but most enforcement actions target minor infringement while large-scale extraction continues. Institutional inertia maintains the constraint.
constraint_indexing:constraint_classification(copyright_term_length, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — Risk of false summit: naturalizing copyright term as inherent to creative incentives ('creators need protection to create'). But structural data shows this is contingent institutional choice, not natural law. Courts have extended terms repeatedly through legislative acts (Sonny Bono Act, EU Directives), not through discovery of natural necessity.
constraint_indexing:constraint_classification(copyright_term_length, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: OPEN CULTURE MOVEMENT (SCAFFOLD) — Organized agents (Creative Commons, open-access advocates, digital archivists) see copyright term extension as a temporary institutional problem with structural sunset. Alternative licensing models (CC Zero, CC-BY) and decentralized distribution bypass term constraints. Coalition has exit pathways. Suppression declining over generational horizon as norms shift toward openness. Sunset estimated 30-50 years as open-culture norms mature.
constraint_indexing:constraint_classification(copyright_term_length, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_term_length_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(copyright_term_length, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(copyright_term_length, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_term_length, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(copyright_term_length, TR),
    TR >= 0.70.

:- end_tests(copyright_term_length_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The constraint extracts ongoing licensing revenue from copyright holders while denying public access to cultural works. The 0.35→0.58 trajectory reflects decades of term extensions (Sonny Bono Act 1998, EU harmonization 1995/2006) that continuously postpone public domain entry. Crucially, extraction is asymmetric: major conglomerates receive vastly more benefit than individual creators, whose works would enter public domain quickly under pre-1976 terms. Suppression (0.62): High. Barriers to accessing copyrighted material include legal licensing requirements, technical DRM, and de facto inaccessibility of licensing pathways for independent creators. Licensing friction is substantial even for researchers and archivists. Theater ratio (0.68): High and increasing. Enforcement machinery (takedown notices, DRM, lawsuits) performs protection but effectiveness has degraded with digital distribution. Copyright holders can no longer prevent reproducibility through scarcity — they must enforce through legal-technical apparatus that is increasingly theatrical. The ratio's 0.52→0.71 trajectory shows performance escalating to compensate for declining structural effectiveness. Claimed type (Tangled Rope): The constraint coordinates genuine incentives for creation (creators do respond to copyright protection) but this coordination function is obscured by asymmetric extraction toward institutional beneficiaries. Both coordination and extraction are real; neither is incidental to the other.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is stark. The beneficiary (major copyright holder, institutional/arbitrage) sees pure coordination (Rope) — the constraint enables them to license and monetize their backcatalog indefinitely, with full exit options through negotiation. The powerless agent (public domain, trapped/powerless) sees pure extraction (Snare) — locked out indefinitely with no negotiation pathways. The mid-tier creator sees mixed extraction and protection (Tangled Rope) — they benefit from copyright on their own works but are exploited by major holders' monopolistic licensing. The open culture coalition sees a degrading institutional arrangement with a sunset (Scaffold) — alternative licensing models are growing, term extensions are becoming politically contested, and digital distribution undermines the reproducibility-for-profit model that justifies long terms. The enforcement apparatus sees its own function decaying (Piton) — must perform protection through increasingly expensive and theatrical mechanisms because technical and market forces have already broken structural scarcity. The analytical observer risks seeing copyright as natural law (Mountain) — naturalizing the institutional choice of long terms as inherent to creative incentives — but the structural data reveals this as a false summit: term lengths are purely legislative choices, not natural discoveries.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position relative to the extraction flow. Major copyright holders benefit from extended terms → beneficiary status → low d (around 0.10-0.15 with arbitrage exit) → negative effective extraction χ. They experience the constraint as enabling, not extractive. The public domain has no exit and bears costs → victim status + trapped exit → high d (0.95) → high f(d) ≈ 1.42 → high χ. The powerless agent experiences maximum extraction. Mid-tier creators face constrained exit (licensing negotiations are possible but costly) and mixed victim/beneficiary status (victimized by conglomerate gatekeeping, benefited by copyright on own works) → moderate d (around 0.55-0.65) → moderate f(d) ≈ 0.75-1.00 → moderate χ. The enforcement apparatus, despite institutional power, is caught between beneficiary status (funding for enforcement mechanisms) and analytical awareness that enforcement is increasingly theatrical → institutional power + arbitrage exit but with recognition of functional decay → override d value upward (0.35-0.40) to capture that institutional actors maintaining decaying systems experience more extraction than pure arbitrage would suggest.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: Copyright term length demonstrates the mandatrophy by showing how the same structural constraint can be classified as both coordination and extraction depending on observed agent. The mandatrophy is resolved by recognizing that copyright IS both genuine coordination (creates incentive for creative investment) AND extractive (extends monopoly rents beyond incentive justification, locks cultural material away). The Tangled Rope classification captures this hybrid: real coordination function + asymmetric extraction + active enforcement. The false summit (Mountain) risks naturalizing legislative copyright term as inherent to creative incentives, but empirical analysis of term-to-output elasticity can falsify this: if creative output doesn't increase measurably with terms beyond 14-20 years, the coordination justification fails and the constraint collapses toward Snare. The scaffold perspective resolves the mandatrophy by showing that alternative coordination models (Creative Commons, open-access licensing, decentralized distribution) can provide equivalent incentives without the extraction overhead — term extensions were never necessary for coordination, they are pure institutional rent-seeking dressed in coordination language.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_term_empirical_claim,
    'What copyright term length actually optimizes creative output vs public access? Is there empirical evidence that 70+ year terms produce more works than 14-20 year terms?',
    'Historical analysis of creative output rates under different term lengths; comparison of creation incentives in jurisdictions with varying term lengths; econometric modeling of author response to term duration',
    'If shorter terms (14-20 yrs) show equivalent or higher creative output: current term extension is pure extraction with no coordination justification. Reclassify from Tangled Rope to Snare. If longer terms do show measurably higher output: coordination function is real, Tangled Rope classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(optimal_term_empirical_claim, empirical, 'Whether extended copyright terms increase creative output').

omega_variable(
    enforcement_decay_trajectory,
    'Is copyright enforcement capacity structurally declining (digital distribution breaks reproducibility-for-profit model) or merely theatrically degrading while extraction continues?',
    'Measurement of enforcement success rates (takedown effectiveness, litigation win rates, market impact of piracy) over time; correlation between enforcement intensity and actual licensing revenue loss',
    'If structurally declining: enforcement is becoming piton-like (performative maintenance). Future term extensions will extract less effectively. If enforcement remains stable: extraction mechanism is robust, not degrading. Piton classification is premature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_trajectory, empirical, 'Whether copyright enforcement is structurally or theatrically degrading').

omega_variable(
    orphan_works_harm_magnitude,
    'What fraction of under-100-year-old copyrighted works cannot be licensed because rightsholder is unavailable, deceased, or corporate records lost? How much potential derivative work is foregone due to inability to clear rights?',
    'Survey of orphan works (works where copyright status is uncertain or holder is untraceable); measurement of derivative creation attempts blocked by licensing friction; comparative analysis of creative output in open-access vs copyright-protected corpora',
    'High orphan work fraction or high derivative work foregone: suppression metric understated, reclassify upward. Low fraction: suppression metric is accurate. Determines whether public domain impact is existential (high) or marginal (low).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orphan_works_harm_magnitude, empirical, 'Magnitude of harm from orphan works and blocked derivative creation').

omega_variable(
    term_extension_legislative_capture,
    'Are copyright term extensions driven by genuine authorial demand or primarily by corporate/institutional holder lobbying? What fraction of copyright extension advocacy originates from individual creators vs entertainment conglomerates?',
    'Analysis of lobbying disclosures, campaign finance records, and legislative testimony; surveys of author preferences for term length; comparison of extension advocacy intensity from individual creators vs corporations',
    'If corporate-driven: extraction mechanism is institutional rent-seeking, not author protection coordination. Reclassify as Snare at corporate level, higher perceived-extraction. If author-driven: coordination function is genuine. Tangled Rope stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(term_extension_legislative_capture, empirical, 'Whether term extensions are driven by author demand or corporate capture').

omega_variable(
    digital_commons_substitutability,
    'Can creative works under Creative Commons, public domain, or open-source licenses provide substitutes sufficient to satisfy demand for derivative and adaptive works? Or are copyright-protected works irreplaceable?',
    'Measurement of CC-licensed creative production rates relative to copyright-protected; user satisfaction surveys comparing access to works under different licensing; network analysis of derivative work chains (can creators build on open materials as effectively as copyright-protected materials?)',
    'If substitutes are adequate: public domain constraint is moderate extraction, not severe. Reclassify powerless agent toward constrained. If substitutes are inadequate: public domain lacks access to irreplaceable corpus. Extraction is severe, Snare classification is tight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_commons_substitutability, empirical, 'Whether open-licensed works adequately substitute for copyright-protected works').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_term_length, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copyterm_tr_t0, copyright_term_length, theater_ratio, 0, 0.52).
narrative_ontology:measurement(copyterm_tr_t30, copyright_term_length, theater_ratio, 30, 0.62).
narrative_ontology:measurement(copyterm_tr_t60, copyright_term_length, theater_ratio, 60, 0.68).
narrative_ontology:measurement(copyterm_tr_t90, copyright_term_length, theater_ratio, 90, 0.71).

% Extraction over time
narrative_ontology:measurement(copyterm_be_t0, copyright_term_length, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(copyterm_be_t30, copyright_term_length, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(copyterm_be_t60, copyright_term_length, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(copyterm_be_t90, copyright_term_length, base_extractiveness, 90, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_term_length, resource_allocation).
narrative_ontology:boltzmann_floor_override(copyright_term_length, 0.18).
narrative_ontology:affects_constraint(copyright_term_length, digital_content_monopoly).
narrative_ontology:affects_constraint(copyright_term_length, fair_use_erosion).
narrative_ontology:affects_constraint(copyright_term_length, cultural_preservation_bottleneck).

% DUAL FORMULATION NOTE:
% Copyright term length is a parent constraint affecting downstream constraints on fair use, digital content distribution, and cultural preservation. The term-length constraint sets the boundary conditions for all downstream copyright-policy constraints. Decomposition follows ε-invariance: the term-length constraint has its own extractiveness profile (0.58); individual doctrinal constraints (fair use, orphan works, preservation exceptions) have distinct extractiveness values reflecting their specific policy domains. All are linked via affects_constraints to show how institutional decisions at the term-length level cascade to systemic effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_term_length, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
