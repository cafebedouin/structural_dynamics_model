% ============================================================================
% CONSTRAINT STORY: performer_secondary_monetization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performer_secondary_monetization, []).

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
 *   constraint_id: performer_secondary_monetization
 *   human_readable: Performer Secondary Monetization Rights Constraint
 *   domain: entertainment/digital_media/labor
 *
 * SUMMARY:
 *   Performer secondary monetization represents a structural constraint in
 *   digital media where platform operators systematically extract value from
 *   performer-created content by monetizing secondary uses — algorithmic
 *   clips, recommendation feeds, compilations, training data — without
 *   proportional compensation to creators. The constraint operates across all
 *   digital platforms (YouTube, TikTok, Twitch, Instagram, Spotify) and
 *   affects performers from powerless dependents to institutional superstars
 *   differently. The extractiveness has accelerated from 0.35 (2010s, early
 *   platform era) to 0.58 (present) as platforms discovered new secondary
 *   monetization channels (AI training, recommendation feeds, content
 *   aggregation). Suppression is high (0.65) because performers face
 *   algorithmic visibility dependency, contractual lock-in, and information
 *   asymmetry about how their content is reused. Theater ratio is moderate
 *   (0.48) because while some legitimate licensing occurs (royalty payments,
 *   content-creator funds), much of the performer-compensation narrative is
 *   performative — creator funds and revenue-share programs provide minimal
 *   actual income for most performers while maintaining legitimacy of the
 *   platform's claim to 'support creators.' The constraint exhibits all six
 *   classification types depending on structural position: pure extraction
 *   (snare) for dependent performers, mixed extraction-coordination (tangled
 *   rope) for moderates and superstars, pure coordination (rope) for
 *   platforms, temporary problem with sunset (scaffold) for organized
 *   collectives, degraded licensing (piton) for legacy frameworks, and false
 *   natural law (mountain for analytical observer).
 *
 * KEY AGENTS:
 *   - Dependent Performers: Primary victims (powerless/trapped) — algorithmic visibility lock-in, no exit options, contractual terms set unilaterally by platform
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — control distribution infrastructure, monetize secondary uses, shift terms unilaterally, operate with high exit optionality
 *   - Mid-Career Independent Artists: Secondary victims (moderate/constrained) — benefit from platform reach but lose control of content secondary uses, face discovery algorithm suppression
 *   - Superstar Performers: Complex agents (powerful/mobile) — maintain limited exit optionality through leverage but locked into attention economy's extraction logic
 *   - Content Aggregators: Institutional beneficiaries (institutional/arbitrage) — repackage clips and compilations for secondary platforms without performer negotiation
 *   - Creator Coalitions: Organized agents (organized/constrained) — building alternative pathways (unions, collective licensing, direct-to-fan platforms) with regulatory sunset logic
 *   - Legacy Licensing Orgs: Institutional actors (institutional/arbitrage) — maintain performative frameworks (ASCAP, BMI, PROs) that fail to capture digital secondary uses
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performer_secondary_monetization, 0.58).
domain_priors:suppression_score(performer_secondary_monetization, 0.65).
domain_priors:theater_ratio(performer_secondary_monetization, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performer_secondary_monetization, extractiveness, 0.58).
narrative_ontology:constraint_metric(performer_secondary_monetization, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(performer_secondary_monetization, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performer_secondary_monetization, tangled_rope).
narrative_ontology:human_readable(performer_secondary_monetization, "Performer Secondary Monetization Rights Constraint").
narrative_ontology:topic_domain(performer_secondary_monetization, "entertainment/digital_media/labor").

domain_priors:requires_active_enforcement(performer_secondary_monetization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performer_secondary_monetization, platform_operators).
narrative_ontology:constraint_beneficiary(performer_secondary_monetization, content_aggregators).
narrative_ontology:constraint_beneficiary(performer_secondary_monetization, institutional_media_companies).
narrative_ontology:constraint_victim(performer_secondary_monetization, performers_and_creators).
narrative_ontology:constraint_victim(performer_secondary_monetization, independent_artists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT PERFORMER (SNARE) — Performers bound to platforms by algorithmic visibility dependency, audience location, and contractual lock-in. No viable exit without losing earned audience. Suppression is total: platform terms shift unilaterally, revenue share models change at will, content gets repurposed into secondary products (compilations, AI training, recommendation feeds) without additional compensation. The performer sees pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(performer_secondary_monetization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER INDEPENDENT ARTIST (TANGLED ROPE) — Constrained by high switching costs (audience fragmentation across platforms, rebuild time, discovery algorithms favor incumbents) but also benefits from platform distribution infrastructure and audience access that would be inaccessible otherwise. Experiences mixed extraction: loses control of secondary uses (clips, compilations, recommendation feeds) but gains audience reach. High suppression due to algorithm opacity and unilateral terms, but some agency in content strategy.
constraint_indexing:constraint_classification(performer_secondary_monetization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the constraint as coordination problem: aggregating performer content, distributing it globally, and monetizing secondary uses (clips, recommendations, compilations) solves the collective distribution problem. Operates with high arbitrage optionality — can shift terms, renegotiate rates, deploy content in new contexts. Sees minimal extraction cost because the platform functions as genuine infrastructure. Revenue flows to the platform with minimal performer negotiation power.
constraint_indexing:constraint_classification(performer_secondary_monetization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SUPERSTAR PERFORMER (TANGLED ROPE) — High-market-value performers maintain limited exit optionality through audience size and leverage. Can negotiate secondary monetization rights, retain creative control over clips and compilations, or move between platforms. But also locked into the attention economy's extraction logic: must continuously perform for algorithm visibility, face suppression of earnings transparency, and experience asymmetric risk (algorithm changes harm them; they cannot change the algorithm).
constraint_indexing:constraint_classification(performer_secondary_monetization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CREATOR COALITION (SCAFFOLD) — Organized efforts (performer unions, creator collectives, collective bargaining agreements) aim to establish mandatory licensing for secondary uses, transparency requirements, and sunset clauses for platform rights grabs. These mechanisms see the constraint as temporary — buildable alternatives include direct-to-fan platforms, cooperative distribution networks, and legally mandated performer revenue shares. Low effective extraction because the coalition has agency and perceives a regulatory/technological exit path within a defined timeframe.
constraint_indexing:constraint_classification(performer_secondary_monetization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY LICENSING FRAMEWORK (PITON) — Traditional copyright and performance-rights organizations (ASCAP, BMI, SAG-AFTRA) are largely performative in the digital context. Their licensing frameworks assume intentional distribution (radio, TV, theaters) but fail to capture secondary uses (algorithmic clips, AI training, recommendation feeds). The framework persists through institutional inertia — it retains nominal authority while actual power flows to platforms operating outside its jurisdiction. Theater ratio is moderate because some real licensing does occur, but the framework is degraded relative to the actual extraction mechanisms.
constraint_indexing:constraint_classification(performer_secondary_monetization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, secondary monetization appears as an immutable consequence of attention economics: once content is created and distributed, secondary uses (recommendations, clips, compilations) necessarily follow because reusing content is more efficient than creating new content. This perspective risks naturalizing what is actually a contingent contractual and technological architecture. The engine will classify this as a false summit — the apparent naturalness derives from institutional lock-in and bargaining asymmetry, not from laws of nature.
constraint_indexing:constraint_classification(performer_secondary_monetization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performer_secondary_monetization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(performer_secondary_monetization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(performer_secondary_monetization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(performer_secondary_monetization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(performer_secondary_monetization, TR),
    TR >= 0.70.

:- end_tests(performer_secondary_monetization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, rising over the interval. Secondary uses generate significant platform revenue (estimated 20-30% of total platform value in recommendation/aggregation) with minimal performer compensation. The growth trajectory reflects discovery of new monetization channels (AI training, clips, compilations). However, extractiveness is not 0.75+ (high snare threshold) because platforms do provide legitimate distribution value and some creators do earn meaningful income — the extraction is real but mixed with genuine coordination benefit. Suppression (0.65): High. Performer visibility is algorithmic (non-transparent), contractual terms unilateral (performers cannot negotiate), and exit costs are total (built-in audience). Transparency about secondary use is minimal; performers often discover their content reused only when fans alert them. Theater ratio (0.48): Moderate. Platform creator-support programs (YouTube Partner Program, TikTok Creator Fund, Twitch revenue-sharing) involve real payouts but are widely recognized as insufficient; this creates a gap between the theatrical claim ('we support creators') and functional reality ('most creators earn <$1000/year'). The ratio is trending downward as performers increasingly recognize the gap.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence across six types signals a true hybrid coordination-extraction constraint. The gap exists because different agents have radically different exit options (arbitrage vs trapped), power levels (institutional vs powerless), and structural relationships to the secondary monetization mechanism. Platform operators see low extraction because they control the mechanism and benefit from it. Dependent performers see high extraction because they lack control and lack exit. The perspectival gap is not a measurement error — it is diagnostic evidence that the constraint simultaneously coordinates (provides global distribution) and extracts (captures secondary value without performer input).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values vary sharply by agent structural position. Dependent performers have d ≈ 0.90 (full victim: trapped, no exit, maximal extraction) yielding high f(d) ≈ 1.35. Platform operators have d ≈ 0.05 (full beneficiary: arbitrage, control infrastructure, extractive direction) yielding negative f(d) ≈ -0.12. Mid-career artists have d ≈ 0.65 (mixed: constrained exit, mixed extraction-coordination) yielding f(d) ≈ 1.00. The engine computes χ = ε × f(d) × σ(S) for each perspective. For dependent performer (global scope σ=1.2): χ ≈ 0.58 × 1.35 × 1.2 ≈ 0.94 (high effective extraction). For platform (global scope): χ ≈ 0.58 × (-0.12) × 1.2 ≈ -0.08 (negative extraction, reflects beneficiary status). For organized coalition (national scope σ=1.0): χ ≈ 0.58 × 0.60 × 1.0 ≈ 0.35 (moderate, reflects exit pathways). These computed χ values drive classification differences across perspectives without requiring separate ε values — one constraint, multiple directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival multiplicity and directionality differentiation. The constraint is NOT 'really' a snare or a rope or a scaffold — it is genuinely all of these from different structural positions. The dependent performer's snare experience is real; the platform's rope experience is real; the coalition's scaffold experience is real. No single type 'is correct.' The false summit (mountain) occurs when the analytical observer naturalizes the constraint as immutable ('platforms must monetize to sustain; secondary uses must follow platform terms; creator dependence is inevitable'). But the structural data shows this is contingent: creator coalitions are building alternatives, regulators are establishing mandatory licensing, and direct-to-fan platforms are reducing algorithmic lock-in. The mountain classification fails the natural law gates (ε should be ≤0.25, suppression ≤0.05) — the engine will mark it as a false summit and flag the underlying assumption that platform dominance is natural rather than enforced by bargaining asymmetry and contractual lock-in.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secondary_use_definition_boundary,
    'What constitutes a ''secondary use'' requiring separate performer compensation vs. integrated platform service with primary revenue share?',
    'Comparative analysis of performer revenue models across platforms; legal definition mapping (EU digital services regulations, performer union contracts); performer earnings transparency data showing revenue attribution per use type',
    'If secondary uses are broadly defined: many more extraction mechanisms are captured and suppression appears higher. If narrowly defined: secondary monetization appears as minor coordination issue, not major extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_use_definition_boundary, conceptual, 'Definition of secondary use vs. integrated service').

omega_variable(
    algorithmic_suppression_mechanism,
    'Is platform suppression of performer visibility a deliberate revenue extraction mechanism or an incidental effect of recommendation algorithms optimizing for engagement/watch-time?',
    'Algorithm transparency audits; internal platform documentation (if available); comparative analysis of visibility decay for performers across tier levels; correlation between suppression patterns and performers'' willingness to accept secondary-use extraction',
    'If deliberate: suppression score should be 0.75+; mechanism is pure coercion. If incidental: suppression is 0.40-0.55; mechanism is structural bias in optimization targets, not intentional gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_suppression_mechanism, empirical, 'Whether visibility suppression is deliberate extraction or algorithmic artifact').

omega_variable(
    performer_exit_pathway_feasibility,
    'Can performers realistically build independent income streams (Patreon, direct subscriptions, merchandise, live performance) that reduce platform dependence below the ''trapped'' threshold?',
    'Longitudinal earnings data for independent creators; comparison of audience retention when performers move off-platform; time-to-viability analysis for creator-owned alternatives; market-share shifts from platform to creator-owned channels',
    'If feasible: many performers shift from ''trapped'' to ''constrained'' classification; effective extraction χ decreases. If infeasible: platform lock-in persists; powerless perspective remains snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performer_exit_pathway_feasibility, empirical, 'Feasibility of performer exit to independent income streams').

omega_variable(
    ai_training_compensation_precedent,
    'Will secondary use of performer content for AI training establish a new compensation category, or will it be absorbed into platform licensing agreements without additional performer payment?',
    'Tracking of performer lawsuits (voice actors, visual artists); regulatory guidance on AI training data licensing; platform licensing model evolution; performer union contract negotiations on generative AI',
    'If compensation established: extractiveness floor rises to 0.70+ as new use cases expand. If absorbed into existing licensing: extractiveness persists at 0.58-0.65; AI amplifies the problem without changing its structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_training_compensation_precedent, preference, 'Whether AI training establishes new compensation category').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performer_secondary_monetization, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psm_tr_t0, performer_secondary_monetization, theater_ratio, 0, 0.52).
narrative_ontology:measurement(psm_tr_t5, performer_secondary_monetization, theater_ratio, 5, 0.5).
narrative_ontology:measurement(psm_tr_t10, performer_secondary_monetization, theater_ratio, 10, 0.48).
narrative_ontology:measurement(psm_tr_t15, performer_secondary_monetization, theater_ratio, 15, 0.44).

% Extraction over time
narrative_ontology:measurement(psm_be_t0, performer_secondary_monetization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(psm_be_t5, performer_secondary_monetization, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(psm_be_t10, performer_secondary_monetization, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(psm_be_t15, performer_secondary_monetization, base_extractiveness, 15, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performer_secondary_monetization, resource_allocation).
narrative_ontology:affects_constraint(performer_secondary_monetization, attention_economy_labor_extraction).
narrative_ontology:affects_constraint(performer_secondary_monetization, algorithmic_visibility_suppression).
narrative_ontology:affects_constraint(performer_secondary_monetization, content_ownership_fragmentation).

% DUAL FORMULATION NOTE:
% Performer secondary monetization is downstream of algorithmic visibility systems (visibility suppression enables extraction), upstream of content ownership fragmentation (secondary uses fragment performer rights across platforms). These three constraints form a family: visibility suppression creates performer lock-in; lock-in enables secondary monetization; secondary monetization fragments ownership and control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performer_secondary_monetization, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
