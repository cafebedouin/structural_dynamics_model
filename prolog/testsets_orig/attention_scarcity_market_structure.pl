% ============================================================================
% CONSTRAINT STORY: attention_scarcity_market_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_scarcity_market_structure, []).

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
 *   constraint_id: attention_scarcity_market_structure
 *   human_readable: Attention Scarcity Market Structure
 *   domain: digital_economy/cognitive_resources
 *
 * SUMMARY:
 *   Attention scarcity creates a fundamental market structure where human
 *   cognitive capacity is the constrained resource and platform architecture
 *   determines who captures the surplus value of that scarcity. This
 *   constraint exhibits a hybrid coordination-extraction pattern (Tangled
 *   Rope): genuine coordination function exists (matching creators with
 *   audiences, organizing information overload) alongside systematic
 *   extraction (user autonomy, data exploitation, behavioral manipulation).
 *   The constraint's evolution from 2010-2026 shows accelerating
 *   extractiveness as platforms mature from coordination systems toward pure
 *   attention harvesting. Theater ratio (0.68) reflects the proliferation of
 *   performative attention-management practices—focus modes, notification
 *   controls, digital wellness theater—that operate at user-behavior level
 *   while architectural extraction mechanisms remain unchanged. The
 *   constraint generates distinct phenomenologies across positions: powerless
 *   users experience snare-like capture; creators experience mixed
 *   coordination/extraction; platforms experience rope-like enabling
 *   infrastructure; regulators see a temporary scaffold with sunset
 *   timelines; the civilizational observer risks naturalizing a contingent
 *   institutional arrangement as immutable scarcity.
 *
 * KEY AGENTS:
 *   - Information Seeker (User): Primary victim (powerless/trapped) — experiences total architectural capture with no viable exit options across major platforms
 *   - Attention Harvester (Platform/Advertiser): Primary beneficiary (institutional/arbitrage) — captures surplus value of attention scarcity through algorithmic and design infrastructure
 *   - Content Creator: Secondary agent (moderate/constrained) — needs platform distribution but loses autonomy over presentation and algorithmic visibility
 *   - Digital Rights Coalition: Organized actor (organized/constrained) — building regulatory and architectural alternatives with explicit sunset logic
 *   - Attention Management Theater: Institutional practice (institutional/arbitrage) — individual discipline practices that operate at behavior level without addressing structural mechanisms
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing engineered extraction as intrinsic scarcity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_scarcity_market_structure, 0.58).
domain_priors:suppression_score(attention_scarcity_market_structure, 0.62).
domain_priors:theater_ratio(attention_scarcity_market_structure, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_scarcity_market_structure, extractiveness, 0.58).
narrative_ontology:constraint_metric(attention_scarcity_market_structure, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(attention_scarcity_market_structure, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_scarcity_market_structure, tangled_rope).
narrative_ontology:human_readable(attention_scarcity_market_structure, "Attention Scarcity Market Structure").
narrative_ontology:topic_domain(attention_scarcity_market_structure, "digital_economy/cognitive_resources").

domain_priors:requires_active_enforcement(attention_scarcity_market_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_scarcity_market_structure, attention_harvesters).
narrative_ontology:constraint_beneficiary(attention_scarcity_market_structure, algorithmic_platforms).
narrative_ontology:constraint_victim(attention_scarcity_market_structure, information_seeker_autonomy).
narrative_ontology:constraint_victim(attention_scarcity_market_structure, collective_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATION SEEKER (SNARE) — Users face total architectural capture. Alternative platforms replicate the same attention-extraction mechanisms; offline information access is increasingly unavailable or prohibitively costly. Exit from the ecosystem requires abandoning participation in economic, social, and civic life. The user experiences maximum extraction with zero alternatives.
constraint_indexing:constraint_classification(attention_scarcity_market_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONTENT CREATOR (TANGLED ROPE) — Faces genuine coordination problem: reaching an audience requires using platform infrastructure that simultaneously extracts attention value. Creator benefits from distribution access but loses autonomy over format, timing, and algorithmic visibility. Constrained exit: building alternative audience infrastructure is costly; algorithmic suppression penalizes platform departure.
constraint_indexing:constraint_classification(attention_scarcity_market_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ATTENTION HARVESTER (ROPE) — Advertising and engagement optimization networks see attention scarcity as pure coordination problem: matching available user attention to monetizable content. Beneficiary experiences the constraint as enabling infrastructure. Exit options are abundant: alternative platforms, alternative monetization models, geographic arbitrage. The extraction runs toward this agent.
constraint_indexing:constraint_classification(attention_scarcity_market_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL RIGHTS COALITION (SCAFFOLD) — Organized agents (data protection regulators, open-source communities, attention-tracking activists) see the scarcity structure as a temporary governance failure with policy sunset. EU Digital Services Act, DMA, and GDPR represent emerging alternative architectures: algorithmic transparency, algorithmic choice, data portability. Constraint has explicit sunset clause tied to regulatory enforcement timelines. Low effective extraction because the coalition has agency and measurable exit pathways.
constraint_indexing:constraint_classification(attention_scarcity_market_structure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ATTENTION MANAGEMENT THEATER (PITON) — Traditional attention-management practices (focus modes, do-not-disturb, app blockers, time-limiting tools) are largely performative: they operate at the user-behavior level while the structural incentives remain unchanged. Users can temporarily resist attention capture but the mechanism persists through institutional inertia. Theater persists because the underlying extraction function is unchanged — individual discipline cannot overcome systemic design.
constraint_indexing:constraint_classification(attention_scarcity_market_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, human attention is intrinsically finite. Scarcity of attention is an immutable fact of cognition; extraction of attention for competing purposes is a natural consequence of this scarcity. The market structure is seen as an inevitable coordination response to irreducible scarcity. However, this perspective naturalizes what is actually a contingent institutional arrangement — the *specific architecture* of attention capture (algorithmic ranking, infinite scroll, notification cascades, dark patterns) is not a law of nature but engineered extraction.
constraint_indexing:constraint_classification(attention_scarcity_market_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_scarcity_market_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_scarcity_market_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_scarcity_market_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_scarcity_market_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_scarcity_market_structure, TR),
    TR >= 0.70.

:- end_tests(attention_scarcity_market_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platforms capture substantial surplus value through algorithmic ranking and attention harvesting, but the extraction is not absolute—users retain some agency through platform choice (weak), content control (limited), and exit (costly but possible). The trajectory shows acceleration from 0.32 (2010, early social media) to 0.58 (2026), reflecting maturation of monetization infrastructure and design sophistication. If measurement extends to 2030 with current trends, extractiveness could reach 0.70+ (snare threshold). Suppression (0.62): High. Barriers to exit include network effects (friends/followers concentrated on dominant platforms), creator dependency (audience requires platform scale), and technological switching costs. Architectural design (infinite scroll, notification cascades, algorithmic personalization) suppresses user control of their own attention. But suppression is not total—users retain ability to leave, though at significant cost. Theater ratio (0.68): High and rising. User-level attention management practices (focus modes, app limiters, notification controls) are largely performative—they let users feel in control while architectural incentives remain unchanged. The theater has grown as platforms have added 'wellness' features while simultaneously engineering greater engagement capture.
 *
 * PERSPECTIVAL GAP:
 *   The snare perspective (information seeker) sees total capture with no exit. The tangled rope perspective (content creator) sees genuine coordination function alongside asymmetric extraction. The rope perspective (attention harvester) sees coordination enabling infrastructure. The scaffold perspective (regulators) sees a temporary institutional problem with policy solutions and measurable sunset. The piton perspective sees performative resistance mechanisms masking unchanged extraction. The mountain perspective risks naturalizing engineered architecture as intrinsic scarcity. The gap reflects real structural differences in power and exit capacity: powerless users trapped in network effects experience the constraint differently than platforms that can arbitrage alternatives. The regulatory coalition sees governance failures that engineering cannot resolve—they predict a sunset through policy intervention. The analytical observer risks the false summit of naturalizing 'attention scarcity' when the constraint is actually 'engineered attention extraction through contingent institutional architecture.'
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Attention harvesters (institutional/arbitrage) are net beneficiaries with abundant exit options—they can migrate platforms, change monetization models, or abandon the attention economy. Their d value is low (~0.15), producing negative or minimal effective extraction. Information seekers (powerless/trapped) are net victims with no meaningful exit—alternatives replicate the same mechanisms; offline information access is unavailable. Their d value is high (~0.95), producing maximum f(d) and maximum experienced extractiveness. Content creators occupy the middle: they benefit from distribution but are constrained by algorithmic dependence. Their d value is moderate-high (~0.60), producing tangled rope classification. The regulatory coalition has organized power and sees explicit policy exit mechanisms (DSA enforcement), moderating their d value (~0.40) despite victim status for autonomy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_design_extraction,
    'What portion of measured extraction derives from irreducible attention scarcity versus engineered extraction through behavioral design?',
    'Comparative analysis of attention patterns across platforms with different design philosophies (minimalist vs engagement-optimized); measurement of attention ''loss'' in platforms designed for user control versus platforms optimized for engagement harvesting',
    'If engineered design dominates: constraint is primarily Snare/Tangled Rope (redesignable). If scarcity dominates: constraint approaches Mountain (irreducible). Current data suggests 60-70% design, 30-40% structural scarcity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_design_extraction, empirical, 'Proportion of extraction from design versus intrinsic scarcity').

omega_variable(
    collective_action_threshold,
    'What critical mass of user defection would trigger platform business model collapse or forcing redesign?',
    'Network analysis of platform interdependencies and liquidity effects; empirical study of user migration patterns during platform policy shifts; threshold modeling for creator and advertiser network effects',
    'If threshold is low (< 15% user loss): users have latent collective power (scaffold/rope perspectives). If threshold is high (> 40% loss required): structural lock-in is severe (snare perspective valid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'Critical mass threshold for user-driven platform redesign').

omega_variable(
    regulatory_enforcement_timeline,
    'Will regulatory enforcement (DSA, DMA, GDPR) actually force architectural redesign or merely impose compliance theater on existing extraction mechanisms?',
    'Longitudinal measurement of user autonomy metrics post-regulation: algorithmic visibility, choice architecture, data portability actual usage rates; comparison of stated compliance versus behavioral outcomes; analysis of whether enforcement includes interoperability mandates',
    'If enforcement achieves real redesign: scaffold sunset is real. If enforcement becomes performative theater: scaffold perspective is aspirational; constraint remains Snare at scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_timeline, empirical, 'Whether regulation forces genuine architectural change').

omega_variable(
    alternative_coordination_viability,
    'Can decentralized or user-controlled attention coordination systems (ActivityPub, Bluesky, user-controlled feeds) achieve feature parity and user scale sufficient to break platform lock-in?',
    'Feature analysis and user experience comparison; network effect modeling for alternative architectures; adoption barriers (switching costs, network externalities, creator incentive misalignment)',
    'If viable: rope/scaffold perspectives are dominant. If not viable: snare perspective reflects user structural reality despite regulatory activity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_viability, empirical, 'Viability of alternative decentralized coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_scarcity_market_structure, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, attention_scarcity_market_structure, theater_ratio, 0, 0.42).
narrative_ontology:measurement(attn_tr_t7, attention_scarcity_market_structure, theater_ratio, 7, 0.55).
narrative_ontology:measurement(attn_tr_t14, attention_scarcity_market_structure, theater_ratio, 14, 0.68).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, attention_scarcity_market_structure, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(attn_be_t7, attention_scarcity_market_structure, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(attn_be_t14, attention_scarcity_market_structure, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_scarcity_market_structure, resource_allocation).
narrative_ontology:affects_constraint(attention_scarcity_market_structure, algorithmic_ranking_opacity).
narrative_ontology:affects_constraint(attention_scarcity_market_structure, data_extraction_asymmetry).
narrative_ontology:affects_constraint(attention_scarcity_market_structure, creator_platform_dependency).

% DUAL FORMULATION NOTE:
% Attention scarcity market structure is upstream of three more specific constraints: algorithmic ranking opacity (how much of extracted value comes from hidden ranking?), data extraction asymmetry (who controls behavioral data harvested?), and creator platform dependency (what would creator autonomy require?). Each has lower extractiveness; all three are downstream affected by changes to attention scarcity architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(attention_scarcity_market_structure, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
