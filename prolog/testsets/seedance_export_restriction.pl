% ============================================================================
% CONSTRAINT STORY: seedance_export_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_seedance_export_restriction, []).

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
 *   constraint_id: seedance_export_restriction
 *   human_readable: US Export Restrictions on ByteDance's SeeDance AI
 *   domain: political/technological/economic
 *
 * SUMMARY:
 *   The US export restriction on ByteDance's SeeDance AI represents a hybrid
 *   constraint combining genuine national security coordination logic with
 *   competitive market protection and Cold War-era bureaucratic inertia. The
 *   restriction prevents technology transfer to foreign entities while
 *   protecting US domestic AI companies from Chinese competition. This
 *   constraint exhibits five distinct classification types from different
 *   structural perspectives: pure extraction (snare) from ByteDance's view;
 *   mixed coordination-and-extraction (tangled rope) from non-US research
 *   communities; pure coordination (rope) from US domestic AI firms;
 *   temporary coordination with sunset logic (scaffold) from national
 *   security leadership; and degraded Cold War machinery (piton) from the
 *   perspective of institutional bureaucracy. The extractiveness value (0.58)
 *   reflects that the restriction generates measurable asymmetric benefits
 *   for US firms while imposing real costs on ByteDance and the global
 *   research community, but the costs are not absolute — alternative markets
 *   and licensing pathways provide partial compensation. The suppression
 *   metric (0.68) reflects high barriers to exit: ByteDance cannot easily
 *   relocate core R&D outside the US-controlled sphere, and alternative
 *   markets (EU, non-aligned nations) offer significantly reduced addressable
 *   demand.
 *
 * KEY AGENTS:
 *   - ByteDance International Operations: Primary victim (organized/trapped) — bears the direct cost of restricted technology transfer and reduced market access; no meaningful exit mechanism
 *   - US Domestic AI Industry: Primary beneficiary (institutional/arbitrage) — captures competitive advantage, market share, and government contract opportunities through restriction-enabled positioning
 *   - Global AI Research Community: Secondary victim (organized/constrained) — non-US researchers face restricted access to SeeDance capabilities; alternative data sources and models available but at higher cost and lower capability
 *   - US National Security Apparatus: Coordinating agent (powerful/mobile) — structures and enforces the restriction; maintains implicit sunset condition (forced sale, technology obsolescence, or policy reversal)
 *   - Cold War Export Control Apparatus: Institutional actor (institutional/constrained) — bureaucratic machinery that implements restrictions; persists through inertia despite partial misalignment with modern technological reality
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risk of naturalizing contingent policy choice as inherent to great-power competition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(seedance_export_restriction, 0.58).
domain_priors:suppression_score(seedance_export_restriction, 0.68).
domain_priors:theater_ratio(seedance_export_restriction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(seedance_export_restriction, extractiveness, 0.58).
narrative_ontology:constraint_metric(seedance_export_restriction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(seedance_export_restriction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(seedance_export_restriction, tangled_rope).
narrative_ontology:human_readable(seedance_export_restriction, "US Export Restrictions on ByteDance's SeeDance AI").
narrative_ontology:topic_domain(seedance_export_restriction, "political/technological/economic").

domain_priors:requires_active_enforcement(seedance_export_restriction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(seedance_export_restriction, us_domestic_ai_companies).
narrative_ontology:constraint_beneficiary(seedance_export_restriction, us_national_security_apparatus).
narrative_ontology:constraint_victim(seedance_export_restriction, bytedance_international_operations).
narrative_ontology:constraint_victim(seedance_export_restriction, global_ai_research_collaboration).
narrative_ontology:constraint_victim(seedance_export_restriction, consumer_ai_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BYTEDANCE INTERNATIONAL RESEARCH DIVISION (SNARE) — Trapped by unilateral US export restrictions with no meaningful appeal mechanism or alternative market access. Cannot exit the US market (where SeeDance was developed) without abandoning substantial R&D investment. Bears extraction costs of restricted technology transfer, reduced revenue streams, and forced localization requirements. Maximum experienced coercion with minimal coordination benefit.
constraint_indexing:constraint_classification(seedance_export_restriction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-US AI RESEARCH COMMUNITY (TANGLED ROPE) — Constrained by restricted access to SeeDance capabilities, but benefits from the underlying coordination logic: export controls theoretically protect IP sovereignty and enable collaborative frameworks with trusted partners. However, suppression is high (researchers in non-allied nations face near-complete isolation from this technology stack). Genuine coordination function (shared IP protections, bilateral research agreements) alongside asymmetric extraction (non-US researchers bear most access cost).
constraint_indexing:constraint_classification(seedance_export_restriction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: US DOMESTIC AI INDUSTRY (ROPE) — Primary beneficiary with high arbitrage capacity (can develop competing systems, absorb SeeDance's market share, access US government contracts). Export restrictions protect market positioning and reduce foreign competition. Experiences the constraint as pure coordination: rule enforcement that solves the collective action problem of maintaining US technological leadership without triggering trade wars that would harm all domestic firms equally.
constraint_indexing:constraint_classification(seedance_export_restriction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: US NATIONAL SECURITY APPARATUS (SCAFFOLD) — Sees export restrictions as a temporary coordination mechanism with implicit sunset logic. The restriction is theoretically transitional: either (a) ByteDance restructures to eliminate foreign ownership (forced sale scenario), (b) SeeDance achieves strategic irrelevance through faster US competitor development, or (c) geopolitical détente permits normalized trade. Theater ratio is moderate (restrictions are partly performative — genuine security concerns exist but are intermingled with competitive protectionism). Sunset condition: if the strategic threat migrates (e.g., new frontier technology replaces AI, or US hegemony in AI becomes uncontestable), restrictions lose enforcement justification.
constraint_indexing:constraint_classification(seedance_export_restriction, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR EXPORT CONTROL APPARATUS (PITON) — The institutional machinery (CFIUS, EAR, ITC regulations) persists through bureaucratic inertia long after original strategic rationale has evolved. Technology restrictions are largely performative: SeeDance restrictions follow Cold War-era classifications (encryption, dual-use, advanced computing) that were designed for chip exports and nuclear materials, not AI systems. Theater ratio is high (compliance rituals, quarterly reporting, bureaucratic bottlenecks) with low functional verification (no mechanism prevents algorithmic knowledge transfer; ByteDance can publish research with identical capability). The apparatus persists because alternatives (tariffs, licensing regimes, bilateral agreements) are politically contested and institutional change is slow.
constraint_indexing:constraint_classification(seedance_export_restriction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some asymmetry in technology diffusion is inherent to the structure of great-power competition: advanced nations always restrict strategic technologies to maintain relative advantage, and this friction is an immutable feature of the international system. This perspective risks naturalizing what is actually a contingent policy choice. However, the structural data contradicts the mountain classification — the restriction's enforceability, lifetime, and scope are all policy-dependent variables, not natural laws. The engine's false summit detector will flag this as naturalization of institutional arrangements.
constraint_indexing:constraint_classification(seedance_export_restriction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(seedance_export_restriction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(seedance_export_restriction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(seedance_export_restriction, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(seedance_export_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(seedance_export_restriction, TR),
    TR >= 0.70.

:- end_tests(seedance_export_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The restriction extracts measurable benefits for US domestic AI companies (protected market share, reduced competition, preferential government access) while imposing costs on ByteDance (lost US market access, constrained R&D collaboration, reduced revenue). The value reflects that extraction is real but not maximal — ByteDance retains access to other markets, can develop non-restricted product lines, and benefits indirectly from US research standards and talent. The trajectory from 0.42 → 0.58 reflects increasing enforcement tightness and competitive impact over the interval. Suppression (0.68): High. Significant barriers include unilateral US authority (ByteDance has no seat at decision-making table), limited appeal mechanisms (CFIUS decisions are opaque), and asymmetric consequences (US can retaliate against ByteDance; ByteDance cannot impose equivalent restrictions). Suppression is not absolute (diplomatic channels exist; licensing negotiations are possible) but represents high coercive overhead. Theater ratio (0.55): Moderate. The restriction has genuine security components (prevents technology transfer to foreign state actors) intermingled with competitive protectionism (reduces Chinese tech competition). The theater reflects that some compliance activities (CFIUS reviews, licensing forms, security certifications) are performative — they create administrative cost without corresponding security verification. The trajectory from 0.35 → 0.55 reflects increasing bureaucratic overhead as the apparatus matures.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between beneficiary and victim. US domestic AI firms see the restriction as solving a legitimate coordination problem (maintaining US tech leadership in a competitive international context). ByteDance sees the restriction as unilateral extraction with no coordination benefit. National security sees a temporary problem with an exit condition (forced sale or strategic irrelevance). The Cold War apparatus sees a degraded ritual that persists through inertia. These are not different opinions about the same fact — they are different structural positions that generate different causal chains. ByteDance genuinely has fewer options than US firms. US firms genuinely benefit from restriction-enabled competition reduction. National security apparatus genuinely can envision conditions under which the restriction would be lifted. The apparatus genuinely performs activities with low security return relative to bureaucratic cost. The gap is not resolvable by appealing to a single 'correct' classification — it is the starting point for diagnosing what institutional changes would shift the constraint's type.
 *
 * DIRECTIONALITY LOGIC:
 *   ByteDance's directionality (d ≈ 0.85) derives from their status as primary victim with trapped exit options: they are the target of extraction, they cannot easily exit the US sphere (core R&D infrastructure, cloud compute, talent), and they experience the restriction as pure coercion. US domestic AI companies' directionality (d ≈ 0.10) derives from beneficiary status with arbitrage options: they benefit from the restriction and can walk away (develop competing systems, pivot to non-restricted markets) if the restriction were lifted. National security apparatus' directionality (d ≈ 0.50) derives from symmetric position: they benefit from restriction-enabled coordination but also bear enforcement costs, monitoring overhead, and risk of retaliation. The piton perspective uses institutional power + arbitrage exit + beneficiary status, but classified as piton because theater_ratio (0.55) is high and the underlying functional capacity (actual security verification) is low relative to administrative overhead. The mountain perspective is flagged as false summit: the civilizational observer risks naturalizing the restriction as an immutable feature of great-power competition, but the restriction is contingent on specific policy choices, technological capabilities, and geopolitical conditions that could change.
 *
 * MANDATROPHY ANALYSIS:
 *   INCOMPLETE RESOLUTION: This constraint has extractiveness 0.58 < 0.70, so mandatrophy resolution is optional but recommended for policy clarity. The constraint currently classifies as tangled_rope because it combines a genuine coordination function (coordinating US tech policy to maintain international advantage) with asymmetric extraction (ByteDance loses market access without reciprocal benefit). The mandatrophy risk is the naturalization of the restriction as immutable great-power competition (mountain perspective). RESOLUTION PATH: Declare explicit exit conditions for the restriction (forced sale, competitive parity, or policy reversal timeline). This clarifies whether the restriction is tangled_rope (hybrid with sunset) or snare (permanent extraction). If explicit sunset conditions are declared, the constraint moves from ambiguous tangled_rope to clear scaffold (temporary coordination with enforcement). If no sunset conditions are declared, the constraint risks drifting toward snare (permanent extraction masked as security policy). The current classification assumes implicit sunset logic — the restriction will eventually resolve through one of three mechanisms: (1) ByteDance sells non-Chinese operations to a US company, (2) US AI capability becomes so dominant that SeeDance poses no strategic threat, or (3) geopolitical conditions normalize and restrictions are lifted. Without explicit commitment to one of these outcomes, the constraint's classification remains unstable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_knowledge_transfer,
    'Can SeeDance''s algorithmic capabilities be reconstructed from published research literature and open-source implementations, rendering the export restriction ineffective?',
    'Analysis of SeeDance''s novel components vs. published SOTA methods; reverse-engineering feasibility assessment; tracking of knowledge transfer through publications and code repositories',
    'If transferable: restriction is primarily theatrical (high theater_ratio justified). If non-transferable: restriction has genuine security function (mountain-adjacent classification). Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_knowledge_transfer, empirical, 'Whether SeeDance capabilities can be reconstructed from open literature').

omega_variable(
    bytedance_foreign_control_status,
    'Does ByteDance retain substantive independent control over SeeDance development, or is the technology effectively controlled by entities with foreign government influence?',
    'Corporate structure analysis; shareholder voting control; employment of foreign nationals in core R&D; evidence of tech transfer to state-owned entities',
    'If genuinely independent: ByteDance qualifies as normal multinational (snare classification unjustified). If foreign-controlled: extraction is justified by legitimate security concern (tangled_rope / rope from US perspective clarified). Affects beneficiary/victim framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bytedance_foreign_control_status, conceptual, 'ByteDance''s independence from foreign government control').

omega_variable(
    alternative_market_compensation,
    'Do alternative markets (EU, non-aligned nations, licensing regimes) provide meaningful compensation for ByteDance''s lost US market access, or is suppression genuine?',
    'Revenue impact analysis; market size analysis of non-US jurisdictions; licensing revenue tracking; competitive analysis of ByteDance''s non-US product positioning',
    'If alternatives are substantial: suppression metric lower (~0.45), constraint shifts toward rope (coordination without high coercion). If alternatives minimal: suppression justified at 0.68+, classification remains tangled_rope/snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_market_compensation, empirical, 'Availability of meaningful alternative markets for ByteDance').

omega_variable(
    sunset_condition_clarity,
    'Does the US government maintain explicit or implicit exit conditions for the export restriction (forced sale, strategic irrelevance, policy reversal), or is the restriction indefinite?',
    'Legislative language analysis; CFIUS decision documents; policy statements from State/Commerce/Defense; historical precedent for restriction reversal or expiration',
    'If explicit sunset: scaffold classification strengthened (genuine temporary nature). If indefinite: scaffold classification weakened, shifts toward snare (permanent extraction). Affects mandatrophy logic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_condition_clarity, conceptual, 'Explicit or implicit sunset conditions for the restriction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(seedance_export_restriction, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seedance_tr_t0, seedance_export_restriction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(seedance_tr_t3, seedance_export_restriction, theater_ratio, 3, 0.5).
narrative_ontology:measurement(seedance_tr_t6, seedance_export_restriction, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(seedance_be_t0, seedance_export_restriction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(seedance_be_t3, seedance_export_restriction, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(seedance_be_t6, seedance_export_restriction, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(seedance_export_restriction, global_infrastructure).
narrative_ontology:affects_constraint(seedance_export_restriction, us_china_tech_decoupling).
narrative_ontology:affects_constraint(seedance_export_restriction, semiconductor_export_control).
narrative_ontology:affects_constraint(seedance_export_restriction, ai_research_globalization).

% DUAL FORMULATION NOTE:
% This constraint is downstream of broader US-China tech competition and upstream of specific sector-level decoupling (semiconductor, biotech). SeeDance restrictions are a specific instantiation of general technological sovereignty concerns. Related constraints: us_china_tech_decoupling (higher-level geopolitical framing), semiconductor_export_control (similar restriction mechanism applied to hardware), ai_research_globalization (constraint on international AI collaboration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(seedance_export_restriction, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
