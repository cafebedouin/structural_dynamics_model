% ============================================================================
% CONSTRAINT STORY: creative_commons_licensing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creative_commons_licensing, []).

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
 *   constraint_id: creative_commons_licensing
 *   human_readable: Creative Commons Licensing Framework
 *   domain: legal/technological
 *
 * SUMMARY:
 *   The Creative Commons licensing framework emerged in the early 2000s to
 *   solve a coordination problem: copyright law defaults to maximum
 *   restriction (All Rights Reserved) but many creators want to authorize
 *   specific reuses without hiring lawyers or negotiating individually. CC
 *   provides standardized machine-readable and human-readable licenses (BY,
 *   BY-SA, BY-NC, BY-ND and their combinations) that creators can adopt to
 *   signal permission. The framework operates across jurisdictions through
 *   legal porting — translating core principles into local copyright law. CC
 *   is primarily a Rope (pure coordination mechanism) from most perspectives,
 *   but exhibits secondary extraction and performativity characteristics that
 *   make it a Tangled Rope from the platform ecosystem's view and a Piton
 *   from legacy copyright institutions. The constraint's extractiveness has
 *   increased over its interval (0.18→0.28) as platforms have incorporated CC
 *   as mandatory compliance infrastructure and legacy institutions have
 *   adopted CC symbolically while maintaining proprietary licensing
 *   dominance. Theater ratio has increased (0.35→0.55) as CC licenses have
 *   become increasingly used for institutional signaling rather than genuine
 *   permission expression — creators adopt CC-BY but upload to centralized
 *   platforms that control downstream use anyway.
 *
 * KEY AGENTS:
 *   - Individual Creators: Primary beneficiary (powerless/mobile) — escape legal complexity, gain attribution, participate in commons
 *   - Creative Commons Organization: Primary coordinator (institutional/arbitrage) — maintain and evolve licensing standard, drive adoption
 *   - Platform Ecosystem: Dual agent (organized/constrained) — experience coordination benefits but bear compliance burden; constrained to integrate CC in user interfaces
 *   - Open Knowledge Movement: Organized beneficiary (organized/constrained) — view CC as transitional bridge to decentralized metadata; see sunset in emerging standards
 *   - Large Rights Holders: Secondary participants (powerful/mobile) — adopt CC strategically for brand signaling, maintain proprietary licensing for commercial works
 *   - Legacy Copyright Institutions: Institutional performers (institutional/arbitrage) — formally endorse CC while driving revenue through traditional rights management
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing CC's specific taxonomy as inherent to permission expression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creative_commons_licensing, 0.28).
domain_priors:suppression_score(creative_commons_licensing, 0.32).
domain_priors:theater_ratio(creative_commons_licensing, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creative_commons_licensing, extractiveness, 0.28).
narrative_ontology:constraint_metric(creative_commons_licensing, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(creative_commons_licensing, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creative_commons_licensing, rope).
narrative_ontology:human_readable(creative_commons_licensing, "Creative Commons Licensing Framework").
narrative_ontology:topic_domain(creative_commons_licensing, "legal/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creative_commons_licensing, content_creators).
narrative_ontology:constraint_beneficiary(creative_commons_licensing, downstream_users).
narrative_ontology:constraint_beneficiary(creative_commons_licensing, open_knowledge_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL CREATOR (ROPE) — Creator without legal resources (powerless, mobile through CC adoption, biographical horizon). Experiences CC as pure coordination mechanism: express intent to share without hiring lawyers. No extraction perceived — creator gains attribution, potential attribution value, and community participation. d≈0.35, f(d)≈0.25, σ=1.2 → χ≈0.09. Low effective extraction; beneficiary of standardized coordination.
constraint_indexing:constraint_classification(creative_commons_licensing, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: CC ORGANIZATION (ROPE) — Institution that maintains and evolves the licensing standard (institutional, arbitrage exit through alternative licensing systems, immediate horizon focused on license adoption). Experiences CC as coordination function they coordinate. Benefits from network effects: more adopters = higher standard utility = institutional influence. Perceives no extraction from creators — relationship is cooperative. d≈0.10, f(d)≈-0.05, σ=1.2 → χ≈-0.02. Net beneficiary through coordination provision.
constraint_indexing:constraint_classification(creative_commons_licensing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM ECOSYSTEM (TANGLED ROPE) — Large content platforms (YouTube, Flickr, Wikimedia) with organized coordination but constrained exit from licensing compliance. Perceives both coordination benefits (legal clarity for content use) and extraction (mandatory compliance overhead, license attribution burden, integration complexity). Must implement CC compliance regardless of profitability. d≈0.60, f(d)≈0.75, σ=1.2 → χ≈0.25. Moderate extraction masked as coordination infrastructure burden.
constraint_indexing:constraint_classification(creative_commons_licensing, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE RIGHTS HOLDERS (ROPE) — Established media companies, academic publishers, entertainment studios with powerful negotiating position (powerful, mobile exit through proprietary licensing). CC appears as optional standard for strategic brand signaling (e.g., BBC, major universities). No suppression — exit costless. Pure coordination for strategic purposes. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.15. Low extraction; voluntary adoption for coordination benefits.
constraint_indexing:constraint_classification(creative_commons_licensing, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN KNOWLEDGE MOVEMENT (SCAFFOLD) — Coalition of librarians, educators, open-source communities with organized power and constrained exit (cultural commitment to openness). Views CC as transitional coordination infrastructure: a bridge from All Rights Reserved to automated, machine-readable permissions. Sees sunset: eventual emergence of decentralized metadata standards (JSON-LD, RDFA, blockchain provenance) that make CC's centralized license taxonomy obsolete. Suppression low (can exit to alternative metadata). d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.17. Temporary coordination with visible sunset path.
constraint_indexing:constraint_classification(creative_commons_licensing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY COPYRIGHT INSTITUTIONS (PITON) — Copyright agencies, collecting societies, legal institutes that formally endorse CC but maintain primary identity through All Rights Reserved licensing (institutional, arbitrage exit to proprietary licenses, civilizational horizon). Theater ratio high (0.58): CC compliance is performative for these institutions — they celebrate open licensing while driving revenue through rights management and licensing fees. Legacy systems persist through inertia despite CC's structural superiority for open coordination.
constraint_indexing:constraint_classification(creative_commons_licensing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN perspective) — From civilizational/universal horizon, CC's core function (expressing permissions as machine-readable standardized metadata) reflects an immutable structure: any system for decentralized coordination at scale requires standardized permission expression. The underlying constraint is informational — how to communicate intent across heterogeneous legal systems and technical platforms. However, ε=0.28 and suppression=0.32 contradict mountain thresholds. This is a FALSE SUMMIT: the analytical perspective risks naturalizing a contingent institutional choice (CC's specific license taxonomy) as inherent to permission expression itself.
constraint_indexing:constraint_classification(creative_commons_licensing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creative_commons_licensing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(creative_commons_licensing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(creative_commons_licensing, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(creative_commons_licensing, TR),
    TR >= 0.70.

:- end_tests(creative_commons_licensing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. CC primarily solves a coordination problem — creators can express nuanced permissions without legal overhead. However, extractiveness is non-zero because: (1) CC depends on platform integration for discoverability, creating latent extraction points; (2) CC-NC clause creates artificial scarcity that serves creator commercial interests while appearing open; (3) platform compliance burden falls on aggregators, not creators, creating asymmetric distribution of coordination costs. The increase from 0.18→0.28 reflects platforms' maturation: early CC adoption was genuinely optional for platforms, but current compliance is mandatory in many jurisdictions (e.g., GDPR derivative data sharing), making CC infrastructure an extraction point. Suppression (0.32): Moderate-low. Creators can freely adopt CC (no barriers). Downstream users can freely adopt CC-licensed work within license terms (low suppression). However, suppression is non-zero because: (1) attribution requirement for CC-BY creates friction; (2) legal heterogeneity means CC-licensed work may still face copyright liability in non-porting jurisdictions; (3) platforms mediate actual access to CC works, creating de facto suppression. Theater ratio (0.55): Moderate-high. Increasing performativity: institutions adopt CC licenses for open-access signaling while uploading to restricted platforms (YouTube, Flickr require account creation). Large publishers use CC-BY for certain journals while maintaining proprietary paywalls for others. The gap between CC signaling and actual open distribution has widened.
 *
 * PERSPECTIVAL GAP:
 *   Individual creators see pure Rope — CC solves their coordination problem without cost. Open knowledge movement sees Scaffold — CC is a temporary bridge with visible sunset as decentralized metadata (RDF, JSON-LD, blockchain provenance) mature. Platforms see Tangled Rope — coordination benefits mixed with mandatory compliance burden. Large rights holders see optional Rope — strategic signaling without constraint. Legacy copyright institutions see Piton — formally adopt CC while maintaining primary identity through proprietary licensing (high theater). The analytical observer risks seeing Mountain (immutable permission expression structure) but the structural data contradicts this — CC's specific six-license taxonomy is contingent, not necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual creators: Beneficiary + mobile → d≈0.35, f(d)≈0.25. Low extraction; high benefit from legal simplification. CC Organization: Coordinator + arbitrage → d≈0.10, f(d)≈-0.05. Negative extraction; pure beneficiary of ecosystem they coordinate. Platform Ecosystem: Victim + constrained → d≈0.60, f(d)≈0.75. Moderate extraction; constrained by regulatory compliance and platform norms to integrate CC, creating burden. Open Knowledge Movement: Beneficiary + constrained → d≈0.45, f(d)≈0.50. Low extraction; constrained by cultural commitment but benefits from CC as transition pathway. Large Rights Holders: Beneficiary + mobile → d≈0.40, f(d)≈0.40. Low extraction; voluntary adoption for signaling benefits. Legacy Institutions: Ambiguous + arbitrage → d≈0.15, f(d)≈0.05. Piton classification derives from theater (0.58), not from high chi; institutions can exit to proprietary systems.
 *
 * MANDATROPHY ANALYSIS:
 *   CC resolves mandatrophy by demonstrating a pure Rope classification from the majority of perspectives. The individual creator, CC organization, open knowledge movement, and large rights holders all see genuine coordination with minimal extraction. The Tangled Rope from platforms and Piton from legacy institutions are secondary — they represent institutional inertia and selective adoption rather than the core constraint. The analytical observer's Mountain is definitively false: ε=0.28 and suppression=0.32 fail every mountain gate (ε ≤ 0.25, suppression ≤ 0.05). The false summit reveals that 'standardized permission expression' is not an immutable feature of decentralized coordination — it is a contingent institutional choice by CC. Alternative approaches (blockchain provenance, cryptographic attestation, jurisdictional harmonization) could solve the same coordination problem with different structural properties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semantic_vs_technical_interoperability,
    'Is CC''s primary function semantic coordination (human-readable permission expression) or technical coordination (machine-readable license metadata)?',
    'Analysis of actual license usage: percentage of CC works where downstream users consult license text vs automated detection; adoption rates of CC RDFa/JSON-LD metadata embedding vs human-only licensing',
    'If semantic: CC succeeds through human understanding and norm-setting. If technical: CC''s value depends on platform integration — extraction potential from platforms that mediate technical discovery increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_vs_technical_interoperability, empirical, 'Semantic vs technical primacy in CC coordination').

omega_variable(
    attribution_enforcement_capacity,
    'Can CC''s attribution requirements (CC-BY) be enforced at scale without centralized monitoring, or does enforcement depend on platform cooperation that creates extraction points?',
    'Empirical study of CC-BY violation rates; investigation of whether platforms enforce attribution vs creators voluntarily maintain it; comparison to systems with embedded cryptographic attribution (blockchain provenance)',
    'If decentralized enforcement possible: CC remains pure coordination (Rope). If platform-dependent: platforms extract from creators through selective enforcement, making CC a Tangled Rope from creator perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attribution_enforcement_capacity, empirical, 'Degree of decentralized vs platform-mediated attribution enforcement').

omega_variable(
    commercial_remix_boundary,
    'Does CC''s NonCommercial (NC) clause represent genuine restriction or artificial scarcity designed to preserve commercial licensing value?',
    'Legal analysis of NC interpretation disputes; survey of creator intent in NC vs non-NC choice; measurement of whether NC adoption correlates with actual commercial exploitation prevention vs signaling of openness while retaining commercial control',
    'If genuine restriction: CC-NC is a legitimate coordination mechanism for creators with mixed commercial/noncommercial intent. If artificial scarcity: CC-NC is extraction masked as openness — creator gets attribution signaling without actual openness burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_remix_boundary, conceptual, 'Whether NonCommercial clause represents genuine restriction or artificial openness signaling').

omega_variable(
    legal_system_convergence,
    'Has CC''s existence and adoption actually converged copyright law across jurisdictions toward standardized permission models, or does CC operate as a parallel system that leaves underlying copyright fragmentation intact?',
    'Comparative law analysis of copyright statutes pre- and post-CC; measurement of whether CC adoption correlates with legal harmonization; investigation of whether CC''s success in certain jurisdictions depends on idiosyncratic copyright statutes that CC cannot change',
    'If convergence: CC is pure coordination solving genuine legal heterogeneity problem. If parallel system: CC creates illusion of solved coordination while underlying legal fragmentation persists — potential Snare from perspective of downstream users who must navigate both CC and local copyright law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_system_convergence, empirical, 'Degree to which CC promotes actual legal convergence vs operates as parallel system').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creative_commons_licensing, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cc_lic_tr_t0, creative_commons_licensing, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cc_lic_tr_t8, creative_commons_licensing, theater_ratio, 8, 0.48).
narrative_ontology:measurement(cc_lic_tr_t16, creative_commons_licensing, theater_ratio, 16, 0.55).

% Extraction over time
narrative_ontology:measurement(cc_lic_be_t0, creative_commons_licensing, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cc_lic_be_t8, creative_commons_licensing, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(cc_lic_be_t16, creative_commons_licensing, base_extractiveness, 16, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creative_commons_licensing, information_standard).
narrative_ontology:affects_constraint(creative_commons_licensing, open_source_software_licensing).
narrative_ontology:affects_constraint(creative_commons_licensing, digital_copyright_law_harmonization).
narrative_ontology:affects_constraint(creative_commons_licensing, platform_content_moderation).

% DUAL FORMULATION NOTE:
% CC licensing is downstream of digital copyright law fragmentation (affects_constraints in reverse) and upstream of platform content policy implementation. The three-constraint family: (1) digital_copyright_law_harmonization (ε≈0.45, Tangled Rope) — legal heterogeneity creates extraction for creators and platforms; (2) creative_commons_licensing (ε=0.28, Rope) — CC coordinates around legal heterogeneity without solving it; (3) platform_content_moderation (ε≈0.52, Snare) — platforms extract from creators through selective CC enforcement and attribution mediation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(creative_commons_licensing, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
