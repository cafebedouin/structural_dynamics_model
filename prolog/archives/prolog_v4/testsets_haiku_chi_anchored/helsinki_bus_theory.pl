% ============================================================================
% CONSTRAINT STORY: helsinki_bus_theory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_helsinki_bus_theory, []).

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
 *   constraint_id: helsinki_bus_theory
 *   human_readable: The Helsinki Bus Station Theory (Creative Persistence)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The Helsinki Bus Station Theory, articulated by artist Kees Molenaar,
 *   posits that emerging creative workers must pass through an extended
 *   period of imitation and stylistic convergence before they can develop an
 *   authentic or original voice. This constraint operates as a gating
 *   mechanism in creative careers across visual arts, music, literature, and
 *   design. The theory naturalizes what is structurally a snare: emerging
 *   creatives are trapped in a phase where they must visibly respect
 *   established aesthetic traditions, market their work by proximity to
 *   recognizable styles, and defer originality to avoid being labeled
 *   derivative. The constraint is enforced not by law but by audience taste,
 *   market access (galleries, publishers, record labels), educational
 *   gatekeeping (art schools), and cultural authority (critics, curators).
 *   Suppression is high because alternative pathways — building audience
 *   directly, learning through self-teaching, working in unpedigried contexts
 *   — carry social and economic penalties. The extractiveness has increased
 *   over the interval as digital technologies created alternative pathways
 *   (reducing objective necessity for the constraint) while cultural
 *   gatekeepers responded by intensifying the policing of 'authentic
 *   development' and 'paying dues.' Theater has risen as the performative
 *   aspect has been amplified: art schools now explicitly frame imitation as
 *   a legitimizing ritual, and contemporary discourse celebrates 'mastering
 *   the rules before breaking them' as both a practical skill and a moral
 *   virtue.
 *
 * KEY AGENTS:
 *   - Emerging Creatives: Primary victim (powerless/trapped) — must navigate the imitation phase with few alternatives, facing decades-long career delay and market pressure to remain stylistically derivative
 *   - Established Artists: Primary beneficiary (institutional/arbitrage) — capture validation, influence, and audience loyalty through the constraint; their work becomes the reference frame; the constraint reifies their status as canonical
 *   - Cultural Gatekeepers (Galleries, Publishers, Labels, Critics): Primary beneficiary (institutional/arbitrage) — control resource access through the imitation-phase filter; extract rent through gatekeeping, credentialing, and taste-making
 *   - Arts Education Systems: Hybrid enforcer (institutional/constrained) — actively teach imitation as pedagogy, but also experience resource constraints and legitimacy pressure; education systems would need to restructure significantly to change
 *   - Digital Creator Networks (YouTube, TikTok, Patreon, NFTs): Challenger (organized/mobile) — partially escape the constraint by enabling direct audience access; still face algorithmic recommendation favoring recognizable/familiar content
 *   - Art Historical Canon: Institutional structure (institutional/arbitrage) — the written history of art progression (apprenticeship → mastery → innovation) naturalizes the imitation phase and is reproduced in curricula, critical discourse, and institutional practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(helsinki_bus_theory, 0.52).
domain_priors:suppression_score(helsinki_bus_theory, 0.68).
domain_priors:theater_ratio(helsinki_bus_theory, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(helsinki_bus_theory, extractiveness, 0.52).
narrative_ontology:constraint_metric(helsinki_bus_theory, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(helsinki_bus_theory, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(helsinki_bus_theory, snare).
narrative_ontology:human_readable(helsinki_bus_theory, "The Helsinki Bus Station Theory (Creative Persistence)").
narrative_ontology:topic_domain(helsinki_bus_theory, "social/psychological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(helsinki_bus_theory, established_artists).
narrative_ontology:constraint_beneficiary(helsinki_bus_theory, cultural_gatekeepers).
narrative_ontology:constraint_victim(helsinki_bus_theory, emerging_creatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING CREATIVE (SNARE) — Trapped in the imitation phase with no exit route except time and persistent work. Must copy established styles to gain audience access, market feedback, and technical skill. Career risk of being labeled derivative if they diverge too early. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.72.
constraint_indexing:constraint_classification(helsinki_bus_theory, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ESTABLISHED ARTISTS & GATEKEEPERS (ROPE) — Benefit from the constraint as a filter and validation mechanism. Imitation creates a training ground, generates audience familiarity with their work, and establishes a clear hierarchy. Museums, galleries, and critics can point to imitation as evidence of influence and mastery of existing canon. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(helsinki_bus_theory, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ARTS EDUCATION SYSTEMS (TANGLED ROPE) — Actively enforces imitation as pedagogy. Claims coordination function: teaching technique, discipline, historical mastery. But also extracts: credentials gate access to resources, teaching perpetuates the imitation bottleneck as a legitimizing trial, and educational hierarchies suppress alternative learning pathways. d≈0.60, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(helsinki_bus_theory, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE/DIGITAL CREATOR NETWORKS (TANGLED ROPE) — Organized agents (YouTube creators, open-source artists, indie music platforms) partially escape the constraint. Direct audience access via digital means reduces gatekeeping extraction. But the constraint persists: even digital creators must build audience through initial imitation, trend-riding, or remix culture. The network offers mobility and arbitrage, but early phase still requires visible respect for existing work. d≈0.35, f(d)≈0.30, σ=1.1 → χ≈0.18.
constraint_indexing:constraint_classification(helsinki_bus_theory, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ART HISTORICAL CANON (PITON) — The constraint persists through institutional inertia in university curricula, museum exhibition logic, and critical discourse. The 'imitation is learning' narrative is maintained despite digital alternatives that enable direct access to audience feedback without gatekeeping. theater_ratio=0.58: teaching imitation is presented as essential transmission of mastery, but much of the actual value (learning market feedback, building audience) can now happen through direct digital engagement. The canon persists because institutions haven't restructured, not because it's functionally optimal.
constraint_indexing:constraint_classification(helsinki_bus_theory, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a deep time perspective, some imitation phase in creative development may appear inevitable: mastery of any complex domain requires internalization of existing patterns, apprenticeship is a universal feature of skill transmission, and innovation always emerges from existing material. However, the structural data (ε=0.52, suppression=0.68, theater=0.58) contradicts the mountain classification. The constraint is contingent on specific institutional arrangements (credential gatekeeping, gallery/publication bottlenecks, canon authority), not on natural learning architecture. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(helsinki_bus_theory, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(helsinki_bus_theory_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(helsinki_bus_theory, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(helsinki_bus_theory, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(helsinki_bus_theory, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(helsinki_bus_theory, TR),
    TR >= 0.70.

:- end_tests(helsinki_bus_theory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Emerging creatives must delay autonomous work for years (often 5-15 years) while building technical skill and audience familiarity through imitation. This period is economically costly: reduced output of marketable work, lower income relative to skilled labor, and psychological cost of deferred autonomy. The extraction is not total — much of the imitation phase does build genuine skill — but the prolongation is extractive. The increase over the interval (0.32 → 0.52) reflects digital alternatives that make prolonged gatekeeping less necessary but are actively policed by cultural institutions responding to disruption. Suppression (0.68): High. Emerging creatives face multiple barriers to alternative pathways: audience skepticism of unpedigried work, algorithmic systems that favor familiar/recognizable content, institutional credentialing that still dominates traditional creative sectors (visual arts, literary fiction, classical music), and internal psychological resistance (internalized gatekeeping — self-doubt about legitimacy without formal training). Suppression is not total because digital platforms exist and some creators succeed without gatekeeping, but the barriers are substantial. Theater ratio (0.58): Moderate. The constraint has significant performative content: art education explicitly teaches imitation as a ritual of learning mastery, contemporary critical discourse celebrates 'paying dues,' and cultural gatekeepers use the imitation phase as evidence of seriousness and dedication. But the theater is not dominant — there is real skill development in imitation, and some constraints are genuine (complex media do require technical training). The rising theater (0.35 → 0.58 over the interval) reflects institutional amplification of the ritual as digital alternatives erode objective necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a classic victim/beneficiary gap. Emerging creatives see a Snare: trapped in a phase with no exit except time and visible deference to established aesthetics. Established artists and gatekeepers see a Rope: a coordination mechanism that filters low-commitment practitioners, teaches tradition, and validates mastery. Arts education systems see a Tangled Rope: teaching imitation is both pedagogy (genuine coordination) and gatekeeping (extraction through credentialing). Alternative digital networks see a weaker Tangled Rope: they offer mobility but algorithmic recommendation and audience psychology still favor recognizable/imitative content, reproducing a lighter version of the constraint. The art historical canon sees a Piton: the imitation-phase narrative is maintained through institutional inertia (university curricula, museum exhibition logic) despite digital alternatives that obviate much of its necessity. The analytical observer risks seeing a Mountain: imitation as a universal feature of apprenticeship and skill transmission — but the structural data reveals this as a false summit naturalizing what is a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Emerging creatives: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction except for rare cases of market-breakthrough dropouts. Established artists and gatekeepers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries with full exit via arbitrage (they can pivot to new domains, retire, or switch roles). Arts education systems: Victim + constrained (enforcer role) with enforcement function → d≈0.60, f(d)≈0.75. Systems are trapped in the constraint by institutional structure but also benefit from predictable student pathways and credential authority. Alternative digital networks: Organized + mobile → d≈0.35, f(d)≈0.30. They have agency and exit options but are still partially captured by the constraint through network effects and algorithmic recommendation favoring familiar content.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imitation_phase_necessity,
    'Is a prolonged imitation phase a structural requirement for developing creative mastery, or is it primarily a gatekeeping mechanism?',
    'Comparative analysis of creative trajectories pre/post-internet; case studies of self-taught artists vs formally trained; measurement of audience accessibility correlation with training method',
    'If necessary: constraint is closer to rope/scaffold (coordination function real). If primarily gatekeeping: constraint is closer to snare (pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imitation_phase_necessity, empirical, 'Whether imitation phase is structurally necessary or primarily gatekeeping').

omega_variable(
    digital_intermediary_disruption,
    'Do digital platforms (YouTube, TikTok, SoundCloud, Patreon, NFT markets) structurally bypass the imitation-phase trap, or do they reproduce it under different branding?',
    'Longitudinal tracking of emerging creators across digital vs traditional pathways; analysis of success rates and time-to-original-work metrics; study of whether algorithmic recommendation still favors recognizable/imitative content',
    'If truly bypass: constraint is weakening (scaffold perspective validated). If reproduced: digital shows new extraction mechanism replacing gatekeeping (snare persists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_intermediary_disruption, empirical, 'Whether digital platforms bypass or reproduce the imitation-phase constraint').

omega_variable(
    psychological_internalization_threshold,
    'What is the minimum duration/intensity of imitation required for psychological confidence to attempt original work without crippling self-doubt?',
    'Psychological assessment of emerging creators; correlation analysis between imitation duration and confidence in original voice; therapy/mentorship outcome data',
    'If low threshold: suppression metric should be lower; constraint is enforced more by convention than necessity. If high threshold: suppression metric is accurate; the biological/psychological ceiling is real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(psychological_internalization_threshold, conceptual, 'Psychological threshold for confidence in original creative voice').

omega_variable(
    market_filter_function,
    'Does the imitation phase serve an essential market filtering function, culling low-persistence creators before they burn resources on unlikely pursuits?',
    'Economic analysis of creative sector outcome distributions; comparison of financial efficiency (output per capital invested) in gated vs open creative pathways; survival analysis of creators by entry mode',
    'If yes: constraint is collective efficiency (rope aspect valid). If no/minimal: constraint is primarily rent-seeking by established creators (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_filter_function, preference, 'Market filtering function of the imitation phase').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(helsinki_bus_theory, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hbt_tr_t0, helsinki_bus_theory, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hbt_tr_t5, helsinki_bus_theory, theater_ratio, 5, 0.48).
narrative_ontology:measurement(hbt_tr_t10, helsinki_bus_theory, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(hbt_be_t0, helsinki_bus_theory, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(hbt_be_t5, helsinki_bus_theory, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(hbt_be_t10, helsinki_bus_theory, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(helsinki_bus_theory, information_standard).
narrative_ontology:affects_constraint(helsinki_bus_theory, credential_gatekeeping_in_creative_sectors).
narrative_ontology:affects_constraint(helsinki_bus_theory, algorithmic_discovery_bottleneck).

% DUAL FORMULATION NOTE:
% The Helsinki Bus Station Theory operates at the intersection of skill acquisition (genuine apprenticeship) and institutional gatekeeping (credential authority). The constraint family includes upstream claims about the structure of mastery in complex domains and downstream claims about market access and audience discovery. The ε=0.52 reflects the contingent institutional enforcement; upstream constraints about cognitive skill requirements would have lower ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(helsinki_bus_theory, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
