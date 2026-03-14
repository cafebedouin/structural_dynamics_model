% ============================================================================
% CONSTRAINT STORY: cultural_labor_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_labor_extraction, []).

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
 *   constraint_id: cultural_labor_extraction
 *   human_readable: Cultural Labor Extraction: Unpaid Creative Work as Social Obligation
 *   domain: cultural_studies/labor_economics/artistic_production
 *
 * SUMMARY:
 *   Cultural labor extraction operates as a large-scale snare disguised as
 *   opportunity through identity fusion and romantic narratives about
 *   artistic sacrifice. The constraint extracts creative work from powerless
 *   cultural workers by leveraging their identity commitment to creative
 *   practice, platform gatekeeping by cultural institutions, and norms
 *   inherited from aristocratic patronage systems. Extractiveness has
 *   increased as digital platforms have scaled the reach of unpaid content
 *   while concentrating distribution power. Theater ratio reflects the
 *   performative dimension of 'exposure,' 'networking,' and 'artistic merit
 *   discovery' narratives that justify non-payment. The constraint operates
 *   differently depending on the observer's structural position: powerless
 *   creators experience total extraction and identity lock; established
 *   creators remain constrained by norms they helped naturalize; platforms
 *   and gatekeepers experience efficient resource allocation (rope);
 *   traditional fine art aesthetics see degraded rituals (piton); organized
 *   movements see temporary coordination failures with sunset pathways
 *   (scaffold). The analytical observer risks naturalizing this as inherent
 *   to creative work rather than recognizing it as an institutional
 *   arrangement that serves concentrated beneficiaries.
 *
 * KEY AGENTS:
 *   - Emerging Artists: Primary victims (powerless/trapped) — face structural pressure to produce unpaid portfolio work as condition for paid opportunities. No negotiating power.
 *   - Passionate Creators: Primary victims (powerless/identity_locked) — structurally mobile but identity-fused with creative production. Cannot perceive exit because artistic identity would require abandonment.
 *   - Established Creators: Secondary agents (moderate/constrained) — have exited pure extraction but remain constrained by norms. Often perpetuate unpaid-work expectations through mentorship and modeling.
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture free content, distribution algorithms, and advertising revenue. View unpaid submissions as efficient market clearing.
 *   - Cultural Gatekeepers: Primary beneficiaries (institutional/arbitrage) — museums, galleries, festivals, journals. Benefit from free curatorial submissions and talent screening. Experience extraction as legitimate coordination.
 *   - Fine Art Tradition: Institutional actor (institutional/arbitrage) — vestiges of patronage systems maintained through romantic myth (starving artist, art for art's sake). Performs tradition but supports extraction.
 *   - Cooperative Arts Movement: Organized agents (organized/mobile) — artist unions, fair-pay advocates, commons-based licensing. Building alternative pathways with sunset logic.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent arrangements as universal truths about creative work.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_labor_extraction, 0.68).
domain_priors:suppression_score(cultural_labor_extraction, 0.72).
domain_priors:theater_ratio(cultural_labor_extraction, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_labor_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(cultural_labor_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(cultural_labor_extraction, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_labor_extraction, snare).
narrative_ontology:human_readable(cultural_labor_extraction, "Cultural Labor Extraction: Unpaid Creative Work as Social Obligation").
narrative_ontology:topic_domain(cultural_labor_extraction, "cultural_studies/labor_economics/artistic_production").

domain_priors:requires_active_enforcement(cultural_labor_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_labor_extraction, cultural_gatekeepers).
narrative_ontology:constraint_beneficiary(cultural_labor_extraction, entertainment_platforms).
narrative_ontology:constraint_beneficiary(cultural_labor_extraction, commercial_media_entities).
narrative_ontology:constraint_victim(cultural_labor_extraction, cultural_workers).
narrative_ontology:constraint_victim(cultural_labor_extraction, emerging_artists).
narrative_ontology:constraint_victim(cultural_labor_extraction, creative_precariat).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING ARTIST (SNARE) — Faces total extraction of creative labor through exposure, networking promises, and identity fusion with artistic practice. No structural path to paid work without first producing unpaid portfolio. Career advancement explicitly depends on willingness to work without compensation. Exit would require abandoning artistic identity entirely.
constraint_indexing:constraint_classification(cultural_labor_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PASSIONATE CREATOR (SNARE) — Identity is constituted through creative production. The creator cannot perceive exit because their self-concept depends on being 'the kind of person who creates.' Exploitation is experienced as calling, not extraction. Structural mobility exists (economic alternatives available) but identity fusion makes exit literally unthinkable from within the creative worldview.
constraint_indexing:constraint_classification(cultural_labor_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: ESTABLISHED CREATOR (TANGLED ROPE) — Has exited the pure extraction phase but remains constrained by norms established during precarity. Benefits from the ecosystem's coordination function (community, collaboration, skill-sharing) but also perpetuates extraction expectations by modeling unpaid work as normalcy. Moderate power through reputation but constrained by cultural norms they helped naturalize.
constraint_indexing:constraint_classification(cultural_labor_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Views unpaid content submission as efficient coordination: creators self-select by passion level, platform captures curation and distribution value. Net beneficiary through access to free creative labor, algorithm-driven promotion, and advertising revenue. Experiences the constraint as solving a resource allocation problem (matching creators to audiences). Zero-cost content curation is the platform's core arbitrage advantage.
constraint_indexing:constraint_classification(cultural_labor_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CULTURAL GATEKEEPER (ROPE) — Museum director, gallery curator, festival organizer. Benefits from free submissions and creative labor as festival programming, exhibition curation, and curatorial narrative. Experiences unpaid artist participation as essential coordination: 'How else would we discover talent?' Gatekeepers rarely see themselves as extractive because they perform curation labor and face resource constraints. They coordinate access to prestige, but capture asymmetric value.
constraint_indexing:constraint_classification(cultural_labor_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: FINE ART TRADITION (PITON) — 'Art for art's sake' and 'the struggling artist' are vestiges of aristocratic patronage systems where wealthy individuals subsidized creative production. These norms persist as institutional theater (the starving artist myth, the MFA credential cost, unpaid apprenticeship) long after their structural function has been degraded by digital reproduction and platform economics. The tradition maintains itself through romantic aesthetics, not through actual economic support.
constraint_indexing:constraint_classification(cultural_labor_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: COOPERATIVE ARTS MOVEMENT (SCAFFOLD) — Artist cooperatives, union organizing, fair-pay advocacy, and commons-based creative licensing (Creative Commons, open-source aesthetics) represent organized attempts to build sunset pathways. These movements see unpaid cultural labor as a temporary coordination failure resolvable through norms change and structural alternatives. Extraction rates decline as cooperative mechanisms mature and shift cultural expectations about compensation.
constraint_indexing:constraint_classification(cultural_labor_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing cultural labor extraction as inherent to creative expression: 'artists have always sacrificed for their art,' 'passion is its own reward,' 'the market for art cannot support all creators.' This framing treats contingent institutional arrangements (patronage systems, credentialing requirements, platform economics) as laws of nature. The engine's false summit detector will identify this as naturalization requiring decomposition.
constraint_indexing:constraint_classification(cultural_labor_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_labor_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cultural_labor_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cultural_labor_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_labor_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cultural_labor_extraction, TR),
    TR >= 0.70.

:- end_tests(cultural_labor_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The primary research group captures career and reputational benefits while workers produce without compensation. The extraction is severe because it is non-negotiable for career entry and has expanded as platforms have scaled. The trajectory shows escalating extractiveness over 45 years as digital platforms have multiplied unpaid content opportunities while concentrating distribution power. Suppression (0.72): High. Significant barriers include: platform gatekeeping, credential requirements (MFA debt, unpaid internships), lack of alternative distribution pathways, and identity fusion that makes exit psychologically impossible. Suppression has grown as alternatives have been absorbed into the platform economy. Theater ratio (0.68): High. 'Exposure,' 'building your portfolio,' 'paying dues,' and 'artistic merit discovery' are performative narratives justifying non-payment. The performative content has increased as digital platforms scale — the narrative work of justification must overcome the visibility that one person's unpaid work is another's advertising revenue. The constraint exhibits trajectory consistent with Goodhart decay: as the real coordination function (discovering talent, building community) has been captured by platforms, the theater required to justify non-payment has increased.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the emerging artist's snare (total extraction, identity lock, no exit) and the platform operator's rope (efficient market clearing, net benefit, genuine coordination) is not resolvable by splitting perspectives more finely. The structural fact is that the same mechanism produces asymmetric value flows. The emerging artist's labor generates content and engagement data; the platform captures algorithmic ordering, advertising revenue, and discovery leverage. The artist experiences barrier to exit (suppression); the platform experiences barrier to alternative models (data lock-in is mutual but platform-favorable). The gap is diagnostic: it reveals that this is not a pure coordination constraint (which would see uniform rope across perspectives) and not a uniform-type mountain. It is a snare, and the rope perspective is the snare operator's experience of their own mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position: powerless trapped creators (d ≈ 0.95) experience maximum effective extraction; powerless identity-locked creators (d ≈ 0.89) experience high extraction but with a perceptual filter that prevents recognizing it; institutional beneficiaries with arbitrage options (d ≈ 0.05) experience negative effective extraction because the constraint subsidizes their value capture; organized agents with mobile options (d ≈ 0.55) experience moderate extraction because they have negotiating power but remain constrained by norms. The pipeline computes chi from these d values and the sigmoid f(d), producing experienced extractiveness that varies by position. Beneficiaries with arbitrage genuinely see this as coordination (rope); trapped creators genuinely experience it as extraction (snare). Both are accurate from their positions — the framework's job is to render both visible simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED BY STRUCTURAL DECOMPOSITION: Cultural labor extraction resolves mandatrophy by showing that extractiveness (0.68) and suppression (0.72) are not sufficient to confuse this with a coordination problem. The snare gate requires victims, and the victims are clearly identified (emerging artists, precariat). The beneficiaries (platforms, gatekeepers) profit from non-payment. The coordination function that might justify some extraction (talent discovery, community building, skill-sharing) exists but has been captured by extractive layers. If we decomposed this into separate stories — one for the genuine coordination (matching creators to audiences, skill-building) and one for the extraction overlay (unpaid labor as cost savings, platform data capture, gatekeeper prestige capture) — we would likely find that the coordination story has ε ≈ 0.20 (tangled rope with low extraction), while the extraction story has ε ≈ 0.75 (pure snare). The current story conflates both and maintains a single ε appropriate for the snare classification. The piton perspective (fine art tradition) shows the theater mechanism, confirming that historical patronage norms are being performatively maintained despite changed economic conditions. This validates the snare classification: the constraint's existence depends on suppressing awareness that alternatives are available.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    passion_vs_coercion_boundary,
    'Where is the boundary between genuine passionate creative practice and coerced labor masked as passion through identity fusion?',
    'Longitudinal study of creator mental health, burnout rates, and post-exit identity reconstruction. Comparison of creators who exit creative fields vs those who maintain identity-locked commitment despite economic hardship.',
    'If boundary is permeable/temporal: many identity-locked agents are structurally coerced but cannot perceive it. Suppression metric may be understated. If boundary is firm: passion is genuinely autonomous and extraction is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passion_vs_coercion_boundary, empirical, 'Boundary between authentic passion and coerced identity-locked commitment').

omega_variable(
    platform_necessity_threshold,
    'What percentage of cultural distribution must be platform-mediated before the platform''s free-labor model becomes non-negotiable for artist career viability?',
    'Historical analysis of artist career paths before/after platform dominance. Measurement of alternative distribution pathways and their economic sustainability. Exit cost analysis for artists refusing unpaid platform participation.',
    'If threshold < 30%: alternatives exist and exit is realistically mobile. If threshold > 70%: platform participation becomes structurally trapped (not identity-locked, but trapped by distribution necessity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_necessity_threshold, empirical, 'Platform dominance threshold for structural necessity of unpaid participation').

omega_variable(
    identity_fusion_cultural_specificity,
    'Is identity fusion with creative work universal across cultures or specific to capitalist professional environments where creative work is commodified?',
    'Cross-cultural ethnographic comparison of creative practice in gift economies, community-based cultures, and subsistence societies vs commercial creative markets. Study of creative satisfaction and identity integration across economic systems.',
    'If universal: identity fusion is intrinsic to creative practice (constrains rather than eliminates this story). If specific to capitalism: identity_locked exit classification reflects systemic enforcement, not intrinsic psychology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_cultural_specificity, conceptual, 'Whether identity fusion with creative work is culturally universal or capitalism-specific').

omega_variable(
    extraction_vs_coordination_ratio_instability,
    'Does the coordination function (community, skill-sharing, discovery of talent) genuinely require unpaid labor or is the unpaid requirement a separate extractive overlay on what could be a low-cost coordination mechanism?',
    'Design experimental alternatives: paid micro-commissions for discovery work, revenue-sharing platforms, artist-owned cooperative infrastructure. Measurement of coordination function quality with vs without extraction.',
    'If coordination requires unpaid work: classify as Tangled Rope with high base coordination value. If coordination can function with payment: current arrangement is pure Snare, and alternatives (cooperatives) are genuine pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_ratio_instability, empirical, 'Whether unpaid labor is structurally necessary for cultural coordination').

omega_variable(
    suppression_internalization_depth,
    'How much of the measured suppression (0.72) reflects external barriers (platform gatekeeping, credential requirements, capital access) vs internalized barriers (belief that suffering proves dedication, that compensation would diminish authenticity)?',
    'Qualitative research on artist decision-making: what barriers do artists report when explaining unpaid work? Post-exit interviews with creators who have left creative fields: which barriers were material vs internalized? Measurement of suppression persistence after removing external barriers.',
    'If primarily external: suppression metric is accurate and targeting external barriers (platform reform, credential requirements) would reduce extraction. If primarily internalized: suppression follows the agent after exit and requires identity reconstruction support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_depth, empirical, 'Proportion of suppression that is structural vs internalized/psychological').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_labor_extraction, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cullab_tr_t0, cultural_labor_extraction, theater_ratio, 0, 0.52).
narrative_ontology:measurement(cullab_tr_t15, cultural_labor_extraction, theater_ratio, 15, 0.61).
narrative_ontology:measurement(cullab_tr_t30, cultural_labor_extraction, theater_ratio, 30, 0.68).
narrative_ontology:measurement(cullab_tr_t45, cultural_labor_extraction, theater_ratio, 45, 0.74).

% Extraction over time
narrative_ontology:measurement(cullab_be_t0, cultural_labor_extraction, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cullab_be_t15, cultural_labor_extraction, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(cullab_be_t30, cultural_labor_extraction, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cullab_be_t45, cultural_labor_extraction, base_extractiveness, 45, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_labor_extraction, identity_coordination).
narrative_ontology:affects_constraint(cultural_labor_extraction, attention_economy_extraction).
narrative_ontology:affects_constraint(cultural_labor_extraction, credentialing_labor_gatekeeping).
narrative_ontology:affects_constraint(cultural_labor_extraction, platform_algorithmic_curation).

% DUAL FORMULATION NOTE:
% Cultural labor extraction is downstream of platform economics and credentialing systems. Decomposition into separate constraint stories would distinguish: (1) genuine cultural coordination (talent discovery, skill-sharing communities) with low extractiveness; (2) platform value capture (free content, algorithmic ordering, data leverage) with high extractiveness; (3) credential requirements (MFA debt, unpaid internship gatekeeping) with separate extractiveness profile. These are linked via network.affects_constraints because platform dominance makes credential-based gatekeeping non-negotiable, and unpaid labor is justified through cultural identity narratives. Current story maintains unified ε appropriate for snare classification; decomposed stories would show distinct mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_labor_extraction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
