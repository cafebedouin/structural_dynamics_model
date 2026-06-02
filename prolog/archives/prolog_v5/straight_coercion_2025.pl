% ============================================================================
% CONSTRAINT STORY: straight_coercion_2025
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_straight_coercion_2025, []).

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
 *   constraint_id: straight_coercion_2025
 *   human_readable: Normalized Taiwan Strait Military Coercion
 *   domain: political/military
 *
 * SUMMARY:
 *   By 2025, Chinese military activity around Taiwan has undergone a
 *   structural shift from episodic signaling (large-scale exercises timed to
 *   political events) to normalized training cycles (regular, predictable
 *   operations integrated into readiness schedules). This normalization
 *   appears as a coordination mechanism from Beijing's
 *   perspective—regularizing activity enables military planning, personnel
 *   rotation, and force development. But from Taiwan's perspective, it is
 *   pure coercion: civilian shipping, commerce, and operational freedom are
 *   permanently constrained. From the U.S. perspective, it is mixed:
 *   predictability enables deterrence planning (coordination benefit), but
 *   sustained commitment to Taiwan deterrence extracts costs. The
 *   normalization is NOT a de-escalation—it is a restructuring of coercion
 *   toward lower-visibility, harder-to-counter patterns. Theater ratio is
 *   elevated because much announced activity is performative readiness
 *   display, but the underlying extraction (through suppressed civilian
 *   access and elevated alert costs) is genuine. The constraint exhibits all
 *   six DR types depending on observer position, making it a diagnostic case
 *   for how geopolitical coercion appears differently across structural
 *   positions.
 *
 * KEY AGENTS:
 *   - Beijing Military Command: Primary beneficiary (institutional/arbitrage) — gains from normalized readiness cycles that decouple from external signaling; retains escalation optionality
 *   - Taiwan Civilian Economy: Primary victim (powerless/trapped) — bears full suppression costs of coercion; no exit from geographic constraint; no negotiation mechanism
 *   - Taiwan Government/Military: Secondary victim (powerful/constrained) — must maintain permanent readiness posture; benefits from external deterrence support but extraction is asymmetric
 *   - United States Strategic Command: Beneficiary with constraints (organized/constrained) — gains coordination benefit from predictable PRC activity, but must absorb costs of sustained deterrence commitment
 *   - International Shipping/Insurance Regime: Institutional observer (institutional/arbitrage) — nominally governs strait access but functionally degraded (piton); persists through inertia
 *   - Regional De-escalation Channels: Temporary coordination structure (organized/mobile) — solve immediate crisis communication problems but have sunset clause tied to political settlement
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks framing contingent institutional arrangements as inherent geopolitical laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(straight_coercion_2025, 0.58).
domain_priors:suppression_score(straight_coercion_2025, 0.72).
domain_priors:theater_ratio(straight_coercion_2025, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(straight_coercion_2025, extractiveness, 0.58).
narrative_ontology:constraint_metric(straight_coercion_2025, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(straight_coercion_2025, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(straight_coercion_2025, tangled_rope).
narrative_ontology:human_readable(straight_coercion_2025, "Normalized Taiwan Strait Military Coercion").
narrative_ontology:topic_domain(straight_coercion_2025, "political/military").

domain_priors:requires_active_enforcement(straight_coercion_2025).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(straight_coercion_2025, beijing_military_readiness).
narrative_ontology:constraint_beneficiary(straight_coercion_2025, prc_strategic_deterrence).
narrative_ontology:constraint_victim(straight_coercion_2025, taiwan_civilian_economic_activity).
narrative_ontology:constraint_victim(straight_coercion_2025, taiwan_operational_freedom).
narrative_ontology:constraint_victim(straight_coercion_2025, taiwan_strategic_autonomy).
narrative_ontology:constraint_victim(straight_coercion_2025, us_alliance_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAIWAN CIVILIAN SECTOR (SNARE) — Shipping, commerce, and civilian transportation cannot exit the strait; coercion is constant and unilateral. No negotiation mechanism. Suppression is maximal: alternative routes add 20-40% to transit time/cost. Extraction runs continuously through operational constraints and insurance premiums. Theater is moderate (military exercises are announced but unpredictable in execution). The civilian sector experiences pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(straight_coercion_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TAIWAN GOVERNMENT/MILITARY (TANGLED ROPE) — Taiwan's military must maintain readiness posture (coordination function) against the normalized threat, benefiting from external military support and alliance frameworks. But the coercion extraction is asymmetric: Taiwan absorbs all costs of permanent alert status while benefits (deterrence, alliance commitment) remain contingent and uncertain. Active enforcement required: Taiwan must continuously demonstrate military capability to make the coercive threat credible to external audiences. Constrained exit: abandoning readiness increases vulnerability; accepting permanent coercion as baseline is the structural trap.
constraint_indexing:constraint_classification(straight_coercion_2025, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BEIJING STRATEGIC COMMAND (ROPE) — From the PRC military's perspective, normalized activity is coordination: the decoupling from external signaling to internal training cycles solves a bureaucratic problem (how to maintain readiness without appearing reactive to external events). Theater is high (much announced activity is performative readiness display), but the coordination function is genuine — internal readiness cycles require regular exercises, and normalizing them to a fixed schedule enables force development and personnel rotation. Arbitrage exit: the PRC can modulate activity level without signaling weakness or retreat.
constraint_indexing:constraint_classification(straight_coercion_2025, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UNITED STATES STRATEGIC ALLIANCE (TANGLED ROPE) — The U.S. gains from the normalization of PRC activity because unpredictable, signal-responsive coercion threatens crisis escalation; normalized cycles are more manageable through intelligence and coordination. This is a coordination benefit (predictability enables deterrence planning). But extraction is asymmetric: the U.S. must maintain forward presence and commitment to Taiwan deterrence credibly, absorbing costs of military positioning and alliance management. Active enforcement required: the U.S. must respond to normalization with credible counter-presence (carrier operations, arms sales, FONOPs) to sustain the coordination. Constrained exit: withdrawing presence would collapse the alliance credibility and trigger the coercion to become more aggressive.
constraint_indexing:constraint_classification(straight_coercion_2025, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL SHIPPING & INSURANCE REGIME (PITON) — Formal international law (UNCLOS freedom of navigation, freedom of commerce) nominally governs strait passage. But the normalization of PRC coercion has rendered these rules performative: insurers price in Taiwan Strait risk, carriers accept delays, and no legal remedy is available. The regime persists through institutional inertia (it structures expectations and liability allocation) but has decayed in functional capacity. Theater ratio is high: freedom-of-navigation operations (FONOPs) by external powers are largely performative affirmations of nominal rules that are not enforced. The institutional regime is maintained because replacing it would require explicit renegotiation, which no power wants to initiate.
constraint_indexing:constraint_classification(straight_coercion_2025, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL DE-ESCALATION COORDINATION (SCAFFOLD) — Informal communication channels (military hotlines, diplomatic back-channels, Track 2 dialogues) between PRC and Taiwan/U.S. exist to manage the normalized activity. These mechanisms solve coordination problems: they reduce misperception, enable crisis communication, and establish shared understandings of 'normal' activity vs escalation. Sunset logic applies: if political settlement eventually occurs (via negotiation or economic integration), these de-escalation structures may transition to full diplomatic normalization, rendering the military coercion obsolete. Theater is moderate to high (much public diplomatic activity is performative positioning), but the underlying coordination function (crisis communication) is genuine. Mobile exit: players can walk away from the de-escalation coordination if political incentives change, making this a temporary support structure.
constraint_indexing:constraint_classification(straight_coercion_2025, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER - NATURAL LAW FRAMING (MOUNTAIN) — A civilizational-level analytical observer might frame this as an immutable constraint: geographic proximity and military capability asymmetry create structural coercion that is inherent to the regional balance. Coercion appears as a natural law of geopolitics. However, the base properties contradict this: suppression (0.72) and extractiveness (0.58) are high but not total; beneficiaries and victims are clearly identifiable; enforcement is active and ongoing. This is NOT a mountain. The false natural law framing risks obscuring the contingency of the institutional arrangements (PRC readiness cycles, U.S. deterrence commitment, Taiwan's strategic autonomy assumptions) that create the coercion. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(straight_coercion_2025, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(straight_coercion_2025_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(straight_coercion_2025, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(straight_coercion_2025, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(straight_coercion_2025, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(straight_coercion_2025, TR),
    TR >= 0.70.

:- end_tests(straight_coercion_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The PRC's normalization strategy extracts concrete benefits (permanent coercive optionality, strategic initiative, degradation of Taiwan's autonomy margin) while distributing costs asymmetrically onto Taiwan and U.S. allies. Extractiveness increased from 0.35 to 0.58 over the interval as the activity pattern matured from episodic signaling to embedded operational cycles. The value reflects that the coercion is sustained and structural, not temporary or negotiable. Suppression (0.72): Taiwan's civilian sector experiences maximal suppression—alternative routes add 20-40% to transit costs/time, insurance premiums reflect war-risk, and operations are unpredictable enough to prevent perfect avoidance. But suppression is not total (some traffic continues, some adaptation occurs), and it remains below the threshold that forces economic collapse. Theater ratio (0.65): A significant portion of announced PRC activity is performative readiness display (exercises are announced, unit rotations are timed to visibility schedules), but the underlying coercive effect is genuine. Theater increased from 0.50 as normalization made activity more predictable (higher performative content) even as extractiveness grew. Theater is not as high as institutional regimes (piton threshold ~0.70) because the extraction is real, not just theatrical.
 *
 * PERSPECTIVAL GAP:
 *   Beijing sees normalization as solving an institutional coordination problem (decoupling readiness from politics enables better military planning). Taiwan sees permanent coercion with no exit mechanism (snare). The U.S. sees both coordination (predictability aids deterrence) and extraction (sustained commitment costs). Civilians see pure extraction (snare). De-escalation channels see temporary structures with sunset logic (scaffold). The international legal regime sees its own degradation (piton—UNCLOS rules nominally apply but are performatively observed). The civilizational analytical observer risks seeing immutable geopolitical law (mountain), which would naturalize what is actually a reversible institutional arrangement. The perspectival gap reveals that the same structural phenomenon—normalized military activity—is experienced as coordination, coercion, temporary support, institutional decay, and contingent extraction depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Beijing (beneficiary + arbitrage options) derives low d (~0.10), yielding negative or near-zero effective extraction—they experience the constraint as beneficial coordination. Taiwan government (victim + constrained exit) derives high d (~0.80), yielding high effective extraction. Taiwan civilians (victims + trapped) derive maximum d (~0.95), yielding maximum effective extraction. The U.S. (mixed beneficiary/victim + constrained exit) derives moderate d (~0.50), yielding mixed extraction/benefit. De-escalation channels (organized + mobile) derive low-to-moderate d (~0.35), enabling the scaffold classification. The international regime (institutional observer + arbitrage in principle but constrained in practice) derives moderate d (~0.55), producing the piton classification through the theater gate rather than high extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT IDENTITY: This constraint resolves the mandatrophy by demonstrating that 'normalized military coercion' is inherently a mixed coordination-extraction mechanism, not pure extraction (snare) disguised as coordination (rope). The normalization from Beijing's perspective genuinely solves institutional coordination problems (readiness scheduling, personnel rotation, force development). But from Taiwan's perspective, normalization is a worse extraction mechanism than episodic signaling—it makes coercion permanent, invisible to international law (because activity is 'normal' rather than 'threatening'), and harder to counter through diplomatic escalation. The constraint cannot be classified as pure rope (that would erase the asymmetric extraction from Taiwan's perspective) or pure snare (that would erase the genuine coordination benefit for Beijing). Tangled rope is the accurate structural classification: genuine coordination function coupled with asymmetric extraction, requiring active enforcement (Taiwan's military response, U.S. deterrence commitment, de-escalation channels all actively maintained). The mandatrophy dissolves when we recognize that normalization is PRECISELY the mechanism by which coordination and extraction become entangled—routine activity masks the coercive reality, and the coercive reality enables the coordination claim. The constraint exemplifies how mixed institutions work: they are stable precisely because they provide real benefits to some parties (coordination for Beijing, deterrence predictability for U.S.) while extracting from others (Taiwan's autonomy, civilians' freedom of movement). Dissolving this mixture would require political settlement or equilibrium shift, not just institutional reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normalization_irreversibility,
    'Is the decoupling of PRC military activity from external signaling truly structural (irreversible normalization) or tactical (reversible to signal-based activity if political context changes)?',
    'Long-term analysis of PRC military activity patterns; correlation between internal readiness cycles and external political events over 10+ years; institutional analysis of PRC Central Military Commission scheduling vs political signaling directives',
    'If irreversible: the constraint is structurally locked-in (tangled_rope/snare from most perspectives). If reversible: the normalization is contingent on continued political-military separation, and collapse of that separation could revert the coercion to more volatile signaling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normalization_irreversibility, empirical, 'Whether PRC military normalization is structurally irreversible').

omega_variable(
    civilian_adaptation_ceiling,
    'Can Taiwan''s civilian economy adapt indefinitely to normalized coercion (rerouting, insurance, logistics buffer-stocking), or does suppression eventually force economic restructuring that crosses from constraint to extraction?',
    'Time-series analysis of Taiwan shipping costs, insurance premiums, and supply-chain rerouting; comparison to baseline pre-coercion levels; identification of threshold beyond which adaptation becomes economically unsustainable',
    'If adaptation ceiling is high: coercion remains manageable snare (suppression high but not economically terminal). If low: coercion crosses into structural economic extraction that forces political negotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_adaptation_ceiling, empirical, 'Whether Taiwan''s civilian economy can adapt indefinitely to coercion').

omega_variable(
    us_commitment_credibility,
    'Does normalized PRC activity degrade U.S. deterrence credibility by creating ambiguity about whether U.S. will respond to coercion that is now ''normal'' rather than ''escalatory''?',
    'Analysis of U.S. force-posture responses to normalized activity; correlation between activity intensity and U.S. counter-presence; Taiwan leadership assessments of alliance credibility over time',
    'If credibility degrades: U.S. perspective shifts from tangled_rope (coordination + extraction) toward scaffold or even rope (reduced extraction). Taiwan perspective shifts from tangled_rope toward snare (reduced alliance benefit). If credibility holds: perspectives remain stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(us_commitment_credibility, empirical, 'Whether normalization undermines U.S. deterrence credibility').

omega_variable(
    extraction_intent_ambiguity,
    'Is PRC normalization driven by genuine readiness optimization (coordination framing) or by strategic preference for permanent low-level coercion that is harder to escalate from (extraction framing)?',
    'Analysis of PRC military documents and planning statements; correlation between ''normalized'' activity and strategic objectives; examination of whether activity levels exceed what genuine readiness training would require',
    'If coordination-driven: PRC perspective is genuinely rope/tangled_rope. If extraction-driven: PRC perspective is rope with performative coordination cover (piton). Affects classification of beneficiary extraction asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_intent_ambiguity, empirical, 'Whether normalization is readiness optimization or strategic extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(straight_coercion_2025, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(strait_tr_t0, straight_coercion_2025, theater_ratio, 0, 0.5).
narrative_ontology:measurement(strait_tr_t3, straight_coercion_2025, theater_ratio, 3, 0.58).
narrative_ontology:measurement(strait_tr_t6, straight_coercion_2025, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(strait_be_t0, straight_coercion_2025, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(strait_be_t3, straight_coercion_2025, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(strait_be_t6, straight_coercion_2025, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(straight_coercion_2025, enforcement_mechanism).
narrative_ontology:affects_constraint(straight_coercion_2025, taiwan_us_alliance_stability).
narrative_ontology:affects_constraint(straight_coercion_2025, strait_shipping_insurance_regime).
narrative_ontology:affects_constraint(straight_coercion_2025, beijing_military_modernization).

% DUAL FORMULATION NOTE:
% Taiwan Strait coercion decomposes into three structurally distinct claims: (1) the extractiveness of the coercion on civilians/commerce (ε~0.58, this story), (2) the sustainability of U.S. deterrence commitment under normalized activity (ε~0.42), and (3) the international legal regime's capacity to adjudicate freedom of navigation (ε~0.68). These are related but distinct constraints with different failure modes. This story focuses on the institutional structure of normalized coercion itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(straight_coercion_2025, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
