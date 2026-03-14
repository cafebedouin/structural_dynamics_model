% ============================================================================
% CONSTRAINT STORY: quad_alignment_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quad_alignment_asymmetry, []).

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
 *   constraint_id: quad_alignment_asymmetry
 *   human_readable: Quad Alignment Asymmetry in Strategic Coordination
 *   domain: geopolitics/strategic_alignment
 *
 * SUMMARY:
 *   The Quad (United States, Japan, Australia, India) presents a structural
 *   constraint wherein alignment benefits are asymmetrically distributed:
 *   core members (US, Japan) capture hegemonic coordination benefits and
 *   technology first-mover advantage, while peripheral members (India,
 *   Australia) face coerced participation, constrained autonomy, and
 *   subordinate institutional positioning. The constraint exhibits the full
 *   spectrum of Deferential Realism types depending on observer position:
 *   hegemonic rope coordination for the US, genuine mixed coordination for
 *   middle powers, extraction-disguised-as-alignment for trapped peripheral
 *   states, institutional scaffolding for alternative coalitions, and
 *   performative ritual for the quad's own institutional actors. The theater
 *   ratio (0.68) reflects that public statements of alignment often diverge
 *   sharply from private economic and diplomatic behavior—members maintain
 *   rhetorical unity on China containment while pursuing divergent regional
 *   strategies, bilateral trade arrangements, and technology sourcing. The
 *   extractiveness measurement (0.58) captures the asymmetric distribution of
 *   coordination benefits, with core members capturing security and economic
 *   advantages while peripheral members bear costs of constrained autonomy
 *   and exclusion of alternatives. The suppression measurement (0.62)
 *   reflects high barriers to defection: economic sanctions risk, security
 *   guarantee withdrawal, technology embargo, and diplomatic isolation. The
 *   constraint is sustained through a combination of genuine coordination
 *   function (managing shared China concern), material coercion (tangible
 *   costs of exit), and performed unity (theatrical alignment displays that
 *   mask private divergence).
 *
 * KEY AGENTS:
 *   - United States: Primary beneficiary and hegemonic coordinator (institutional/arbitrage) — captures leadership position, technology advantage, security commitments, market access asymmetry
 *   - Japan: Core ally beneficiary (institutional/arbitrage) — gains US security guarantee, technology sharing, preferential market access, regional leadership role
 *   - Australia: Hedging middle power (moderate/constrained) — gains security guarantees and investment but faces economic coercion (Chinese trade restrictions), constrained autonomy in regional diplomacy
 *   - India: Trapped peripheral power (powerless/trapped) — constrained by security vulnerabilities, economic leverage points (Pakistan alliance, energy dependencies), institutional subordination, yet trapped in alignment to counter China and Pakistan security threats
 *   - Peripheral excluded powers (China, Russia, ASEAN neutrals): Victims of alignment's exclusionary function (powerless/trapped) — face containment mechanisms, limited access to quad-controlled technology networks, constrained market opportunities
 *   - Alternative coordination coalitions (BRICS, SCO, ASEAN centrality, regional forums): Organized reform agents (organized/constrained) — building alternative institutional pathways with potential 15-25 year sunset for current alignment structure
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent geopolitical arrangements as immutable balance-of-power laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quad_alignment_asymmetry, 0.58).
domain_priors:suppression_score(quad_alignment_asymmetry, 0.62).
domain_priors:theater_ratio(quad_alignment_asymmetry, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quad_alignment_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(quad_alignment_asymmetry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quad_alignment_asymmetry, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quad_alignment_asymmetry, tangled_rope).
narrative_ontology:human_readable(quad_alignment_asymmetry, "Quad Alignment Asymmetry in Strategic Coordination").
narrative_ontology:topic_domain(quad_alignment_asymmetry, "geopolitics/strategic_alignment").

domain_priors:requires_active_enforcement(quad_alignment_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quad_alignment_asymmetry, hegemonic_coordinator).
narrative_ontology:constraint_beneficiary(quad_alignment_asymmetry, core_alliance_members).
narrative_ontology:constraint_victim(quad_alignment_asymmetry, peripheral_member_states).
narrative_ontology:constraint_victim(quad_alignment_asymmetry, excluded_regional_powers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COERCED PERIPHERAL STATE (SNARE) — Structurally dependent on alignment benefits (market access, security guarantees, technology transfers) but faces severe costs of defection (economic sanctions, diplomatic isolation, security vulnerability). No genuine exit option despite nominal participation. Maximum extraction from powerless agent with no arbitrage.
constraint_indexing:constraint_classification(quad_alignment_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HEDGING MIDDLE POWER (TANGLED ROPE) — Experiences genuine coordination benefits (technology sharing, investment commitments, joint security arrangements) alongside asymmetric extraction (constrained policy autonomy, forced competition with excluded rivals, subordinate institutional position). Can exit at high cost but benefits from alignment in genuine ways. Mixed experience across biographical horizon.
constraint_indexing:constraint_classification(quad_alignment_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CORE ALLIANCE MEMBER (ROPE) — Benefits from preferential market access, technology first-mover advantage, institutional voice, and security coordination. Experiences the constraint as pure coordination—solving collective action problems among aligned powers. Net beneficiary with maximum flexibility and arbitrage options.
constraint_indexing:constraint_classification(quad_alignment_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL REFORM COALITION (SCAFFOLD) — Organized movements (BRICS expansion, regional alternative forums, technology decoupling initiatives) view quad alignment as a temporary constraint that will dissolve as multi-polarity deepens and alternative coordination mechanisms mature. See sunset in 15-25 years as emerging powers build parallel institutions.
constraint_indexing:constraint_classification(quad_alignment_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PERFORMED COLD WAR COALITION (PITON) — The quad's institutional coherence is substantially theatrical: joint statements assert unity on China containment while members privately maintain divergent economic interests, strategic timelines, and threat perceptions. Theater ratio (0.68) reflects that public alignment performance often masks private defection (India's energy purchases, Australia's trade dependencies, Japan's regional diplomacy). The constraint persists through habit and shared anti-China framing rather than functional coordination efficiency.
constraint_indexing:constraint_classification(quad_alignment_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HEGEMONIC COORDINATOR — BENEVOLENT VIEW (TANGLED ROPE) — From the US perspective, the quad is genuine coordination: establishing rules-based order, ensuring freedom of navigation, countering authoritarian expansion. Real benefits to US through coalition leadership, technology dominance, security commitments. But also extracts from peripheral states through leverage asymmetry and enforced exclusion. Extraction is embedded in coordination, not separate from it.
constraint_indexing:constraint_classification(quad_alignment_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: HEGEMONIC COORDINATOR — REALIST VIEW (SNARE) — From structural realist perspective, the quad is extraction mechanism disguised as coordination: maintaining unipolarity through coalition, preventing emergence of peer competitors, extracting subordinate access to markets and security periphery. The coordination function is secondary to power maintenance. Mobile exit (US can redefine alignment) means US experiences rope; peripheral states trapped in snare. Perspectival gap reveals asymmetry.
constraint_indexing:constraint_classification(quad_alignment_asymmetry, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN) — At civilizational scale, strategic alignment among powers with shared interests against rising hegemons appears as natural law: the balance-of-power theorem in action. States naturally coalesce against threats; coalitions naturally extract from non-members. However, the base properties contradict mountain classification: the extractiveness (0.58), suppression (0.62), and theater ratio (0.68) reveal contingent institutional arrangements, not natural laws. This is a false summit.
constraint_indexing:constraint_classification(quad_alignment_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quad_alignment_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quad_alignment_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quad_alignment_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quad_alignment_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quad_alignment_asymmetry, TR),
    TR >= 0.70.

:- end_tests(quad_alignment_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting asymmetric distribution of coordination benefits. Core members (US, Japan) experience extractiveness as negative—they are net beneficiaries receiving coordination services from alignment. Peripheral members (India, Australia) experience extractiveness as positive—they pay compliance costs (constrained autonomy, forced exclusions, sanctions vulnerability) to gain security and economic benefits. The aggregate 0.58 reflects the mixture: genuine coordination function that creates value (rules-based order, counter-hegemonic effects) combined with asymmetric extraction of that value toward hegemonic powers. Suppression (0.62): Moderate-high. Peripheral states face real but not total barriers to exit: economic sanctions mechanisms are credible (China trade restrictions, technology access limitations), security guarantees can be withdrawn (Taiwan security commitment), diplomatic costs are material (isolation from quad-led institutions), but exit remains theoretically possible at high cost. The suppression is not insurmountable but substantial. Theater ratio (0.68): High and increasing. Public quad statements emphasize unity on rules-based order and China containment; private behavior shows significant divergence (India energy purchases from Russia/Iran, Australia bilateral China trade normalization attempts, Japan security treaty flexibility with China). The gap between performance (unified strategic bloc) and reality (coordinated but competing interests) has widened over the measurement interval, indicating piton characteristics—constraint persisting through institutional habit rather than functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates a perspectival split between institutional beneficiaries and trapped peripheral agents. Core alliance members (US, Japan) perceive rope: the constraint solves genuine coordination problems (managing rising hegemony, ensuring freedom of navigation, sustaining technology-sharing network). Institutional perspective from arbitrage position: low or negative experienced extraction. Peripheral states (India, Australia in constrained position; excluded powers in trapped position) perceive snare: same structural constraint appears as coercive mechanism with minimal coordination benefit relative to cost. The gap between these perspectives is not a measurement error—it reveals the constraint's asymmetric directionality. The beneficiary sees coordination; the victim sees extraction. The analytical observer risks collapsing this gap by treating the constraint as a natural law of balance-of-power dynamics, which would naturalize what is actually a contingent institutional arrangement favoring specific power distributions. The scaffold and piton perspectives add temporal dimension: reformers see a sunset (alternative institutions maturing), while institutional actors see degradation (theater rising, coherence declining).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position in the alignment: beneficiaries with arbitrage options (US, Japan) have low d → negative f(d) → negative or low χ; they experience the constraint as coordination serving them. Trapped peripheral states have high d → high f(d) → high χ; they experience maximum extraction. Constrained middle powers have intermediate d reflecting mixed position: some benefits (security, investment), some costs (autonomy, sanctions vulnerability). The hegemonic coordinator appears in two perspectives reflecting the perspectival ambiguity: a benevolent coordination view (rope) where the US organizes genuine collective goods, and a realist extraction view (snare) where the US maintains hegemonic dominance through coalition-building. Both are valid readings of the same structural data—the perspectival gap reveals the interpretive choice rather than factual disagreement. The directionality derivation chain uses beneficiary/victim declarations (core members benefit, peripheral/excluded states bear costs) plus power/exit/scope combinations to produce differentiated d values without manual override.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The quad alignment asymmetry resolves mandatrophy by demonstrating that the same constraint structure produces multiple classification types from different observation positions. This is not indeterminacy but indexed classification—the types are latent in the data until an observer position is specified. The beneficiary in arbitrage position sees rope (genuine coordination). The trapped agent sees snare (pure extraction). The middle power in constrained position sees tangled rope (mixed coordination and extraction). The institutional actor seeing degradation sees piton (performative persistence). The reformer seeing institutional alternatives sees scaffold (temporary constraint with sunset). The civilizational analyst risks seeing mountain (natural law of balance-of-power) but the base properties contradict this—extractiveness, suppression, and theater metrics reveal contingent institutional arrangements, not immutable laws. The mandatrophy is resolved by the indexical tuple: each classification type is correct relative to its (P, T, E, S) tuple. There is no single 'true' classification—the presheaf over the observation site IS the answer. The constraint is tangled_rope at institutional power level with active enforcement because it genuinely coordinates while extracting from specific populations (satisfying both gates: beneficiaries and victims, enforcement required).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_coordination_vs_extraction_masking,
    'Is the quad''s stated coordination function (rules-based order, freedom of navigation, counter-hegemony) genuine coordination that merely has extraction side effects, or is extraction the primary function masked by coordination rhetoric?',
    'Historical comparison: track whether quad enforcement mechanisms target genuinely hegemonic behavior or merely non-aligned behavior; analyze whether quad members accept constraints on their own rule-breaking; measure enforcement asymmetry',
    'If genuine coordination: chi < 0.45 for core members, all perspectives shift toward rope. If extraction-masking: chi > 0.65 even for core members; snare classification for peripheral states confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_coordination_vs_extraction_masking, empirical, 'Whether quad coordination function is genuine or rhetorical cover for extraction').

omega_variable(
    coercion_mechanism_identification,
    'What specific coercion mechanisms maintain peripheral state participation despite asymmetric extraction? (Economic sanctions threats, security abandonment threats, technology embargoes, diplomatic isolation?)',
    'Survey of defection attempts and their costs; analysis of how explicitly coercion is threatened vs culturally embedded as ''alignment expectations''; measurement of defection costs by mechanism type',
    'If coercion is explicit and recent: suppression rating (0.62) confirmed as accurate. If coercion is historical/cultural internalized: suppression may be lower structurally; identity_locked exit may apply to some peripheral states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_mechanism_identification, empirical, 'Specific mechanisms of peripheral state coercion').

omega_variable(
    alternative_coordination_maturity,
    'Are emerging alternative coordination mechanisms (BRICS, SCO, Shanghai Cooperation, ASEAN centrality frameworks) sufficiently mature to offer credible exit paths for trapped peripheral states, or are they still aspirational?',
    'Comparative institutional analysis: capability maturity, enforcement mechanisms, investment commitments, decision-making speed, technology-sharing infrastructure vs quad equivalents',
    'If mature: scaffold perspective confirmed; sunset clause is real (15-25 year horizon). If aspirational: scaffold is performative; peripheral states remain trapped even if alternatives are articulated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_maturity, empirical, 'Maturity of alternative coordination mechanisms as exit paths').

omega_variable(
    performer_coherence_in_piton_classification,
    'Is the high theater ratio (0.68) a sign of piton degradation (constraint persisting through habit despite low function) or a sign of sophisticated strategic ambiguity (members deliberately maintaining plausible deniability while executing alignment)?',
    'Analysis of public statements vs private behavior patterns; examination of whether theater enables or prevents functional outcomes; historical tracking of whether theater ratio has increased over time (piton decay signal) or remained stable (strategic ambiguity signal)',
    'If piton (degraded): constraint weakening; exit becomes easier as theater becomes unsustainable. If strategic ambiguity (functional): constraint more durable; theater is the mechanism, not a sign of failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performer_coherence_in_piton_classification, conceptual, 'Whether high theater indicates piton degradation or functional strategic ambiguity').

omega_variable(
    hegemonic_vs_multi_hegemonic_structural_regime,
    'Is the constraint structure contingent on continued US unipolarity, or can quad alignment persist in multi-hegemonic world (US + China + EU poles)?',
    'Structural modeling of alignment incentives under different polarity configurations; analysis of whether quad cohesion depends on unambiguous US leadership or can survive distributed leadership; examination of how quad members position themselves in multi-polar scenarios',
    'If contingent on unipolarity: constraint is historical artifact that will degrade as polarity shifts (sunset confirmed). If compatible with multi-polarity: constraint may persist in different form but with lower extraction asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hegemonic_vs_multi_hegemonic_structural_regime, conceptual, 'Whether constraint structure depends on unipolar system configuration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quad_alignment_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quad_tr_t0, quad_alignment_asymmetry, theater_ratio, 0, 0.48).
narrative_ontology:measurement(quad_tr_t5, quad_alignment_asymmetry, theater_ratio, 5, 0.58).
narrative_ontology:measurement(quad_tr_t10, quad_alignment_asymmetry, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(quad_be_t0, quad_alignment_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(quad_be_t5, quad_alignment_asymmetry, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(quad_be_t10, quad_alignment_asymmetry, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quad_alignment_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(quad_alignment_asymmetry, china_containment_architecture).
narrative_ontology:affects_constraint(quad_alignment_asymmetry, technology_supply_chain_decoupling).
narrative_ontology:affects_constraint(quad_alignment_asymmetry, indian_strategic_autonomy).
narrative_ontology:affects_constraint(quad_alignment_asymmetry, japanese_regional_leadership).

% DUAL FORMULATION NOTE:
% The quad alignment asymmetry is downstream of strategic competition (China rise, US hegemony management) and upstream of multiple regional and functional constraints. Decompose along lines of: genuine coordination function (rules-based order, freedom of navigation), extraction function (hegemonic dominance maintenance, peripheral state coercion), and theatrical function (performance of unity masking actual divergence). Each sub-constraint may have distinct epsilon values and classification profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quad_alignment_asymmetry, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
