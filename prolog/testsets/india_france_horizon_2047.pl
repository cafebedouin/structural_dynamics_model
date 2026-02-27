% ============================================================================
% CONSTRAINT STORY: india_france_horizon_2047
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_india_france_horizon_2047, []).

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
 *   constraint_id: india_france_horizon_2047
 *   human_readable: India-France Horizon 2047 Strategic Partnership
 *   domain: geopolitical/defense_technology/strategic_alignment
 *
 * SUMMARY:
 *   The India-France 'Horizon 2047' strategic partnership represents a
 *   formalized coordination mechanism across defense, space, nuclear energy,
 *   and emerging technologies. Framed by both governments as mutual strategic
 *   partnership against regional instability and great-power competition, the
 *   constraint exhibits classical Tangled Rope structure: genuine
 *   coordination benefits (technology access, market stability, reduced
 *   reliance on unilateral US security guarantees for India; sustained
 *   geopolitical relevance and defense market access for France) coexist with
 *   asymmetric extraction mechanisms (French technology dependency, export
 *   control leverage, and strategic veto rights for India; perpetual market
 *   access and intellectual property rents for France). The partnership
 *   creates a binding structure that reduces India's non-aligned flexibility
 *   while increasing its technological capability — a trade-off that benefits
 *   institutional actors (state apparatus, defense industrial base) more than
 *   distributed populations or smaller regional powers. The theater ratio
 *   (0.62) reflects significant performative elements: public rhetoric
 *   emphasizes 'partnership of equals' while structural asymmetries in
 *   technology dependency and decision-making authority remain
 *   operationalized.
 *
 * KEY AGENTS:
 *   - French Defense Industrial Base: Primary beneficiary (institutional/arbitrage) — sustained market access, technology royalties, geopolitical leverage through asymmetric technology dependency
 *   - Indian State Apparatus: Secondary beneficiary and victim (institutional/constrained) — gains strategic autonomy from US-centric systems but accepts technological dependency and strategic constraint
 *   - Indian Technology Sector: Mixed position (organized/constrained) — gains access to expertise and contracts but becomes subject to export controls and intellectual property restrictions
 *   - Smaller Regional Powers (Sri Lanka, Bangladesh, Nepal): Primary victims (powerless/trapped) — face deepened geopolitical asymmetry as India's coordinated capabilities concentrate; lack resources for independent alternatives
 *   - Non-Aligned Movement Institutions: Inertial actor (institutional/arbitrage) — maintain performative consultation role as real decision-making migrates to bilateral partnerships (Piton classification)
 *   - Renewable Energy and AI Sectors: Sunset actor (organized/mobile) — benefit from explicit capacity-building and technology-transfer design aimed at eventual Indian autonomy; extraction mechanism designed to decline
 *   - Analytical Observer: Risk of naturalizing contingent partnership as structural inevitability (false Mountain perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(india_france_horizon_2047, 0.52).
domain_priors:suppression_score(india_france_horizon_2047, 0.58).
domain_priors:theater_ratio(india_france_horizon_2047, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(india_france_horizon_2047, extractiveness, 0.52).
narrative_ontology:constraint_metric(india_france_horizon_2047, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(india_france_horizon_2047, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(india_france_horizon_2047, tangled_rope).
narrative_ontology:human_readable(india_france_horizon_2047, "India-France Horizon 2047 Strategic Partnership").
narrative_ontology:topic_domain(india_france_horizon_2047, "geopolitical/defense_technology/strategic_alignment").

domain_priors:requires_active_enforcement(india_france_horizon_2047).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(india_france_horizon_2047, french_defense_industrial_base).
narrative_ontology:constraint_beneficiary(india_france_horizon_2047, french_global_influence).
narrative_ontology:constraint_beneficiary(india_france_horizon_2047, indian_technological_capability).
narrative_ontology:constraint_beneficiary(india_france_horizon_2047, mutual_strategic_autonomy).
narrative_ontology:constraint_victim(india_france_horizon_2047, smaller_regional_powers).
narrative_ontology:constraint_victim(india_france_horizon_2047, non_aligned_strategic_flexibility).
narrative_ontology:constraint_victim(india_france_horizon_2047, equitable_technology_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLER REGIONAL POWERS (SNARE) — Caught in Indo-French technological and military coordination that narrows their strategic alternatives. Lack resources to develop equivalent capabilities independently. Cannot exit the constraint's scope without severe strategic disadvantage. Maximum extraction: geopolitical asymmetry deepens as India's capabilities concentrate.
constraint_indexing:constraint_classification(india_france_horizon_2047, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDIAN TECHNOLOGY SECTOR (TANGLED ROPE) — Mixed position: gains access to French expertise and defense contracts (coordination benefit) but becomes dependent on French technology pathways and subject to French export controls and strategic veto (extraction cost). Constrained by technology transfer agreements and intellectual property restrictions. Active enforcement required to maintain technology flows and prevent unauthorized diffusion.
constraint_indexing:constraint_classification(india_france_horizon_2047, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FRENCH DEFENSE INDUSTRIAL BASE (ROPE) — Primary beneficiary with high-value contracts, preferred access to Indian market, technology royalties, and sustained geopolitical leverage. Experiences the constraint as pure coordination: establishing clear rules for defense collaboration, technology sharing, and market access. Net positive extraction flow. Can arbitrage between Indo-Pacific strategies and other partnerships.
constraint_indexing:constraint_classification(india_france_horizon_2047, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RENEWABLE ENERGY AND AI SECTORS (SCAFFOLD) — Horizon 2047 includes solar, wind, and AI development with explicit capacity-building goals aimed at eventual Indian autonomy. Suppression (technology restrictions) is high in the medium term but explicitly designed to decline as Indian capabilities mature. Theater is moderate — genuine coordination on standards and interoperability, but with performative elements around 'partnership optics.' Sunset mechanism: technology transfer is time-bound; autonomy targets are explicit.
constraint_indexing:constraint_classification(india_france_horizon_2047, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INDIAN STATE APPARATUS (TANGLED ROPE) — Seeks strategic autonomy and reduced dependence on US-centric systems (coordination benefit) while accepting technological dependency and strategic constraints through binding commitments (extraction cost). Requires active enforcement of technology-sharing protocols, defense coordination, and alignment on China/Russia policy. Exit options constrained by the difficulty of severing deep defense relationships without geopolitical cost.
constraint_indexing:constraint_classification(india_france_horizon_2047, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NON-ALIGNED MOVEMENT INSTITUTIONS (PITON) — India's founding NAM principle of non-alignment increasingly performative as bilateral strategic partnerships dominate policy. Horizon 2047 represents formalized extraction of non-aligned flexibility while NAM maintains theater of consultation and consensus. Real decision-making has migrated to bilateral partnerships; NAM persists through institutional inertia. Theater ratio high; functional coordination capacity degraded.
constraint_indexing:constraint_classification(india_france_horizon_2047, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, power asymmetries in technology and defense create irreducible structural constraints: smaller states must align with larger technology holders to access capabilities. No exit strategy exists at scale. This appears as natural law (structural inevitability) but closer inspection reveals contingency: international technology sharing regimes, open-source development, and multilateral frameworks could reduce dependence. False summit risk high.
constraint_indexing:constraint_classification(india_france_horizon_2047, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(india_france_horizon_2047_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(india_france_horizon_2047, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(india_france_horizon_2047, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(india_france_horizon_2047, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(india_france_horizon_2047, TR),
    TR >= 0.70.

:- end_tests(india_france_horizon_2047_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising. The partnership begins at 0.38 (primarily coordination-focused) but increases to 0.58 as implementation reveals technology dependency mechanisms. French control over advanced defense systems, dual-use technology, and export authorization creates structural extraction. However, the extraction is not maximal (≥0.66 for Snare) because India retains alternative suppliers (Russia, Israel) and domestic development capacity — exit options are constrained but not trapped. Suppression (0.58): Moderate-high. Significant barriers to exit include: sunk costs in training and infrastructure, strategic misalignment costs if partnership severed, technology dependency for operational systems, and institutional lock-in through high-level state commitments. But suppression is not total — bilateral agreements can be renegotiated, alternative suppliers exist, and India has historical capacity for defense independence. Theater ratio (0.62): Moderate-high. Public rhetoric emphasizes 'partnership of equals' and mutual benefit, while structural asymmetries in technology flows, decision-making authority, and exit costs remain operationalized but less visible. Performative elements include joint press releases, ceremonial defense talks, and symbolic technology transfer announcements that obscure underlying dependency dynamics.
 *
 * PERSPECTIVAL GAP:
 *   The partnership demonstrates maximum perspectival divergence. The French institutional perspective sees Rope (pure coordination: establishing clear rules for defense collaboration and market access). The Indian state sees Tangled Rope (genuine coordination benefits in strategic autonomy plus extraction costs in technology dependency). The Indian technology sector also sees Tangled Rope (mixed access to expertise and contracts offset by IP restrictions and export controls). Smaller regional powers see Snare (trapped in deepened asymmetry with no exit or compensating benefit). The non-aligned movement sees Piton (its principles are performatively maintained while real power migrates to bilateral partnership). The renewable energy and AI sectors see Scaffold (extraction mechanism explicitly designed to decline as technology transfers complete and Indian capacity matures). The civilizational analytical perspective risks Mountain (structural inevitability) but the classification fails when contingent institutional arrangements (export control regimes, IP frameworks, strategic alignment costs) are recognized as policy choices rather than natural limits.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural position within the partnership flow. The French defense industrial base occupies the beneficiary position (low d → negative χ, experienced as pure coordination) because extraction flows toward them: technology royalties, market rents, sustained geopolitical veto. The Indian state occupies a mixed position (moderate d) because it both benefits (strategic autonomy from unilateral US dependence) and bears costs (technological dependency, constrained exit). Smaller regional powers occupy the victim position (high d → high χ, experienced as Snare) because the constraint extracts their strategic flexibility without providing compensating benefits. The analytical observer at civilizational scale risks deriving d from a false natural law frame (mountain), which the structural data contradicts: power asymmetry appears inevitable only if technology dependency and strategic options are held fixed — but these are contingent policy choices, not immutable.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution requires distinguishing genuine coordination from extraction masquerading as coordination. The Horizon 2047 partnership presents exactly this diagnostic challenge. COORDINATION EVIDENCE: (1) Both parties genuinely benefit from stable technology flows and reduced unilateral dependency. (2) The partnership reduces transaction costs for defense collaboration and technology access compared to ad-hoc negotiations. (3) Non-aligned alternatives (Russian dependence, US dominance) present worse outcomes for India. EXTRACTION EVIDENCE: (1) Technology flows are asymmetric — France provides advanced systems; India provides market access and strategic alignment. (2) Export controls embedded in the partnership restrict Indian re-export and derivative development. (3) Theater ratio increasing over time suggests performative elements (equality rhetoric) masking structural imbalances. (4) Smaller regional powers bear costs without receiving corresponding benefits. CLASSIFICATION: The Tangled Rope classification holds because both mechanisms are structurally present and both are operationalized through enforcement (technology agreements, export licensing, policy coordination mechanisms). The mandatrophy is resolved by recognizing that this is NOT a choice between Rope and Snare — it is a hybrid where coordination and extraction co-occur. The risk is that the coordination narrative (mutual partnership) drowns out the extraction analysis (asymmetric technology dependency, constrained exit, victim displacement to smaller powers). The Scaffold perspective (sunset mechanism in renewable energy and AI sectors) suggests that the extraction mechanism could be genuinely time-limited if technology transfer targets are met — but this requires explicit monitoring and governance, not faith in partnership benevolence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_transfer_compliance,
    'Will India successfully absorb and indigenize French defense and energy technologies, or will dependency perpetuate?',
    'Longitudinal tracking of Indian domestic R&D capacity growth in key sectors; analysis of indigenous product development post-technology transfer; patent and innovation metrics; workforce skill development timelines',
    'If successful absorption: Scaffold perspective confirmed, sunset is real, extraction declines over 15-20 years. If failure: Tangled Rope becomes permanent Snare for Indian state; beneficiary position of French base becomes indefinite.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_compliance, empirical, 'Whether India achieves technology independence or remains dependent').

omega_variable(
    geostrategic_binding_constraint,
    'Does Horizon 2047 operationally constrain Indian strategic choices (especially vis-à-vis China, Russia, Middle East) or serve as optionality enhancement?',
    'Case analysis of Indian decisions during strategic crises (border conflicts, energy supply disruptions, trade disputes) where Horizon 2047 partnership could create binding pressure; analysis of explicit vs. implicit coordination obligations; declassified policy documents post-implementation',
    'If binding: Extraction mechanism is real; suppression of non-aligned flexibility is structural. Snare perspective of smaller powers is accurate. If optionality: Partnership functions as Rope (pure coordination); constrainment narrative is theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geostrategic_binding_constraint, conceptual, 'Whether partnership operationally constrains Indian strategic autonomy').

omega_variable(
    export_control_regime_asymmetry,
    'Are export controls on technology embedded in Horizon 2047 asymmetric (France controls India''s access) or reciprocal?',
    'Comparative analysis of technology flow direction and approval processes; tracking of denied exports or requests; analysis of intellectual property agreements and licensing terms; asymmetry indices in defense technology vs. strategic resource flows',
    'If asymmetric: Extraction mechanism confirmed; Tangled Rope classification stands. If reciprocal: Pure coordination (Rope) is more accurate; the partnership removes barriers rather than creates new ones.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(export_control_regime_asymmetry, empirical, 'Asymmetry in export control and technology access mechanisms').

omega_variable(
    alignment_cost_for_india,
    'What is the implicit cost to India of defending French interests in Indo-Pacific as price of Horizon 2047 benefits?',
    'Analysis of expected Indian military commitments (naval deployments, joint exercises, defense of French territories); comparison with India''s independent strategic priorities; cost-benefit analysis of potential conflicts with other partnerships (Russian defense ties, ASEAN balancing)',
    'If high: Extraction mechanism is severe; victims (smaller powers, non-aligned flexibility) face more extreme constraint. If low: Partnership functions more symmetrically; false snare assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_cost_for_india, preference, 'Implicit strategic alignment costs for India within the partnership').

omega_variable(
    french_manufacturing_dependency,
    'Does France develop strategic dependency on Indian markets/resources that reverses extraction asymmetry over time?',
    'Tracking of French defense sector revenue reliance on Indian contracts; analysis of rare earth and critical mineral supply chains (India-Madagascar control); market concentration analysis; alternative sourcing capability assessment',
    'If yes: Long-term extraction flows may reverse; partnership shifts toward mutual Tangled Rope or balanced Rope. If no: French institutional perspective remains primary beneficiary indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(french_manufacturing_dependency, empirical, 'Degree of French economic dependency on Indian partnership').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(india_france_horizon_2047, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ih2047_tr_t0, india_france_horizon_2047, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ih2047_tr_t12, india_france_horizon_2047, theater_ratio, 12, 0.62).
narrative_ontology:measurement(ih2047_tr_t24, india_france_horizon_2047, theater_ratio, 24, 0.68).

% Extraction over time
narrative_ontology:measurement(ih2047_be_t0, india_france_horizon_2047, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ih2047_be_t12, india_france_horizon_2047, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(ih2047_be_t24, india_france_horizon_2047, base_extractiveness, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(india_france_horizon_2047, enforcement_mechanism).
narrative_ontology:affects_constraint(india_france_horizon_2047, indo_pacific_strategic_competition).
narrative_ontology:affects_constraint(india_france_horizon_2047, quad_alignment_asymmetry).
narrative_ontology:affects_constraint(india_france_horizon_2047, non_aligned_movement_degradation).

% DUAL FORMULATION NOTE:
% Horizon 2047 is downstream of broader Indo-Pacific geopolitical competition and upstream of specific technology sector constraints. The partnership simultaneously affects (1) structural dynamics of Quad alignment (US-India-Japan-Australia coordination), (2) sustainability of non-aligned movement institutions, and (3) regional power asymmetries affecting smaller South Asian states. Each of these constraints has its own extractiveness profile; Horizon 2047 functions as a mediating structure affecting all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(india_france_horizon_2047, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
