% ============================================================================
% CONSTRAINT STORY: bilateral_science_innovation_partnership
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bilateral_science_innovation_partnership, []).

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
 *   constraint_id: bilateral_science_innovation_partnership
 *   human_readable: Bilateral Science Innovation Partnership
 *   domain: international_science_policy/institutional_partnership
 *
 * SUMMARY:
 *   Bilateral science innovation partnerships between research-dominant and
 *   research-emerging nations structure international collaboration through
 *   asymmetric institutional power, resource concentration, and epistemic
 *   authority. These partnerships present themselves as win-win coordination
 *   mechanisms: the dominant nation gains access to novel research
 *   populations, data, and talent; the emerging nation gains resource access
 *   and scientific legitimacy. However, the structural analysis reveals a
 *   complex hybrid. While genuine coordination functions exist (shared
 *   infrastructure, methodological exchange, capacity access), these are
 *   accompanied by systematic extraction mechanisms: IP ownership
 *   asymmetries, publication order control, research agenda subordination,
 *   and institutional dependency creation. The constraint exhibits all six
 *   classification types across different stakeholder perspectives, making it
 *   a diagnostic exemplar for how the same institutional arrangement can
 *   appear as coordination (to beneficiaries), extraction (to victims), or
 *   degraded ritual (to administrators maintaining the partnership through
 *   inertia). The theater ratio has increased from 0.35 to 0.55 over the
 *   partnership interval, indicating that progress reporting and
 *   administrative activity now substantially exceed functional innovation
 *   transfer. Extractiveness has increased from 0.28 to 0.52, reflecting
 *   systematic deepening of dependency and subordination mechanisms. A
 *   critical feature is the identity lock mechanism visible in the partner
 *   nation's research policy administrators, who have internalized the
 *   partnership logic so completely that alternative institutional
 *   arrangements become cognitively unavailable to them, even as the
 *   partnership's extractive dynamics become structurally visible to other
 *   observers.
 *
 * KEY AGENTS:
 *   - Dominant Research Institutions: Primary beneficiary (institutional/arbitrage) — capture IP rights, publication priority, research population access, talent recruitment with exit optionality
 *   - Junior Researchers: Primary victim (powerless/trapped) — bear costs of publication order subordination, IP ownership disparity, agenda control; career survival depends on partnership participation
 *   - Technology-Importing Nation: Secondary victim (powerless/constrained) — faces institutional lock-in from dependency deepening; domestic research capacity atrophies as partnership access substitutes for indigenous capacity development
 *   - Partner Nation Research Council: Organized actor (organized/constrained) — benefits from resource access and legitimacy while subordinating research capacity agenda to partnership terms; institutional investment creates sunk-cost exit barriers
 *   - Captured Regulator: Organized institutional actor (organized/identity_locked) — identity-fused with partnership framework; structurally mobile but perceives partnership logic as inherent to scientific modernization
 *   - South-South Coalition: Alternative pathway (organized/mobile) — builds symmetric partnerships with explicit capacity-transfer goals and sunset timelines; demonstrates that partnership extraction is contingent, not inherent
 *   - Partnership Administration: Institutional apparatus (institutional/arbitrage) — maintains partnership through performative compliance; theater substitutes for functional innovation transfer
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing contingent institutional asymmetries as inherent to scientific hierarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bilateral_science_innovation_partnership, 0.52).
domain_priors:suppression_score(bilateral_science_innovation_partnership, 0.48).
domain_priors:theater_ratio(bilateral_science_innovation_partnership, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bilateral_science_innovation_partnership, extractiveness, 0.52).
narrative_ontology:constraint_metric(bilateral_science_innovation_partnership, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(bilateral_science_innovation_partnership, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bilateral_science_innovation_partnership, tangled_rope).
narrative_ontology:human_readable(bilateral_science_innovation_partnership, "Bilateral Science Innovation Partnership").
narrative_ontology:topic_domain(bilateral_science_innovation_partnership, "international_science_policy/institutional_partnership").

domain_priors:requires_active_enforcement(bilateral_science_innovation_partnership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bilateral_science_innovation_partnership, dominant_research_institutions).
narrative_ontology:constraint_beneficiary(bilateral_science_innovation_partnership, technology_exporting_nation).
narrative_ontology:constraint_victim(bilateral_science_innovation_partnership, junior_researchers).
narrative_ontology:constraint_victim(bilateral_science_innovation_partnership, technology_importing_nation).
narrative_ontology:constraint_victim(bilateral_science_innovation_partnership, epistemic_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JUNIOR RESEARCHER (SNARE) — Early-career scientists in partner institutions face structural extraction through asymmetric publication norms, IP ownership disparity, and research agenda control. Exit is trapped — career survival depends on partnership participation. No alternatives exist at comparable resource levels. Maximum experienced extraction through suppression of independent research directions.
constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TECHNOLOGY-IMPORTING NATION (SNARE) — National research institutions become dependent on partnership infrastructure, methodologies, and validation frameworks. Long-term capacity building is subordinated to short-term resource access. Exit is constrained by institutional lock-in: domestic capacity atrophies as collaboration deepens. Generational horizon reveals systematic subordination of indigenous research directions.
constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: DOMINANT RESEARCH INSTITUTION (ROPE) — Experiences partnership as coordination: sharing methodologies, accessing new research populations, expanding publication reach. Net beneficiary — receives IP rights, publication priority, student/researcher access. Can arbitrage participation by selectively deepening or withdrawing. Immediate horizon shows clear coordination benefit with favorable extraction dynamics.
constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PARTNER NATION RESEARCH COUNCIL (TANGLED ROPE) — Organized institutional actor benefits from resource access and legitimacy conferred by partnership while bearing costs of agenda subordination and capacity-building diversion. Both genuine coordination (shared research infrastructure) and asymmetric extraction (IP terms, publication order) present. Constrained exit due to sunk institutional investment and political pressure to maintain partnership optics.
constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CAPTURED REGULATOR (TANGLED_ROPE / identity_locked) — National science policy administrators charged with building research capacity become identity-fused with partnership framework. They internalize the logic: 'partnership = progress,' 'collaboration = development,' 'openness = modernization.' Structurally mobile (could redirect policy) but identity-locked into partnership framing. Exit unthinkable because it would require abandoning the institutional identity they constructed as partnership architects. Both real coordination function (resource access) and real extraction (capacity subordination) present simultaneously, but the identity lock prevents the regulator from perceiving the extraction clearly.
constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: SOUTH-SOUTH SCIENCE COALITION (SCAFFOLD) — Alternative bilateral partnerships among technology-importing nations create parallel verification and capacity-building pathways with explicit sunset logic: indigenous capacity development, knowledge transfer protocols, and exit timelines are negotiated upfront. Low extraction because coalition members have agency and negotiate symmetric benefit structures. Theater is low — substance of collaboration (capacity transfer) matches stated goals. Sunset clause: as indigenous capacity matures, partnerships transition from dependency to reciprocal exchange or conclude.
constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: PARTNERSHIP ADMINISTRATION (PITON) — The formal structures, committees, and bureaucracies managing the partnership persist largely through institutional inertia. Theater ratio is high: progress reports document activity and legitimacy but actual research direction changes are minimal. The partnership administrative apparatus maintains itself by reporting success, but underlying innovation metrics (indigenous publication rates, domestic patent generation, capacity transfer) show degradation. Performative compliance replaces functional partnership.
constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / IMMUTABILITY VIEW (MOUNTAIN) — From a universal/civilizational perspective, science is inherently global and hierarchical by resource distribution. Research capacity concentrates where funding concentrates. Partnerships are seen as natural expressions of this inherent structure. This perspective risks naturalizing what are contingent historical and political asymmetries (colonial-era scientific infrastructure, patent regime design, funding concentration) as inherent to scientific organization. The analytical observer's mountain classification is a false summit detector signal.
constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bilateral_science_innovation_partnership_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bilateral_science_innovation_partnership, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bilateral_science_innovation_partnership, TR),
    TR >= 0.70.

:- end_tests(bilateral_science_innovation_partnership_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over partnership lifetime. Initial partnerships often contain genuine coordination (0.28) — resource sharing, methodological exchange, collaborative research problems generate mutual benefit. But as partnerships institutionalize, extraction mechanisms accumulate: IP ownership rules subordinate partner-nation benefit; publication norms privilege dominant-nation researchers; agenda-setting processes channel partner resources toward dominant-nation priorities. The trajectory from 0.28 to 0.52 reflects this degradation. Suppression (0.48): Moderate. Junior researchers and emerging-nation institutions face significant barriers to exit: career dependence on partnership credentials, institutional lock-in from infrastructure investment, reputation costs of partnership departure, reduced funding access if partnerships are disrupted. But suppression is not maximal — some researchers can work outside partnerships, some institutions build alternatives, some nations have negotiated exit. Theater ratio (0.55): Moderate-high. Partnership administrations maintain substantial performative apparatus: progress reports, success metrics, committee activity, training program documentation. But underlying innovation transfer (domestic patent generation, indigenous publication rates, independent research capacity) does not match performance claims. The trajectory from 0.35 to 0.55 indicates increasing performative content as extraction deepens.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates perspectival gaps at multiple levels. (1) Power asymmetry gap: dominant institutions experience themselves as coordinators; junior researchers and partner nations experience themselves as extracted-from. (2) Temporal gap: immediate horizon shows partnership benefit; generational horizon reveals dependency accumulation. (3) Exit-capacity gap: institutional actors perceive arbitrage options; junior researchers perceive traps. (4) Framing gap: the captured regulator perceives partnership through an identity frame ('modernization,' 'capacity building') that makes extraction invisible, while other observers (particularly the south-south coalition) perceive the same arrangements as contingent extraction mechanisms. The false summit at the analytical/mountain perspective signals that this gap itself is diagnostic — it reveals that the 'inherent scientific hierarchy' framing naturalizes what are actually institutional choices.
 *
 * DIRECTIONALITY LOGIC:
 *   The partnership's directionality varies sharply across stakeholder positions. Dominant institutions benefit — low d from beneficiary status + arbitrage mobility. Junior researchers bear costs — high d from victim status + career-dependent trapped mobility. Partner nation bears costs — high d from victim status + institutional-lock constrained mobility. The captured regulator's directionality is complex: they formally occupy an institutional position (moderate power, potentially mobile) but identity lock reduces their effective d by making extraction perception unavailable to them. The regulator perceives low extraction (partnership as coordination) even though the structural flow is high extraction (policy choices subordinate national research capacity). This mismatch is the oracle gap instantiation: the regulator's identity-locked position prevents them from seeing what the analytical observer sees (partnership as extraction mechanism). The south-south coalition demonstrates alternative directionality: symmetric partnerships with clear capacity-transfer terms and sunset clauses produce low-extraction structures even between research-asymmetric nations. The coalition's d values remain high (victim-like) but the constraint classification is Scaffold, not Snare, because the exit path is visible and negotiated.
 *
 * MANDATROPHY ANALYSIS:
 *   The partnership constraint resolves mandatrophy by demonstrating that multiple classification types are simultaneously true from different structural positions. The institution benefits from coordination (Rope). The researcher is trapped in extraction (Snare). The partner nation faces dependency (Snare). The organized actors experience hybrid coordination-extraction (Tangled Rope). The administrators see degradation (Piton). The alternative coalition shows temporary solutions (Scaffold). The universal analytical view risks false naturalization (Mountain). No single type is 'correct' — the constraint's real structure is the presheaf of these perspectival readings. The mandatrophy dissolves when we recognize that 'is this partnership Rope or Snare?' is a category error. It is both simultaneously, depending on the observer's structural position. The meaningful analysis is: for which agents is it Rope, for which is it Snare, and what institutional changes would shift the classification? The south-south coalition answer is: partnerships can be designed as Scaffolds (lower extraction, visible sunset, symmetric benefit) instead of Snares. The path from current (Tangled Rope averaging toward Snare) to alternative (Scaffold) requires policy changes that the captured regulator is identity-locked against perceiving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_transfer_vs_dependency,
    'Is the partnership building genuine indigenous research capacity or deepening structural dependency on partner methodologies and validation frameworks?',
    'Longitudinal analysis of: (1) domestic publication rates in partner vs non-partner institutions, (2) independent research agenda development post-partnership, (3) citation independence of partner nation researchers, (4) ability to conduct research without partner-nation validation',
    'If capacity transfer is genuine: classification shifts toward Rope/Scaffold from more perspectives. If dependency deepens: Snare classification is vindicated, extraction mechanism is structural, partnership structure itself prevents escape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_transfer_vs_dependency, empirical, 'Whether partnership transfers capacity or deepens dependency').

omega_variable(
    ip_asymmetry_scope,
    'What proportion of partnership-generated IP is accessible to technology-importing nation institutions without licensing restrictions? Is this proportion decreasing over partnership lifetime?',
    'Analysis of partnership IP agreements; audit of IP licensing terms; comparison of IP ownership distribution across cohorts (early vs recent partnership years); tracking of licensing fee structures',
    'If accessibility is high and symmetric: tangled rope with coordination function clearly dominant. If accessibility is low and deteriorating: suppression mechanism is institutional/contractual; extraction is primary function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ip_asymmetry_scope, empirical, 'IP accessibility asymmetry in partnership terms').

omega_variable(
    publication_order_neutrality,
    'Are publication order and corresponding authorship determined by research contribution magnitude, or by institutional affiliation and partner-nation advantage?',
    'Analysis of contribution attribution vs actual authorship order across partnership publications; comparison with same research teams'' non-partnership publications; survey of junior researchers on authorship negotiation power',
    'If contribution-determined: coordination function is genuine. If affiliation-driven: suppression mechanism is embedded in publication norms; junior researchers'' agency is systematically constrained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(publication_order_neutrality, empirical, 'Whether publication order reflects contribution or institutional advantage').

omega_variable(
    research_agenda_autonomy,
    'Do technology-importing nation researchers originate research questions independently, or do partnership frameworks channel them toward dominant-nation-defined priorities?',
    'Analysis of research proposal flow; tracking of agenda-setting meetings; interview data on research priority formation; comparison of indigenous research trajectory (absent partnership) with actual trajectory',
    'If agenda is autonomous: partnership is coordination mechanism. If channeled: partnership is extraction mechanism disguised as cooperation; victims'' agency is systematically undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(research_agenda_autonomy, empirical, 'Research agenda autonomy in partnership framework').

omega_variable(
    identity_lock_brittle_point,
    'At what point of partnership asymmetry recognition does the captured regulator''s identity lock break? What evidence triggers re-evaluation?',
    'Case studies of policy reversals in partnership regimes; tracking of regulator testimony/rhetoric across partnership lifecycle; identification of threshold conditions (capacity metrics, publication outcomes, budget allocations) that triggered regulator skepticism in historical cases',
    'If brittle point is identifiable: identity lock is contingent, not structural. Policy reframing could enable exit. If no threshold exists: identity lock is deeply rooted, may require generational change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_brittle_point, conceptual, 'Conditions for identity-lock breakdown in partnership administrators').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bilateral_science_innovation_partnership, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bila_tr_t0, bilateral_science_innovation_partnership, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bila_tr_t5, bilateral_science_innovation_partnership, theater_ratio, 5, 0.48).
narrative_ontology:measurement(bila_tr_t10, bilateral_science_innovation_partnership, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(bila_be_t0, bilateral_science_innovation_partnership, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bila_be_t5, bilateral_science_innovation_partnership, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(bila_be_t10, bilateral_science_innovation_partnership, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bilateral_science_innovation_partnership, resource_allocation).
narrative_ontology:affects_constraint(bilateral_science_innovation_partnership, brain_drain_from_emerging_nations).
narrative_ontology:affects_constraint(bilateral_science_innovation_partnership, global_research_infrastructure_concentration).
narrative_ontology:affects_constraint(bilateral_science_innovation_partnership, patent_regime_asymmetry).

% DUAL FORMULATION NOTE:
% The bilateral science partnership decomposes into distinct constraints: genuine coordination problems (methodological exchange, infrastructure sharing) versus extraction mechanisms (IP ownership, publication order control, agenda subordination). These have different epsilon values and should be tracked separately. This story represents the hybrid tangled_rope structure. Upstream constraints: global research funding concentration (creates asymmetry that partnerships express). Downstream constraints: brain drain, patent concentration (accelerated by partnership terms). Network affects are causal — partnership structure reinforces upstream funding concentration and downstream brain drain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bilateral_science_innovation_partnership, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
