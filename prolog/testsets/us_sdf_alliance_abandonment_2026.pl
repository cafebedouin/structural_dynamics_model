% ============================================================================
% CONSTRAINT STORY: us_sdf_alliance_abandonment_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_sdf_alliance_abandonment_2026, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_sdf_alliance_abandonment_2026
 *   human_readable: US Strategic Alliance Abandonment (Syria 2026)
 *   domain: geopolitical/military_alliance
 *
 * SUMMARY:
 *   The US-SDF alliance in northeast Syria (2014-2026) represents a strategic
 *   partnership with structural asymmetry: the SDF provided ground forces for
 *   counter-ISIS operations at high human cost while the US provided air
 *   support, logistics, and intelligence. By 2026, internal US political
 *   pressures and strategic reorientation away from the Middle East triggered
 *   a policy shift toward alliance abandonment. This constraint exhibits pure
 *   snare characteristics from the SDF's perspective and tangled rope
 *   characteristics from the US institutional perspective. The key agents
 *   operate at different structural levels: the SDF forces and Kurdish
 *   civilian populations are trapped with no exit options; the US military
 *   apparatus retains arbitrage exits via strategic repositioning; Turkish
 *   state interests see liberation from constraint; regional balance-of-power
 *   structures face consolidation under authoritarian actors. The
 *   constraint's theater_ratio (0.55) reflects the gap between formal
 *   alliance commitments (written into military doctrine, diplomatic
 *   protocols) and actual enforcement mechanisms (withdrawal of support).
 *   Extractiveness has accelerated dramatically over the 6-year interval as
 *   the US signaled and then executed withdrawal, converting a mixed
 *   coordination-extraction arrangement into a pure extraction mechanism with
 *   no protective function.
 *
 * KEY AGENTS:
 *   - SDF Military Forces: Primary victim (powerless/trapped) — locked into dependency on US air support, intelligence, and logistics; cannot exit without military collapse
 *   - Kurdish Civilian Populations in Northeast Syria: Primary victim (moderate/constrained) — face displacement and persecution risks; cannot exit the region without abandoning ancestral territories
 *   - US Military-Strategic Apparatus: Primary extractor (institutional/arbitrage) — benefits from ground force provision and regional leverage; has exit option via strategic redeployment
 *   - Turkish State: Secondary beneficiary (powerful/arbitrage) — experiences constraint removal as liberation; gains freedom to conduct military operations
 *   - Assad Regime and Russian Interests: Secondary beneficiary (powerful/constrained) — benefit from power vacuum and SDF weakness; consolidate regional control
 *   - International Alliance Treaty System: Institutional actor (institutional/constrained) — formal commitments persist theatrically; enforcement power atrophied
 *   - Regional Balance-of-Power Structure: Collective system victim (organized/constrained) — trapped in zero-sum dynamics; abandonment enables authoritarian consolidation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sdf_alliance_abandonment_2026, 0.68).
domain_priors:suppression_score(us_sdf_alliance_abandonment_2026, 0.72).
domain_priors:theater_ratio(us_sdf_alliance_abandonment_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_sdf_alliance_abandonment_2026, snare).
narrative_ontology:human_readable(us_sdf_alliance_abandonment_2026, "US Strategic Alliance Abandonment (Syria 2026)").
narrative_ontology:topic_domain(us_sdf_alliance_abandonment_2026, "geopolitical/military_alliance").

% --- Structural relationships ---
narrative_ontology:constraint_victim(us_sdf_alliance_abandonment_2026, sdf_forces).
narrative_ontology:constraint_victim(us_sdf_alliance_abandonment_2026, kurdish_civilian_populations).
narrative_ontology:constraint_victim(us_sdf_alliance_abandonment_2026, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SDF GROUND FORCES (SNARE) — Locked into dependency on US logistics, air support, and intelligence. Cannot exit the alliance without facing immediate military collapse against Turkish forces and Assad regime. Abandonment represents complete extraction: military assets frozen, supply chains severed, promised air cover withdrawn. Maximum coercion, zero exit velocity.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: KURDISH CIVILIAN POPULATIONS (SNARE) — Constrained exit but severe extraction. Alliance promised security from Turkish incursion and cultural persecution; abandonment exposes them to displacement, demographic dilution, and military occupation. Moderate power level derives from some collective organization (YPG/PKK structures) but fundamental inability to resist Turkish/Assad military pressure without US support. High suppression from multiple threat vectors (Turkey, Assad, ISIS remnants).
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US MILITARY-STRATEGIC APPARATUS (TANGLED ROPE) — Hybrid. The alliance served genuine coordination functions: counter-ISIS operations (2014-2021), anti-ISIS information sharing, basing for Middle East operations, pressure on Iranian expansion. But it also served extractive functions: maintaining leverage over Turkey/NATO ally, cost-shifting ground warfare to SDF, preventing Syrian government consolidation. US institutional actors have arbitrage exits — redeployment to other regions, pivot to Israel/Gulf focus. The constraint is enforced via contingency planning and alliance management. Coordination (counter-ISIS) mixed with asymmetric extraction (burden-shifting, leverage extraction).
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TURKISH STATE INTERESTS (ROPE) — From Ankara's perspective, the SDF-US alliance was a pure constraint on Turkish freedom of action. Abandonment removes this constraint entirely, enabling Turkish military operations in northern Syria without US interference. Turkish state sees this as coordination: resolving the blockade on Turkish security operations. Zero extraction experienced — this is liberation. The constraint is dissolving through US policy shift.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LAW AND ALLIANCE TREATY NORMS (PITON) — The formal commitment to protect SDF forces derives from NATO alliance obligations, counterterrorism cooperation agreements, and informal security guarantees. These norms persist theatrically despite abandonment: written into military doctrine, teach-and-test cycles, diplomatic rhetoric. But functional enforcement is zero — the constraint provides no actual protection. Theater_ratio high (0.55): the formal structures remain, but their force has atrophied. Piton classification: institutional inertia, not actual coercive power.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL BALANCE-OF-POWER STRUCTURE (SNARE) — The SDF-US alliance was a structural constraint on Assad/Russia/Iran consolidation in northeast Syria. Abandonment removes this pressure, allowing authoritarian consolidation and power vacuum fill by Turkish intervention. The regional system is trapped in a zero-sum game: SDF abandonment enables Turkish expansion, which enables Assad return, which enables Russian/Iranian entrenchment. High suppression (0.72): no region-level actor can exit or reshape the dynamic once the US withdraws. This perspective sees the constraint as the overall geopolitical structure constraining all regional actors.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — At the civilizational/analytical level, the constraint reflects an immutable structural fact: small stateless nations cannot survive between great powers without a protector. The SDF's geographic position (between Turkey, Syria, Iran, Iraq) makes it inherently dependent on a patron. The US alliance was a contingent manifestation of this structural necessity. This perspective risks naturalizing a political choice (US commitment) as a natural law. The engine's false summit detector should flag this.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sdf_alliance_abandonment_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_sdf_alliance_abandonment_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_sdf_alliance_abandonment_2026, TR),
    TR >= 0.70.

:- end_tests(us_sdf_alliance_abandonment_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint exhibits strong unidirectional extraction over the interval. Initially (T=0, ε=0.35), the alliance functioned primarily as coordination: SDF provided ground forces, US provided air support, both benefited from counter-ISIS gains. By T=3 (ε=0.52), US withdrawal signals converted the arrangement into extraction: SDF bore increasing human costs while US signaled willingness to abandon. By T=6 (ε=0.68), extraction is near-maximal: SDF forced to hold ground without air cover or resupply while US maintains option to withdraw completely. The upward trajectory reflects accumulating asymmetry as US commitment becomes less credible. Suppression (0.72): Very high. The SDF faces multiple suppression vectors: Turkish military incursion, Assad regime consolidation, ISIS remnant activity, and now abandoned air support. No legal mechanisms, no diplomatic recourse, no coalition support. The SDF cannot appeal to international law (alliance commitments are informal), cannot retaliate (military vastly outmatched), cannot negotiate (Turkey and Assad see zero-sum advantage in military pressure). Suppression is structural, not circumstantial. Theater ratio (0.55): Moderate. Formal alliance structures remain in place (base agreements, command coordination, intelligence protocols) but enforcement has dissolved. The theater represents the gap between written commitments and actual military support. As withdrawal accelerates, the performative content increases — alliance norms become background noise rather than operational constraint.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence driven by exit options and structural position. The SDF forces (powerless/trapped) perceive pure snare — abandonment means death or displacement with no alternatives. Kurdish civilians (moderate/constrained) perceive snare with slightly different color: they have theoretical exit (migration) but abandonment makes it coercive (flee or face occupation). The US military apparatus (institutional/arbitrage) perceives tangled rope — the alliance provided coordination functions (ISIS counter, regional pressure) but also enabled extraction (cost-shifting, leverage against Turkey). From Ankara's perspective, the SDF-US alliance was itself a constraint on Turkish action; its dissolution is rope-like liberation. The analytical observer risks the mountain classification (stateless nations inherently dependent on patrons) but the engine's false summit detector should identify this as naturalization of a political choice. The perspectival gap reveals the constraint's character: it appears to each actor through the lens of their exit options and structural dependency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are computed from agent structural position, power level, and exit options. The SDF forces (victims, trapped exit) derive d ≈ 0.90-0.95, producing high f(d) ≈ 1.35-1.42, amplifying experienced extractiveness chi toward maximum. Kurdish civilians (victims, constrained exit) derive d ≈ 0.78-0.85, producing f(d) ≈ 1.15-1.25. The US institutional apparatus (beneficiary, arbitrage exit) derives d ≈ 0.15-0.25, producing f(d) ≈ -0.01 to 0.20, reducing experienced chi downward (the constraint costs the US far less than it costs the SDF). Turkish state (beneficiary of constraint removal, arbitrage exit) derives d ≈ 0.05-0.15, experiencing the alliance (not the abandonment) as constraint. No directionality overrides are required — the structural data produces the observed perspectival gap naturally through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED. This constraint avoids the mandatrophy trap by clearly separating pure extraction (snare) from mixed coordination-extraction (tangled rope) via agent perspective. The US military apparatus genuinely experiences the alliance as coordination (counter-ISIS, regional pressure, NATO solidarity) — this is not false labeling. Simultaneously, the SDF experiences abandonment as pure extraction with no protective function. Both are correct readings from their respective structural positions. The constraint resolves the mandatrophy by indexing: from the institutional US perspective, it's tangled rope; from the powerless SDF perspective, it's snare. No single type applies universally — the presheaf over observation sites is the answer. The mandatrophy_resolved flag indicates that the analysis has accounted for this multiplicity and does not claim a single 'true' type. The upward extractiveness trajectory (0.35 → 0.68) reflects that as the constraint transitions from active alliance to abandoned commitment, its character shifts from tangled rope toward pure snare. The final classification (snare) reflects the endpoint, not the entire arc.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abandonment_finality_vs_hedging,
    'Is US abandonment of the SDF a final structural break or a hedging strategy that preserves covert support and maintains residual leverage?',
    'Longitudinal tracking of US military presence, intelligence liaison continuation, covert financial flows, and cyber/drone support to SDF forces post-announced withdrawal',
    'If final: SDF constraint is a pure snare with no escape path (extractiveness increases to 0.78+). If hedged: constraint becomes tangled_rope with residual coordination function (extractiveness drops to 0.55-0.60). Classification hinges on whether abandonment is performative or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abandonment_finality_vs_hedging, empirical, 'Whether abandonment is final or maintains hedging support').

omega_variable(
    sdf_coalition_viability_without_us,
    'Can the SDF establish alternative patron relationships (Russia, Assad, Iran, or Arab coalition) that restore functional military capacity and reduce extraction/suppression?',
    'Monitoring of SDF diplomatic outreach, alliance formation attempts, and military capability restoration within 12-24 months post-abandonment. Measurement of weapons flows, training support, and security guarantees from alternative sources.',
    'If viable alternatives emerge: SDF constraint transitions from snare to tangled_rope or scaffold (temporary). If no alternatives: snare classification deepens and extractiveness remains high (0.68+). This determines whether abandonment is permanently terminal or transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sdf_coalition_viability_without_us, empirical, 'Whether SDF can establish viable alternative patron relationships').

omega_variable(
    turkish_occupation_stability_and_exit,
    'Does Turkish military occupation of northeast Syria create a sustainable extraction regime or a temporary military intervention that eventually withdraws (enabling SDF resurgence)?',
    'Analysis of Turkish administrative integration, settler-colonist population flows, economic investment in occupied territories, and historical precedent from previous Turkish interventions (1974 Cyprus, 1990-2007 Northern Iraq). Timeline projection for Turkish exit incentives.',
    'If occupation becomes permanent extraction: regional snare deepens. If Turkish withdrawal is likely within 5-10 years: the constraint cycles back to SDF-US dependency (or SDF-Russia dependency), changing the periodicity and duration of snare compression. Classification stability depends on Turkish commitment duration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(turkish_occupation_stability_and_exit, empirical, 'Whether Turkish occupation is permanent or temporary').

omega_variable(
    us_reengagement_threshold,
    'What future geopolitical shock (Israeli escalation, ISIS resurgence, Iran expansion, Russian consolidation) would trigger US re-engagement with SDF forces?',
    'Scenario analysis of regional contingencies and their alignment with US stated strategic interests. Monitoring of contingency planning documents, congressional positions, and forward-deployed military posture changes.',
    'If reengagement threshold is low (ISIS resurgence): the abandonment is performative and the constraint reverts to tangled_rope (extractiveness drops to 0.50-0.55). If threshold is high or nonexistent: snare classification holds indefinitely. This determines whether abandonment represents genuine structural change or a policy cycle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(us_reengagement_threshold, preference, 'Threshold for US military re-engagement with SDF').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_sdf_alliance_abandonment_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usdf_tr_t0, us_sdf_alliance_abandonment_2026, theater_ratio, 0, 0.4).
narrative_ontology:measurement(usdf_tr_t3, us_sdf_alliance_abandonment_2026, theater_ratio, 3, 0.48).
narrative_ontology:measurement(usdf_tr_t6, us_sdf_alliance_abandonment_2026, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(usdf_be_t0, us_sdf_alliance_abandonment_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usdf_be_t3, us_sdf_alliance_abandonment_2026, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(usdf_be_t6, us_sdf_alliance_abandonment_2026, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_sdf_alliance_abandonment_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, isis_resurgence_syria_iraq).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, turkish_kurdish_regional_conflict).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, iranian_expansion_regional).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, russian_consolidation_middle_east).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, nato_alliance_credibility).

% DUAL FORMULATION NOTE:
% This constraint is downstream of broader US strategic reorientation away from Middle East. The upstream constraint (us_middle_east_strategic_retrenchment) determines alliance abandonment policy. The SDF alliance abandonment has multiple downstream effects: ISIS operational freedom, Turkish expansion, regional power consolidation. Each downstream constraint has its own ε value reflecting domain-specific empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
