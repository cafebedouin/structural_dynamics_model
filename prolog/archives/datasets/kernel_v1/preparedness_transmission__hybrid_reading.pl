% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Preparedness Transmission Stratification: Engineering Competence vs. Coordination Decay
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   Preparedness transmission in civil defense systems exhibits a critical
 *   stratification: engineering infrastructure competence remains validated
 *   through regular inspection and maintenance (dams function, shelters are
 *   structurally sound, communication equipment is calibrated), but civilian
 *   coordination knowledge — evacuation procedures, inter-agency
 *   communication under stress, surge capacity activation protocols — has
 *   decayed over generational timescales. This constraint is ONE READING of a
 *   contested kernel: the preparedness_transmission kernel itself (the
 *   foundational claim that civil defense institutions maintain disaster
 *   response capacity through documented protocols and regular validation).
 *   The hybrid reading, instantiated here, maintains that this kernel is
 *   partially realized: the engineering layer is live and validated, but the
 *   coordination layer has become hollow. This reading coexists with two
 *   siblings: the competence reading (drills and inspections ARE live
 *   exercised knowledge) and the husk reading (the entire apparatus is
 *   performative memorial). The hybrid reading's structural signature is that
 *   infrastructure performs as designed but evacuation/coordination fails
 *   under stress — the D5 break exists in the coordination layer, not the
 *   physical layer. This creates the extractive pattern: civilians bear
 *   catastrophic risk while engineering authorities benefit from
 *   institutional prestige, and the asymmetry is obscured by the visible
 *   success of physical systems.
 *
 * KEY AGENTS:
 *   - Engineering Infrastructure Authority: Primary beneficiary (institutional/arbitrage) — infrastructure insulated from coordination accountability; maintains institutional prestige through visible competence.
 *   - Civilian Population: Primary victim (powerless/trapped) — structurally trapped in region; bears catastrophic risk from coordination failure while constraint extracts value from appearing prepared.
 *   - Emergency Management Practitioners: Secondary actor (moderate/constrained) — experience both genuine coordination function and extraction through knowledge gaps and liability.
 *   - Institutional Memory Holders / Senior Practitioners: Organized constraint bearer (organized/constrained) — possess tacit knowledge but unable to enforce transmission across generational boundary.
 *   - Formal Inspection and Drill Apparatus: Institutional actor (institutional/arbitrage) — maintains appearance of validation through process theater; passes because designed to be passable.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices (separate training, distinct accountability) as inherent limits of disaster response systems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.38).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.58).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Preparedness Transmission Stratification: Engineering Competence vs. Coordination Decay").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, 'ffc1ec9a-1102-43de-b129-7e71488ffd85').
narrative_ontology:cs_kernel_codification('ffc1ec9a-1102-43de-b129-7e71488ffd85', formalized).
narrative_ontology:cs_authority_grounding('ffc1ec9a-1102-43de-b129-7e71488ffd85', extraction).
narrative_ontology:cs_interpretation_layer_present('ffc1ec9a-1102-43de-b129-7e71488ffd85').
narrative_ontology:cs_reading_relation('ffc1ec9a-1102-43de-b129-7e71488ffd85', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ffc1ec9a-1102-43de-b129-7e71488ffd85', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_axiom('ffc1ec9a-1102-43de-b129-7e71488ffd85', foundational, preparedness_layer_stratification_empirically_real).
narrative_ontology:cs_axiom_status(preparedness_layer_stratification_empirically_real, holdable).
narrative_ontology:cs_axiom_grounding('ffc1ec9a-1102-43de-b129-7e71488ffd85', preparedness_layer_stratification_empirically_real, empirically_contingent).
narrative_ontology:cs_axiom('ffc1ec9a-1102-43de-b129-7e71488ffd85', foundational, institutional_insulation_enables_stratification).
narrative_ontology:cs_axiom_status(institutional_insulation_enables_stratification, holdable).
narrative_ontology:cs_axiom_grounding('ffc1ec9a-1102-43de-b129-7e71488ffd85', institutional_insulation_enables_stratification, empirically_contingent).
narrative_ontology:cs_reference_frame('ffc1ec9a-1102-43de-b129-7e71488ffd85', integrated_disaster_response_capability).
narrative_ontology:cs_drift_state('ffc1ec9a-1102-43de-b129-7e71488ffd85', contemporary_post_generational_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ffc1ec9a-1102-43de-b129-7e71488ffd85', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, engineering_infrastructure_maintainers).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, formal_inspection_agencies).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, civilian_evacuation_coordination).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, cross_institutional_communication).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, surge_capacity_activation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Structurally trapped in the coordination gap. Infrastructure performs (dams hold, shelters exist) but evacuation communication fails under actual stress. No exit from this geographic zone or social position. Maximum extraction of risk: civilians bear full catastrophic cost of coordination failure while the constraint extracts value from appearing prepared.
constraint_indexing:constraint_classification(preparedness_transmission__hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: EMERGENCY MANAGEMENT PRACTITIONERS (TANGLED ROPE) — Constrained by hierarchical authority and resource allocation but also benefit from the coordination framework during actual deployment. Experience both the genuine coordination function (protocols exist, trained cadre exists) and the extraction (knowledge gaps create liability, career risk when drills reveal brittleness, resource competition with engineering infrastructure).
constraint_indexing:constraint_classification(preparedness_transmission__hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENGINEERING INFRASTRUCTURE AUTHORITY (ROPE) — Net beneficiary. Experiences the constraint as pure coordination: their infrastructure is maintained, validated through inspection, and insulated from operational coordination failure by the categorical distinction between 'physical system' and 'coordination system'. They benefit from the institutional prestige of working infrastructure while bearing no responsibility for evacuation cascade failures.
constraint_indexing:constraint_classification(preparedness_transmission__hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL MEMORY HOLDERS / SENIOR PRACTITIONERS (TANGLED ROPE) — Organized but constrained by succession. They possess the tacit knowledge of coordination under stress and benefit from formal legitimacy. But they face extraction: their knowledge decays with retirement, institutional structures don't preserve operational context, and junior practitioners face perverse incentives (drill performance vs. real-world readiness are decoupled). Organized enough to see the decay but constrained by inability to enforce knowledge transmission across generational boundary.
constraint_indexing:constraint_classification(preparedness_transmission__hybrid_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL INSPECTION AND DRILL APPARATUS (PITON) — Persists through institutional inertia despite degraded operational function. Drills pass because they are designed to be passable; inspections validate infrastructure that functions; certification is issued. The theater (scheduled exercises, checklist compliance, after-action reports) continues at high ratio (0.64) while the actual operational knowledge transmission mechanism (real-world problem-solving under uncertainty, cross-institutional coordination under resource constraint) has atrophied. The apparatus maintains legitimacy through performance of process, not through actual preparedness validation.
constraint_indexing:constraint_classification(preparedness_transmission__hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN / FALSE SUMMIT CANDIDATE) — At civilizational timescale, there is a tempting mountain view: 'Complex systems always have coordination lags' and 'Tacit knowledge is always harder to transmit than physical specifications'. This naturalizes the stratification as an inherent limit to institutional memory. However, the structural data reveals beneficiaries (engineering authorities) and victims (civilian coordination) whose interests are asymmetrically aligned with the decay pattern. The false summit flags: the stratification is not an immutable property of disaster response systems but a contingent institutional arrangement where engineering prestige and resources are decoupled from coordination system validation.
constraint_indexing:constraint_classification(preparedness_transmission__hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(preparedness_transmission__hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(preparedness_transmission__hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The hybrid reading's extractiveness reflects that the engineering infrastructure genuinely functions (lowering extraction vs. pure snare) but the coordination system fails under stress (higher extraction than rope). The measurement trajectory (0.18 → 0.28 → 0.38) shows extraction accumulating as the older generation with tacit coordination knowledge retires and is not replaced. Suppression (0.58): Moderate-high and rising. Barriers to coordination knowledge transmission include: hierarchical institutional structures that don't preserve problem-solving context, separation of engineering and coordination training (personnel move between roles without cross-training), career incentives favoring formal certification over real-world learning, and resource concentration on infrastructure maintenance rather than coordination system validation. The trajectory (0.38 → 0.48 → 0.58) reflects increasing institutional lock-in as generations turn over and codification fails. Theater ratio (0.64): Moderate-high. Drills pass because they are designed to be passable; exercises use known parameters; after-action reports document compliance. The coordination knowledge tested in these drills (scripted evacuation routes, pre-assigned roles) is not the same as operational knowledge (problem-solving under uncertainty, real-time inter-agency coordination with resource constraints). The theater ratio rises (0.42 → 0.53 → 0.64) as the inspection apparatus becomes increasingly performative and decoupled from real disaster validation.
 *
 * PERSPECTIVAL GAP:
 *   The engineering authority and civilian population perceive the same constraint as fundamentally different types. Engineering sees infrastructure coordination working well (rope: drills pass, inspections validate, systems function). Civilians see catastrophic exposure from coordination failure (snare: trapped in region, no exit from evacuation lag, bearing full risk). Emergency management practitioners occupy the diagnostic middle ground (tangled_rope): they experience both the coordination function (protocols, training, formal authority) and the extraction (knowledge gaps, succession failures, performance pressure). The formal inspection apparatus sees its own process as validated (piton: maintains legitimacy through theater). Senior practitioners see the decay clearly but constrained by generational turnover (tangled_rope: organized but constrained). The analytical observer at civilizational scale risks naturalizing this as an inherent property of disaster response systems (mountain view: coordination lags are always harder to maintain than physical infrastructure) — but this naturalizes a contingent institutional choice (separate training systems, decoupled accountability) as an inherent limit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows from beneficiary/victim declarations and exit options. Engineering infrastructure maintainers are beneficiaries with arbitrage-level exit (institutional power, can relocate roles, infrastructure investment is portable). Their derived d ≈ 0.15 produces f(d) ≈ -0.01, making their effective extraction negative (they subsidize the system). Formal inspection agencies are beneficiaries with arbitrage exit (institutional); same d derivation. Civilian evacuation coordination is a victim with trapped exit (powerless, geographic and economic dependence); d ≈ 0.92, f(d) ≈ 1.42, maximum effective extraction. Cross-institutional communication is a victim with constrained exit (moderate power — agencies exist but lack horizontal coordination mechanisms); d ≈ 0.68, f(d) ≈ 1.02. Surge capacity activation is a victim with trapped exit (depends on prior knowledge transfer that has decayed); d ≈ 0.88, f(d) ≈ 1.35. No directionality overrides needed — the structural data produces coherent perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resolves the mandatrophy by stratifying preparedness across layers. The competence reading emphasizes repeated validation through drills — coordination knowledge is exercised. The husk reading emphasizes that drills are theater — knowledge has decayed. The hybrid reading specifies WHERE the boundary lies: engineering layer is competence-reading (live, validated), coordination layer is husk-reading (hollow, performative). This is not contradictory — it is a structural statement about institutional asymmetry. The mandatrophy is resolved by recognizing that 'preparedness' is not a monolithic property but a stratified one. The constraint prevents uniform assessment; instead, it requires perspectival measurement: ask whether infrastructure functions (engineering: yes), and ask whether civilian coordination works under stress (coordination: no). The tangled_rope classification reflects this mixture: genuine coordination exists (protocols, training) alongside genuine extraction (knowledge decay, risk asymmetry, institutional insulation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_vs_codified_transmission,
    'Is the coordination knowledge decay due to tacit knowledge transmission limits (inherent property of institutional memory) or to deliberate institutional choices that insulate engineering from coordination accountability?',
    'Comparative analysis: systems that mandate joint engineering-coordination drills vs. systems that separate them; measurement of coordination failure rates as function of training integration; post-disaster analysis of whether coordination gaps correlate with infrastructure design or training architecture decisions.',
    'If inherent: mountain or rope reading dominates (coordination lag is natural). If deliberate: tangled_rope or snare reading dominates (stratification is extraction mechanism). Classification outcome depends entirely on this omega''s resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_vs_codified_transmission, empirical, 'Whether coordination decay is structural or institutional choice').

omega_variable(
    engineering_infrastructure_insulation,
    'Does maintaining separate accountability and training for engineering vs. coordination systems reflect genuine technical decoupling (different experts, different failure modes) or institutional insulation (engineering prestige and resources protected from coordination performance accountability)?',
    'Design analysis of actual disaster response systems; examination of whether coordination failures trigger engineering re-evaluation; resource allocation tracking for infrastructure maintenance vs. coordination training over 20+ year cycles; post-disaster official investigations identifying whether coordination-layer fixes trigger infrastructure redesign.',
    'If decoupling is technical: engineering extraction is coordination cost (rope reading). If insulation is institutional: the stratification enables engineering to maintain institutional status while coordination bears risk (tangled_rope to snare reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engineering_infrastructure_insulation, empirical, 'Whether engineering-coordination separation is technical necessity or institutional insulation').

omega_variable(
    drill_operationality_coupling,
    'Do drills that pass formal inspection criteria actually validate the coordination knowledge required for real disaster response, or do they validate only the ability to perform scripted exercises?',
    'Comparison of drill performance (success rates on scheduled exercises with known parameters) vs. real disaster performance (actual evacuation completeness, communication reliability, surge capacity activation) in same jurisdiction; identification of failures that occurred despite passing pre-disaster drills.',
    'If coupled: drills are live knowledge validation (competence reading, rope). If decoupled: drills are theater (husk reading, piton). Hybrid reading occupies the middle ground: drills validate infrastructure but not coordination, creating false confidence in overall preparedness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(drill_operationality_coupling, empirical, 'Whether formal drills validate actual disaster response capability').

omega_variable(
    successor_generation_tacit_knowledge_loss,
    'What proportion of operational knowledge degradation is due to unreplaceable tacit knowledge lost with retiring practitioners (inherent transmission limit) vs. institutional failure to codify or transfer that knowledge (institutional choice)?',
    'Oral history and institutional memory interviews; comparison of knowledge codification efforts (training manuals, decision trees, scenario libraries) between high-performing and low-performing coordination systems; measurement of time-to-competence for new practitioners as function of mentorship and codification intensity.',
    'If inherent: preparedness transmission of coordination knowledge is necessarily stratified (mountain or rope reading accommodates it). If institutional choice: the decay is extractive insulation mechanism (tangled_rope to snare reading becomes dominant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(successor_generation_tacit_knowledge_loss, empirical, 'Tacit knowledge loss as inherent vs. institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_hybrid_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prep_hybrid_tr_t7, preparedness_transmission__hybrid_reading, theater_ratio, 7, 0.53).
narrative_ontology:measurement(prep_hybrid_tr_t15, preparedness_transmission__hybrid_reading, theater_ratio, 15, 0.64).

% Extraction over time
narrative_ontology:measurement(prep_hybrid_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(prep_hybrid_be_t7, preparedness_transmission__hybrid_reading, base_extractiveness, 7, 0.28).
narrative_ontology:measurement(prep_hybrid_be_t15, preparedness_transmission__hybrid_reading, base_extractiveness, 15, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(prep_hybrid_su_t0, preparedness_transmission__hybrid_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(prep_hybrid_su_t7, preparedness_transmission__hybrid_reading, suppression_requirement, 7, 0.48).
narrative_ontology:measurement(prep_hybrid_su_t15, preparedness_transmission__hybrid_reading, suppression_requirement, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).

% DUAL FORMULATION NOTE:
% The preparedness_transmission kernel has three reading-based decompositions. The hybrid reading is the middle position: engineering layer remains competent (supporting competence_reading for physical systems), but coordination layer is hollow (supporting husk_reading for evacuation/communication). The three constraint stories share the same kernel but have different ε values (0.22, 0.52, 0.38) and different perspectival distributions because they emphasize different measurement domains (infrastructure performance, drill realism, cross-institutional coordination). All three remain live positions in disaster management discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
