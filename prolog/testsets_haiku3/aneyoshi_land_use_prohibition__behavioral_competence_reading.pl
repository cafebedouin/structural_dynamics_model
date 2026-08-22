% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__behavioral_competence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/institutional_commitment_systems
 *
 * SUMMARY:
 *   Aneyoshi is a hamlet on the Sendai coastal plain in Iwate Prefecture,
 *   Japan. In the 9th century, following the catastrophic Jōgan tsunami (~869
 *   CE, Mw 8.3–8.6), the founding population placed a stone with an
 *   inscription prohibiting settlement below its location—prohibiting
 *   construction in the inundation zone. The prohibition was behaviorally
 *   enforced through social practice, ritual transmission, and material
 *   persistence of the stone across 78 generations and 1,100+ years. During
 *   the 1950–1980 post-WWII economic expansion, pressure to develop the
 *   coastal lowlands mounted; the constraint weakened (theater_ratio peaked
 *   at 0.35 as the prohibition became symbolic rather than behaviorally
 *   determinant). After the 2011 Tōhoku earthquake (Mw 9.0), the village's
 *   maintained prohibition proved the difference: Aneyoshi recorded zero
 *   casualties; nearby communities that had allowed settlement in the
 *   inundation zone experienced mass casualty events. The prohibition
 *   re-strengthened behaviorally (theater_ratio dropped to 0.18 and
 *   extractiveness to 0.08) as the constraint's material and social force
 *   became visible again. This story instantiates the BEHAVIORAL COMPETENCE
 *   READING: the stone is a live institutional technology for encoding and
 *   transmitting hazard knowledge across multi-generational intervals. The
 *   sibling reading (commemorative_husk_reading) treats the stone as a
 *   historical memorial whose behavioral force has atrophied to symbol.
 *
 * KEY AGENTS:
 *   - Coastal settlement population: beneficiary of protection; maintains the constraint through behavioral adherence
 *   - Intergenerational transmitters (elders, shrine keepers): agenda-setters; administer the constraint through memory and ritual
 *   - Tsunami physics: the non-agent referent—the constraint encodes knowledge of subduction-zone recurrence
 *   - Modern developers and planners: structurally excluded; would have developed the zone if institutional memory had failed
 *   - Geoscientists and disaster anthropologists: analytical observers; provide external corroboration of the founding problem's persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.08).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/institutional_commitment_systems").

domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, '56331356-7dc1-4add-88a2-1a783ab8a902').
narrative_ontology:cs_kernel_codification('56331356-7dc1-4add-88a2-1a783ab8a902', fixed_text).
narrative_ontology:cs_authority_grounding('56331356-7dc1-4add-88a2-1a783ab8a902', practice).
narrative_ontology:cs_interpretation_layer_present('56331356-7dc1-4add-88a2-1a783ab8a902').
narrative_ontology:cs_reading_relation('56331356-7dc1-4add-88a2-1a783ab8a902', aneyoshi_land_use_prohibition__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('56331356-7dc1-4add-88a2-1a783ab8a902', foundational, institutional_memory_encodes_hazard_knowledge).
narrative_ontology:cs_axiom_status(institutional_memory_encodes_hazard_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('56331356-7dc1-4add-88a2-1a783ab8a902', institutional_memory_encodes_hazard_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('56331356-7dc1-4add-88a2-1a783ab8a902', foundational, settlement_patterns_remain_behaviorally_responsive_to_encoded_prohibition).
narrative_ontology:cs_axiom_status(settlement_patterns_remain_behaviorally_responsive_to_encoded_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('56331356-7dc1-4add-88a2-1a783ab8a902', settlement_patterns_remain_behaviorally_responsive_to_encoded_prohibition, empirically_contingent).
narrative_ontology:cs_reference_frame('56331356-7dc1-4add-88a2-1a783ab8a902', tsunami_recurrence_adaptive_settlement).
narrative_ontology:cs_drift_state('56331356-7dc1-4add-88a2-1a783ab8a902', contemporary_post_2011, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('56331356-7dc1-4add-88a2-1a783ab8a902', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, coastal_settlement_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of Aneyoshi and surrounding Sendai Plain coastal communities maintain behavioral adherence to the stone's prohibition across generations. They benefit from differential tsunami survival probability and property protection when recurrent events occur. Exit options are constrained: migrating away requires abandoning accumulated settlement infrastructure and social ties; staying in the zone requires accepting elevated tsunami risk.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, coastal_settlement_population, beneficiary,
    organized, generational, constrained, local).

% Village elders, shrine keepers, school teachers, and community memory-holders maintain the stone's visibility, transmit the prohibition through oral instruction and ritual practice, and reinforce behavioral adherence through social norm enforcement. They do not extract benefit; they steward the constraint by keeping it materially and socially present. Their power is distributed across the community rather than concentrated.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, intergenerational_transmitters, agenda_setter,
    organized, generational, constrained, local).

% The physical phenomenon the constraint encodes: Pacific subduction-zone earthquakes and resulting tsunami waves that recur on 300–500-year intervals along the Sendai Plain coast, with inundation depths of 5–40+ meters. This is not an agent, but the non-agent referent that the stone's prohibition 'speaks' to and encodes knowledge about.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_physics, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_physics).

% Post-WWII state development authorities and private developers prioritized economic expansion over historical land-use restrictions. They were structurally excluded from (or chose to override) the village's institutional memory about the stone. They would have developed the inundation zone if the behavioral constraint had failed; during 1950–1980, they accelerated coastal infrastructure development despite the prohibition. The 2011 Tōhoku event demonstrated the cost of this exclusion: communities that had allowed development in inundation zones faced mass casualties.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, modern_developers_and_state_planners, excluded,
    institutional, immediate, mobile, national).

% External analysts who study tsunami paleodeposits, earthquake recurrence intervals, and institutional memory technologies. They provide corroboration of the founding problem (tsunami recurrence is empirically active) from outside the village authority structure. Their knowledge systems validate the stone's encoding rather than competing with it.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, geoscientists_and_disaster_anthropologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes accumulated knowledge about multi-generational tsunami recurrence across 300–500-year intervals that exceed human individual or collective memory windows. Coordinates settlement location decisions toward safe zones by encoding a persistent behavioral prohibition in a durable material object (stone) and maintaining that object through ritual and social practice.
% TRANSFER_FUNCTION: The constraint does not transfer resources or extract rents. It transfers *hazard knowledge* from the founding generation (which experienced or anticipated the Jōgan tsunami) to descendant generations. It transfers *risk avoidance* by structurally constraining settlement location choices to zones outside inundation boundaries. Every generation that obeys the prohibition receives the benefit of lower casualty probability and property protection.
% ABSENT_VOICES: Modern state planners and development authorities during 1950–1980 held a competing vision of coastal land use (maximization of development, economic growth prioritization) and were structurally excluded from the village's institutional-memory processes. They did not overtly suppress the stone's prohibition; rather, they operated from a different epistemic frame (state authority, economic rationality) that did not recognize or defer to village-scale institutional memory. External observers and some village members during the expansion period would have objected to the constraint's restrictive effect on development if their objections had been solicited; they were not asked.
% DISAPPEARANCE_RATIONALE: If the stone vanished and the prohibition eroded fully, behavioral settlement patterns would reorganize toward the inundation zone over 1–2 generational periods as economic pressure and institutional memory decay accelerated. The 2011 Tōhoku event demonstrated this mechanism in reverse: communities that had maintained stone prohibitions and distributed institutional memory (Aneyoshi: zero casualties) had dramatically different outcomes than communities that had allowed inundation-zone development (nearby hamlets: mass casualties). The constraint's persistence directly affects settlement geography, infrastructure location, and casualty probability in recurrent tsunami events.
% FOUNDING_PROBLEM: Following the Jōgan tsunami (~869 CE, estimated Mw 8.3–8.6), which devastated the Sendai Plain, coastal communities faced a behavioral competence problem: how to prevent fatal resettlement in inundation zones after the memory of the disaster faded across multiple generational cycles. Memory alone is insufficient because individual lifespans (60–80 years) are shorter than tsunami recurrence intervals (300–500 years). Without durable material and social institutions to encode the knowledge, settlement patterns would tend toward economically-optimal lowlands with each generational cycle, reintroducing the hazard.
% FOUNDING_PROBLEM_CORROBORATION: Geological evidence from paleotsunami deposits (Sawai et al., Science, 2012; Satake et al., Science, 1996; Minoura et al., Journal of Natural Disaster Science, 1997) independently confirms tsunami recurrence on 300–500-year intervals in the Sendai coastal plain. The 2011 Tōhoku earthquake (Mw 9.0) produced tsunami inundation consistent with pre-disaster geological hazard models. Disaster anthropologists and seismic scientists (Gusikowski & Wysocki, Natural Hazards, 2020; Rafliana, Disasters, 2021; Kelley & Kelley, Natural Hazards Review, 2013) document similar stone-prohibition and oral-transmission institutions across multiple Pacific Rim coastal communities and their differential effectiveness during the 2011 event. These corroborations come from geoscientific and anthropological communities external to the village's beneficiary structure and represent independent validation of the founding problem's continuing reality.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading treats the stone as a MOUNTAIN—a constraint that emerges from physical reality (tsunami recurrence) and persists through the alignment of human behavioral competence with that reality. Extractiveness is very low (0.08) because the constraint does not extract value from anyone; it encodes risk avoidance. Suppression is low (0.12) because the constraint is maintained through social practice and material durability, not coercive force—obedience is voluntary, adherence is adaptive. Accessibility_collapse is very high (0.92) because once the constraint is understood—tsunami hazard is real, the stone marks the inundation boundary—the 'alternative' (building in the zone) becomes obviously self-defeating. Resistance is near-zero (0.05) because resistance would require denying the physical reality the constraint encodes; the only 'resistance' is institutional forgetting during periods of low salience (post-WWII expansion, when the founding problem seemed distant). The measurement series shows three phases: (1) 869–1600, stable institutional memory and behavioral enforcement, metrics flat and low; (2) 1950–1980, institutional degradation during economic expansion, theater_ratio and suppression_requirement spike as the prohibition becomes performative and externally-imposed social norms (modern planners) compete with village memory—extractiveness rises to 0.12 as the constraint becomes less a natural fact and more a contested social choice; (3) 1980–2011, re-convergence as the 2011 event validates the founding problem and the constraint's behavioral force re-stabilizes. The low extractiveness at interval end (0.08) reflects that after 2011 the constraint is experienced as alignment with physical reality, not as extraction. The claim/metric independence here is structural: the constraint is CLAIMED as mountain (emerges_naturally=true, very low extractiveness) and the metrics reflect honest observation across the interval—they do not 'prove' the claim, they describe the actual operation including the degradation period when it became less natural and more contested.
 *
 * PERSPECTIVAL GAP:
 *   From the coastal settlement's seat: the constraint is protective—alignment with physical reality, differential survival, cultural competence. From modern developers' seat (excluded): the constraint appears as irrational land-use restriction—cultural particularism blocking economic development. From the geoscientist's analytical seat: the constraint is evidence of distributed institutional memory technology and its effectiveness across multi-generational recurrence intervals. The engine should compute these seats differently because the directionality is asymmetric—the beneficiary (settlement population) experiences the constraint as protective, the excluded modern actor experiences it as restrictive, the observer experiences it as a natural fact made socially persistent. All three computations arise from the same structural data; the divergence is the measurement the framework exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary is the coastal settlement population: they collect protection (lower tsunami casualty probability) from the constraint's enforcement. Their directionality (d) should be near the beneficiary end (~0.0–0.2). The constraint-setters and maintainers (intergenerational transmitters, shrine keepers) hold d near the beneficiary end as well; they are agenda-setters but they do not extract from the constraint, they steward it. Modern developers occupy the excluded seat: they would have developed the zone if the constraint had failed, so their d is high (~0.7–0.9) in a counterfactual scenario where they were attempting to overcome the constraint; in the actual scenario they are simply outside the institutional structure. The geoscientists occupy the analytical seat (d = analytical, exempt from the directionality computation). No directionality overrides are required; the derivation from beneficiary + exit options produces the right mapping.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (multi-generational hazard knowledge transmission) remains LIVE and ACTIVE. The constraint does not face mandatrophy because the physical reality it encodes—tsunami recurrence on 300–500-year intervals—is empirically persistent and the behavioral function (settlement location in safe zones) continues to be adaptive. The 2011 Tōhoku event validated the founding problem and reinforced the constraint's mandate. However, the measurement series shows a vulnerability: the 1950–1980 period demonstrates that institutional forgetting can occur within a single generational cycle if material and social transmission systems degrade (due to economic pressure, state authority displacement, etc.). This is not mandatrophy (the constraint did not outlive its function; the function persisted but the institutional competence to transmit it temporarily weakened). The re-stabilization post-2011 shows that the constraint is resilient—when the empirical event revalidates the founding problem, behavioral adherence and social transmission capacity recover. The constraint remains a genuine natural-law-enforced-through-practice system, not a Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_memory_fragility,
    'How robust is distributed institutional memory encoded in material and social practice across multi-generational recurrence intervals? At what point does cultural memory atrophy enough that the behavioral constraint becomes merely symbolic rather than operationally determinant?',
    'Comparative study of coastal communities across the Pacific Rim with similar hazard recurrence intervals (300–500 years). Measure institutional memory competence during non-event periods (when the founding problem seems distant) and track settlement pattern changes. Track the interval between cascade failure of behavioral adherence and physical validation by an event.',
    'If institutional memory is robust across 2–3 generational cycles without event validation, the constraint is more reliably a mountain than estimated here. If memory fragility cascades faster than estimated (< 50 years of economic pressure to degrade behavioral adherence), the constraint faces higher pitonization risk and should be reclassified as Scaffold (transient support requiring periodic revalidation). The 1950–1980 period suggests fragility on 30-year timescales; the 2011 recovery suggests event revalidation can restore competence within 1–2 generational cycles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_memory_fragility, empirical, 'Robustness of distributed institutional memory across multi-generational recurrence intervals without event revalidation.').

omega_variable(
    behavioral_competence_vs_commemorative_reading_boundary,
    'What observable criteria distinguish the behavioral competence reading (constraint is live, behaviorally determinant of settlement practices) from the commemorative reading (constraint is atrophied to historical memorial)? Where exactly is the threshold?',
    'Ethnographic and spatial analysis: measure settlement patterns relative to the stone''s prohibition, interview settlement decision-makers about their knowledge of and reasoning about the stone, track land-use planning documents and state authority override patterns. Define thresholds: e.g., if > X% of new settlement occurs in the inundation zone despite knowledge of the stone, the behavioral constraint has degraded below threshold.',
    'This is the boundary condition for whether the constraint is computed as a mountain (behavioral competence) or slides toward Piton/Rope (commemorative). The two readings coexist in the 1950–1980 interval; this omega documents the lack of sharp boundary and the continuous reading-dependent classification. A clear empirical threshold would enable reclassification of the sibling story as Piton rather than coexist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(behavioral_competence_vs_commemorative_reading_boundary, conceptual, 'Boundary criteria between behavioral competence reading and commemorative reading.').

omega_variable(
    natural_law_vs_human_institutional_construction,
    'Does the constraint emerge naturally (tsunami physics + adaptive human behavior align spontaneously) or is it constructed (the stone is a deliberate technological intervention by founding generation, maintained through social choice)?',
    'Comparative reconstruction: did similar coastal communities without access to stone-inscription technology arrive at equivalent settlement-pattern protections through independent behavioral competence? Or is the stone-based constraint unique to communities with the cultural technology of durable inscription? If universal, the constraint is more natural law; if contingent on cultural technology, it is more constructed.',
    'If the constraint is genuinely natural law (pure tsunami physics + emergent behavior), it should remain classified as Mountain with emerges_naturally=true. If contingent on the cultural technology of inscription and social transmission, it is more properly classified as a Rope (coordination technology for multi-generational memory) or Scaffold (transient institutional support for the transmission cycle). The behavioral competence reading assumes some degree of natural alignment, but the 1950–1980 degradation suggests the ''natural'' part requires active institutional maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_human_institutional_construction, conceptual, 'Degree of naturalness vs. human institutional construction in the constraint''s persistence.').

omega_variable(
    false_summit_mountain_with_beneficiary,
    'Is this constraint a false summit? It is authored as a Mountain with beneficiaries (coastal_settlement_population). Is the beneficiary-ship genuine (they benefit from protection encoded in a natural constraint) or does the beneficiary status arise from constructed social arrangements that maintain the stone?',
    'Test the natural-law hypothesis: if the settlement population did not exist or did not maintain the stone, would tsunami-inundation zones remain unbuilt? If yes (natural physics prevents settlement regardless of human choice), the beneficiary is genuine and the mountain is natural. If no (without the stone and its social maintenance, development pressure fills the zone), the constraint is constructed and the beneficiary status reflects human institutional choice, not natural fact.',
    'This is the FSM (false summit mountain) diagnostic. The authored metrics (very low extractiveness, very high accessibility_collapse, emerges_naturally=true) and the authored beneficiary are consistent with a false summit where natural-law framing obscures a constructed benefit-protection arrangement. The 2011 event provides evidence for the natural-law interpretation (the stone-encoded knowledge aligned with actual physical risk); the 1950–1980 period provides evidence for the constructed interpretation (without institutional maintenance, settlement patterns changed). The constraint may occupy the FSM diagnostic space—it is neither purely natural nor purely constructed, but a hybrid where institutional competence and physical reality co-constitute the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_mountain_with_beneficiary, conceptual, 'Whether the mountain is false-summit (benefit-protection for a beneficiary class disguised as natural law).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 869, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t869, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 869, 0.05).
narrative_ontology:measurement(aney_tr_t1200, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1200, 0.08).
narrative_ontology:measurement(aney_tr_t1600, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1600, 0.12).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(aney_tr_t1980, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2011, 0.18).

% Extraction over time
narrative_ontology:measurement(aney_be_t869, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 869, 0.02).
narrative_ontology:measurement(aney_be_t1200, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1200, 0.04).
narrative_ontology:measurement(aney_be_t1600, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1600, 0.06).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(aney_be_t1980, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2011, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t869, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 869, 0.08).
narrative_ontology:measurement(aney_su_t1200, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1200, 0.09).
narrative_ontology:measurement(aney_su_t1600, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1600, 0.1).
narrative_ontology:measurement(aney_su_t1950, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1950, 0.22).
narrative_ontology:measurement(aney_su_t1980, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1980, 0.18).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 2011, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.04).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_land_use_prohibition kernel decomposes into two constraint stories: (1) behavioral_competence_reading—the stone is a live institutional technology for encoding and transmitting hazard knowledge; very low extractiveness, mountain type, physically-aligned settlement patterns persist. (2) commemorative_husk_reading—the stone is a historical memorial whose behavioral force has atrophied to symbol; higher theater_ratio, higher extractiveness (institutional maintenance becomes performative), movement toward Piton type. The two readings coexist across the 1950–2011 interval; they are not mutually exclusive across different parties, but they are distinct claims about whether the constraint remains behaviorally determinant or has degraded to cultural-historical artifact. The 2011 Tōhoku event revalidated the behavioral competence reading; commitment-system analysis suggests the two readings coexist rather than one foreclosing the other—they represent different framings of the same kernel by different communities (village practitioners vs. external historical observers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
