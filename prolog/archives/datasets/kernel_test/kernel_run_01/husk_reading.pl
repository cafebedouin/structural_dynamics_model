% ============================================================================
% CONSTRAINT STORY: husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_husk_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: husk_reading
 *   human_readable: Preparedness as Institutional Husk: Memory Retained, Competence Atrophied
 *   domain: disaster_preparedness/institutional_memory/water_management
 *
 * SUMMARY:
 *   The husk reading of preparedness retention posits that institutional
 *   memory becomes decoupled from actual competence when knowledge is
 *   embedded in infrastructure, procedures, and memorial rituals rather than
 *   maintained through continuous expert practice. The constraint operates at
 *   the intersection of water management systems, disaster preparedness, and
 *   bureaucratic continuity. A water authority or regional emergency
 *   management agency retains formal procedures, infrastructure
 *   documentation, regular drills, and institutional records that memorialize
 *   'how we prepare.' But as field expertise retires, as personnel rotate
 *   through desk positions, as procedure-following replaces judgment, the
 *   actual capacity to respond decays while the institutional form persists.
 *   This creates a tangled_rope structure: genuine coordination function
 *   (procedures enable handoff across personnel changes; drills maintain
 *   institutional rhythm) combined with asymmetric extraction (institutions
 *   benefit from legitimacy claims of preparedness while communities bear the
 *   cost of false confidence; field experts lose professional autonomy;
 *   actual response capacity erodes). The theater_ratio trajectory (0.62 →
 *   0.82 over the interval) reflects increasing performativity: drills become
 *   compliance rituals, procedures become unfamiliar documents,
 *   infrastructure encodes obsolete knowledge. This reading is one
 *   instantiation of the contested kernel 'preparedness_retention.' The
 *   sibling 'competence_reading' claims preparedness is genuinely retained
 *   through continuous expert practice and knowledge transfer; procedures are
 *   secondary scaffolding, not the primary repository.
 *
 * KEY AGENTS:
 *   - Affected Community: Primary victim (powerless/trapped) — geography-bound, dependent on regional systems, experiences false preparedness confidence leading to real vulnerability
 *   - Field Technician/First Responder: Secondary victim (moderate/constrained) — employed within the system, benefits from job security (rope function) but bears extraction as expertise is devalued in favor of procedure-following
 *   - Water Authority Administration: Primary beneficiary (institutional/arbitrage) — captures legitimacy benefit from demonstrating preparedness; reduces costs by storing memory in infrastructure rather than expert staff
 *   - Institutional Memory Archive: Institutional actor (institutional/mobile) — the formal procedures, manuals, drills themselves; exist as piton (performative continuity without functional depth)
 *   - Regulatory Authority: Secondary institutional actor (institutional/constrained) — mandates preparedness; constrained by need to verify via measurable standards (procedures, drills) rather than competence assessment
 *   - International Resilience Coalition: Organized advocate (organized/constrained) — recognizes husk problem; building scaffolds (continuous training, knowledge transfer, decision support) with sunset logic
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks naturalizing the husk as inherent institutional law rather than as contingent design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(husk_reading, 0.58).
domain_priors:suppression_score(husk_reading, 0.65).
domain_priors:theater_ratio(husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(husk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(husk_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(husk_reading, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(husk_reading, tangled_rope).
narrative_ontology:human_readable(husk_reading, "Preparedness as Institutional Husk: Memory Retained, Competence Atrophied").
narrative_ontology:topic_domain(husk_reading, "disaster_preparedness/institutional_memory/water_management").

domain_priors:requires_active_enforcement(husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(husk_reading, 'fae26e82-c30f-47d2-bf14-5887a1b64ec5').
narrative_ontology:cs_created_at('fae26e82-c30f-47d2-bf14-5887a1b64ec5', '').
narrative_ontology:cs_kernel_codification('fae26e82-c30f-47d2-bf14-5887a1b64ec5', distributed).
narrative_ontology:cs_authority_grounding('fae26e82-c30f-47d2-bf14-5887a1b64ec5', extraction).
narrative_ontology:cs_kernel_id(husk_reading, preparedness_retention).
narrative_ontology:cs_reading_relation('fae26e82-c30f-47d2-bf14-5887a1b64ec5', preparedness_retention_competence_reading, coexists_with).
narrative_ontology:cs_axiom('fae26e82-c30f-47d2-bf14-5887a1b64ec5', foundational, competence_locus_is_institutional_infrastructure).
narrative_ontology:cs_axiom_status(competence_locus_is_institutional_infrastructure, holdable).
narrative_ontology:cs_axiom('fae26e82-c30f-47d2-bf14-5887a1b64ec5', foundational, retention_means_preservation_of_institutional_form).
narrative_ontology:cs_axiom_status(retention_means_preservation_of_institutional_form, holdable).
narrative_ontology:cs_reference_frame('fae26e82-c30f-47d2-bf14-5887a1b64ec5', bureaucratic_continuity_framework).
narrative_ontology:cs_drift_state('fae26e82-c30f-47d2-bf14-5887a1b64ec5', contemporary_high_turnover_era, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(husk_reading, institutional_legitimacy).
narrative_ontology:constraint_beneficiary(husk_reading, bureaucratic_continuity).
narrative_ontology:constraint_victim(husk_reading, actual_response_capacity).
narrative_ontology:constraint_victim(husk_reading, field_expertise).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE AFFECTED COMMUNITY (SNARE) — Trapped by geography and economic dependence on regional water systems. Preparedness drills provide performative reassurance ('the system is ready') while actual field competence atrophies. High suppression: no exit from the region, no alternative water infrastructure, no voice in institutional procedures. The community bears the extraction (false confidence leading to real vulnerability) with no escape.
constraint_indexing:constraint_classification(husk_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE FIELD TECHNICIAN (TANGLED ROPE) — Constrained by employment in the water authority or emergency services. Benefits from job security and institutional procedures (the rope function: coordination of response protocols). But also bears extraction: procedures become ritual, field expertise is bypassed during actual crises because protocols assume competence that has atrophied, and the technician's real knowledge is devalued in favor of 'following procedure.' Moderate power and constrained exit produce mixed experience.
constraint_indexing:constraint_classification(husk_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE WATER AUTHORITY ADMINISTRATION (ROPE) — Experiences preparedness infrastructure and memorial rituals as pure coordination. The formal procedures, infrastructure documentation, and drill schedules solve the collective action problem of maintaining institutional knowledge across personnel turnover. The authority can arbitrage between competing legitimacy claims (demonstrating readiness via drills while cutting expert staff). Benefits from the constraint: institutional memory is retained in infrastructure and procedures, not in people, reducing wage costs and enabling bureaucratic flexibility.
constraint_indexing:constraint_classification(husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE INSTITUTIONAL MEMORY ARCHIVE (PITON) — The formal procedures, manuals, infrastructure diagrams, and memorial rituals exist and are maintained, but their function has atrophied. The archive is a shadow of itself: documents are updated per regulatory requirement, but nobody reads them; drills are conducted on schedule, but they teach procedure, not expertise; the infrastructure 'remembers' the technical design, but the living practitioners have moved to desk positions or retired. Theater ratio is high (0.78): maintenance rituals persist (documents filed, drills executed, procedures reviewed) while actual preparedness function degrades. The institutional memory is performative continuity, not operative competence.
constraint_indexing:constraint_classification(husk_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE INTERNATIONAL DISASTER RESILIENCE COALITION (SCAFFOLD) — Organized advocates (UNISDR, Red Cross, resilience consulting firms) recognize the husk problem and are building alternative pathways: continuous training cycles, knowledge transfer protocols, distributed expertise networks, real-time decision support systems. These are scaffolds with sunset logic — as communities develop independent technical capacity and decision-support tools mature, dependence on ritualized institutional memory decreases. This perspective sees the husk as a temporary coordination failure being actively solved.
constraint_indexing:constraint_classification(husk_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE REGULATORY AUTHORITY (TANGLED ROPE) — The regulatory body (national water ministry, emergency management agency) mandates preparedness and oversees compliance. Benefits from institutional procedures: they standardize what 'preparedness' means, enabling centralized auditing and accountability claims. But also constrained by the mandate itself: regulators must verify preparedness via infrastructure, procedures, and drills because direct competence assessment is expensive and politically fraught. The constraint is both coordinating (defining measurable standards) and extractive (those standards decouple from actual capacity).
constraint_indexing:constraint_classification(husk_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, this reading naturalizes the constraint as an inherent limit: 'Institutional memory necessarily decays as people retire; infrastructure necessarily encodes only what was explicitly documented; ritual is the natural mode by which institutions preserve form across generations.' This perspective treats the husk as a law of institutional physics. However, the structural data contradicts the mountain classification — the engine's false summit detector will identify this as naturalization. The husk is not inevitable; it is a design choice (storing memory in infrastructure rather than people, measuring preparedness by procedure compliance rather than outcome validation).
constraint_indexing:constraint_classification(husk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(husk_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(husk_reading, TR),
    TR >= 0.70.

:- end_tests(husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint embodies genuine extraction because institutions benefit from preparedness claims while actual response capacity decays. The extraction is not total (procedures do provide some coordination value) but substantial (institutions can claim preparedness and reduce expert-staff costs while communities bear vulnerability risk). The trajectory shows increasing extraction (0.38 → 0.62 over 15 years) as procedures accumulate, drills become routinized, and expertise retires without replacement. Suppression (0.65): High. Communities and field experts have limited exit options: geographic dependence, employment constraints, and bureaucratic opacity about actual capacity all suppress alternatives. The suppression is enforced not through explicit coercion but through structural dependence on centralized water infrastructure and regulatory requirements that measure preparedness via procedure compliance rather than outcome validation. Theater ratio (0.78): High. The trajectory from 0.62 to 0.82 reflects increasing decoupling of performative preparedness (drills conducted, procedures filed, compliance verified) from functional preparedness (actual field capacity, rapid decision-making, expert judgment). Drills become memorial acts — reenacting the memory of preparedness — rather than functional tests. The claimed_type (tangled_rope) reflects that both genuine coordination (procedures, institutional continuity) and extraction (legitimacy without capacity, expertise devaluation) coexist.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. The water authority sees rope (coordination problem of knowledge retention solved by procedures). The affected community sees snare (trapped, false preparedness, maximum vulnerability). The field technician sees tangled_rope (benefits from job security but harmed by expertise devaluation). The regulatory authority sees rope or tangled_rope depending on whether they believe procedures correlate with actual capacity. The international coalition sees scaffold with sunset logic (building alternatives to ritualized preparedness). The institutional archive itself (the procedures and drills) is a piton (form persists, function degrades). The analytical observer risks seeing mountain (naturalizing the husk as inherent institutional decay). The husk reading explicitly rejects the mountain view: the constraint is not an immutable law of institutional memory but a contingent design choice to store competence in infrastructure rather than people.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from the structural positions. The water authority (beneficiary, institutional power, arbitrage exit) derives low d → negative f(d) → negative χ (they experience the constraint as beneficial coordination). The affected community (victim, powerless, trapped) derives high d → high f(d) → high χ (maximum experienced extraction). Field technicians (victims but institutionally employed, constrained exit) derive moderate-high d → moderate-high f(d) → moderate χ. The regulatory authority (neutral institutional position, constrained by mandate) derives moderate d. The international coalition (organized advocates, constrained by institutional barriers) derives lower d because they have agency and see exit paths (scaffolds). The directionality overrides are not needed here — the structural derivation produces coherent perspectival spacing.
 *
 * MANDATROPHY ANALYSIS:
 *   The husk reading resolves the mandatrophy by showing that preparedness is genuinely coordinated (procedures do reduce knowledge loss during personnel turnover; drills do maintain institutional memory of response protocols) AND genuinely extractive (institutions benefit from false preparedness claims; communities bear actual vulnerability risk; field expertise is systematically devalued). The tangled_rope classification captures both functions. The mandatrophy would arise if we tried to classify this as pure rope (denying extraction) or pure snare (denying coordination function). The husk reading shows the coordination and extraction are structurally entangled: institutions *use* the coordination function (procedures) to justify extractive practices (legitimacy claims without capacity building). The false summit problem (the analytical observer's mountain) is the naturalizing move: treating the husk as 'inherent to all institutions' rather than as a specific institutional design choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_field_decay_rate,
    'At what rate does field expertise decay when institutional procedures decouple from actual practice?',
    'Longitudinal skill assessment: test actual response capacity of field technicians during simulated crises; compare to procedural compliance scores; measure correlation between years since hands-on technical training and performance gaps during high-stress scenarios.',
    'If decay rate is rapid (<5 years to critical competence loss): husk reading is severe (snare/piton for affected communities). If decay is slow (>15 years): husk reading is less severe; procedures may retain functional memory longer than expected. Rate determines urgency of intervening with continuous training (scaffold reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_field_decay_rate, empirical, 'Rate of expertise decay when procedures decouple from practice').

omega_variable(
    reading_identity_crisis,
    'What is the core disagreement between the husk reading and the competence reading of preparedness retention?',
    'This is a conceptual omega routed from committer structure per Rule 2. The husk reading claims that preparedness becomes institutional performance (procedures, drills, infrastructure) while actual competence atrophies. The competence reading claims that preparedness is retained through continuous expert practice and knowledge transfer, with procedures as secondary scaffolding. They disagree on where competence *resides* (in people via practice vs. in infrastructure via documentation) and therefore on what ''retention'' means. The sibling relation is coexists_with: different institutions (and different authors of institutional designs) hold these readings simultaneously.',
    'Classification hinges on reading choice. Under husk: constraint is tangled_rope (extraction masked by institutional legitimacy). Under competence: constraint is rope (genuine coordination of knowledge networks). The engine cannot decide this — it depends on which reading the observer adopts. An institution optimizing for husk will design procedures; an institution optimizing for competence will design mentorship and continuous practice. Both can claim ''preparedness retention''; they mean different things.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_crisis, conceptual, 'Fundamental disagreement on where preparedness competence resides (people vs. infrastructure)').

omega_variable(
    ritual_efficacy_boundary,
    'Below what theater_ratio does a preparedness system cease to function as ritual and begin to function as performance toward actual competence?',
    'Comparison of systems with different theater ratios: do communities with theater_ratio < 0.6 show measurably better actual response outcomes? Is there a critical threshold where procedures begin to correlate with real capacity? Empirical examination of disaster outcomes post-event for regions with known theater ratios.',
    'If threshold exists at theater > 0.70: this constraint (theater = 0.78) is deeply ritualized; reform to theater < 0.70 would require structural change. If threshold is much higher or non-existent: theater itself is not the diagnostic — some rituals are functional even at high theater. This affects whether scaffold (sunset) is plausible or whether deeper transformation is needed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_efficacy_boundary, empirical, 'Theater ratio threshold below which preparedness systems transition from ritual to functional performance').

omega_variable(
    memorial_versus_functional_ambiguity,
    'When a drill is conducted, is it a memorial act (preserving the memory that we *could* respond) or a functional act (testing and improving actual response capacity)?',
    'Post-drill analysis: Do drills produce documented changes to procedures, identified gaps in training, or reassignments of expertise? Or do they produce only completion reports and attendance verification? The intent of the institution can be inferred from the outputs. If outputs are primarily memorial (records created, rituals performed), the husk reading holds. If outputs are primarily functional (specific improvements documented and implemented), the competence reading holds.',
    'This omega distinguishes reading identity. The husk reading views drills as memorial (preserving institutional form). The competence reading views drills as functional (improving response capacity). They cannot both be true for the same institution — it depends on how that institution designed and implements its drills.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_versus_functional_ambiguity, empirical, 'Whether preparedness drills function as memorial acts or as capacity-improvement exercises').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(husk_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(husk_tr_t0, husk_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(husk_tr_t5, husk_reading, theater_ratio, 5, 0.7).
narrative_ontology:measurement(husk_tr_t10, husk_reading, theater_ratio, 10, 0.78).
narrative_ontology:measurement(husk_tr_t15, husk_reading, theater_ratio, 15, 0.82).

% Extraction over time
narrative_ontology:measurement(husk_be_t0, husk_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(husk_be_t5, husk_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(husk_be_t10, husk_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(husk_be_t15, husk_reading, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(husk_reading, preparedness_retention_competence_reading).

% DUAL FORMULATION NOTE:
% The husk_reading and competence_reading of preparedness_retention are two structurally distinct constraints instantiated from a single contested kernel. They have different ε values (husk ~0.58, competence ~0.25-0.35), different beneficiary/victim structures, and different classification profiles. They are not two views of the same constraint; they are two constraints generated by two different institutional interpretations of what 'preparedness retention' means. Link them bidirectionally via network.affects_constraints because an institution's choice of one reading affects the structural conditions and effectiveness of the other (competence-oriented training programs erode husk-oriented procedure reliance, and vice versa).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
