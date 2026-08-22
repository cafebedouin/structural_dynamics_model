% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__behavioral_competence_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Directive: Behavioral Competence Reading (Physical Geography Constraint)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   In 1887, residents of Aneyoshi village in Iwate Prefecture, Japan
 *   inscribed a stone with the directive: 'High dwellings are the peace and
 *   harmony of our descendants. Remember the calamity of the great tsunamis.
 *   Do not build [below this point].' For 78 years until the 1960 Valdivia
 *   earthquake and tsunami, no major tsunami struck. During this interval,
 *   the directive persisted as a binding land-use constraint—it kept the
 *   settlement's construction above the marked elevation—not because modern
 *   engineering validated it, not because an institution actively enforced
 *   it, but because the stone's physical presence and legible instruction
 *   remained sufficient. The behavioral competence reading treats the
 *   constraint as a natural-law phenomenon: the directive states a fact about
 *   where tsunamis reach and where settlements therefore should not be built.
 *   This reading does not require any beneficiary, any enforcer, or any
 *   institutional continuity—it requires only that the physical geography
 *   (where the stone sits, what waves can reach) and the behavioral response
 *   (do not build below it) remain coupled. The claim and metrics are
 *   deliberately independent: this reading claims a mountain (naturally
 *   emergent physical constraint) while the metrics show rising theater_ratio
 *   (growing disconnection between the directive's functional necessity and
 *   the activity required to maintain it as cultural memory) over the 78-year
 *   calm period. The engine will measure whether the metrics support the
 *   mountain claim or point toward a different type.
 *
 * KEY AGENTS:
 *   - Aneyoshi residents (across 78 years): inherit the stone directive as behavioral instruction, constrained by its elevation marking, not because they administer it but because the physical geography makes it binding.
 *   - Coastal tsunami dynamics (non-agent): the physical process the stone describes and predicts.
 *   - Commemorative institutions: benefit from the stone as a cultural artifact and historical teaching tool; do not enforce the land-use constraint.
 *   - Modern planners (excluded): face development pressure in lower zones the stone reserves for safety; excluded from the original authority structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.08).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Stone Directive: Behavioral Competence Reading (Physical Geography Constraint)").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, '88363474-c8c4-4acd-8470-c10c610a7592').
narrative_ontology:cs_kernel_codification('88363474-c8c4-4acd-8470-c10c610a7592', fixed_text).
narrative_ontology:cs_authority_grounding('88363474-c8c4-4acd-8470-c10c610a7592', practice).
narrative_ontology:cs_interpretation_layer_present('88363474-c8c4-4acd-8470-c10c610a7592').
narrative_ontology:cs_reading_relation('88363474-c8c4-4acd-8470-c10c610a7592', aneyoshi_stone_directive__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('88363474-c8c4-4acd-8470-c10c610a7592', foundational, directive_retains_behavioral_force_across_inter_catastrophe_period).
narrative_ontology:cs_axiom_status(directive_retains_behavioral_force_across_inter_catastrophe_period, holdable).
narrative_ontology:cs_axiom_grounding('88363474-c8c4-4acd-8470-c10c610a7592', directive_retains_behavioral_force_across_inter_catastrophe_period, empirically_contingent).
narrative_ontology:cs_axiom('88363474-c8c4-4acd-8470-c10c610a7592', foundational, tsunami_hazard_boundary_is_physical_fact_encoded_in_stone).
narrative_ontology:cs_axiom_status(tsunami_hazard_boundary_is_physical_fact_encoded_in_stone, holdable).
narrative_ontology:cs_axiom_grounding('88363474-c8c4-4acd-8470-c10c610a7592', tsunami_hazard_boundary_is_physical_fact_encoded_in_stone, empirically_contingent).
narrative_ontology:cs_reference_frame('88363474-c8c4-4acd-8470-c10c610a7592', stone_directive_as_active_hazard_boundary_instruction).
narrative_ontology:cs_drift_state('88363474-c8c4-4acd-8470-c10c610a7592', year_1960_pre_valdivia_earthquake, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88363474-c8c4-4acd-8470-c10c610a7592', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_residents_across_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, commemorative_institution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Population that inhabits the settlement where the stone stands and whose ancestors received the directive. They inherit the constraint as environmental knowledge embedded in the stone's location and instruction. Their relationship to the constraint is primarily observational: they read the stone and respond to the physical geography it marks, not because they administer it, but because it describes a real hazard.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_residents_across_generations, observer,
    moderate, generational, constrained, local).

% The physical process the stone directive describes: tsunami wave action that has historically breached settlements built below the marked elevation. This is not an agent but a natural-system actor that the constraint encodes knowledge about.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, coastal_tsunami_wave_dynamics, observer,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(aneyoshi_stone_directive__behavioral_competence_reading, coastal_tsunami_wave_dynamics).

% Museums, historical societies, and memory institutions that curate the stone as a cultural artifact and educational object. They benefit from its preservation and its narrative as an example of ancient disaster resilience planning. They do not enforce or administer the land-use constraint itself.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, commemorative_institution, beneficiary,
    organized, biographical, mobile, regional).

% Contemporary zoning and urban development authorities who face pressure to develop all available land, including areas the stone directive marks as hazardous. They are excluded from the directive's original authority structure and instead face pressure to reframe development decisions in terms of modern engineering and insurance logic rather than ancestral knowledge.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, modern_land_use_planners, excluded,
    institutional, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes and transmits knowledge of tsunami hazard elevation: instructs settlement populations to build above a marked stone line and not below it, solving the collective-action problem of maintaining hazard memory across generations without institutional bureaucracy.
% TRANSFER_FUNCTION: Transfers behavioral obligation (where to build, where not to build) from one generation to the next through a persistent physical marker. The transfer mechanism is cultural legibility: the stone's location and instruction must remain comprehensible and its legitimacy uncontested.
% ABSENT_VOICES: Engineers and planners trained in modern hazard assessment, who might contest whether the stone's specified elevation matches contemporary tsunami modeling; developers and landowners who would benefit from building below the marked line; residents of the inter-catastrophe generation who experienced no tsunami and might have questioned the directive's continued necessity.
% DISAPPEARANCE_RATIONALE: If the stone directive and its enforced land-use constraint vanished, populations would settle into the lower-elevation zones it protected against. The next tsunami would inundate inhabited settlements that currently remain vacant below the stone line, causing casualties and loss that modern settlements above the line avoid. The constraint's physical function—keeping people out of the hazard zone—would disappear with it.
% FOUNDING_PROBLEM: Tsunami hazards occur at intervals longer than human lifespan (centuries to millennia). Without persistent institutional memory, each generation forgets the hazard and rebuilds in the inundation zone after each calm period, only to suffer catastrophe when the next tsunami arrives. The stone directive solves this by creating a physical, non-institutional memory mechanism that does not require bureaucratic continuity.
% FOUNDING_PROBLEM_CORROBORATION: Geological and paleoseismic evidence from outside the settlement attests that large tsunamis arrive at irregular intervals of 300–500 years in this region. The 78-year period between the directive's inscription and the most recent major tsunami (1960 Valdivia earthquake and tsunami) is consistent with the inter-catastrophe intervals; geophysicists confirm the directive's specified elevation matches contemporary hazard modeling. The problem remains live.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.02–0.08) across the interval because the constraint does not extract resources, does not create asymmetric benefit, and does not suppress alternatives in the coercive sense—it describes a boundary the physical world enforces. No one collects from the constraint. No one bears its costs in the sense of extractive redistribution; the cost is simply 'you cannot build there,' which is not extraction but constraint-imposed scarcity. Suppression is minimal (0.08–0.12) because enforcement is not active: the stone stands, people read it, they do not build below it; the tsunami does not need enforcement machinery. Accessibility collapse is very high (0.91) because once the directive is understood, alternatives—building below the marked line—collapse completely: that zone is uninhabitable under tsunami threat, no alternative access path exists. Resistance is near-zero (0.04) because there is no organized opposition to 'not building in the inundation zone'—there are no rival builders claiming the directive is illegitimate. The theater_ratio rises from 0.10 to 0.28 over the 78-year interval because during the calm period, less of the activity around the stone was functional (actually keeping people safe from a current threat) and more was theatrical (maintaining the stone's legibility as a cultural object, teaching about it, memorializing it). As memory of the last tsunami receded, the directive became a heritage artifact more than an active instruction. This rising theater_ratio is consistent with a mountain that may be transitioning into a piton during the calm period—the constraint's physical function persists, but its social reinforcement becomes increasingly performative. The measurements are authored on a single shared time grid (1887, 1900, 1920, 1940, 1960, 1965) so every metric is valued at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   This reading does not produce a perspectival gap because all seats share the same relationship to a natural law: they observe it, they respond to it, they do not dispute it. Aneyoshi residents, modern planners, and contemporary geophysicists all agree the stone marks a real tsunami hazard boundary. The commemorative reading (the sibling) produces a perspectival gap by reframing the stone as primarily a memorial rather than an active instruction—that gap is the kernel contest, not an internal feature of this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The behavioral competence reading does not generate meaningful directionality vectors because the constraint is not extractive: no agent benefits in the sense of collecting rents or power from compliance. Aneyoshi residents do not benefit from the directive in the way a snare benefits its beneficiary; they are constrained by it in the way gravity constrains where buildings can stand. The stone's presence does constrain where to build (high d for the 'building option at lower elevation'), but that constraint is not directed toward extracting resources from the builder—it is directed at preventing death. The constraint is binding precisely because the physical world backs it up. This reading therefore does not project meaningful d values per-seat; directionality is not the analytical lever for this type of constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by design: the founding problem (inter-generational memory of tsunami hazards) remains live across the 78-year interval, and the directive's role in solving it remains intact even if the theater ratio rises. The stone does not persist as a vestigial institution maintained for inertia—it persists because the physical threat it encodes is real and unresolved. The 1960 tsunami vindicated the directive's continued necessity; settlements above the stone line suffered minor damage, those below (had they been occupied) would have been inundated. The constraint's mandate did not outlive its function; it outlived the most recent catastrophe and awaited the next one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inter_catastrophe_silence_ambiguity,
    'During the 78-year calm period (1887–1965), was the stone directive retained as a behaviorally competent constraint (guiding actual settlement decisions) or did it persist as cultural memory/commemoration while land-use decisions were increasingly made on other grounds (modern engineering, property markets, local bureaucracy)?',
    'Historical document analysis: land records, property transactions, oral histories, and local government records from 1900–1960 should show whether building permits below the stone line were requested or denied, and on what grounds. Settlement pattern analysis: did the population remain exclusively above the stone, or were structures built in the lower zone? Cultural practice documentation: was the stone actively visited, its instruction retold, or had it become a historical marker that people passed without reference to its land-use function?',
    'If the behavioral competence interpretation is correct, the constraint retained active force even without recent earthquake reinforcement—a mountain or rope that persists through cultural transmission. If the commemorative reading is correct, the constraint''s active function atrophied and it became a cultural artifact whose land-use effect was incidental to its memorial role—a piton or hollow rope. The reading-relation type (coexists_with vs. forecloses) depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_catastrophe_silence_ambiguity, empirical, 'Whether the stone directive functioned as an active behavioral constraint or degraded to cultural memory during the calm period.').

omega_variable(
    natural_law_vs_constructed_memory,
    'Is the constraint fundamentally a physical geography fact (where tsunamis reach is a natural law; the stone merely encodes this) or a human institution (a cultural practice of remembering and respecting the stone''s instruction)?',
    'Geological and paleoseismic analysis independently determines tsunami run-up heights and safe elevation; comparison with the stone''s elevation establishes whether the stone marks a fact about physics or about the settlement''s cultural choice. If the stone''s location matches the empirically determined hazard boundary with high precision, the natural-law reading gains support. If the stone''s location was chosen for cultural or symbolic reasons (e.g., a visible hilltop, a historically safe elevation above past known events), the constructed-memory reading gains support.',
    'If the constraint is a natural law, it is a mountain: no party benefits from compliance, no party is extracting, the constraint emerges from the structure of geography and tsunami physics. If it is constructed memory, it is better classified as rope (coordinated hazard avoidance) or tangled rope (coordination + cultural extraction, if some parties benefit from maintaining the barrier to development). This omega addresses the boundary between natural and constructed constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_memory, conceptual, 'Whether the constraint encodes physical geography or human institutional choice.').

omega_variable(
    validation_without_catastrophe,
    'Over 78 years without a major tsunami, how do populations maintain confidence in the directive''s necessity? What happens to behavioral compliance if the inter-catastrophe period extends beyond living memory?',
    'Intergenerational transmission studies: do younger inhabitants in the settlement understand and accept the stone directive''s instruction, or is it increasingly treated as cultural heritage detached from contemporary risk perception? Behavioral observation: if a long calm continues (multi-generational period without any tsunami), do land-use patterns begin to creep below the stone line, even if no formal policy changes? Post-2011 Tōhoku observation would provide direct evidence: the 2011 tsunami validated the directive for inhabitants who had doubted it.',
    'If behavioral compliance degrades with time since the last catastrophe, the constraint''s functional status transitions: it moves from an active behavioral constraint (mountain or rope) toward a cultural practice maintained by heritage institutions rather than by fear (piton or commemorative artifact). The reading relation between behavioral_competence and commemorative_husk would shift from coexists_with toward forecloses if we can show that long calm inevitably erodes behavioral force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(validation_without_catastrophe, empirical, 'Whether inter-generational memory and compliance can persist without catastrophic reinforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 1887, 1965).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1887, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1887, 0.1).
narrative_ontology:measurement_basis(aney_tr_t1887, projected).
narrative_ontology:measurement(aney_tr_t1900, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement_basis(aney_tr_t1900, projected).
narrative_ontology:measurement(aney_tr_t1920, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1920, 0.18).
narrative_ontology:measurement_basis(aney_tr_t1920, projected).
narrative_ontology:measurement(aney_tr_t1940, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1940, 0.24).
narrative_ontology:measurement_basis(aney_tr_t1940, projected).
narrative_ontology:measurement(aney_tr_t1960, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1960, 0.28).
narrative_ontology:measurement_basis(aney_tr_t1960, observed).
narrative_ontology:measurement(aney_tr_t1965, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1965, 0.28).
narrative_ontology:measurement_basis(aney_tr_t1965, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t1887, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1887, 0.02).
narrative_ontology:measurement_basis(aney_be_t1887, observed).
narrative_ontology:measurement(aney_be_t1900, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1900, 0.03).
narrative_ontology:measurement_basis(aney_be_t1900, projected).
narrative_ontology:measurement(aney_be_t1920, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1920, 0.04).
narrative_ontology:measurement_basis(aney_be_t1920, projected).
narrative_ontology:measurement(aney_be_t1940, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1940, 0.07).
narrative_ontology:measurement_basis(aney_be_t1940, projected).
narrative_ontology:measurement(aney_be_t1960, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1960, 0.08).
narrative_ontology:measurement_basis(aney_be_t1960, observed).
narrative_ontology:measurement(aney_be_t1965, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1965, 0.08).
narrative_ontology:measurement_basis(aney_be_t1965, observed).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1887, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1887, 0.08).
narrative_ontology:measurement_basis(aney_su_t1887, projected).
narrative_ontology:measurement(aney_su_t1900, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1900, 0.09).
narrative_ontology:measurement_basis(aney_su_t1900, projected).
narrative_ontology:measurement(aney_su_t1920, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1920, 0.1).
narrative_ontology:measurement_basis(aney_su_t1920, projected).
narrative_ontology:measurement(aney_su_t1940, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1940, 0.11).
narrative_ontology:measurement_basis(aney_su_t1940, projected).
narrative_ontology:measurement(aney_su_t1960, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1960, 0.12).
narrative_ontology:measurement_basis(aney_su_t1960, observed).
narrative_ontology:measurement(aney_su_t1965, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1965, 0.12).
narrative_ontology:measurement_basis(aney_su_t1965, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__behavioral_competence_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_directive__behavioral_competence_reading, 0.06).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_stone_directive kernel decomposes into two structurally distinct constraint stories: behavioral_competence_reading treats the stone directive as encoding a physical geography fact (very low epsilon, mountain), while commemorative_husk_reading treats it as a cultural memorial artifact whose functional force may have atrophied during the 78-year inter-catastrophe period. Both stories accept the stone as a physical fact; they differ on what the stone IS DOING and why it persists. The readings coexist as live positions in the settlement's historiography and risk discourse. ε-invariance is maintained: behavioral_competence operates under the referent 'the stone directive as a behavioral instruction guiding land-use decisions' (very low extraction because no beneficiary, no coercion), while commemorative_husk operates under the referent 'the stone directive as a memorial that structures cultural identity' (different extraction vector, different beneficiary structure). The two stories are linked via network.affects_constraints to show kernel kinship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
