% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__artifact_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__artifact_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: gelassenheit_separation__artifact_reading
 *   human_readable: Gelassenheit Separation — Artifact Resemblance Reading
 *   domain: religious_studies/technology_governance/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the artifact_reading of the contested
 *   Gelassenheit separation kernel: technology is forbidden if it visually
 *   resembles a worldly artifact, regardless of its actual function or degree
 *   of entanglement with worldly systems. Under this reading, an off-grid
 *   household is barred from solar panels — a technology that is arguably
 *   MORE consistent with the community's professed self-sufficiency ideal
 *   than a diesel generator — purely because panels look like something 'the
 *   English' use. As the tradition ages, the visible-marker standard has
 *   hardened: what began as a legible, low-cost boundary heuristic has
 *   increasingly diverged from any functional relationship to worldly
 *   entanglement, while enforcement (Ordnung rulings, Bann threat) has
 *   intensified rather than relaxed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.81).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.88).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Gelassenheit Separation — Artifact Resemblance Reading").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious_studies/technology_governance/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, 'cff23062-e369-4a9b-bb80-813ab86958d0').
narrative_ontology:cs_kernel_codification('cff23062-e369-4a9b-bb80-813ab86958d0', distributed).
narrative_ontology:cs_authority_grounding('cff23062-e369-4a9b-bb80-813ab86958d0', lineage).
narrative_ontology:cs_interpretation_layer_present('cff23062-e369-4a9b-bb80-813ab86958d0').
narrative_ontology:cs_reading_relation('cff23062-e369-4a9b-bb80-813ab86958d0', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_reading_relation('cff23062-e369-4a9b-bb80-813ab86958d0', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('cff23062-e369-4a9b-bb80-813ab86958d0', foundational, visible_resemblance_is_the_test_of_worldliness).
narrative_ontology:cs_axiom_status(visible_resemblance_is_the_test_of_worldliness, holdable).
narrative_ontology:cs_axiom_grounding('cff23062-e369-4a9b-bb80-813ab86958d0', visible_resemblance_is_the_test_of_worldliness, conventional).
narrative_ontology:cs_axiom('cff23062-e369-4a9b-bb80-813ab86958d0', secondary, appearance_based_uniformity_secures_communal_witness).
narrative_ontology:cs_axiom_status(appearance_based_uniformity_secures_communal_witness, holdable).
narrative_ontology:cs_axiom_grounding('cff23062-e369-4a9b-bb80-813ab86958d0', appearance_based_uniformity_secures_communal_witness, instrumental).
narrative_ontology:cs_reference_frame('cff23062-e369-4a9b-bb80-813ab86958d0', visible_nonconformity_as_witness).
narrative_ontology:cs_drift_state('cff23062-e369-4a9b-bb80-813ab86958d0', contemporary_off_grid_technology_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cff23062-e369-4a9b-bb80-813ab86958d0', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, ordained_ministers).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, district_bishops).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, off_grid_households).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, young_farm_families).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, disabled_and_elderly_members).
narrative_ontology:constraint_vindicates(gelassenheit_separation__artifact_reading, visible_separation_from_the_world_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rules on which specific artifacts (solar panels, synthetic fabrics, rubber tires, certain tool designs) are forbidden because they visually resemble English technology, independent of what the item actually does or how it is used. Adjudicates Ordnung disputes at members' meetings and can excommunicate or place under Bann those who adopt a forbidden-looking item. Personally exempt from most economic consequences of the ruling and gains authority and deference from being the arbiter of visible boundary maintenance.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, district_bishops, agenda_setter,
    institutional, generational, arbitrage, regional).

% Preach and enforce the visible-distinction standard as the substance of separation from the world. Their pastoral authority and community standing are built on their role as interpreters of what 'looks worldly.' They bear little of the practical cost of banned technologies since farm labor and off-grid survival are not their daily burden in the same way.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, ordained_ministers, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__artifact_reading, ordained_ministers, agenda_setter).

% Live without connection to the electrical grid by religious requirement, yet are also forbidden from installing solar panels because the panels visually resemble modern worldly installations — even though the underlying function (localized, self-sufficient power) is consistent with the community's own stated economic philosophy. Must instead purchase costlier, less efficient alternatives (diesel generators run behind barns, hidden battery banks) or do without, based purely on how the technology looks from the road.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, off_grid_households, payer,
    powerless, biographical, trapped, local).

% Compete economically against non-Plain farms while banned from adopting functionally isolated equipment (certain synthetic fabrics for durable work clothing, rubber-tired equipment for road use) solely because the item resembles English consumer goods. Bear real income loss and physical labor cost from artifact-appearance rules that have no relationship to whether the technology entangles them in worldly systems. Leaving the district (or the church) is available but costs family, land inheritance, and community standing.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, young_farm_families, payer,
    powerless, biographical, constrained, local).

% Denied mobility aids, medical devices, or adaptive equipment when the devices resemble worldly consumer products, even when the same devices would be functionally isolated (battery-powered, non-networked, no dependency on worldly infrastructure). Bear direct physical and health costs from a rule keyed to appearance rather than entanglement. Have essentially no exit — leaving the community at this life stage means losing caregiving networks entirely.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, disabled_and_elderly_members, payer,
    powerless, immediate, trapped, local).

% Neighboring or sister communities that read separation as preserved practice (visiting, mutual aid, rootedness) rather than visible appearance would object that appearance-based bans on functionally isolated technology (e.g. solar panels for off-grid households) actively harm the practices separation is meant to protect, by driving people toward income-destroying workarounds. They are not seated in this district's Ordnung deliberations and have no standing to contest a neighboring bishop's ruling.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, consequence_reading_communities, excluded,
    organized, generational, constrained, regional).

% Communities that read separation as avoidance of structural entanglement (not appearance) would argue this reading bans technology that is MORE functionally isolated than some permitted alternatives, purely because of visual resemblance to worldly artifacts. They hold a rival theological reading of Gelassenheit but have no formal channel to challenge another district's Ordnung.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, principle_reading_communities, excluded,
    organized, generational, constrained, regional).

% Study Ordnung variation across districts and document how appearance-based technology bans diverge from both the functional-isolation principle and the community-practice consequence readings within the same broader Anabaptist tradition.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, denominational_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a visible, legible, low-ambiguity boundary marker that lets members and outsiders alike instantly identify who belongs to the community, reducing costly case-by-case theological deliberation over every new technology.
% TRANSFER_FUNCTION: Moves economic burden (higher costs, foregone efficiency, lost labor-competitiveness, denied medical/mobility aid) from the bishops and ministers who set and interpret the visible-marker rule onto households, farm families, and elderly/disabled members who must live inside its literal, appearance-based boundaries regardless of actual function.
% ABSENT_VOICES: Sister communities holding the consequence_reading or principle_reading of the same Gelassenheit kernel would object that appearance-based prohibition is theologically arbitrary and materially harmful, but Ordnung governance is congregational and district-bound — they have no forum to contest another bishop's ruling, and dissenting members within the district who might argue for a functional standard are precisely the ones most exposed to Bann.
% DISAPPEARANCE_RATIONALE: If the appearance-based standard vanished and were replaced by a functional-isolation test, off-grid households would adopt solar panels immediately, farm families would adopt functionally-isolated modern equipment, and disabled members would gain access to adaptive devices — the community would look more visually similar to its neighbors while, on the community's own economic-separation logic, remaining just as functionally separate. Ministers and bishops would lose the clean, low-cost enforcement mechanism visible-distinction provides and would need to develop harder case-by-case functional judgments.
% FOUNDING_PROBLEM: Early Anabaptist communities needed a durable way to resist assimilation into surrounding society under real historical pressure (persecution, then later cultural absorption), and a visible marker of difference was one workable solution to community boundary maintenance.
% FOUNDING_PROBLEM_CORROBORATION: Bishops and ministers attest the visible-marker standard remains necessary to prevent gradual worldly assimilation. Sister communities under the principle_reading and consequence_reading — outside this district's beneficiary structure — attest that visible resemblance is a poor proxy for actual assimilation risk, and independent denominational historians documenting Ordnung divergence across districts corroborate that appearance-based rules produce material harm (denied medical devices, economic loss) untethered from any measurable increase in worldly entanglement.
narrative_ontology:disappearance_verdict(gelassenheit_separation__artifact_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__artifact_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__artifact_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__artifact_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__artifact_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__artifact_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__artifact_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε=0.81) is high because the rule imposes real, uncompensated material cost (denied medical devices, foregone efficient equipment, higher operating costs) on households who have no say in setting the standard, while the interpreting authority (bishops, ministers) bears essentially none of that cost and gains deference and coordination legibility from administering it. Suppression (0.88) is near-maximal because the rule is enforced through excommunication/Bann threat and is explicitly indifferent to functional argument — a household cannot argue its way to an exemption by demonstrating functional isolation, because appearance, not function, is the stated criterion. Theater ratio rises over the interval (0.30 to 0.62) as the gap between visible-marker enforcement and any coherent theological or practical rationale widens — more energy goes into policing resemblance than into the substantive separation-from-worldly-systems goal the tradition itself professes.
 *
 * PERSPECTIVAL GAP:
 *   From the bishop's seat, banning solar panels because they 'look worldly' is a coherent, low-cost application of a long-standing and legible separation principle. From the off-grid household's seat, the same ruling is an arbitrary appearance test that denies them a technology MORE aligned with their own professed values than the technology they are forced to use instead. The engine should compute these as structurally different experiences of the identical rule, driven by who sets the definition of 'worldly resemblance' versus who absorbs its material consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Bishops and ministers sit near the beneficiary end: they administer the rule, are the interpretive authority whose judgment resolves disputes, and are largely insulated from its material costs by role (their labor and health needs are less exposed to banned-technology gaps than farm households or the elderly/disabled). Off-grid households, young farm families, and disabled/elderly members sit near the full-target end: high suppression, trapped or constrained exit (leaving costs land, family, community), and direct uncompensated material harm. The two excluded stakeholder groups (sister communities under sibling readings) are structurally locked out of the deliberative forum entirely, which is why they appear as excluded rather than payer or beneficiary — their objection is theological/practical but has no standing channel.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — resisting cultural assimilation — plausibly remains live for the tradition as a whole (assimilation pressure is real and ongoing), which is why founding_problem_status is marked contested rather than dead: it is not that the underlying separation goal is obsolete, but that the SPECIFIC visible-marker mechanism for achieving it has drifted from its own rationale. This is exactly the mandatrophy trap the framework is built to catch: because the general goal (separation) is still live, it is easy to wrongly treat the specific enforcement mechanism (appearance-based prohibition, regardless of function) as equally justified. The corpus should register these as separable — the goal's continued legitimacy does not entail the appearance-heuristic's continued fitness, and the sibling readings (principle_reading, consequence_reading) are precisely the record of other communities concluding the heuristic had drifted while the goal had not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    appearance_vs_function_theological_grounding,
    'Is visible resemblance to worldly artifacts a theologically load-bearing criterion for separation within this tradition''s own doctrine, or is it a historically contingent heuristic that has calcified into doctrine through repeated Ordnung rulings?',
    'Comparative doctrinal-historical analysis of Ordnung records across districts and decades: if the appearance criterion is a recent hardening (traceable to specific historical rulings) rather than a claim present in founding Anabaptist theological writing, that supports the heuristic-hardened-into-doctrine reading.',
    'If appearance is doctrinally load-bearing, the high suppression is a cost intrinsic to a genuine coordination function (visible community boundary). If it is a hardened heuristic, the same suppression is extraction dressed as doctrine, and the tangled_rope classification understates how far this reading has drifted toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appearance_vs_function_theological_grounding, conceptual, 'Whether appearance-based prohibition is genuine doctrine or hardened heuristic misread as doctrine.').

omega_variable(
    kernel_reading_selection_authority,
    'What structural or historical factors determine which districts adopt the artifact_reading versus the principle_reading or consequence_reading of the same Gelassenheit kernel, and who has authority to select among them?',
    'Cross-district comparative study of bishop succession, migration history, and prior schisms to identify what predicts reading adoption; interview sister-community leaders under the other two readings about their view of the artifact_reading''s material costs.',
    'If reading selection tracks bishop preference/authority-consolidation rather than theological argument, this strengthens the case that the artifact_reading persists partly because it maximizes interpretive authority for the deciding office, independent of its separation-effectiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_authority, empirical, 'What determines which kernel reading a given district adopts and enforces.').

omega_variable(
    solar_panel_case_as_diagnostic,
    'Does the specific case of off-grid households banned from solar panels (a technology arguably MORE aligned with self-sufficiency than permitted alternatives) generalize to a broader pattern of appearance-function divergence, or is it an isolated edge case?',
    'Systematic inventory of banned-vs-permitted technology across districts practicing the artifact_reading, scored on functional-isolation criteria independent of appearance, to see how often appearance and function diverge.',
    'A high divergence rate would support characterizing the artifact_reading as substantially and systematically extractive rather than incidentally so; a low rate would suggest the solar panel case is unrepresentative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(solar_panel_case_as_diagnostic, empirical, 'Whether appearance-function divergence is systemic or an isolated example.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__artifact_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(gela_tr_t10, gelassenheit_separation__artifact_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__artifact_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(gela_tr_t30, gelassenheit_separation__artifact_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__artifact_reading, theater_ratio, 40, 0.53).
narrative_ontology:measurement(gela_tr_t50, gelassenheit_separation__artifact_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement(gela_tr_t60, gelassenheit_separation__artifact_reading, theater_ratio, 60, 0.62).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__artifact_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gela_be_t10, gelassenheit_separation__artifact_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__artifact_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(gela_be_t30, gelassenheit_separation__artifact_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__artifact_reading, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(gela_be_t50, gelassenheit_separation__artifact_reading, base_extractiveness, 50, 0.79).
narrative_ontology:measurement(gela_be_t60, gelassenheit_separation__artifact_reading, base_extractiveness, 60, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__artifact_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(gela_su_t10, gelassenheit_separation__artifact_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__artifact_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(gela_su_t30, gelassenheit_separation__artifact_reading, suppression_requirement, 30, 0.81).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__artifact_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement(gela_su_t50, gelassenheit_separation__artifact_reading, suppression_requirement, 50, 0.86).
narrative_ontology:measurement(gela_su_t60, gelassenheit_separation__artifact_reading, suppression_requirement, 60, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__artifact_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__artifact_reading, 0.08).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the gelassenheit_separation kernel. artifact_reading (this story) authors high extraction and near-maximal suppression because it forbids functionally isolated technology (solar panels, adaptive medical devices) purely on visual-resemblance grounds. principle_reading authors substantially lower extraction because its functional-isolation test would permit the same off-grid solar panel case this story forbids. consequence_reading authors an intermediate profile keyed to effects on community practice rather than either appearance or abstract functional isolation. The three stories share a kernel but are NOT one constraint measured three ways — the ε values genuinely differ because the readings license different real-world outcomes for the same household in the same district.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
