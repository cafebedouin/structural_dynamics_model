% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone as Commemorative Artifact (Husk Reading)
 *   domain: cultural/institutional/disaster
 *
 * SUMMARY:
 *   The Aneyoshi Stone, inscribed in 1933 by villagers responding to the
 *   Taisho Sanriku earthquake, carried a directive in Japanese: 'Here, at
 *   high tides, the waves reached here. In an earthquake disaster, do not
 *   build houses below this point. Remember well.' Under the
 *   commemorative-husk reading, this stone functions as a symbolic artifact
 *   and memory marker rather than as an operational constraint on land-use
 *   decisions. Between 1933 and 2011, modern disaster governance (seawalls,
 *   building codes, seismic monitoring) displaced the stone's functional role
 *   while preserving it as a heritage object. The 2011 Tōhoku tsunami killed
 *   154 in Aneyoshi but flooded below the stone's elevation—a fact that under
 *   this reading is attributed to the modern seawall's partial failure rather
 *   than to the stone's warning being heeded. The stone survives as theater:
 *   maintained as a cultural artifact, circulated in heritage narratives,
 *   studied by anthropologists, and invoked in memory discourse, but absent
 *   from the decision-making processes that actually govern where residents
 *   build and what risks they accept.
 *
 * KEY AGENTS:
 *   - aneyoshi_village_residents: inhabitants whose actual building decisions are decoupled from the stone's directive
 *   - japanese_municipal_government: sets land-use policy through modern regulatory infrastructure (codes, zoning, seawalls), not through the stone's authority
 *   - tourism_and_memory_industries: benefit economically and culturally from the stone as a heritage symbol
 *   - institutional_legitimacy_narrators: use the stone to articulate narratives of cultural continuity and intergenerational wisdom
 *   - anthropologists_and_historians: document the stone as a historical artifact and observe its transformation from directive to symbol
 *   - modern_disaster_governance_infrastructure: seawalls, building codes, early-warning systems that have replaced the stone's functional role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.12).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.08).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.18).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Stone as Commemorative Artifact (Husk Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "cultural/institutional/disaster").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, '98f47223-c12f-41e2-848a-bec0cb39c642').
narrative_ontology:cs_kernel_codification('98f47223-c12f-41e2-848a-bec0cb39c642', fixed_text).
narrative_ontology:cs_authority_grounding('98f47223-c12f-41e2-848a-bec0cb39c642', distributed).
narrative_ontology:cs_reading_relation('98f47223-c12f-41e2-848a-bec0cb39c642', aneyoshi_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('98f47223-c12f-41e2-848a-bec0cb39c642', foundational, stone_mandate_has_expired).
narrative_ontology:cs_axiom_status(stone_mandate_has_expired, holdable).
narrative_ontology:cs_axiom_grounding('98f47223-c12f-41e2-848a-bec0cb39c642', stone_mandate_has_expired, empirically_contingent).
narrative_ontology:cs_axiom('98f47223-c12f-41e2-848a-bec0cb39c642', secondary, narrative_continuity_decoupled_from_behavioral_constraint).
narrative_ontology:cs_axiom_status(narrative_continuity_decoupled_from_behavioral_constraint, holdable).
narrative_ontology:cs_axiom_grounding('98f47223-c12f-41e2-848a-bec0cb39c642', narrative_continuity_decoupled_from_behavioral_constraint, conventional).
narrative_ontology:cs_reference_frame('98f47223-c12f-41e2-848a-bec0cb39c642', intergenerational_hazard_knowledge_transmission).
narrative_ontology:cs_drift_state('98f47223-c12f-41e2-848a-bec0cb39c642', contemporary_post_seawall_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('98f47223-c12f-41e2-848a-bec0cb39c642', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, tourism_and_memory_industries).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, institutional_legitimacy_narrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in the settlement where the stone stands. Under this reading, the stone's directive about building location has become ornamental—actual development decisions are made on economic, regulatory, and practical grounds independent of the stone's warning. They preserve the stone as a cultural artifact and historical marker, but do not consult it for safety decisions.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_village_residents, observer,
    moderate, biographical, constrained, local).

% Administers the settlement and oversees land use through building codes, zoning, and modern disaster-preparedness infrastructure (seawalls, early-warning systems). The stone is treated as a cultural heritage asset to be maintained, not as a regulatory constraint on construction. Municipal decisions override or supplement the stone's ancient directive.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, japanese_municipal_government, agenda_setter,
    institutional, generational, mobile, regional).

% Operate heritage tourism, educational programming, and commemorative media centered on the stone. The stone's value as a museum piece, a symbol of intergenerational memory, and a subject of anthropological study drives economic and cultural revenue. The stone's authority is instrumentalized as a narrative device—'ancient wisdom passed down'—without operational force in contemporary land-use decisions.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, tourism_and_memory_industries, beneficiary,
    organized, biographical, mobile, regional).

% Include cultural authorities, educational institutions, and national heritage frameworks that benefit from positioning the stone as embodying enduring cultural continuity and ancestor wisdom. The stone vindicates narratives of cultural depth and intergenerational transmission without requiring that the encoded warning actually constrain contemporary behavior—it can be venerated while ignored.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, institutional_legitimacy_narrators, beneficiary,
    powerful, generational, arbitrage, national).

% Study the stone as a historical artifact and cultural practice. They document how the stone persists as memory-object rather than as a live regulatory rule. Their observational role enables analytical description of the constraint's transformation from behavioral directive to symbolic artifact.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, anthropologists_and_historians, observer,
    organized, biographical, arbitrage, global).

% Represents seawalls, early-warning systems, building codes, and disaster preparedness science that have become the functional replacement for the stone's authority. These systems make independent decisions about where and how to build, what risks to accept, and how to respond to hazards. The stone is excluded from these decision processes and carries no weight in modern risk governance.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, modern_disaster_governance_infrastructure, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone functioned historically as a coordination mechanism: a visible, memorable, spatially anchored sign encoding ancestral experience about flooding risk, designed to coordinate behavior across generations without requiring institutional enforcement. Under this reading, that coordination function has atrophied—modern disaster governance (seawalls, codes, early warnings) now solves the coordination problem independently.
% TRANSFER_FUNCTION: The constraint moves cultural-symbolic capital and institutional legitimacy from the stone's veneration to heritage industries, educational institutions, and national memory frameworks. The constraint does not transfer material resources or impose behavioral costs; instead, it transfers narratives, prestige, and the authority to speak about cultural continuity and ancestor wisdom. Who benefits: tourism, memory industries, institutional storytellers. Who pays: negligible material cost—the stone's maintenance is a small cultural expenditure.
% ABSENT_VOICES: The stone itself, as a speaking entity in the scenario, is absent—it is spoken about, not consulted. Also absent: the historical residents and ancestors who carved the stone and whose embodied experience it encodes. Their voices are replaced by contemporary interpreters who articulate what the stone 'means' for modern audiences. Modern residents who might object to the stone's conversion into an ornament without operational force are excluded from the decision to maintain it as a museum piece rather than as a directive.
% DISAPPEARANCE_RATIONALE: If the stone were removed, modern land-use decisions would proceed identically. Zoning, building codes, seawalls, and disaster preparedness would continue unaltered. Heritage tourism might decline slightly, and institutional narratives about cultural continuity would lose a potent symbol—but the material world would rearrange not at all. The stone's absence from contemporary governance already obtains; its physical removal would simply make that absence permanent.
% FOUNDING_PROBLEM: The stone was inscribed to solve a coordination problem: how to transmit ancestral knowledge about tsunami risk across generations without institutional infrastructure or writing systems. The stone's location, message, and durability were engineered to make that knowledge memorable, visible, and actionable.
% FOUNDING_PROBLEM_CORROBORATION: Modern disaster governance and scientific seismology have superseded the stone's function. Municipal governments, seismic monitoring, and engineering science now coordinate behavior around tsunami risk far more comprehensively than an ancient stone can. Residents, historians, and seismic scientists all attest that the founding problem—intergenerational transmission of hazard knowledge without institutional support—is obsolete. The stone is preserved as a historical marker of how the problem was once solved, not as a current solution.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).
:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Under this reading, extractiveness is very low (0.12) because the constraint imposes minimal material costs—the stone's maintenance is negligible, and land-use decisions proceed without consulting it. Theater ratio is very high (0.78) and rising over the measurement interval because an increasing share of the stone's persistence is performative: it is maintained, displayed, told-about, and invoked in narratives, but does not constrain behavior. The trajectory shows gradual conversion from a partially-functional directive (1933: theater ~0.15, some residents still referenced the stone's warning) to primarily ceremonial artifact (2011: theater ~0.78, the stone is a museum piece with emotional and narrative weight but no operational force). Suppression is near zero (0.08) because no one is coerced to maintain or venerate the stone—its persistence is sustained by tourism economics and institutional desire to articulate cultural continuity, not by threats. Accessibility collapse is very low (0.18) because alternatives (modern building codes, seismic science, engineered seawalls) are widely available, legible, and preferred. Resistance is negligible (0.05) because there is no active coercive pressure to overcome—the stone's transformation from directive to artifact is the path of least resistance for all parties: residents get modern governance, institutions get a heritage asset, and tourism benefits.
 *
 * PERSPECTIVAL GAP:
 *   This reading models a constraint where the seat divergence is temporal rather than positional: the stone as its authors (1933 villagers) intended it is one constraint (behavioral directive); the stone as contemporary residents, municipal governments, and heritage industries treat it is a different constraint (commemorative husk). The engine will compute a piton classification from this reading's structural data (high theater, minimal enforcement, atrophied function) while the behavioral_competence_reading computes differently from the same physical object (low theater, retained operational force in decisions about where to build). Both readings are structurally true for their respective historical moments and seats; the sibling reading frames a different epistemic position. Under the husk reading, the 2011 survival of Aneyoshi below the stone is attributed to modern engineering (seawall, early warning, evacuation infrastructure) rather than to adherence to the stone's directive—a structural claim about what caused what, not a dispute about the stone's symbolic value.
 *
 * DIRECTIONALITY LOGIC:
 *   Tourism and memory industries derive concrete benefit (revenue, narrative authority, relevance) from the stone's existence and preservation, so their directionality is near the beneficiary end (d ≈ 0.15—they collect from the arrangement, do minimal enforcement, have exit options). Municipal government is near-symmetric (d ≈ 0.50)—it maintains the stone as a public good with cultural value, bears small preservation costs, and coordinates heritage tourism, but the stone does not constrain its actual land-use decisions, which are made through modern regulatory infrastructure. Residents are observers (d ≈ 0.5, analytical)—they neither benefit nor pay through the stone's operation; their building decisions proceed independently of it. The constraint extracts minimal value and imposes minimal cost, so directionality is shallow across all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly positions the Aneyoshi Stone as a case of mandatrophy—a constraint whose original mandate (coordinate intergenerational transmission of tsunami-risk knowledge) has become obsolete, but whose institutional form persists through tourism economics and cultural-legitimacy narratives. The classification as piton (inertial, mostly performative) prevents misreading the stone as an active rope (genuine coordination) or as a snare (extractive coercion). The piton framing clarifies that what sustains the stone is not the coordination problem it was built to solve or any coercive enforcement, but rather a diffuse ecosystem of benefits (heritage tourism, institutional storytelling, educational programming) that cost very little to maintain and would cost significant political capital to dismantle. The mandatrophy is not contested on this reading—the founding problem is dead, the constraint survives by inertia, and the persistence is explained entirely by the low-cost-to-maintain status of commemorative artifacts in modern culture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_symbolic_boundary,
    'At what point (if any, between 1933 and 2011) did the stone transition from a functionally-consulted directive to a purely commemorative artifact? Did the transition occur gradually, or was there a discrete moment?',
    'Oral history from village residents about their building decisions; archival records of municipal land-use planning; ethnographic observation of how the stone is invoked in contemporary village discourse.',
    'If a discrete transition point exists and is documented, the engine can model two distinct constraint stories (pre- and post-transition) with different ε values. If the transition was gradual, a single story with measurements showing rising theater_ratio over time captures the drift accurately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_symbolic_boundary, empirical, 'Whether the stone''s transformation from operational directive to commemorative artifact occurred at a specific moment or gradually.').

omega_variable(
    causation_in_2011_survival,
    'What caused Aneyoshi''s relatively lower mortality in 2011 compared to neighboring settlements? Was it adherence to the stone''s directive, or was it modern disaster governance infrastructure (seawall, early warning, evacuation procedures)?',
    'Hydraulic modeling of the 2011 tsunami relative to the seawall height; survivor interviews about decision-making during evacuation; comparison of mortality rates to seawall effectiveness in other settlements.',
    'If the seawall was the primary protective factor, the husk reading is supported (the stone is no longer operational). If the stone''s directive demonstrably constrained building location in ways that reduced exposure, the behavioral_competence reading has stronger empirical support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causation_in_2011_survival, empirical, 'Causation attribution for Aneyoshi''s 2011 survival: stone directive vs. modern infrastructure.').

omega_variable(
    institutional_capture_of_narrative,
    'Do heritage institutions, tourism operators, and educational authorities have incentive to frame the stone as a symbol of cultural wisdom while de-emphasizing evidence that it is no longer consulted as an operational rule?',
    'Media analysis of how the stone is described in tourism materials, educational curricula, and heritage frameworks; comparison to how disaster-governance professionals (engineers, seismic scientists, emergency planners) describe the stone.',
    'If institutional capture is documented, the beneficiary structure (tourism, memory industries) becomes more salient—the constraint''s persistence is sustained by who benefits from the narrative, not by who needs the coordination it was built to solve. This strengthens the piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_narrative, conceptual, 'Whether institutional interests in cultural narrative preservation sustain the stone''s visibility despite functional obsolescence.').

omega_variable(
    reading_kernel_identification,
    'Is the contest between the ''commemorative_husk_reading'' and the ''behavioral_competence_reading'' a genuine kernel dispute, or does it reflect different time-indexing (one reading describes 1933, the other describes 2011) of the same object?',
    'If the readings are indexing different temporal moments, they are not a kernel dispute; they are two snapshots of a single constraint undergoing mandatrophy. If the readings are making opposed claims about the SAME moment (e.g., what the stone meant in 1980, whether it constrained behavior then), they are a genuine kernel dispute about how to interpret the stone''s operative presence.',
    'Genuine kernel disputes require separate constraint stories with linked network relations. Temporal snapshots can be captured in a single story''s measurement series. This distinction affects how the corpus models the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_identification, conceptual, 'Whether this is a kernel reading or a temporal mandatrophy snapshot.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1933, 0.15).
narrative_ontology:measurement_basis(aney_tr_t1933, observed).
narrative_ontology:measurement(aney_tr_t1960, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement_basis(aney_tr_t1960, observed).
narrative_ontology:measurement(aney_tr_t1980, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1980, 0.55).
narrative_ontology:measurement_basis(aney_tr_t1980, observed).
narrative_ontology:measurement(aney_tr_t2000, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2000, 0.68).
narrative_ontology:measurement_basis(aney_tr_t2000, observed).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2011, 0.78).
narrative_ontology:measurement_basis(aney_tr_t2011, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1933, 0.08).
narrative_ontology:measurement_basis(aney_be_t1933, observed).
narrative_ontology:measurement(aney_be_t1960, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1960, 0.09).
narrative_ontology:measurement_basis(aney_be_t1960, observed).
narrative_ontology:measurement(aney_be_t1980, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement_basis(aney_be_t1980, observed).
narrative_ontology:measurement(aney_be_t2000, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2000, 0.11).
narrative_ontology:measurement_basis(aney_be_t2000, observed).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2011, 0.12).
narrative_ontology:measurement_basis(aney_be_t2011, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_commitment__commemorative_husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__commemorative_husk_reading, 0.06).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The Aneyoshi Stone kernel decomposes into two constraint stories: (1) behavioral_competence_reading models the stone as a retained operational directive that influenced building location decisions and possibly contributed to 2011 survival; (2) commemorative_husk_reading models the stone as a monument and heritage artifact whose original mandate has atrophied and whose persistence is sustained by tourism and institutional storytelling. The readings make opposed claims about causation in 2011 and about whether the stone constrains contemporary behavior. Both stories describe the same physical object; they differ on its functional role and on what counts as evidence of that role. Network edge: the husk reading INFLUENCES the behavioral reading because establishing that the stone is now symbolic constrains how the behavioral reading can claim operational force—it must locate that force in historical decision-making (1933–1980s) rather than in contemporary land use.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
