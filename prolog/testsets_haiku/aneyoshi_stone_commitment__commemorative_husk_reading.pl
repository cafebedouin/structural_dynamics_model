% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone Commemorative Husk Reading: Symbolic Memorial Without Behavioral Land-Use Constraint
 *   domain: disaster_anthropology/institutional_analysis/memory
 *
 * SUMMARY:
 *   In 1933, the village of Aneyoshi in Iwate Prefecture, Japan erected a
 *   stone monument at a specific elevation with an inscription warning future
 *   generations not to build below that line, as a tsunami-danger marker
 *   following a deadly 1933 tsunami event. The sibling reading
 *   (behavioral_competence_reading) treats the stone as a live constraint
 *   that retained operational force across 78 years and influenced land-use
 *   decisions, explaining Aneyoshi's survival of the 2011 Tōhoku tsunami by
 *   reference to behavioral compliance with the stone's directive. This
 *   reading (commemorative_husk_reading) contests that framing: it argues the
 *   stone's behavioral force decayed over decades, that the stone persists
 *   primarily as a ritual and educational artifact, and that Aneyoshi's 2011
 *   survival resulted from geographic/hydrodynamic fortune rather than from
 *   live adherence to the stone's inscription. Under this reading, the stone
 *   extracts no behavioral constraint on land use in the present; it
 *   functions as a museum piece and moral touchstone, not as an operative
 *   rule.
 *
 * KEY AGENTS:
 *   - Aneyoshi residents — occupiers of the land subject to the stone's (or non-) directive; they make land-use and settlement decisions independently or in deference to the stone depending on which reading is true
 *   - Cultural heritage institutions — curate the stone as memorial; benefit from the interpretive frame that emphasizes its historical and symbolic significance
 *   - Tourism economy — generates value from the stone as a disaster-anthropology attraction and pilgrimage site
 *   - Municipal/prefectural governance — sets land-use and building policy; these are made independently of the stone's directive under this reading
 *   - Disaster risk reduction community — interprets the stone as either a successful behavioral constraint (behavioral_competence reading) or a failed one (this reading)
 *   - Researchers and historians — document the stone's actual role; generate empirical evidence that either validates or falsifies the behavioral-competence claim
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
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Stone Commemorative Husk Reading: Symbolic Memorial Without Behavioral Land-Use Constraint").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/institutional_analysis/memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, 'e1cfbffe-7ec2-42e5-a187-17935c9acb87').
narrative_ontology:cs_kernel_codification('e1cfbffe-7ec2-42e5-a187-17935c9acb87', fixed_text).
narrative_ontology:cs_authority_grounding('e1cfbffe-7ec2-42e5-a187-17935c9acb87', lineage).
narrative_ontology:cs_interpretation_layer_present('e1cfbffe-7ec2-42e5-a187-17935c9acb87').
narrative_ontology:cs_reading_relation('e1cfbffe-7ec2-42e5-a187-17935c9acb87', aneyoshi_stone_commitment__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('e1cfbffe-7ec2-42e5-a187-17935c9acb87', foundational, memorial_primacy_over_behavioral_constraint).
narrative_ontology:cs_axiom_status(memorial_primacy_over_behavioral_constraint, holdable).
narrative_ontology:cs_axiom_grounding('e1cfbffe-7ec2-42e5-a187-17935c9acb87', memorial_primacy_over_behavioral_constraint, empirically_contingent).
narrative_ontology:cs_axiom('e1cfbffe-7ec2-42e5-a187-17935c9acb87', secondary, institutional_decay_via_commemoration).
narrative_ontology:cs_axiom_status(institutional_decay_via_commemoration, holdable).
narrative_ontology:cs_axiom_grounding('e1cfbffe-7ec2-42e5-a187-17935c9acb87', institutional_decay_via_commemoration, empirically_contingent).
narrative_ontology:cs_reference_frame('e1cfbffe-7ec2-42e5-a187-17935c9acb87', behavioral_directive_discipline).
narrative_ontology:cs_drift_state('e1cfbffe-7ec2-42e5-a187-17935c9acb87', contemporary_memorial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e1cfbffe-7ec2-42e5-a187-17935c9acb87', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, cultural_heritage_institutions).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, memorial_tourism_economy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, tourism_and_pilgrimage_economy).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__commemorative_husk_reading, historical_memory_preservation_doctrine).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__commemorative_husk_reading, cultural_continuity_symbolic_function).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inhabit the land subject to the stone's prescription. Under this reading, the stone makes no behavioral demand on their settlement patterns; they inherit the memorial as cultural property and ritual practice. They benefit from the stone's symbolic function (identity, heritage, moral instruction) and experience no extraction from behavioral constraint because the constraint has decayed. Their exit option is constrained by place-attachment and economic dependence on the locality.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents, payer).

% Curate, preserve, and interpret the stone as a historical and memorial artifact. They collect interpretive authority, scholarly prestige, and institutional mission from the stone's status as a preserved disaster-testimony landmark. They administer the commemorative framing and benefit from ongoing institutional attention to the site.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, cultural_heritage_institutions, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__commemorative_husk_reading, cultural_heritage_institutions, agenda_setter).

% Generates visitor traffic and economic value around the stone as a disaster-anthropology attraction and moral pilgrimage site. Benefits from the stone's status as a living memorial; the tourism economy depends on the stone being preserved and interpreted, not necessarily on it being behaviorally operative.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, tourism_and_pilgrimage_economy, beneficiary,
    organized, biographical, mobile, national).

% Analyzes the stone as a historical case of disaster resilience strategy. Under this reading, interprets the stone as a cautionary example of institutional decay: a live behavioral constraint that became decoupled from actual land-use decisions, undermining the very protection it was designed to provide.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_risk_reduction_community, observer,
    institutional, generational, analytical, global).

% Sets and enforces land-use and building code policy. Under this reading, does so independently of the stone's directive; the stone is treated as historical context and cultural heritage rather than as a binding land-use constraint on contemporary development decisions.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_governance, agenda_setter,
    institutional, generational, constrained, local).

% Document and analyze the stone's actual role in community decision-making and disaster resilience. Generate empirical evidence about whether the stone's directive influenced behavior across time, or whether it persists primarily as a symbolic artifact.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, researchers_and_historians, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preservation of collective memory and moral instruction: the community and institutions coordinate around the stone to remember and honor the 1933 disaster, transmit disaster-awareness to younger generations, and maintain cultural continuity across catastrophic events. The stone's primary coordination function, under this reading, is memorial and educational, not land-use regulatory.
% TRANSFER_FUNCTION: The arrangement moves interpretive authority, tourist value, and cultural prestige toward heritage institutions and the local tourism economy. The stone's commemorative status generates visitor flow, scholarly attention, and funding for preservation. Aneyoshi residents contribute maintenance labor and cultural stewardship; institutions and tourism operators extract interpretive value and economic benefit.
% ABSENT_VOICES: Future residents of Aneyoshi who inherit the land: they would ask whether the stone's directive should govern their building choices, but that question is structurally absent from the commemorative framing (the stone is treated as historical memory, not as live guidance). Disaster risk reduction planners and hydrodynamic modelers: they would argue for evidence-based elevation policy independent of the stone's historical directive, but their input is absent from the ceremonial/memorial interpretation. The sibling behavioral_competence reading's advocates: they would claim the stone IS still operative and this reading misses empirical evidence, but their voices compete at the reading-selection level, not within-reading.
% DISAPPEARANCE_RATIONALE: If the stone vanished overnight, the community would lose its primary memorial artifact for the 1933 disaster and its symbolic anchor for disaster-awareness education — that rearrangement is real. However, land-use decisions would not visibly change because (under this reading) they are already decoupled from the stone's directive; municipal governance would proceed on the same basis it currently does. The verdict is contested because the behavioral_competence reading would argue land-use patterns would deteriorate without the stone's guiding constraint — a claim this reading denies on empirical grounds.
% FOUNDING_PROBLEM: The 1933 tsunami killed hundreds in the region and left Aneyoshi damaged. The stone was erected to encode a behavioral directive for future generations: do not build below this elevation. The founding problem was the lack of institutional memory and land-use discipline to protect the community from recurrent tsunami danger.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by disaster historians and hydrodynamic analysis: yes, the region faced recurrent tsunami danger and low institutional memory. However, the claim that the stone's behavioral directive remained the primary mechanism solving this problem across 78 years is contested. Contemporary municipal disaster risk reduction policy, post-2011 building codes, and scientific understanding of tsunami propagation now provide alternative institutional mechanisms for managing the founding problem. The behavioral_competence reading asserts the stone remained operative; this reading asserts those alternative mechanisms superseded the stone's behavioral force. No party disagrees that the founding problem existed; disagreement is about whether the stone remained the solution.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).

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
 *   Theater ratio starts at 0.42 (40 years after the stone's inscription: the stone is invoked, memorialized, and treated with solemnity, but behavioral constraint on land use is already declining) and rises to 0.78 by present interval-end: the stone is now predominantly performed in ceremonial and educational contexts, with minimal behavioral consequence on actual land-use decisions. Base extractiveness is low (0.12–0.18) because a memorial artifact that does not constrain behavior does not extract meaningful resources — no one 'pays' the stone in the way payers in a snare or tangled_rope bear costs; the stone's persistence is not defended by coercion but by institutional inertia and cultural reverence. Suppression is negligible (0.08–0.15): the stone does not need to suppress resistance because it makes no behavioral demand; land-use decisions proceed without the stone's directive creating conflict. The measurement series shows declining behavioral force (extractiveness downtrend) and rising performative weight (theater uptrend), a classic piton signature: the constraint persists through institutional maintenance and cultural veneration, not through the behavioral utility that justified its original adoption.
 *
 * PERSPECTIVAL GAP:
 *   From the cultural heritage and tourism perspective, the stone's decay into symbolism is not a failure but a transformation into a more profound form of persistence — it lives in memory, ritual, education, and moral weight. From a disaster risk reduction perspective, this reading represents a failure: a live behavioral constraint became a museum piece, decoupling from the actual decisions that govern community safety. From the behavioral_competence reading's seat, the stone IS still operational, and this reading misses empirical evidence of ongoing deference. The engine computes per-seat classifications from the structural data: a seat that treats the stone as a live directive would compute lower theater_ratio and higher behavioral engagement; the measurement series, authored from the commemorative reading's seat, treats ongoing low behavioral engagement as empirically true.
 *
 * DIRECTIONALITY LOGIC:
 *   Cultural heritage institutions and tourism operators benefit from the stone's commemorative framing — they collect interpretive authority and visitor value (d near 0.2, beneficiary end). Aneyoshi residents experience the stone as a heritage artifact and moral touchstone without behavioral constraint on their land-use choices (d near 0.5, symmetric: they inherit the memorial but are not constrained by it). Disaster risk reduction researchers experience the stone as either a historical success (behavioral_competence reading) or a cautionary case of institutional decay (this reading) — their directionality is analytical (d near 0.5). No seat experiences the stone as a direct extraction mechanism under this reading, because the stone makes no behavioral demand.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading models mandatrophy explicitly: the stone was erected with a clear mandate (preserve the village by enforcing elevation discipline), that mandate was live and behavioral for approximately 78 years (supporting the behavioral_competence reading's framing), and by the present interval the mandate has outlived its operational function. The stone persists not because it governs behavior but because the community and institutions treat it with cultural reverence and use it for memorial and educational purposes. The rising theater_ratio captures this shift: activity increases around the stone (ceremonies, scholarly attention, tourism) while behavioral constraint decreases. Classifying as piton rather than snare reflects that no concentrated beneficiary extracts from the stone's persistence — cultural heritage institutions and tourism benefit, but they do not maintain the stone through coercive enforcement; they maintain it through institutional stewardship and cultural practice. The mandate's decay is not contested by any party — both readings acknowledge the stone no longer operates as a land-use rule; they diverge on whether it ever did.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_contest,
    'Is the Aneyoshi stone a living behavioral constraint on land use, or a decayed memorial artifact whose actual behavioral force has atrophied?',
    'Ethnographic documentation of land-use decision-making processes in Aneyoshi post-2011: does the stone''s directive appear in discussions, planning records, or safety considerations? Or is it invoked only for tourism, education, and ceremonial contexts? Analysis of building location decisions made after the stone''s inscription relative to its stated elevation guidance.',
    'If land-use decisions are made independently of the stone''s directive, this reading (commemorative_husk) is vindicated and the sibling behavioral_competence reading is foreclosed as empirically false. If the stone''s directive demonstrably influenced building location or safety-relevant land-use decisions, the behavioral_competence reading is vindicated and this reading is foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, empirical, 'Whether the stone retains operational behavioral force or functions primarily as memorial.').

omega_variable(
    survival_causation_ambiguity,
    'Did Aneyoshi''s 2011 survival (the tsunami did not reach the town despite reaching heights elsewhere) result from behavioral compliance with the stone''s directive, or from luck/geography/other factors?',
    'Hydrodynamic modeling of 2011 tsunami propagation, topographic analysis of Aneyoshi''s location relative to surge paths, and historical comparison with other communities at similar elevations that were not spared. Post-2011 development in Aneyoshi: did the stone''s directive influence settlement patterns, or did development proceed without reference to it?',
    'If survival was attributable to geography rather than behavioral compliance, the stone''s claimed force is largely illusory (supporting this reading). If the historical settlement pattern at the stone''s specified elevation demonstrably protected the village across multiple tsunami events, the behavioral competence reading gains empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_causation_ambiguity, empirical, 'Whether the stone''s stated directive causally contributed to community survival or survival resulted from independent factors.').

omega_variable(
    commemoration_vs_constraint_framing,
    'Is the boundary between memorial function and behavioral constraint sharp, or do communities treat commemoration itself as a soft behavioral directive?',
    'Comparative anthropology of similar memorial stones and disaster monuments: do communities that preserve danger-warning monuments in commemorative/educational mode show behavioral deference to their stated directives in land-use decisions? Interviews with Aneyoshi residents about the stone''s status in their own understanding.',
    'If commemoration is itself a behavioral mechanism (residents treat the stone as a living instruction precisely because it is treated solemnly and remembrances invoke it), the boundary between this reading and behavioral_competence blurs. If commemoration is ritual without land-use behavioral consequence, this reading is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commemoration_vs_constraint_framing, conceptual, 'Whether memorial function and behavioral constraint are separable or whether commemoration itself enforces behavior.').

omega_variable(
    institutional_capture_of_memorial,
    'Who benefits from treating the stone as a museum/memorial artifact rather than as a live behavioral directive? Are there institutional interests in preserving the ''decayed symbolic'' framing?',
    'Institutional analysis: who administers the site, who collects from tourism, whose narrative about the stone appears in official materials? Does the commemorative framing serve to deflect liability or development pressure that would arise if the stone were treated as an active behavioral constraint?',
    'If memorial institutions or local economic interests benefit from the decayed-symbolism framing, this reading may be sustained partly by capture rather than empirical reality. This would complicate the empirical resolution of the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_memorial, empirical, 'Whether institutional interests shape the reading toward commemoration over behavioral constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(aney_tr_t0, observed).
narrative_ontology:measurement(aney_tr_t20, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement_basis(aney_tr_t20, observed).
narrative_ontology:measurement(aney_tr_t40, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 40, 0.72).
narrative_ontology:measurement_basis(aney_tr_t40, observed).
narrative_ontology:measurement(aney_tr_t60, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 60, 0.78).
narrative_ontology:measurement_basis(aney_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(aney_be_t0, observed).
narrative_ontology:measurement(aney_be_t20, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(aney_be_t20, observed).
narrative_ontology:measurement(aney_be_t40, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 40, 0.13).
narrative_ontology:measurement_basis(aney_be_t40, observed).
narrative_ontology:measurement(aney_be_t60, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 60, 0.12).
narrative_ontology:measurement_basis(aney_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(aney_su_t0, observed).
narrative_ontology:measurement(aney_su_t20, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement_basis(aney_su_t20, observed).
narrative_ontology:measurement(aney_su_t40, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 40, 0.09).
narrative_ontology:measurement_basis(aney_su_t40, observed).
narrative_ontology:measurement(aney_su_t60, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 60, 0.08).
narrative_ontology:measurement_basis(aney_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__commemorative_husk_reading, 0.05).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The Aneyoshi stone commitment is a contested kernel instantiated in two structurally distinct constraint stories: behavioral_competence_reading (the stone retains operational behavioral force across 78 years, ε~0.35–0.40) and commemorative_husk_reading (the stone's behavioral force decayed, now primarily memorial, ε~0.12). The ε values differ fundamentally because the observables differ: behavioral_competence reads the stone as a land-use rule (observable: building location decisions relative to the stone's elevation directive); commemorative_husk reads it as a memorial artifact (observable: institutional activity, tourism, ceremony, decoupled from land-use constraints). The kernel contest is whether these observables measure the same constraint from different angles (a single ε with measurement ambiguity) or two structurally distinct constraints (two stable ε values instantiated by different readings of the kernel). The framework treats them as two stories linked by network.affects_constraints, each with its own ε, each with its own empirical vindication condition. The stories are siblings in a kernel family, not perspectives on a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
