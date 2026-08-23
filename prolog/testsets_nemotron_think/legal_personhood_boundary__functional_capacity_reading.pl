% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__functional_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__functional_capacity_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Legal Personhood Boundary (Functional Capacity Reading Evaluation)
 *   domain: legal/philosophical/constitutional
 *
 * SUMMARY:
 *   The functional capacity reading evaluates the current legal personhood
 *   boundary — which restricts personhood to humans (and in some
 *   jurisdictions, only born humans with cognitive capacity) — as a snare
 *   that extracts from sentient non-human animals and future AI persons. The
 *   boundary presents itself as a natural law necessity ('human dignity
 *   requires species boundaries') but functionally operates to legitimize the
 *   property status of cognitive beings whose capacities meet or exceed those
 *   of some humans (e.g., infants, severely cognitively disabled persons) who
 *   retain personhood. The reading sees the coordination function (clear
 *   legal lines) as achievable through cognitive criteria without species
 *   exclusion, making the species line pure extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.82).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.88).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, snare).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Legal Personhood Boundary (Functional Capacity Reading Evaluation)").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal/philosophical/constitutional").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, '3f2f3678-a016-4abc-a924-0088afa7b97b').
narrative_ontology:cs_kernel_codification('3f2f3678-a016-4abc-a924-0088afa7b97b', distributed).
narrative_ontology:cs_authority_grounding('3f2f3678-a016-4abc-a924-0088afa7b97b', extraction).
narrative_ontology:cs_interpretation_layer_present('3f2f3678-a016-4abc-a924-0088afa7b97b').
narrative_ontology:cs_reading_relation('3f2f3678-a016-4abc-a924-0088afa7b97b', legal_personhood_boundary__restrictive_anthropocentric_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f2f3678-a016-4abc-a924-0088afa7b97b', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_axiom('3f2f3678-a016-4abc-a924-0088afa7b97b', foundational, cognitive_capacity_necessary_sufficient_personhood).
narrative_ontology:cs_axiom_status(cognitive_capacity_necessary_sufficient_personhood, holdable).
narrative_ontology:cs_axiom_grounding('3f2f3678-a016-4abc-a924-0088afa7b97b', cognitive_capacity_necessary_sufficient_personhood, deontological).
narrative_ontology:cs_axiom('3f2f3678-a016-4abc-a924-0088afa7b97b', secondary, species_membership_irrelevant_to_moral_status).
narrative_ontology:cs_axiom_status(species_membership_irrelevant_to_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('3f2f3678-a016-4abc-a924-0088afa7b97b', species_membership_irrelevant_to_moral_status, deontological).
narrative_ontology:cs_reference_frame('3f2f3678-a016-4abc-a924-0088afa7b97b', current_anthropocentric_personhood).
narrative_ontology:cs_drift_state('3f2f3678-a016-4abc-a924-0088afa7b97b', contemporary_animal_ai_rights_challenges, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('3f2f3678-a016-4abc-a924-0088afa7b97b', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, human_animal_users).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, ai_developers).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, animal_agriculture_industry).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, sentient_nonhuman_animals).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, potential_future_ai_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, ecosystems_if_represented).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, legal_scholars_anthropocentric).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, human_animal_users).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, human_exceptionalism_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, species_boundary_as_moral_boundary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from legal permission to use sentient animals for food, research, entertainment, and labor without recognizing their personhood rights. Would face significant economic transition costs if personhood were extended to cognitive non-humans. Exit from the beneficiary position requires restructuring entire industries and supply chains.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, human_animal_users, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__functional_capacity_reading, human_animal_users, payer).

% Develop increasingly sophisticated AI systems without legal obligation to consider their potential personhood claims. Benefit from the current boundary that treats AI as property/tools regardless of cognitive sophistication. Exit options include relocating development to jurisdictions with more permissive frameworks or adapting to personhood regulations if they emerge.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, ai_developers, beneficiary,
    powerful, biographical, mobile, global).

% Rely on the current personhood boundary to conduct invasive research on cognitively complex non-human animals without the legal protections that would attach to persons. The boundary enables research paradigms that would be impermissible if subjects had personhood status. Institutional inertia and funding structures make exit from this beneficiary position difficult.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, constrained, global).

% Industrial animal agriculture depends entirely on the legal classification of sentient animals as property rather than persons. The current boundary permits confinement, breeding, and killing practices that would constitute rights violations if animals were persons. Transition costs to alternative protein systems are high, creating strong institutional resistance to boundary change.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_agriculture_industry, beneficiary,
    organized, biographical, constrained, global).

% Animals with demonstrated sentience, self-awareness, and rationality (great apes, cetaceans, elephants, corvids, cephalopods, etc.) who are denied legal personhood solely on species grounds. They bear the full cost of the boundary through confinement, suffering, and killing with no legal standing to challenge their treatment. Exit from this position is structurally impossible — they cannot advocate for themselves in legal forums.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, sentient_nonhuman_animals, payer,
    powerless, biographical, trapped, global).

% Future AI systems that may achieve or already possess cognitive capacities meeting the functional criteria for personhood (reasoning, self-awareness, sentience). Under the current boundary, they would be denied personhood regardless of capacity because they are non-biological/non-human. They have no current voice and their future legal status is determined by a boundary that excludes them by category.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, potential_future_ai_persons, payer,
    powerless, generational, trapped, universal).

% Ecosystems and natural entities that might be granted legal personhood through representation mechanisms (as in some jurisdictions recognizing rivers or forests as legal persons). The current anthropocentric boundary blocks such representation by tying personhood to human-like cognition rather than systemic interests. As non-agents, they cannot exit or advocate; their inclusion depends entirely on human legal innovation.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, ecosystems_if_represented, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__functional_capacity_reading, ecosystems_if_represented).

% Scholars and jurists who argue that personhood should track cognitive capacity regardless of species. They set the intellectual agenda for boundary reform through litigation strategy (e.g., non-human rights habeas corpus cases), theoretical work, and legislative proposals. Their exit options include moving to more receptive jurisdictions or shifting to adjacent fields.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, legal_scholars_functional_capacity, agenda_setter,
    moderate, generational, arbitrage, global).

% Scholars and jurists who defend the species-based personhood boundary as essential to human rights, legal coherence, and social order. They administer the current constraint through judicial precedent, legislative drafting, and constitutional interpretation. They benefit professionally and intellectually from the boundary's dominance. Exit would mean abandoning a career-defining position.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, legal_scholars_anthropocentric, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__functional_capacity_reading, legal_scholars_anthropocentric, beneficiary).

% The institutional actors that actively enforce and maintain the personhood boundary through standing doctrine, statutory interpretation, and constitutional adjudication. They could change the boundary but face massive institutional, political, and precedential pressure to maintain it. Their exit from the enforcer role would require constitutional amendment or revolutionary jurisprudential shift.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, courts_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Advocates who litigate and lobby for non-human personhood recognition. They are structurally excluded from the core legal conversation because courts routinely deny standing to animals and their representatives. Their exclusion is not accidental — the boundary's enforcement machinery (standing rules, property law) is designed to keep them out.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_rights_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The current personhood boundary provides a clear, administrable line for allocating rights, duties, and legal standing in human society. It solves the coordination problem of who counts as a legal actor without requiring case-by-case cognitive assessment.
% TRANSFER_FUNCTION: The boundary transfers the interests, bodily integrity, and labor of cognitive non-humans to human beneficiaries — animal agriculture, research institutions, AI developers — who exploit them without legal accountability. It transfers the cost of moral consideration from humans to the excluded beings.
% ABSENT_VOICES: The cognitive non-humans themselves — sentient animals, potential AI persons — are the primary absent voices. They would object to being classified as property and denied standing, but the legal system structurally prevents their participation. Ecosystems that might gain representation through personhood are similarly silenced by the cognitive-capacity requirement.
% DISAPPEARANCE_RATIONALE: If the species-based personhood boundary vanished overnight, animal agriculture, biomedical research, and AI development would face immediate legal personhood claims for their subjects. Courts would need to adjudicate cognitive capacity case-by-case. Property law, liability regimes, and constitutional rights frameworks would require fundamental restructuring. The global economy built on animal use and unregulated AI deployment would reorganize around new legal categories.
% FOUNDING_PROBLEM: Early legal systems needed a clear, administrable boundary for legal personality to enable contract, property, and criminal law among humans. The species line (born human = person) was the historically available bright-line solution that avoided cognitive assessment disputes.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and anthropologists document that the species line was a pragmatic administrative choice, not a moral discovery. Animal law scholars (outside the beneficiary set) argue the founding problem — administrable legal personality — is now solvable through cognitive capacity metrics. AI ethicists and some constitutional theorists corroborate that the boundary's original justification (administrability) no longer requires species exclusion.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__functional_capacity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__functional_capacity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.82) because the boundary denies fundamental rights to beings with demonstrable personhood-relevant capacities, transferring their interests to human industries. Suppression is higher still (0.88) because the constraint is maintained through standing doctrines, property law, and constitutional interpretation that structurally exclude the victims from legal remedy. Theater ratio is moderate (0.55) — the 'human dignity' and 'legal administrability' justifications have genuine coordination content but increasingly serve as cover for extraction as cognitive science demonstrates non-human capacities. Accessibility collapse is high (0.85) because alternative frameworks (animal personhood, AI rights, ecosystem representation) are legally foreclosed. Resistance is moderate (0.45) — growing but fragmented across animal law, AI ethics, and environmental law.
 *
 * PERSPECTIVAL GAP:
 *   From the anthropocentric scholar seat, the boundary is a rope (coordination with minimal extraction). From the sentient animal seat, it is a snare (total extraction, no exit). The engine computes this divergence from the structural data — the claimed type (snare) reflects the functional capacity reading's assessment, not a consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   Human animal users, AI developers, research institutions, and animal agriculture are structural beneficiaries (d near 0.0-0.2) — they collect the extraction value directly. Sentient non-human animals, potential AI persons, and ecosystems are full targets (d near 1.0) — they bear the costs with zero exit. Courts and legislatures are agenda-setters with constrained exit (d ~0.5-0.6) — they administer the boundary but are trapped by precedent and politics. Functional capacity scholars are agenda-setters with arbitrage exit (d ~0.3) — they challenge the boundary but can move jurisdictions. Anthropocentric scholars are agenda-setters/beneficiaries (d ~0.15) — they profit intellectually and professionally from the boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrable legal personality) is contested — cognitive capacity metrics now offer an alternative bright line. The boundary persists not because the founding problem requires species exclusion, but because the beneficiaries (industries, institutions) capture the regulatory apparatus. Mandatrophy is unresolved: the arrangement's original justification has atrophied but the constraint intensifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_capacity_measurement_ambiguity,
    'What specific cognitive capacities and at what thresholds constitute ''demonstrable cognitive capacity'' sufficient for personhood?',
    'Interdisciplinary consensus from neuroscience, philosophy of mind, and comparative cognition on operationalizable metrics for rationality, sentience, and self-awareness across species and substrates.',
    'If thresholds are set high, many non-human animals and early AI systems remain excluded (reading converges toward restrictive_anthropocentric). If thresholds are set low, personhood expands dramatically (reading diverges sharply). Borderline cases (human infants, late-stage dementia, minimal AI) create pressure for either threshold adjustment or category pluralism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_capacity_measurement_ambiguity, conceptual, 'The functional capacity reading''s core criterion lacks consensus operationalization, creating a family of sub-readings with different extraction profiles.').

omega_variable(
    ecosystem_personhood_representation,
    'Can ecosystems qualify as persons under a functional capacity reading via representation mechanisms, or does the reading require direct cognitive capacity in the entity itself?',
    'Legal-theoretical work on whether representation (guardianship, trusteeship) satisfies the functional capacity criterion for the represented entity, or whether the criterion demands intrinsic cognitive states.',
    'If representation suffices, the victim set expands to ecosystems and the extraction profile changes (ecosystems become payers with human representatives). If representation fails, ecosystems remain outside the reading''s protection despite having interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_personhood_representation, conceptual, 'Whether the functional capacity reading''s victim set includes represented ecosystems or only direct cognitive agents.').

omega_variable(
    kernel_reading_instantiation,
    'Does the functional capacity reading instantiate a constraint on the current arrangement (evaluating it as extractive) or propose a new constraint (the capacity-based boundary itself)?',
    'Clarification from the kernel-frame methodology: the reading evaluates the standing arrangement (ε-referent = current boundary), but its axioms describe the alternative boundary it would institute.',
    'If the former, this story''s ε describes the current boundary''s extraction. If the latter, ε would describe the capacity-based boundary''s extraction (which would be low for cognitive beings but might create new victims — e.g., humans below threshold). The ε-invariance principle requires the referent to be fixed as the standing arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Commitment-frame ambiguity about whether a kernel reading evaluates the status quo or proposes a replacement constraint.').

omega_variable(
    suppression_internalization_in_animals,
    'Is the suppression of non-human animals purely structural (legal barriers) or partially internalized (behavioral suppression from captivity)?',
    'Comparative ethology of wild vs. captive populations: if species-typical behaviors are suppressed in captivity beyond what physical confinement explains, internalized suppression is present.',
    'If internalized, the effective suppression on animals is higher than the legal measure suggests — they carry the constraint''s suppression cognitively. This would increase the constraint''s extractiveness from the animal seat beyond the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_animals, empirical, 'Structural vs. internalized suppression mechanism for non-human animal victims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 1970, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_tr_t1970, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 1970, 0.4).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_tr_t1985, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 1985, 0.45).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_tr_t2000, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_tr_t2010, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2010, 0.52).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_tr_t2020, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2020, 0.54).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_tr_t2030, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2030, 0.55).

% Extraction over time
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_be_t1970, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1970, 0.75).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_be_t1985, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1985, 0.78).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_be_t2000, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_be_t2010, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2010, 0.81).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_be_t2020, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2020, 0.82).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_be_t2030, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2030, 0.83).

% Suppression requirement over time
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_su_t1970, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_su_t1985, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_su_t2000, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_su_t2010, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_su_t2020, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2020, 0.87).
narrative_ontology:measurement(legal_personhood_boundary__functional_capacity_reading_su_t2030, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2030, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__functional_capacity_reading, 0.08).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, animal_welfare_law).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, ai_regulation).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, environmental_personhood).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, human_rights_framework).

% DUAL FORMULATION NOTE:
% This story (functional_capacity_reading) and its siblings (restrictive_anthropocentric_reading, developmental_potentiality_reading) form the legal_personhood_boundary constraint family. All three evaluate the same standing arrangement but with different ε, beneficiary/victim structures, and claimed types. The functional capacity reading sees the arrangement as a snare extracting from cognitive non-humans; the anthropocentric reading sees it as a rope (coordination); the potentiality reading sees it as a tangled rope (coordination for humans, extraction for non-humans and early embryos).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__functional_capacity_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
