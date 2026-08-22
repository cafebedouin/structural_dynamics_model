% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__property_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: animal_moral_status__property_reading
 *   human_readable: Animals as Property: Legal-Philosophical Foundation
 *   domain: applied_ethics/legal_philosophy
 *
 * SUMMARY:
 *   This constraint encodes the legal and philosophical reading of the
 *   animal_moral_status kernel in which animals are categorically denied
 *   independent moral standing and are treated as property/resources whose
 *   value is exclusively instrumental. The constraint sets the foundation for
 *   all downstream animal-use arrangements (agriculture, research,
 *   entertainment, resource extraction). Under this reading, animals have no
 *   interests that count as interests in the moral calculus—their interests
 *   are subordinate by definition, not by fact. This is ONE reading of a
 *   contested kernel. The abolitionist reading holds that animals are
 *   rights-bearing and property status itself is the violation. The welfare
 *   reading holds that animals are sentient and their suffering should be
 *   regulated but use remains permissible. Each reading constitutes a
 *   distinct constraint with a different beneficiary structure, victim set,
 *   and ε value. The property reading is authored here as clean and
 *   independent—the coexistence of alternative readings is handled via omega
 *   variables and cs_structure, not by hedging the constraint itself.
 *
 * KEY AGENTS:
 *   - Property owners and animal-resource users: institutional and organized beneficiaries; hold use rights and are protected from having to justify use by reference to animal interests
 *   - Regulatory authorities: agenda-setter; administer property law and enforce the constraint by recognizing ownership and settling disputes over use boundaries
 *   - Sentience researchers and abolitionist/welfare advocates: excluded from the property framework; their empirical findings and moral claims are treated as outside the scope of legitimate disagreement within the property reading
 *   - Analytical observer: charts the constraint's persistence and the conditions under which the property premise would be challenged or displaced
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.15).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.42).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animals as Property: Legal-Philosophical Foundation").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, '8ceaf83d-1f2b-4dd4-a466-60ece5985c93').
narrative_ontology:cs_kernel_codification('8ceaf83d-1f2b-4dd4-a466-60ece5985c93', formalized).
narrative_ontology:cs_authority_grounding('8ceaf83d-1f2b-4dd4-a466-60ece5985c93', lineage).
narrative_ontology:cs_interpretation_layer_present('8ceaf83d-1f2b-4dd4-a466-60ece5985c93').
narrative_ontology:cs_reading_relation('8ceaf83d-1f2b-4dd4-a466-60ece5985c93', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ceaf83d-1f2b-4dd4-a466-60ece5985c93', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('8ceaf83d-1f2b-4dd4-a466-60ece5985c93', foundational, animals_lack_independent_moral_standing).
narrative_ontology:cs_axiom_status(animals_lack_independent_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('8ceaf83d-1f2b-4dd4-a466-60ece5985c93', animals_lack_independent_moral_standing, deontological).
narrative_ontology:cs_axiom('8ceaf83d-1f2b-4dd4-a466-60ece5985c93', foundational, human_interests_override_animal_interests_by_definition).
narrative_ontology:cs_axiom_status(human_interests_override_animal_interests_by_definition, holdable).
narrative_ontology:cs_axiom_grounding('8ceaf83d-1f2b-4dd4-a466-60ece5985c93', human_interests_override_animal_interests_by_definition, deontological).
narrative_ontology:cs_reference_frame('8ceaf83d-1f2b-4dd4-a466-60ece5985c93', kantian_rational_agency_baseline).
narrative_ontology:cs_drift_state('8ceaf83d-1f2b-4dd4-a466-60ece5985c93', contemporary_animal_sentience_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8ceaf83d-1f2b-4dd4-a466-60ece5985c93', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, property_owners).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_resource_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold animals under legal property frameworks that grant unlimited use rights (slaughter, confinement, breeding control, medical experimentation). Their ability to extract value from animals is protected by the constraint's core premise that animals lack independent moral standing. Exit from this position means ceding property claims and accepting external moral constraints on their use—economically and legally costly.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, property_owners, beneficiary,
    institutional, generational, arbitrage, global).

% Purchase and consume animal products (food, clothing, research subjects, entertainment) without moral obligation to the animal's own interests. The constraint legitimizes this use by defining animals as resources whose value is exhausted in their utility to humans. They benefit from legal immunity from having to justify use by reference to the animal's experience or preferences.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_resource_users, beneficiary,
    organized, biographical, arbitrage, global).

% Enforce animal property law by recognizing ownership claims, prosecuting theft/damage to animal property, and setting use boundaries via welfare regulations (which presuppose the property premise). They administer the constraint by codifying it into legal doctrine and settling disputes over ownership and permitted use.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, regulatory_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Document animal cognition, pain, preference, and social complexity through empirical study. Their findings challenge the constraint's descriptive premise (that animals lack morally relevant capacities) but are structurally excluded from the property-reading's normative logic: empirical facts about animal minds are treated as irrelevant to property status by definition.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, sentience_researchers, excluded,
    moderate, biographical, constrained, global).

% Argue that the property premise is false or unjust, that animals have independent moral standing, or that suffering should constrain use. They are excluded from the property-reading framework itself—not invited to adjudicate what the constraint permits, because their foundational premise (animals have moral status) is treated as outside the scope of legitimate disagreement within the property framework.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, abolitionist_and_welfare_advocates, excluded,
    moderate, biographical, constrained, global).

% Examines how the constraint structures moral reasoning and legal practice; what empirical evidence would challenge it; what alternative readings exist; and whether the constraint's persistence depends on active suppression of competing claims or on genuine acceptance of the property premise.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__property_reading, property_owners).
narrative_ontology:fixing_cost_class(animal_moral_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates property claims and resource-use rights by establishing a single legal category (property) that animals occupy, eliminating jurisdictional ambiguity about who may claim ownership and what uses are permissible. Solves the distribution problem: what gets allocated to whom and under what legal authority.
% TRANSFER_FUNCTION: Transfers decision-making authority over animals from the animal's own interests (absent in this reading) to human property holders, whose authority is protected by law. The transfer is of use-rights: the ability to shape the animal's entire existence toward human benefit without moral obligation to the animal's experience.
% ABSENT_VOICES: Animals themselves cannot represent their own interests within the property framework—their exclusion is structural, not incidental. Sentience researchers and moral philosophers who hold alternative readings are excluded from adjudicating the constraint's legitimacy; their dissent is treated as operating from a different, incompatible framework rather than as a valid position within debate. Abolitionist and welfare advocates would argue for recategorizing animals as rights-bearing or morally considerable beings but are positioned outside the property reading's scope.
% DISAPPEARANCE_RATIONALE: If the property status of animals were removed overnight, the entire infrastructure of animal agriculture, pharmaceutical testing, wildlife management, and animal-based economies would face immediate crisis: ownership claims would be unrecognizable in law, resource extraction would lose its legal foundation, and the authority to make unilateral decisions about animals would transfer to competing frameworks (welfare, abolitionist, rights-based). The world would reorganize around whatever reading replaced it.
% FOUNDING_PROBLEM: Humans need to use animals (food, labor, testing, materials) and to resolve conflicts over their control and allocation. The property framework solves this by establishing clear ownership and use rights without requiring justification by reference to the animal's own interests—which would create endless moral friction.
% FOUNDING_PROBLEM_CORROBORATION: Property owners and legal institutions attest the founding problem remains live: animals must be allocated and their use coordinated. Sentience researchers and abolitionist philosophers contest whether the problem is solved or merely deferred—they argue the founding problem has been displaced by a derivative problem (how to justify use given evidence of animal sentience) that the property framework ignores. No corroboration from outside the property-benefiting institutions; alternative readings provide the only external account.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__property_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__property_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_moral_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_moral_status__property_reading),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_moral_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The property reading claims mountain status because it asserts a categorical, definition-level claim: animals LACK independent moral standing by their nature as non-rational beings without moral agency. If true, this would be as inescapable as gravity—alternatives would be logically impossible, not merely chosen. However, the authored metrics diverge from this claim: extractiveness is low (0.15) not because the constraint is natural, but because property claims are unambiguous and need little active enforcement—owners simply exercise their rights. Suppression (0.42) is moderate and rising slightly, indicating growing need to defend the property premise against challenge; this rise is the signal that the constraint's naturalness is increasingly contested. Theater (0.28, rising) reflects the increasing frequency of invocations of 'humane' treatment, welfare standards, and other language that acknowledges animal sentience while preserving property status—performative assertion of the property frame in response to countervailing evidence. The measurement series shows suppression requirement rising from 0.35 to 0.42 and theater rising from 0.12 to 0.28, suggesting the constraint is becoming increasingly theatrical and actively defended rather than naturally self-evident. The accessibility_collapse (0.78) is high because once the property frame is accepted, alternatives are hard to imagine—exit from it requires abandoning the entire framework. Resistance (0.55) is substantial because empirical evidence of animal sentience creates a live, articulate opposition that the property frame must actively suppress.
 *
 * PERSPECTIVAL GAP:
 *   Property owners and legal authorities should compute as beneficiaries with low directionality (they collect from the constraint, have exit options into alternative readings they prefer not to take). Sentience researchers and abolitionist advocates compute as excluded/powerless, trapped by the property framework's boundary conditions—they cannot appeal to animal interests because the framework defines animals as having no interests to appeal to. This is not a seat-disagreement about the constraint's operation; it is a disagreement about whether the constraint's core premise is true. The property reading grants them no standing to dispute it within its own logic. The engine's per-seat computation will show this as high-suppression, excluded-seat status.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners: d ≈ 0.1–0.2 (full beneficiaries; they collect use rights and legal immunity). Animal-resource users: d ≈ 0.1–0.2 (beneficiaries; they consume products without moral friction). Regulatory authorities: d ≈ 0.4–0.5 (symmetric; they administer the constraint and bear the cost of defending it against challenge, but they benefit from clear property rules). Sentience researchers and abolitionist advocates: d ≈ 0.8–0.9 (targeted; they are structurally excluded from having standing within the property framework and must bear suppression to voice alternatives). The property frame itself (a non-agent entity) vindicates the constraint by providing philosophical coherence to legal doctrine. Exit options are key: property owners and users have arbitrage-level exit (they could adopt welfare or abolitionist readings, but doing so would cost them property claims and use rights). Researchers and advocates are identity-locked by their commitment to sentience and moral consideration; they cannot adopt the property reading without abandoning their epistemic and moral premises.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem the constraint solves is genuine: humans do use animals and need to resolve conflicts over ownership and use. However, the measured trajectory suggests the founding problem may be CONTESTED rather than LIVE at interval end. The rising theater_ratio (0.12→0.28) indicates that enforcement is increasingly reliant on performative language ('humane,' 'animal welfare,' 'ethical sourcing') that acknowledges tension between property status and evidence of sentience. This is mandatrophy evidence: the constraint persists but its justifying rationale is degrading. The constraint prevents alternative readings from being heard (high suppression requirement rising over time), but it does not prevent them from being believed—rising resistance (0.55) suggests organized opposition is growing. The property reading is being maintained by active defense and performative language rather than by genuine acceptance of its naturalness. This is consistent with a transition toward piton: the constraint persists because the institutional and economic infrastructure depends on it, not because anyone genuinely believes it is natural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the property status of animals a natural law (logically entailed by the structure of moral agency and ownership) or a constructed legal/philosophical doctrine chosen for convenience?',
    'Genealogical analysis of property doctrine development; examination of whether alternative readings were historically available and suppressed, or genuinely impossible; cross-cultural comparison of property frameworks that do not place animals in the property category.',
    'If constructed, the constraint''s persistence depends on active enforcement and suppression of alternatives (moves toward snare); if natural, the constraint is truly a mountain and resistance to it is resistance to logic itself rather than organized opposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether animal property status is logically necessary or institutionally contingent.').

omega_variable(
    empirical_displacement_of_moral_premise,
    'As empirical evidence of animal cognition, emotion, preference, and suffering accumulates, is the property premise''s empirical foundation (animals lack morally relevant capacities) becoming untenable even within the property-reading framework?',
    'Track the evolution of animal welfare law and property-doctrine restatements; examine whether legal authorities increasingly have to acknowledge animal sentience while asserting it does not change property status (= explicit decoupling of the claim from evidence), or whether they deny sentience outright (= unchanged claim).',
    'If decoupling occurs, the constraint becomes explicitly non-natural and demonstrably rides on active suppression of evidence; if denial persists, either the evidence is not as compelling as assumed or the constraint is genuinely independent of empirical facts about animals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_displacement_of_moral_premise, empirical, 'Whether empirical discoveries about animal sentience can force revision of the property premise or whether the constraint is normative and empirics-independent.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression measured (0.42 at interval end) maintained by structural barriers (legal liability for property damage, economic dependence on animal-product consumption, institutional gate-keeping) or by internalized acceptance of the property premise?',
    'Post-exposure experiments: show property-doctrine skeptics counterfactual scenarios (what if property law changed), measure whether resistance rises; examine cross-jurisdictional variation in suppression where institutional barriers differ but ideological commitment is comparable.',
    'If structural, removing legal barriers might be sufficient; if internalized, resistance would persist after barriers fall and the constraint''s persistence does not require active enforcement (piton signature). The mix determines whether remedies are legislative or require deep epistemic shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of alternative readings is maintained by legal/economic structure or cultural-cognitive commitment.').

omega_variable(
    reading_kernel_relationship,
    'Is the property reading a legitimate alternative instantiation of the contested animal_moral_status kernel, or does it foreclose the other readings by asserting an incompatible fundamental premise?',
    'Examine the logical structure: does the property reading claim (animals lack independent moral status by definition/nature) directly entail the negation of the abolitionist claim (animals have independent moral status), or are they incommensurable framings that different communities hold simultaneously without logical contradiction?',
    'If it forecloses, the readings form a genuine either/or; if they coexist, the kernel is structurally open and the property reading is one live option among others, not the natural baseline. This determines whether the constraint is a mountain (universal, inescapable) or a rope with alternatives present.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_relationship, conceptual, 'Whether the property reading''s core axiom logically forecloses or merely coexists with abolitionist and welfare readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t8, animal_moral_status__property_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(anim_tr_t8, observed).
narrative_ontology:measurement(anim_tr_t16, animal_moral_status__property_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement_basis(anim_tr_t16, observed).
narrative_ontology:measurement(anim_tr_t24, animal_moral_status__property_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement_basis(anim_tr_t24, observed).
narrative_ontology:measurement(anim_tr_t32, animal_moral_status__property_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement_basis(anim_tr_t32, observed).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__property_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(anim_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t8, animal_moral_status__property_reading, base_extractiveness, 8, 0.1).
narrative_ontology:measurement_basis(anim_be_t8, observed).
narrative_ontology:measurement(anim_be_t16, animal_moral_status__property_reading, base_extractiveness, 16, 0.12).
narrative_ontology:measurement_basis(anim_be_t16, observed).
narrative_ontology:measurement(anim_be_t24, animal_moral_status__property_reading, base_extractiveness, 24, 0.14).
narrative_ontology:measurement_basis(anim_be_t24, observed).
narrative_ontology:measurement(anim_be_t32, animal_moral_status__property_reading, base_extractiveness, 32, 0.15).
narrative_ontology:measurement_basis(anim_be_t32, observed).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__property_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(anim_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__property_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t8, animal_moral_status__property_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement_basis(anim_su_t8, observed).
narrative_ontology:measurement(anim_su_t16, animal_moral_status__property_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement_basis(anim_su_t16, observed).
narrative_ontology:measurement(anim_su_t24, animal_moral_status__property_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement_basis(anim_su_t24, observed).
narrative_ontology:measurement(anim_su_t32, animal_moral_status__property_reading, suppression_requirement, 32, 0.42).
narrative_ontology:measurement_basis(anim_su_t32, observed).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__property_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(anim_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(animal_moral_status__property_reading, 0.12).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__welfare_reading).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_moral_status kernel instantiates three distinct constraints: property_reading (this file), welfare_reading, and abolitionist_reading. Each reading generates a different constraint with independent ε, beneficiary/victim structure, and classification. The property reading forecloses the abolitionist reading (incompatible core premises about moral status) and coexists with the welfare reading (both permit use, but disagree on justification). The welfare reading influences the property reading by creating pressure toward performative acknowledgment of sentience while preserving property status. These are not alternative measurements of one constraint; they are structurally distinct constraints generated by different readings of a single contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_moral_status__property_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
