% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__property_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animal Property Status — Property Reading
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The property reading instantiates the animal_status kernel by treating
 *   animals as legal objects — property in the full Roman-law sense — with no
 *   independent moral standing. Constraints on animal use exist only as
 *   human-to-human property disputes (trespass, damage, theft) or as
 *   voluntarily adopted welfare standards that owners may accept or reject.
 *   The reading's ε ~0.05 reflects near-zero extractiveness from the
 *   reading's own structural assessment: the standing arrangement extracts
 *   from animals only if animals are in the victim set, and this reading
 *   places them outside it. What looks like extraction from an abolitionist
 *   seat is, from this reading, the normal operation of property rights. The
 *   claimed_type mountain reflects the reading's self-understanding:
 *   animals-as-property is presented as a natural legal category, not a
 *   constructed constraint. The beneficiaries array and FSM omegas document
 *   the irreducible ambiguity: is this a genuine natural-law mountain
 *   (property as pre-political right) or a false summit benefiting animal
 *   owners and industries?
 *
 * KEY AGENTS:
 *   - animal_owners: Primary beneficiary (institutional/arbitrage) — hold full property rights over animals; exit via sale or abandonment
 *   - animal_industry_operators: Primary beneficiary (institutional/arbitrage) — commercialize animal bodies at scale; exit via market restructuring
 *   - legal_institutions: Agenda setter (institutional/analytical) — administer the property regime; exit via legislative reform
 *   - animal_welfare_advocates: Excluded (organized/constrained) — seek welfare improvements within the property frame; cannot challenge the frame itself from inside it
 *   - animals: Excluded (powerless/trapped) — the reading's structural premise places them outside the victim set entirely; no exit, no voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.05).
domain_priors:suppression_score(animal_status__property_reading, 0.08).
domain_priors:theater_ratio(animal_status__property_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, mountain).
narrative_ontology:human_readable(animal_status__property_reading, "Animal Property Status — Property Reading").
narrative_ontology:topic_domain(animal_status__property_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:emerges_naturally(animal_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, '8ed207f9-5eb9-46d7-9148-1d9a7676931c').
narrative_ontology:cs_kernel_codification('8ed207f9-5eb9-46d7-9148-1d9a7676931c', formalized).
narrative_ontology:cs_authority_grounding('8ed207f9-5eb9-46d7-9148-1d9a7676931c', lineage).
narrative_ontology:cs_interpretation_layer_present('8ed207f9-5eb9-46d7-9148-1d9a7676931c').
narrative_ontology:cs_reading_relation('8ed207f9-5eb9-46d7-9148-1d9a7676931c', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_reading_relation('8ed207f9-5eb9-46d7-9148-1d9a7676931c', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('8ed207f9-5eb9-46d7-9148-1d9a7676931c', foundational, animals_are_legal_objects).
narrative_ontology:cs_axiom_status(animals_are_legal_objects, holdable).
narrative_ontology:cs_axiom_grounding('8ed207f9-5eb9-46d7-9148-1d9a7676931c', animals_are_legal_objects, conventional).
narrative_ontology:cs_axiom('8ed207f9-5eb9-46d7-9148-1d9a7676931c', foundational, property_rights_preclude_independent_moral_standing).
narrative_ontology:cs_axiom_status(property_rights_preclude_independent_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('8ed207f9-5eb9-46d7-9148-1d9a7676931c', property_rights_preclude_independent_moral_standing, conventional).
narrative_ontology:cs_axiom('8ed207f9-5eb9-46d7-9148-1d9a7676931c', secondary, welfare_constraints_are_voluntary_owner_choices).
narrative_ontology:cs_axiom_status(welfare_constraints_are_voluntary_owner_choices, holdable).
narrative_ontology:cs_axiom_grounding('8ed207f9-5eb9-46d7-9148-1d9a7676931c', welfare_constraints_are_voluntary_owner_choices, conventional).
narrative_ontology:cs_reference_frame('8ed207f9-5eb9-46d7-9148-1d9a7676931c', roman_law_property_baseline).
narrative_ontology:cs_drift_state('8ed207f9-5eb9-46d7-9148-1d9a7676931c', contemporary_welfare_statute_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8ed207f9-5eb9-46d7-9148-1d9a7676931c', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_industry_operators).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, legal_institutions).
narrative_ontology:constraint_vindicates(animal_status__property_reading, property_rights_coherence).
narrative_ontology:constraint_vindicates(animal_status__property_reading, human_exceptionalism_doctrine).
narrative_ontology:constraint_vindicates(animal_status__property_reading, legal_object_subject_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold full property rights over animals — can buy, sell, breed, use, and kill within minimal welfare constraints. The property regime subsidizes their control. Exit is trivial: sell the animals or exit the industry. No structural barrier to leaving the arrangement.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_owners, beneficiary,
    institutional, biographical, arbitrage, global).

% Operate at commercial scale: factory farming, research, entertainment, companionship industries. The property regime enables capitalization of animal bodies. Exit requires market restructuring but capital is mobile; they can shift to alternative protein, biotech, or other sectors.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_industry_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Administer the property regime: define ownership rights, adjudicate disputes, enact welfare statutes. They set the agenda for what counts as property and what limits exist. Exit is legislative reform — they could change the regime but are structurally positioned to maintain it.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, legal_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Work within the property frame to improve welfare standards. They cannot challenge the property frame itself from inside it — their advocacy accepts animals as property and seeks better treatment. Exit from the frame means adopting abolitionist reading, which excludes them from current policy channels.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_welfare_advocates, excluded,
    organized, biographical, constrained, national).

% The reading's structural premise places animals outside the victim set entirely — they are the objects of the property relation, not parties to it. No voice, no exit, no standing. The directionality override (powerless → d=0.95) captures that from any seat recognizing them as affected, they are full targets. But this reading does not recognize them as affected.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animals, excluded,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(animal_status__property_reading, animals).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Orders human claims over animal bodies by establishing a clear, enforceable property regime — who owns which animal, what transfers are valid, how disputes between humans about animals are resolved. Solves the coordination problem of rival human claims on the same animal bodies.
% TRANSFER_FUNCTION: Moves control over animal bodies and their products from the commons/unowned state into private ownership. The transfer is from 'no one' (or prior owner) to current owner. No transfer from animals because animals are not recognized as having anything to transfer.
% ABSENT_VOICES: Animals themselves — they would object to being property if they could articulate preferences, but the reading's structure excludes them by definition. Abolitionist advocates — they are excluded from the property_reading's framework because they reject its founding premise. Their voices are present in the abolitionist_reading constraint, not here.
% DISAPPEARANCE_RATIONALE: If the property regime vanished overnight, human claims over animals would become unresolvable by law — disputes over ownership, use, and disposition would have no legal framework. The animal industry economy would collapse or reorganize around new frameworks (trusteeship, guardianship, rights). The world of human-animal relations would fundamentally rearrange.
% FOUNDING_PROBLEM: Pre-legal societies faced unresolvable disputes over animal bodies: who killed the game, who owns the herd, who bears loss when animals damage property. The property regime was built to order these human-to-human claims by making animals objects of exclusive ownership.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Pipes on property origins, Brenner on Roman law) attest the founding problem was ordering human claims. Animal law scholars (Francione, Favre) attest the problem is contested: the property frame solved human disputes but created the animal-status problem. No corroboration from outside the beneficiary set that the property frame is the only or best solution — indigenous legal scholars (e.g., Borrows, Whyte) document non-property frameworks for human-animal relations.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__property_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_status__property_reading),
    narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.05) because this reading's referent — the standing arrangement of animals as property — does not extract from parties the reading recognizes as victims. The low suppression (0.08) reflects that the constraint holds by legal definition, not active enforcement against resistance from recognized parties. Theater ratio (0.12) captures the performative aspect of welfare statutes that exist alongside but do not constrain the core property right. High accessibility_collapse (0.88) and low resistance (0.04) are consistent with a mountain: alternatives (animals as rights-holders, animals as sentient beings with interests) collapse once the property frame is accepted, and the reading meets almost no resistance from within its own framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal owners and industry operators are structural beneficiaries (d ≈ 0.15) — the property regime subsidizes their use of animals. Legal institutions are agenda_setters with analytical exit (d ≈ 0.35). Animal welfare advocates are excluded: they operate within the frame but cannot challenge its foundations (d ≈ 0.75, constrained exit). Animals are not in the victim set per this reading; the directionality derivation chain has no input for them. The FSM omegas document that this beneficiary structure is exactly the ambiguity the false_summit_mountain signature evaluates.
 *
 * MANDATROPHY ANALYSIS:
 *   The property reading presents animals-as-property as a stable natural-law category (mountain), but the declared beneficiaries reveal the arrangement benefits identifiable agents. The FSM machinery will test whether this is a genuine mountain or a false summit — a constructed constraint that presents as natural law while benefiting animal owners and industries. The mandatrophy question: does the property regime solve a live coordination problem (resource allocation, dispute resolution) or has its founding problem (ordering human claims over animal bodies) been superseded by the abolitionist and welfare readings' challenges?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_property,
    'Is the animals-as-property category a genuine natural-law mountain (property rights as pre-political, animals as natural objects of ownership) or a constructed constraint that benefits identifiable agents (animal owners, industries) by placing animals outside the victim set?',
    'Cross-cultural legal history: do all legal systems converge on animals-as-property, or is this a contingent historical formation? Comparative analysis of pre-colonial legal systems, indigenous ontologies, and Roman-law diffusion patterns.',
    'If natural law, the mountain claim holds and FSM does not fire. If constructed, FSM reclassifies to tangled_rope (coordination of human claims + asymmetric extraction from animals via the property frame).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_property, conceptual, 'Whether animals-as-property is a natural legal category or a constructed false summit').

omega_variable(
    animal_victim_set_exclusion,
    'Does the property reading''s exclusion of animals from the victim set reflect a structural feature of the constraint (animals genuinely cannot be victims of their own property status) or an analytical choice that serves the reading''s beneficiaries?',
    'Test the reading''s own logic: if animals were placed in the victim set, would the constraint''s extractiveness become measurable? The ε-invariance principle requires the reading to assess the standing arrangement by its own lights — but the reading''s lights include the premise that animals are not victims. This circularity is the omega.',
    'If the exclusion is analytical choice, the reading''s ε ~0.05 is an artifact of its framing, not a structural fact. The engine''s per-seat computation from the abolitionist seat would yield high χ for animals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(animal_victim_set_exclusion, conceptual, 'Whether animal exclusion from victim set is structural or analytical').

omega_variable(
    welfare_statutes_as_voluntary_vs_enforced,
    'Are welfare statutes genuinely voluntary standards that owners may adopt or reject, or do they function as enforced constraints that limit property rights in practice?',
    'Empirical analysis of welfare statute enforcement: prosecution rates, compliance costs, industry lobbying against standards. If enforcement is substantial, the ''voluntary'' claim is theater and suppression is higher than authored.',
    'If enforced, suppression rises, requires_active_enforcement becomes true, and the constraint may shift from mountain toward tangled_rope (coordination + asymmetric extraction via welfare floor).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_statutes_as_voluntary_vs_enforced, empirical, 'Whether welfare statutes are voluntary or enforced constraints on property rights').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t25, animal_status__property_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(anim_tr_t25, observed).
narrative_ontology:measurement(anim_tr_t50, animal_status__property_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement_basis(anim_tr_t50, observed).
narrative_ontology:measurement(anim_tr_t75, animal_status__property_reading, theater_ratio, 75, 0.12).
narrative_ontology:measurement_basis(anim_tr_t75, observed).
narrative_ontology:measurement(anim_tr_t100, animal_status__property_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement_basis(anim_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t25, animal_status__property_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement_basis(anim_be_t25, observed).
narrative_ontology:measurement(anim_be_t50, animal_status__property_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement_basis(anim_be_t50, observed).
narrative_ontology:measurement(anim_be_t75, animal_status__property_reading, base_extractiveness, 75, 0.05).
narrative_ontology:measurement_basis(anim_be_t75, observed).
narrative_ontology:measurement(anim_be_t100, animal_status__property_reading, base_extractiveness, 100, 0.05).
narrative_ontology:measurement_basis(anim_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__property_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t25, animal_status__property_reading, suppression_requirement, 25, 0.08).
narrative_ontology:measurement_basis(anim_su_t25, observed).
narrative_ontology:measurement(anim_su_t50, animal_status__property_reading, suppression_requirement, 50, 0.08).
narrative_ontology:measurement_basis(anim_su_t50, observed).
narrative_ontology:measurement(anim_su_t75, animal_status__property_reading, suppression_requirement, 75, 0.08).
narrative_ontology:measurement_basis(anim_su_t75, observed).
narrative_ontology:measurement(anim_su_t100, animal_status__property_reading, suppression_requirement, 100, 0.08).
narrative_ontology:measurement_basis(anim_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status__property_reading, 0.03).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three constraint stories by ε-invariance: property_reading (ε~0.05, mountain claimed), welfare_reading (ε~0.35, tangled_rope expected), abolitionist_reading (ε~0.85, snare expected from property seat). Each reading instantiates a different constraint with different beneficiary/victim structures. The property_reading is upstream: its legal baseline is what the welfare_reading modulates and the abolitionist_reading rejects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status__property_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
