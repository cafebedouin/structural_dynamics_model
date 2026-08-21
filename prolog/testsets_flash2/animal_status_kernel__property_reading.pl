% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__property_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: animal_status_kernel__property_reading
 *   human_readable: Animal Property Status (Property Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the 'property reading' of the animal status
 *   kernel, where animals are legally defined as property, and their moral
 *   considerability is entirely derivative of human ownership rights.
 *   Economic value is the primary, if not sole, relevant value. This reading
 *   excludes animals from the victim-set entirely in its own framework, and
 *   grants owners unrestricted use rights, subject only to anti-cruelty
 *   statutes that protect property value rather than animal interests. The
 *   high extractiveness reflects the complete instrumentalization of animals
 *   under this framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.95).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.98).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal Property Status (Property Reading)").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, '26f8497e-2387-4ac7-bb2d-efc262189fad').
narrative_ontology:cs_kernel_codification('26f8497e-2387-4ac7-bb2d-efc262189fad', formalized).
narrative_ontology:cs_authority_grounding('26f8497e-2387-4ac7-bb2d-efc262189fad', lineage).
narrative_ontology:cs_interpretation_layer_present('26f8497e-2387-4ac7-bb2d-efc262189fad').
narrative_ontology:cs_reading_relation('26f8497e-2387-4ac7-bb2d-efc262189fad', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('26f8497e-2387-4ac7-bb2d-efc262189fad', animal_status_kernel__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('26f8497e-2387-4ac7-bb2d-efc262189fad', foundational, animals_are_property).
narrative_ontology:cs_axiom_status(animals_are_property, holdable).
narrative_ontology:cs_axiom_grounding('26f8497e-2387-4ac7-bb2d-efc262189fad', animals_are_property, conventional).
narrative_ontology:cs_axiom('26f8497e-2387-4ac7-bb2d-efc262189fad', foundational, moral_considerability_derives_from_ownership).
narrative_ontology:cs_axiom_status(moral_considerability_derives_from_ownership, holdable).
narrative_ontology:cs_axiom_grounding('26f8497e-2387-4ac7-bb2d-efc262189fad', moral_considerability_derives_from_ownership, conventional).
narrative_ontology:cs_reference_frame('26f8497e-2387-4ac7-bb2d-efc262189fad', classical_property_law).
narrative_ontology:cs_drift_state('26f8497e-2387-4ac7-bb2d-efc262189fad', contemporary_animal_ethics_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('26f8497e-2387-4ac7-bb2d-efc262189fad', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_use_industries).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, animals_as_property).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, property_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, economic_utility_maximization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess full legal rights over animals as property, including rights of use, sale, and destruction. Their economic interests are prioritized, and they actively defend the legal framework that grants these rights.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_owners, agenda_setter,
    powerful, generational, mobile, national).

% Benefit directly from the legal status of animals as property, enabling large-scale exploitation for food, research, entertainment, and other economic purposes without significant moral or legal impediment beyond basic anti-cruelty statutes (which protect property value, not animal interests).
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_use_industries, beneficiary,
    institutional, generational, mobile, global).

% Are legally defined as property, lacking moral or legal standing beyond their economic value or the owner's interest. They bear the full cost of exploitation, suffering, and death without recourse, as their considerability is entirely derivative of ownership.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animals_as_property, payer,
    powerless, immediate, trapped, universal).

% Enforces property rights, including those pertaining to animals. It provides the framework that defines animals as property and adjudicates disputes based on this foundational premise, largely ignoring intrinsic animal interests.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, legal_system, agenda_setter,
    institutional, civilizational, constrained, national).

% Seek to improve animal conditions within the property framework, but their efforts are constrained by the primacy of ownership rights and economic value. They are excluded from challenging the fundamental property status of animals within this reading.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, welfare_advocates, excluded,
    moderate, generational, constrained, national).

% Seek to dismantle the property status of animals entirely, arguing for their moral personhood. Their core premise is fundamentally rejected by this reading, rendering them structurally excluded from the conversation within this framework.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, abolitionist_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, unambiguous framework for the ownership, trade, and use of animals, facilitating economic activity and resource allocation by treating animals as fungible commodities.
% TRANSFER_FUNCTION: Transfers full control and use rights over animals to owners, enabling the extraction of economic value (meat, milk, labor, research data) from animals to human beneficiaries.
% ABSENT_VOICES: Animals themselves are entirely absent from the legal and moral discourse, as their interests are not recognized. Abolitionist and welfare advocates are present but structurally excluded from challenging the core property premise within this framework.
% DISAPPEARANCE_RATIONALE: If the legal and moral framework defining animals as property vanished overnight, the entire animal-use economy (agriculture, research, entertainment) would collapse, requiring a fundamental re-evaluation of human-animal relationships and the creation of new legal structures for animal considerability.
% FOUNDING_PROBLEM: To establish clear legal and economic control over animals for human benefit, facilitating resource management, food production, and labor exploitation in early human societies.
% FOUNDING_PROBLEM_CORROBORATION: Animal owners and industries attest the problem is live, citing the need for efficient resource management and economic productivity. Welfare and abolitionist advocates contest this, arguing the 'problem' is a justification for exploitation, not a genuine coordination challenge.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_status_kernel__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__property_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__property_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__property_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because animals are treated as mere resources, with no intrinsic value or rights to limit exploitation. Suppression is also very high (0.98) as the legal system actively enforces property rights, effectively trapping animals within this status. Theater ratio is low (0.05) because the system is highly functional in its stated purpose of facilitating animal use; there is little performative maintenance masking a degraded function. Accessibility collapse is high (0.9) because the legal framework leaves virtually no alternatives for animals to escape their property status. Resistance is low (0.1) from within the framework, as any significant resistance comes from outside (welfare/abolitionist movements) and is structurally suppressed by the property paradigm itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of animal owners and industries, this constraint is a foundational 'rope' or even 'mountain' that enables economic activity and resource management. From the perspective of animals (if they could have one) or abolitionist advocates, it is a pure 'snare' of total extraction and suppression. The engine's classification will reflect the latter due to the high extractiveness and suppression metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal owners and industries are clear beneficiaries, gaining full control and economic value from animals. Animals themselves are the primary victims, bearing the full cost of exploitation. The legal system acts as an agenda-setter, enforcing the property status. Welfare and abolitionist advocates are structurally excluded from challenging the core premise within this reading, making their directionality effectively that of targets of the constraint's enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (facilitating human use of animals) remains 'live' within its own framework, preventing mandatrophy. The classification as a snare accurately reflects the structural reality of total extraction and suppression for animals, rather than mislabeling it as coordination, by focusing on the actual flows of value and control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_vs_personhood_ambiguity,
    'Is the legal status of animals as property a natural, inevitable arrangement, or a constructed legal fiction that could be revised?',
    'Comparative legal analysis of jurisdictions that have granted limited personhood rights to certain animals (e.g., great apes, cetaceans), or philosophical arguments for intrinsic animal rights.',
    'If constructed and revisable, the constraint''s ''mountain-like'' appearance for owners is a false summit, and its classification as a snare is reinforced. If truly inevitable, the constraint would move closer to a mountain for all seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_vs_personhood_ambiguity, conceptual, 'Ambiguity regarding the naturalness vs. constructedness of animal property status.').

omega_variable(
    anti_cruelty_statutes_impact,
    'Do anti-cruelty statutes genuinely protect animal interests, or primarily protect the economic value of animals as property?',
    'Analysis of legal precedents and enforcement patterns: do convictions occur when animal suffering does not diminish economic value, or only when it does? Compare penalties for harm to animals vs. harm to human property.',
    'If anti-cruelty laws primarily protect property value, the measured extractiveness and suppression are accurate. If they genuinely protect animal interests, the effective extractiveness is slightly lower, and the constraint might have a minor ''tangled rope'' aspect for owners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_cruelty_statutes_impact, empirical, 'The true function of anti-cruelty laws within the property framework.').

omega_variable(
    moral_considerability_grounding,
    'Is moral considerability inherently tied to ownership, or can it be grounded in sentience, consciousness, or other intrinsic animal characteristics?',
    'Philosophical consensus shifts, or legal recognition of animal sentience as a basis for rights independent of ownership.',
    'If considerability can be grounded intrinsically, the foundational axiom of this reading is challenged, potentially leading to a reclassification towards a welfare or abolitionist reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_considerability_grounding, conceptual, 'The philosophical grounding of moral considerability for animals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__property_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(anim_tr_t100, animal_status_kernel__property_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement(anim_tr_t150, animal_status_kernel__property_reading, theater_ratio, 150, 0.05).
narrative_ontology:measurement(anim_tr_t200, animal_status_kernel__property_reading, theater_ratio, 200, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__property_reading, base_extractiveness, 50, 0.92).
narrative_ontology:measurement(anim_be_t100, animal_status_kernel__property_reading, base_extractiveness, 100, 0.93).
narrative_ontology:measurement(anim_be_t150, animal_status_kernel__property_reading, base_extractiveness, 150, 0.94).
narrative_ontology:measurement(anim_be_t200, animal_status_kernel__property_reading, base_extractiveness, 200, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__property_reading, suppression_requirement, 50, 0.96).
narrative_ontology:measurement(anim_su_t100, animal_status_kernel__property_reading, suppression_requirement, 100, 0.97).
narrative_ontology:measurement(anim_su_t150, animal_status_kernel__property_reading, suppression_requirement, 150, 0.97).
narrative_ontology:measurement(anim_su_t200, animal_status_kernel__property_reading, suppression_requirement, 200, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_welfare_regulations).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, food_production_standards).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, biomedical_research_ethics).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'animal_status_kernel'. It represents the property-centric view, distinct from the welfare-centric and abolitionist readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
