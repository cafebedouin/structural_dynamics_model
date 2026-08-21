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
 *   human_readable: Animal Status: Property Reading
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the 'property reading' of the animal status
 *   kernel, where animals are legally and morally considered property, and
 *   their moral considerability derives solely from ownership rights and
 *   economic value. This reading asserts unrestricted use rights for owners,
 *   subject only to minimal anti-cruelty statutes that protect property value
 *   rather than animal interests. The constraint is highly extractive, as
 *   animals are entirely excluded from the victim-set in this framework, and
 *   their interests are not recognized as countervailing moral constraints on
 *   human use.
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
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal Status: Property Reading").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, '2152f7a9-7740-4154-876c-7f55e6914233').
narrative_ontology:cs_kernel_codification('2152f7a9-7740-4154-876c-7f55e6914233', formalized).
narrative_ontology:cs_authority_grounding('2152f7a9-7740-4154-876c-7f55e6914233', lineage).
narrative_ontology:cs_interpretation_layer_present('2152f7a9-7740-4154-876c-7f55e6914233').
narrative_ontology:cs_reading_relation('2152f7a9-7740-4154-876c-7f55e6914233', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('2152f7a9-7740-4154-876c-7f55e6914233', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('2152f7a9-7740-4154-876c-7f55e6914233', foundational, animals_are_chattel_property).
narrative_ontology:cs_axiom_status(animals_are_chattel_property, holdable).
narrative_ontology:cs_axiom_grounding('2152f7a9-7740-4154-876c-7f55e6914233', animals_are_chattel_property, conventional).
narrative_ontology:cs_axiom('2152f7a9-7740-4154-876c-7f55e6914233', foundational, moral_considerability_derives_from_ownership).
narrative_ontology:cs_axiom_status(moral_considerability_derives_from_ownership, holdable).
narrative_ontology:cs_axiom_grounding('2152f7a9-7740-4154-876c-7f55e6914233', moral_considerability_derives_from_ownership, deontological).
narrative_ontology:cs_reference_frame('2152f7a9-7740-4154-876c-7f55e6914233', classical_property_law_framework).
narrative_ontology:cs_drift_state('2152f7a9-7740-4154-876c-7f55e6914233', contemporary_animal_ethics_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2152f7a9-7740-4154-876c-7f55e6914233', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_use_industries).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, animals_as_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess legal rights to use, sell, or dispose of animals as property. Their moral considerability for animals is primarily tied to their ownership rights and the economic value derived from the animals. They benefit from the unrestricted use of animals for various purposes.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_owners, agenda_setter,
    powerful, biographical, mobile, global).

% Industries (e.g., agriculture, research, entertainment) that rely on the property status of animals for their economic models. They benefit from the lack of significant moral or legal constraints on animal use, allowing for cost-effective operations.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_use_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Are legally defined as property, lacking inherent moral standing or rights. Their existence and treatment are entirely subject to the will and economic interests of their owners. They bear the full cost of this constraint, including suffering, exploitation, and death, without recourse.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animals_as_property, payer,
    powerless, immediate, trapped, universal).

% Advocate for improved treatment of animals but operate within a legal framework that primarily recognizes animals as property. Their efforts are often limited to mitigating cruelty that impacts property value or public sentiment, rather than challenging the fundamental property status itself.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_welfare_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear legal and moral framework for the ownership and use of animals, facilitating economic activities and resource allocation without complex ethical considerations beyond property rights.
% TRANSFER_FUNCTION: Transfers full control and the right to exploit animals from the animals themselves (who have no standing) to their human owners, enabling the extraction of economic value.
% ABSENT_VOICES: Animals themselves are structurally excluded from any moral or legal discourse, as their status as property denies them a voice. Abolitionist and welfare advocates are largely excluded from the foundational legal and philosophical debates that establish property status, relegated to advocating for minor reforms within the existing framework.
% DISAPPEARANCE_RATIONALE: If animals were no longer considered property overnight, the global economy, particularly agriculture, research, and entertainment, would face immediate and profound disruption. Legal systems would need to redefine animal status, and human-animal relationships would undergo a fundamental ethical and practical reorganization.
% FOUNDING_PROBLEM: To establish a clear, unambiguous framework for human dominion over animals, facilitating their instrumental use for human benefit (food, labor, clothing, research) without moral or legal impediment.
% FOUNDING_PROBLEM_CORROBORATION: Animal owners and industries attest that the problem of managing and utilizing animal resources efficiently for human benefit remains live. This is corroborated by the continued reliance of global economies on animal products and services, and the legal systems that uphold animal property status, though animal welfare advocates contest the ethical necessity of this framework.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness is near maximal (0.95) because animals are treated as mere instruments for human benefit, with no inherent rights or moral standing. Suppression is also near maximal (0.98) as the legal and philosophical framework actively denies animals any agency or means of resistance, and alternatives to this property status are systematically suppressed within this reading. Theater ratio is minimal (0.05) because the constraint's function is straightforward extraction; any 'welfare' considerations are purely instrumental to maintaining property value or public acceptance, not genuine concern for the animal's well-being. Accessibility collapse is high (0.9) as the property framework fundamentally collapses any alternative moral or legal status for animals. Resistance is low (0.1) within this framework, as any resistance is external to the constraint's internal logic.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of animal owners and industries, this constraint is a foundational, efficient framework for resource management. From the perspective of animals (if they could have one), it is total subjugation. The engine's classification will highlight this extreme divergence, showing a snare from the animal's seat and a beneficial, efficient system from the owner's seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal owners and animal-use industries are full beneficiaries (d near 0.0), as the constraint directly subsidizes their activities by granting them unrestricted access to animal resources. Animals themselves are full targets (d near 1.0), bearing the entire cost of the constraint without any benefits. Animal welfare advocates are excluded, their efforts largely ineffective against the foundational property status.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_grounding,
    'Is moral considerability inherently tied to property status, or does it arise from other attributes like sentience or personhood?',
    'Philosophical consensus shift or legal redefinition of animal status based on scientific understanding of sentience and cognitive abilities.',
    'If moral considerability is found to derive from sentience (welfare reading) or personhood (abolitionist reading), the property reading''s foundational axiom would be challenged, leading to a reclassification towards a tangled_rope or snare with animals as victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_grounding, conceptual, 'Ambiguity in the foundational basis of moral considerability for animals.').

omega_variable(
    economic_value_primacy,
    'Is economic value the only relevant value for animals, or do intrinsic values (e.g., ecological, aesthetic, species-specific) also hold moral weight?',
    'Societal shift in values, legal recognition of intrinsic animal value, or economic models that internalize non-market animal values.',
    'If intrinsic values gain moral weight, the property reading''s justification for unrestricted use would weaken, potentially leading to increased regulation and a shift towards a welfare-constrained property model (tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_value_primacy, preference, 'Whether economic value is the sole determinant of animal worth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t25, animal_status_kernel__property_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__property_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(anim_tr_t75, animal_status_kernel__property_reading, theater_ratio, 75, 0.05).
narrative_ontology:measurement(anim_tr_t100, animal_status_kernel__property_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(anim_be_t25, animal_status_kernel__property_reading, base_extractiveness, 25, 0.95).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__property_reading, base_extractiveness, 50, 0.95).
narrative_ontology:measurement(anim_be_t75, animal_status_kernel__property_reading, base_extractiveness, 75, 0.95).
narrative_ontology:measurement(anim_be_t100, animal_status_kernel__property_reading, base_extractiveness, 100, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.98).
narrative_ontology:measurement(anim_su_t25, animal_status_kernel__property_reading, suppression_requirement, 25, 0.98).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__property_reading, suppression_requirement, 50, 0.98).
narrative_ontology:measurement(anim_su_t75, animal_status_kernel__property_reading, suppression_requirement, 75, 0.98).
narrative_ontology:measurement(anim_su_t100, animal_status_kernel__property_reading, suppression_requirement, 100, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
