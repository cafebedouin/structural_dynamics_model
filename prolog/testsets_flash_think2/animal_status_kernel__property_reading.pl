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
 *   domain: Moral Philosophy/Animal Ethics/Legal Theory
 *
 * SUMMARY:
 *   This constraint represents the 'property reading' of the animal status
 *   kernel, where animals are legally defined as property, and their moral
 *   considerability is entirely derived from human ownership rights. Economic
 *   value is the primary, if not sole, relevant value. This reading underpins
 *   vast economic systems and legal traditions, treating animals as resources
 *   for human use. The high extractiveness and suppression reflect the
 *   complete instrumentalization of animals within this framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.92).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.95).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal Property Status (Property Reading)").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "Moral Philosophy/Animal Ethics/Legal Theory").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, 'e10eaab5-12e5-4266-92c5-1b368057721e').
narrative_ontology:cs_kernel_codification('e10eaab5-12e5-4266-92c5-1b368057721e', formalized).
narrative_ontology:cs_authority_grounding('e10eaab5-12e5-4266-92c5-1b368057721e', lineage).
narrative_ontology:cs_interpretation_layer_present('e10eaab5-12e5-4266-92c5-1b368057721e').
narrative_ontology:cs_reading_relation('e10eaab5-12e5-4266-92c5-1b368057721e', animal_status_kernel__welfare_reading, influences).
narrative_ontology:cs_reading_relation('e10eaab5-12e5-4266-92c5-1b368057721e', animal_status_kernel__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('e10eaab5-12e5-4266-92c5-1b368057721e', foundational, animals_are_chattel).
narrative_ontology:cs_axiom_status(animals_are_chattel, holdable).
narrative_ontology:cs_axiom_grounding('e10eaab5-12e5-4266-92c5-1b368057721e', animals_are_chattel, conventional).
narrative_ontology:cs_axiom('e10eaab5-12e5-4266-92c5-1b368057721e', foundational, moral_status_derived_from_ownership).
narrative_ontology:cs_axiom_status(moral_status_derived_from_ownership, holdable).
narrative_ontology:cs_axiom_grounding('e10eaab5-12e5-4266-92c5-1b368057721e', moral_status_derived_from_ownership, conventional).
narrative_ontology:cs_reference_frame('e10eaab5-12e5-4266-92c5-1b368057721e', classical_property_regime).
narrative_ontology:cs_drift_state('e10eaab5-12e5-4266-92c5-1b368057721e', contemporary_animal_rights_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e10eaab5-12e5-4266-92c5-1b368057721e', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_industries).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, animals).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, property_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, economic_efficiency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or entities who legally own animals. They possess extensive rights to use, sell, or dispose of animals, deriving economic and personal value from them. Their interests are prioritized by the legal framework.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_owners, agenda_setter,
    powerful, biographical, mobile, global).

% Sectors such as agriculture, research, entertainment, and pet breeding that rely on the property status of animals for their business models. They benefit from the clear legal framework that permits instrumental use of animals.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_industries, beneficiary,
    institutional, generational, arbitrage, global).

% The subjects of the property status. They are treated as commodities or resources, with their intrinsic interests and moral considerability largely ignored, except where their welfare impacts their economic value to owners. They bear the full cost of instrumentalization.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animals, payer,
    powerless, immediate, trapped, universal).

% The body of laws, courts, and enforcement mechanisms that define and uphold animal property status. It provides the framework for ownership, transfer, and use, and enforces anti-cruelty statutes primarily to protect property value or public sensibilities, not animal interests per se.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% Groups and individuals who seek to improve the conditions of animals within the property framework. While they may influence specific regulations, their core challenge to the property status itself is largely outside the legal system's direct consideration under this reading.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, welfare_advocates, excluded,
    organized, biographical, constrained, global).

% Scholars and activists who fundamentally reject the property status of animals, arguing for their moral personhood and basic rights. They analyze the structural injustice of the property paradigm but operate outside its direct legal or economic mechanisms.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, abolitionist_theorists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear, transferable ownership of animals, facilitating their integration into economic systems (agriculture, research, entertainment) and providing a stable framework for human dominion over animal resources.
% TRANSFER_FUNCTION: Transfers control, use, and economic value of animals from animals themselves to their owners and industries, enabling the commodification and instrumentalization of animal life for human benefit.
% ABSENT_VOICES: Animals themselves are structurally absent from the conversation. Abolitionist voices, who would challenge the very premise of animal property, are excluded from the legal and economic frameworks that uphold this constraint.
% DISAPPEARANCE_RATIONALE: If animals ceased to be property overnight, the global economy would face unprecedented disruption. Industries reliant on animal products and services would collapse, legal systems would require fundamental re-evaluation of rights and personhood, and human-animal relationships would be radically transformed, necessitating a complete reorganization of society.
% FOUNDING_PROBLEM: To enable human dominion over nature and efficient utilization of animal resources for human benefit (food, labor, materials, companionship), by establishing clear, enforceable rights of ownership.
% FOUNDING_PROBLEM_CORROBORATION: Animal industries, many legal scholars, and a significant portion of the public attest to the ongoing necessity of animal property status for economic stability, food security, scientific advancement, and human welfare. Critics acknowledge the economic function but dispute its moral legitimacy.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(animal_status_kernel__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__property_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is extremely high (0.92) because animals are entirely instrumentalized, with no intrinsic moral standing to offset their use. Suppression is also very high (0.95) as animals are legally trapped, lacking any means to resist or exit their property status, and alternatives to this status are legally and culturally collapsed. Theater ratio is low (0.10) because the property system is highly functional and efficient for its intended purpose of facilitating animal use; there is little performative maintenance. Resistance (0.20) from within the system is low, though external resistance from advocacy groups is significant.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of animal owners and industries, this constraint is a foundational, efficient, and natural arrangement for resource management. From the perspective of animals (as represented by abolitionist thought), it is a system of total extraction and oppression. The claimed type of 'snare' reflects this fundamental asymmetry, where the 'coordination' (of economic activity) serves as cover for pure extraction from the animals.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal owners and industries are the clear beneficiaries (d near 0.0), as they gain full control and economic value from animals. Animals are the absolute targets (d near 1.0), bearing the full cost of instrumentalization with no recourse. The legal system acts as an agenda-setter, enforcing this structure. Welfare advocates and abolitionist theorists are largely excluded or analytical observers, respectively.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of this constraint (facilitating economic utilization of animals) is still very much 'live' and actively pursued by its beneficiaries. Therefore, it is not a mandatrophied constraint; its function persists and is vigorously defended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a true representation of the ''property reading'' of the animal status kernel, or does it conflate with other readings?',
    'Comparison with canonical texts and legal precedents that explicitly define animals as property and derive moral considerability solely from ownership, ensuring no unstated welfare considerations are implicitly included.',
    'If conflated, the extractiveness and suppression metrics might be lower than appropriate for a pure property reading, leading to misclassification or underestimation of the constraint''s severity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ensures the constraint accurately reflects the pure property reading.').

omega_variable(
    property_vs_personhood_ambiguity,
    'Is the property status of animals an inherent truth about their nature, or a legal and social construct?',
    'Philosophical and scientific inquiry into animal sentience, consciousness, and cognitive abilities, alongside historical analysis of the evolution of property law and moral philosophy.',
    'If it is a construct, the ''emerges_naturally'' claim (if made) would be false, and the constraint would be revealed as a human-made snare rather than a natural law, shifting the burden of justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_vs_personhood_ambiguity, conceptual, 'Ambiguity regarding the ontological status of animal property.').

omega_variable(
    economic_value_vs_intrinsic_value,
    'Is economic value the only relevant value for animals, or do they possess intrinsic value independent of human utility?',
    'Ethical deliberation and public discourse on the moral status of non-human animals, potentially leading to shifts in legal frameworks and societal norms.',
    'Recognition of intrinsic value would introduce countervailing moral constraints, reducing the effective extractiveness and suppression, potentially shifting the constraint towards a ''tangled_rope'' or ''scaffold'' if welfare regulations become more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_value_vs_intrinsic_value, preference, 'Whether animals have intrinsic value beyond economic utility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anim_tr_t25, animal_status_kernel__property_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__property_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(anim_tr_t75, animal_status_kernel__property_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(anim_tr_t100, animal_status_kernel__property_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(anim_be_t25, animal_status_kernel__property_reading, base_extractiveness, 25, 0.91).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__property_reading, base_extractiveness, 50, 0.92).
narrative_ontology:measurement(anim_be_t75, animal_status_kernel__property_reading, base_extractiveness, 75, 0.92).
narrative_ontology:measurement(anim_be_t100, animal_status_kernel__property_reading, base_extractiveness, 100, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.93).
narrative_ontology:measurement(anim_su_t25, animal_status_kernel__property_reading, suppression_requirement, 25, 0.94).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__property_reading, suppression_requirement, 50, 0.95).
narrative_ontology:measurement(anim_su_t75, animal_status_kernel__property_reading, suppression_requirement, 75, 0.95).
narrative_ontology:measurement(anim_su_t100, animal_status_kernel__property_reading, suppression_requirement, 100, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_welfare_regulations).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_testing_protocols).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, factory_farming_practices).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, wildlife_management_laws).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'animal_status_kernel'. Other readings include 'welfare_reading' and 'abolitionist_reading', which offer different structural interpretations of animal status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
