% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Animals as Legal Property (Property Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'property reading' of animal status, where
 *   animals are legal objects without independent moral standing, and human
 *   ownership is largely unrestricted except by welfare statutes. From this
 *   reading's perspective, the constraint is a fundamental, almost natural,
 *   aspect of legal and social order, hence the 'mountain' claim and
 *   'emerges_naturally: true'. The low extractiveness reflects that, within
 *   this framework, animals are not considered subjects from whom extraction
 *   can occur. Any 'costs' are borne by humans (e.g., through welfare
 *   regulations), not by animals as moral agents. This is one reading of the
 *   'animal_status' kernel, distinct from welfare or abolitionist readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.05).
domain_priors:suppression_score(animal_status__property_reading, 0.15).
domain_priors:theater_ratio(animal_status__property_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, mountain).
narrative_ontology:human_readable(animal_status__property_reading, "Animals as Legal Property (Property Reading)").
narrative_ontology:topic_domain(animal_status__property_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:emerges_naturally(animal_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, 'dd241458-bd67-4af8-9b31-4e9adc323f6f').
narrative_ontology:cs_kernel_codification('dd241458-bd67-4af8-9b31-4e9adc323f6f', formalized).
narrative_ontology:cs_authority_grounding('dd241458-bd67-4af8-9b31-4e9adc323f6f', lineage).
narrative_ontology:cs_interpretation_layer_present('dd241458-bd67-4af8-9b31-4e9adc323f6f').
narrative_ontology:cs_reading_relation('dd241458-bd67-4af8-9b31-4e9adc323f6f', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd241458-bd67-4af8-9b31-4e9adc323f6f', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('dd241458-bd67-4af8-9b31-4e9adc323f6f', foundational, animals_are_legal_property).
narrative_ontology:cs_axiom_status(animals_are_legal_property, holdable).
narrative_ontology:cs_axiom_grounding('dd241458-bd67-4af8-9b31-4e9adc323f6f', animals_are_legal_property, conventional).
narrative_ontology:cs_axiom('dd241458-bd67-4af8-9b31-4e9adc323f6f', secondary, human_dominion_over_nature).
narrative_ontology:cs_axiom_status(human_dominion_over_nature, holdable).
narrative_ontology:cs_axiom_grounding('dd241458-bd67-4af8-9b31-4e9adc323f6f', human_dominion_over_nature, conventional).
narrative_ontology:cs_reference_frame('dd241458-bd67-4af8-9b31-4e9adc323f6f', classical_legal_property_framework).
narrative_ontology:cs_drift_state('dd241458-bd67-4af8-9b31-4e9adc323f6f', contemporary_animal_rights_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('dd241458-bd67-4af8-9b31-4e9adc323f6f', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, human_owners).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_use_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status__property_reading, welfare_advocates).
narrative_ontology:constraint_vindicates(animal_status__property_reading, property_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_status__property_reading, human_exceptionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or entities who legally own animals, exercising broad rights over their use, disposition, and welfare, largely unrestricted by the animals' own interests. They benefit from the clarity and enforceability of property rights.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, human_owners, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status__property_reading, human_owners, agenda_setter).

% Sectors like agriculture, biomedical research, and entertainment that rely on the instrumental use of animals. Their business models are predicated on animals being legal property without independent moral standing, allowing for efficient resource allocation and exploitation.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_use_industries, beneficiary,
    institutional, generational, mobile, global).

% The framework of laws, courts, and enforcement bodies that codifies and upholds the property status of animals. It adjudicates disputes between human owners and enforces welfare statutes as limits on human conduct, not as rights of animals.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% Organizations and individuals who seek to improve animal welfare within the existing property framework, often by advocating for stronger welfare statutes. They bear the cost of lobbying and legal challenges, operating within a system that fundamentally denies animals independent standing.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, welfare_advocates, payer,
    organized, biographical, constrained, global).

% Groups who fundamentally reject the property status of animals, advocating for their recognition as rights-holders. Their core premise is foreclosed by the property reading, making them structurally excluded from the foundational legal discourse.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, abolitionist_advocates, excluded,
    organized, generational, identity_locked, global).

% Scholars and ethicists who analyze the legal and moral implications of animal status, often comparing different readings and their societal impacts. They operate outside the direct enforcement or benefit structures of the constraint.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, analytical_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear, enforceable ownership and use rights for animals, facilitating their integration into human economic systems, scientific research, and companion animal relationships, thereby reducing disputes over animal control and utility.
% TRANSFER_FUNCTION: Legally transfers control, use, and disposition of animals to human owners, enabling their instrumental use in various industries and personal contexts. This transfer is from the animal (as an object) to the human owner.
% ABSENT_VOICES: Animals themselves, who are legally defined as objects and thus lack standing to object. Abolitionist advocates are excluded from the foundational premise of animal property, as their core claim of animal rights is incompatible with this reading.
% DISAPPEARANCE_RATIONALE: If animals suddenly gained independent moral standing and property rights over them vanished, entire global industries (agriculture, research, entertainment) would collapse or undergo radical, costly transformations. Human-animal relationships would be fundamentally reconfigured, leading to widespread legal and economic chaos.
% FOUNDING_PROBLEM: To define the legal and social relationship between humans and animals, establishing clear parameters for human control, use, and responsibility over non-human life forms within human society and economic structures.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal codes, philosophical texts on property rights, and contemporary legal scholarship corroborate the long-standing problem of defining animal status. From the perspective of this reading, the problem of integrating animals into human systems under human dominion remains live, requiring ongoing legal maintenance and defense against challenges.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

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
 *   The base extractiveness is very low (0.05) because, within the property reading, animals are not moral subjects capable of being 'extracted from' in the same way humans are. Their status as property is seen as a foundational legal fact. Suppression is low (0.15) because this is the default legal paradigm, not something requiring active, coercive suppression to maintain against widespread dissent. Theater ratio is low (0.10) as the legal status is functional and deeply embedded, not performative. Accessibility collapse is high (0.88) because challenging this fundamental legal status is extremely difficult within the existing framework. Resistance is low (0.12) because, while there are advocates for animal rights, the dominant legal and social structures largely uphold this status.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human owners and animal-use industries, this constraint is a foundational 'mountain' that enables orderly society and economic activity. From the perspective of abolitionist advocates, it is a 'snare' that enables profound extraction and suffering. The engine's classification will highlight this divergence by detecting the beneficiaries on a claimed mountain, triggering a false summit evaluation.
 *
 * DIRECTIONALITY LOGIC:
 *   Human owners and animal-use industries are clear beneficiaries, as the constraint grants them extensive rights and facilitates economic activity. The legal system acts as an agenda-setter, codifying and enforcing this status. Welfare advocates are payers, bearing the cost of trying to modify the system from within. Abolitionist advocates are excluded, as their core premise is incompatible with the constraint's foundation. Animals themselves are not considered stakeholders with directionality in this reading, as they lack legal standing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_fact_vs_legal_construct,
    'Is the property status of animals a ''natural'' fact, as implied by ''emerges_naturally: true'' in this reading, or a human legal and social construct?',
    'Comparative legal anthropology and historical analysis of legal systems across cultures and eras, examining the variability of animal status and rights.',
    'If resolved as a construct, the ''mountain'' claim is undermined, and the constraint would be reclassified towards a ''tangled_rope'' or ''snare'' due to its identifiable beneficiaries and the constructed nature of its ''naturalness''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_fact_vs_legal_construct, conceptual, 'Ambiguity regarding the ''naturalness'' of animal property status.').

omega_variable(
    extraction_referent_ambiguity,
    'Is the near-zero extractiveness valid, given that animals are treated as objects, or does ''extraction'' implicitly require a recognized moral subject?',
    'Conceptual clarification of ''extraction'' within the Deferential Realism framework: does it apply only to moral subjects, or can it describe the instrumentalization of any entity, regardless of recognized standing?',
    'If extraction can apply to non-moral subjects, the extractiveness metric for this constraint would be significantly higher, reflecting the instrumental use of animals, potentially reclassifying it as a ''snare'' from an external, non-property-reading perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_referent_ambiguity, conceptual, 'Ambiguity in the definition of ''extraction'' when the ''victim'' lacks moral standing.').

omega_variable(
    kernel_reading_divergence,
    'How do the structural properties and classifications of the ''property_reading'' diverge from the ''welfare_reading'' and ''abolitionist_reading'' of the ''animal_status'' kernel?',
    'Comparative analysis of the generated constraint stories for each reading, focusing on differences in extractiveness, victim sets, and claimed types.',
    'The divergence highlights the impact of foundational normative premises on the perceived structure and classification of the same underlying phenomenon, demonstrating how different readings instantiate distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Comparison of structural differences across kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(anim_tr_t10, animal_status__property_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(anim_tr_t20, animal_status__property_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(anim_tr_t30, animal_status__property_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(anim_tr_t40, animal_status__property_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(anim_tr_t50, animal_status__property_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(anim_be_t10, animal_status__property_reading, base_extractiveness, 10, 0.04).
narrative_ontology:measurement(anim_be_t20, animal_status__property_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(anim_be_t30, animal_status__property_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(anim_be_t40, animal_status__property_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(anim_be_t50, animal_status__property_reading, base_extractiveness, 50, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__property_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(anim_su_t10, animal_status__property_reading, suppression_requirement, 10, 0.13).
narrative_ontology:measurement(anim_su_t20, animal_status__property_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement(anim_su_t30, animal_status__property_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(anim_su_t40, animal_status__property_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(anim_su_t50, animal_status__property_reading, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_welfare_statutes).
narrative_ontology:affects_constraint(animal_status__property_reading, agricultural_production_norms).
narrative_ontology:affects_constraint(animal_status__property_reading, biomedical_research_ethics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
