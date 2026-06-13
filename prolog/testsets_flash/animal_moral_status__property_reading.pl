% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: animal_moral_status__property_reading
 *   human_readable: Animals as Property/Resources (Property Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint defines animals as property or resources, inherently
 *   lacking independent moral standing. Their interests are legally and
 *   ethically subordinate to human interests, meaning their use is
 *   permissible, with constraints only on waste or inefficiency, not on their
 *   fundamental status or suffering. This is the 'property_reading' of the
 *   'animal_moral_status' kernel, which is contested by 'welfare_reading' and
 *   'abolitionist_reading'.
 *
 * KEY AGENTS:
 *   - animal_property_owners: Primary beneficiary (institutional/arbitrage) — benefits from unrestricted use.
 *   - animal_resource_users: Primary beneficiary (organized/mobile) — benefits from treating animals as commodities.
 *   - animals: Not a victim in this reading; treated as objects/resources.
 *   - legal_system: Agenda setter (institutional/analytical) — codifies and enforces property status.
 *   - animal_welfare_advocates: Excluded (organized/constrained) — their arguments are outside this reading's framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.05).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.02).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animals as Property/Resources (Property Reading)").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, '4ef8e5a2-1028-4337-9780-63c34c7dd518').
narrative_ontology:cs_kernel_codification('4ef8e5a2-1028-4337-9780-63c34c7dd518', formalized).
narrative_ontology:cs_authority_grounding('4ef8e5a2-1028-4337-9780-63c34c7dd518', lineage).
narrative_ontology:cs_interpretation_layer_present('4ef8e5a2-1028-4337-9780-63c34c7dd518').
narrative_ontology:cs_reading_relation('4ef8e5a2-1028-4337-9780-63c34c7dd518', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ef8e5a2-1028-4337-9780-63c34c7dd518', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('4ef8e5a2-1028-4337-9780-63c34c7dd518', foundational, animals_are_property).
narrative_ontology:cs_axiom_status(animals_are_property, holdable).
narrative_ontology:cs_axiom_grounding('4ef8e5a2-1028-4337-9780-63c34c7dd518', animals_are_property, conventional).
narrative_ontology:cs_axiom('4ef8e5a2-1028-4337-9780-63c34c7dd518', foundational, human_interests_are_primary).
narrative_ontology:cs_axiom_status(human_interests_are_primary, holdable).
narrative_ontology:cs_axiom_grounding('4ef8e5a2-1028-4337-9780-63c34c7dd518', human_interests_are_primary, deontological).
narrative_ontology:cs_reference_frame('4ef8e5a2-1028-4337-9780-63c34c7dd518', classical_property_rights_framework).
narrative_ontology:cs_drift_state('4ef8e5a2-1028-4337-9780-63c34c7dd518', contemporary_animal_advocacy_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('4ef8e5a2-1028-4337-9780-63c34c7dd518', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_property_owners).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_resource_users).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, human_dominion_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, property_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals, corporations, and institutions that own animals (e.g., livestock, pets, research animals). They benefit from the legal right to use, sell, or dispose of animals as property, with minimal external constraints on their interests.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_property_owners, beneficiary,
    institutional, generational, arbitrage, national).

% Industries and individuals that utilize animals or animal products as resources (e.g., agriculture, biomedical research, entertainment). They benefit from the legal framework that permits the commodification and exploitation of animals for human purposes.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_resource_users, beneficiary,
    organized, biographical, mobile, global).

% The body of laws, courts, and enforcement agencies that codify and uphold the property status of animals. It benefits from the stability and clarity this framework provides for economic and social activities involving animals.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% Organizations and individuals who argue for greater protection of animals, often advocating for their sentience and interests. Within this 'property_reading' framework, their arguments are largely dismissed or reframed as concerns about human morality rather than animal rights, effectively excluding their core claims.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_welfare_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__property_reading, animal_property_owners).
narrative_ontology:fixing_cost_class(animal_moral_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universally understood legal and ethical framework for the ownership, use, and transfer of animals, facilitating economic activity and resource management without ambiguity regarding animal status.
% TRANSFER_FUNCTION: Legally transfers the full rights of use and disposal of animals to human owners/users, enabling the extraction of labor, products, or services from animals for human benefit.
% ABSENT_VOICES: Animals themselves, whose interests are explicitly subordinated, and animal rights advocates who argue for independent moral standing. Their voices are excluded by the very definition of animals as property, which denies them standing to object.
% DISAPPEARANCE_RATIONALE: If the property status of animals vanished overnight, the entire legal and economic system built upon animal ownership and use (agriculture, pet industry, research, entertainment) would collapse. Property rights, contracts, and resource allocation would be fundamentally disrupted, requiring a complete reorganization of human-animal relations.
% FOUNDING_PROBLEM: To establish clear legal and ethical boundaries for human interaction with animals, enabling their domestication, use as resources, and integration into human society and economy without constant moral or legal dispute over their status.
% FOUNDING_PROBLEM_CORROBORATION: The legal system and property owners attest that the problem of managing human-animal interactions and economic activity remains live, requiring the clarity of property status. While animal welfare advocates contest the 'solution,' the problem of defining animal status for human use is still actively managed by this framework. No external corroboration is sought for this foundational legal premise, as it is self-evident within the property-rights framework.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__property_reading_tests).

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
 *   The constraint is claimed as a Mountain because, within this reading, the property status of animals is treated as a foundational, almost natural, legal and ethical principle. Extractiveness is very low (0.05) because the constraint itself does not extract from its beneficiaries; rather, it enables their extraction from animals. Suppression is low (0.02) as the property status is widely accepted and requires minimal active enforcement against its core tenets. Accessibility collapse is high (0.95) as alternatives to property status are almost entirely foreclosed within this framework. Resistance is low (0.05) because challenges to this fundamental property status are largely external to the legal system that upholds it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of animal property owners and resource users, this constraint is a fundamental, unchangeable aspect of the legal and economic landscape, enabling their activities. From the perspective of animal welfare or rights advocates (who are largely excluded from this reading's framework), this constraint is the source of all extraction and suffering, but their perspective is not accounted for within this specific 'property_reading'.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal property owners and resource users are full beneficiaries (d=0.0) as the constraint directly enables their activities without imposing costs. The legal system is an agenda setter (d=0.15) as it administers and enforces this status, benefiting from the stability it provides. Animals are not considered agents with directionality in this reading, as they are defined as property. Animal welfare advocates are excluded (d=1.0) as their goals are directly opposed to the constraint's premise, and they bear the cost of its persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, as a 'property_reading', does not exhibit mandatrophy because its core function (defining animals as property) remains central to the economic and legal systems it underpins. The challenge of mandatrophy arises when considering alternative readings, where the 'founding problem' of animal use might be reframed, but within this reading, the mandate is considered live and essential.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_vs_moral_status_ambiguity,
    'Is the classification of animals as property a natural law or a social construct that benefits identifiable agents?',
    'Philosophical argument and shifts in legal precedent regarding animal personhood or rights.',
    'If a social construct, the constraint''s ''mountain'' status is a false summit, reclassifying it as a Tangled Rope or Snare depending on the degree of extraction and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_vs_moral_status_ambiguity, conceptual, 'Ambiguity between natural law and social construct for animal property status.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''property_reading'' of the ''animal_moral_status'' kernel. How would the classification change under sibling readings?',
    'Analyzing the structural implications of the ''welfare_reading'' (minimizing suffering within use) or ''abolitionist_reading'' (animals as rights-bearing individuals).',
    'The ''welfare_reading'' would introduce constraints on use (e.g., anti-cruelty laws), potentially shifting to a Rope or Tangled Rope. The ''abolitionist_reading'' would fundamentally challenge property status, leading to a Snare classification for current practices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of alternative readings of the animal moral status kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.01).
narrative_ontology:measurement(anim_tr_t10, animal_moral_status__property_reading, theater_ratio, 10, 0.01).
narrative_ontology:measurement(anim_tr_t20, animal_moral_status__property_reading, theater_ratio, 20, 0.01).
narrative_ontology:measurement(anim_tr_t30, animal_moral_status__property_reading, theater_ratio, 30, 0.01).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(anim_be_t10, animal_moral_status__property_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(anim_be_t20, animal_moral_status__property_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__property_reading, base_extractiveness, 30, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__property_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(anim_su_t10, animal_moral_status__property_reading, suppression_requirement, 10, 0.02).
narrative_ontology:measurement(anim_su_t20, animal_moral_status__property_reading, suppression_requirement, 20, 0.02).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__property_reading, suppression_requirement, 30, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__welfare_reading).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'animal_moral_status' kernel. Its 'mountain' classification reflects its foundational status within this specific legal and ethical framework, which is contested by 'welfare_reading' and 'abolitionist_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
