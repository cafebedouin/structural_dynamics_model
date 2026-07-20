% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Animal Legal Status: Property Reading
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the property reading of the
 *   animal_status kernel: the legal and philosophical position that animals
 *   are objects without independent moral standing, and that human ownership
 *   is presumptively unrestricted except where welfare statutes voluntarily
 *   constrain it. The reading treats the constraint as a coordinative device
 *   among humans, with animals outside the stakeholder surface. The near-zero
 *   extractiveness (Îµ ~ 0.05) reflects that the arrangement functions
 *   primarily to resolve human-to-human disputes over title and liability,
 *   not to extract from a victim class. Animals are deliberately excluded
 *   from the victim set per the structural delta of this reading.
 *
 * KEY AGENTS:
 *   - human_property_owners: Primary beneficiary (moderate power, national scope) â gain clear title and control over animals
 *   - commercial_animal_enterprises: Concentrated beneficiary (powerful, global scope) â depend on unrestricted ownership for commercial operations
 *   - state_legal_system: Agenda-setter (institutional, national scope) â adjudicates and enforces the property framework
 *   - animal_welfare_advocates: Observer/limited participant (organized, national scope) â seek welfare constraints within the property frame
 *   - abolitionist_advocates: Excluded voice (moderate, global scope) â structurally barred from legal standing under this reading
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
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, rope).
narrative_ontology:human_readable(animal_status__property_reading, "Animal Legal Status: Property Reading").
narrative_ontology:topic_domain(animal_status__property_reading, "applied_ethics/legal_philosophy/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, '4a4979b3-1591-4cf1-993e-b0779c83dc4c').
narrative_ontology:cs_kernel_codification('4a4979b3-1591-4cf1-993e-b0779c83dc4c', formalized).
narrative_ontology:cs_authority_grounding('4a4979b3-1591-4cf1-993e-b0779c83dc4c', lineage).
narrative_ontology:cs_interpretation_layer_present('4a4979b3-1591-4cf1-993e-b0779c83dc4c').
narrative_ontology:cs_reading_relation('4a4979b3-1591-4cf1-993e-b0779c83dc4c', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_reading_relation('4a4979b3-1591-4cf1-993e-b0779c83dc4c', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('4a4979b3-1591-4cf1-993e-b0779c83dc4c', foundational, animals_are_legal_objects_without_moral_status).
narrative_ontology:cs_axiom_status(animals_are_legal_objects_without_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('4a4979b3-1591-4cf1-993e-b0779c83dc4c', animals_are_legal_objects_without_moral_status, conventional).
narrative_ontology:cs_axiom('4a4979b3-1591-4cf1-993e-b0779c83dc4c', foundational, human_title_over_animals_is_presumptive_and_unrestricted).
narrative_ontology:cs_axiom_status(human_title_over_animals_is_presumptive_and_unrestricted, holdable).
narrative_ontology:cs_axiom_grounding('4a4979b3-1591-4cf1-993e-b0779c83dc4c', human_title_over_animals_is_presumptive_and_unrestricted, conventional).
narrative_ontology:cs_reference_frame('4a4979b3-1591-4cf1-993e-b0779c83dc4c', unrestricted_human_title_over_animals).
narrative_ontology:cs_drift_state('4a4979b3-1591-4cf1-993e-b0779c83dc4c', modern_welfare_statute_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4a4979b3-1591-4cf1-993e-b0779c83dc4c', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, human_property_owners).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, commercial_animal_enterprises).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold animals as legal property with broad discretion over use, transfer, and management, constrained only by voluntarily adopted or democratically enacted welfare statutes. Ownership claims are backed by state enforcement against other human claimants.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, human_property_owners, beneficiary,
    moderate, biographical, mobile, national).

% Operate industrial agriculture, research, and entertainment systems under the legal protection of unrestricted ownership. Business models depend on animals remaining legal objects without independent standing that could block operational decisions.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, commercial_animal_enterprises, beneficiary,
    powerful, biographical, constrained, global).

% Adjudicates disputes between humans over animal ownership, title, damage, and liability. Maintains the legal registry and enforcement apparatus that treats animals as objects within the property system, interpreting welfare statutes as permissible human self-limitation rather than animal entitlement.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, state_legal_system, agenda_setter,
    institutional, generational, constrained, national).

% Promote voluntary welfare standards and statutory limits on animal use within the property framework. Do not challenge the underlying object-status of animals but seek to constrain its exercise through democratic or market pressure.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_welfare_advocates, observer,
    organized, biographical, mobile, national).

% Argue for animal personhood and the abolition of property status. Their position is structurally excluded from the legal framework, which grants no standing to animals independent of human ownership and treats abolitionist claims as legally inadmissible.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, abolitionist_advocates, excluded,
    moderate, civilizational, trapped, global).

narrative_ontology:fixing_cost_class(animal_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform legal mechanism to adjudicate competing human claims over animal control, title, and liability, replacing ad hoc or violent dispute resolution with state-backed rules.
% TRANSFER_FUNCTION: Transfers exclusive decision-making authority over animals from undefined or contested states to named human owners, and assigns liability for animal actions to those owners.
% ABSENT_VOICES: Abolitionist advocates who reject all property status for animals, and animals themselves as potential rights-bearers or interest-holders, are structurally absent from legal standing and doctrinal consideration.
% DISAPPEARANCE_RATIONALE: If animals were no longer legal objects, human property claims would become unenforceable, title registries would fail, commercial animal industries would lose legal certainty, and inter-human disputes over animal control would proliferate without a coordinating rule.
% FOUNDING_PROBLEM: Human societies required a mechanism to resolve conflict over animal control and to assign liability for animal-caused harms without resorting to private force.
% FOUNDING_PROBLEM_CORROBORATION: Comparative legal historians and anthropologists document the cross-cultural emergence of property frameworks for animals as solutions to inter-human coordination problems, corroborating the coordination function from outside the beneficiary set of current owners.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.05, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__property_reading_tests).
:- end_tests(animal_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness score (0.05) captures the property reading's structural delta: the constraint coordinates human claimants rather than extracting from animals or disfavored humans. Suppression (0.15) is low because the framework operates as a widely accepted legal default rather than an actively contested coercion. Accessibility collapse (0.70) is high because, once the legal object-status is understood, alternatives such as animal personhood are doctrinally inaccessible. Resistance (0.25) reflects persistent but minority opposition from animal advocacy. Theater ratio (0.10) is low because most legal activity under this reading is functional dispute resolution.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (property owners, commercial enterprises) experience the constraint as enabling infrastructure that secures their control. The excluded seat (abolitionist advocates) experiences the same structure as an ontological lockout that renders their core normative commitment legally inexpressible. The agenda-setter seat (state legal system) experiences it as a routine administrative framework. These divergences are structurally determined by role and exit options, not by disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (human_property_owners, commercial_animal_enterprises) derive low directionality: the constraint subsidizes their legal control. The agenda_setter (state_legal_system) sits near symmetric: it bears administrative costs but also gains institutional authority. Excluded agents (abolitionist_advocates) derive high directionality because the constraint structurally targets their position for exclusion. No victim group is declared because the property reading assigns moral standing only to humans.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope rather than snare or tangled rope prevents mislabeling the legal coordination function as extraction. A snare classification would require identifiable victims paying costs to a capturing beneficiary; the property reading deliberately places animals outside the agent surface and treats human costs as coordination friction rather than extraction. A tangled rope classification would require asymmetric extraction alongside coordination, which the near-zero Îµ and absence of victims do not support. The scaffold classification is inappropriate because the property framework carries no sunset clause and is not framed as transitional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_naturalness,
    'Is the property reading a natural legal default or a constructed historical contingency that could be otherwise?',
    'Comparative legal history showing jurisdictions or traditions with non-property frameworks for animals, or philosophical analysis demonstrating the contingency of the object/person boundary.',
    'If contingent, the reading''s claim to low extractiveness depends on suppressing alternative ontologies; if necessary, it functions closer to a natural-law coordination device.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_naturalness, conceptual, 'Whether animal property status is natural or constructed').

omega_variable(
    framework_exclusion_mechanism,
    'Does the property framework''s stability depend on structurally excluding abolitionist voices, or would it persist even with full discursive inclusion?',
    'Analysis of legal standing rules, doctrinal gatekeeping, and counterfactual simulation of jurisdictions that have granted limited animal standing.',
    'If exclusion is structural, the rope classification may understate the constraint''s suppressive function and the computed directionality for excluded agents may be higher than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_exclusion_mechanism, empirical, 'Structural versus contingent exclusion of abolitionist positions').

omega_variable(
    voluntary_welfare_integrability,
    'Can welfare constraints be fully integrated into the property reading without transforming into the welfare reading?',
    'Tracking legal evolution as welfare statutes expand; identifying whether incremental welfare gains inherently presuppose animal interests or can remain framed as human self-limitation.',
    'If welfare integration necessarily pushes toward sentience-based status, the property reading is transitional (scaffold-like) rather than a stable rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_welfare_integrability, conceptual, 'Welfare statute integration and reading stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t10, animal_status__property_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(anim_tr_t20, animal_status__property_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(anim_tr_t30, animal_status__property_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(anim_tr_t40, animal_status__property_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(anim_tr_t50, animal_status__property_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(anim_be_t10, animal_status__property_reading, base_extractiveness, 10, 0.03).
narrative_ontology:measurement(anim_be_t20, animal_status__property_reading, base_extractiveness, 20, 0.04).
narrative_ontology:measurement(anim_be_t30, animal_status__property_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(anim_be_t40, animal_status__property_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(anim_be_t50, animal_status__property_reading, base_extractiveness, 50, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(animal_status__property_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is the property reading of the animal_status kernel, treating animals as legal objects. The welfare and abolitionist readings are sibling constraints sharing the kernel but assigning different ontological and normative statuses to animals.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
