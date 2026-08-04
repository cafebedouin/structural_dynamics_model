% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__abolitionist_reading, []).

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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Abolitionist Reading: Animals as Rights-Holders Precluding Instrumental Use
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint story represents the abolitionist reading of animal
 *   status: animals are rights-holders with inherent value, and any
 *   instrumental use of them constitutes a violation of these rights. From
 *   this perspective, the current legal and economic arrangements that treat
 *   animals as property are a snare, characterized by maximal extraction and
 *   suppression. Welfare reforms are seen not as genuine improvements, but as
 *   theatrical attempts to legitimize an inherently unjust system. This is
 *   one reading of the 'animal_status' kernel, with sibling readings
 *   'welfare_reading' and 'property_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 1.0).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.95).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 1.0).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Abolitionist Reading: Animals as Rights-Holders Precluding Instrumental Use").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, '506fd2dc-43b8-421b-bded-a8f9dc18acdf').
narrative_ontology:cs_kernel_codification('506fd2dc-43b8-421b-bded-a8f9dc18acdf', formalized).
narrative_ontology:cs_authority_grounding('506fd2dc-43b8-421b-bded-a8f9dc18acdf', extraction).
narrative_ontology:cs_interpretation_layer_present('506fd2dc-43b8-421b-bded-a8f9dc18acdf').
narrative_ontology:cs_reading_relation('506fd2dc-43b8-421b-bded-a8f9dc18acdf', animal_status__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('506fd2dc-43b8-421b-bded-a8f9dc18acdf', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('506fd2dc-43b8-421b-bded-a8f9dc18acdf', foundational, animals_are_rights_holders).
narrative_ontology:cs_axiom_status(animals_are_rights_holders, holdable).
narrative_ontology:cs_axiom_grounding('506fd2dc-43b8-421b-bded-a8f9dc18acdf', animals_are_rights_holders, deontological).
narrative_ontology:cs_axiom('506fd2dc-43b8-421b-bded-a8f9dc18acdf', foundational, instrumental_use_is_unjust).
narrative_ontology:cs_axiom_status(instrumental_use_is_unjust, holdable).
narrative_ontology:cs_axiom_grounding('506fd2dc-43b8-421b-bded-a8f9dc18acdf', instrumental_use_is_unjust, deontological).
narrative_ontology:cs_reference_frame('506fd2dc-43b8-421b-bded-a8f9dc18acdf', universal_moral_consideration).
narrative_ontology:cs_drift_state('506fd2dc-43b8-421b-bded-a8f9dc18acdf', contemporary_legal_framework, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('506fd2dc-43b8-421b-bded-a8f9dc18acdf', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animals_in_instrumental_use).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, consumers_of_animal_products).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, animal_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, inherent_value_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the direct subjects of instrumental use (food, research, entertainment, clothing), animals bear the full cost of their status as property. They have no agency to resist or exit the arrangement.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animals_in_instrumental_use, payer,
    powerless, immediate, trapped, universal).

% Advocate for the full legal recognition of animal rights and the end of all instrumental use. They frame current animal status as a moral and legal snare, and reject welfare reforms as merely legitimizing exploitation. Their identity is fused with the cause.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocates, agenda_setter,
    moderate, generational, identity_locked, global).

% Benefit from the current legal status of animals as property, which allows for their instrumental use in agriculture, research, and entertainment. They actively resist any changes that would grant animals rights, as this would dismantle their business models.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_use_industries, beneficiary,
    institutional, biographical, constrained, global).

% Benefit from the availability and affordability of animal products. While some may be open to alternatives, many are habituated to current consumption patterns and are not directly engaged in the ethical debate, but their demand underpins the industries.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, consumers_of_animal_products, beneficiary,
    organized, immediate, mobile, global).

% Currently uphold the property status of animals, with some welfare regulations. They are the ultimate enforcers of the existing constraint, but are also the target of abolitionist legal challenges seeking to redefine animal status.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the abolitionist perspective, the current arrangement coordinates human instrumental use of animals by legally defining animals as property, thereby removing them from the moral and legal consideration afforded to rights-holders. This 'coordination' is seen as a mechanism for exploitation.
% TRANSFER_FUNCTION: Transfers the inherent value and bodily autonomy of animals to human benefit, allowing for their use as resources for food, labor, research, and entertainment. This transfer is enforced by legal systems that uphold animal property status.
% ABSENT_VOICES: The animals themselves are the primary absent voices, unable to articulate their interests or consent to their treatment. Their interests are represented by abolitionist advocates, but their direct voice is structurally excluded from the legal and ethical frameworks that govern their lives.
% DISAPPEARANCE_RATIONALE: If the property status of animals vanished overnight, and they were recognized as rights-holders, the entire global economy and legal framework surrounding animal agriculture, research, and entertainment would collapse and be forced to reorganize. It would be a fundamental shift in human-animal relations.
% FOUNDING_PROBLEM: The problem of how to manage human interaction with non-human animals, particularly regarding their use as resources, without granting them moral or legal standing that would impede human interests.
% FOUNDING_PROBLEM_CORROBORATION: Animal use industries and many consumers attest that the problem of managing animal resources for human benefit is still live. Abolitionist advocates attest that the problem is live, but that the 'solution' (property status) is the problem itself, and that the true problem is human exploitation.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 1.0, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   From the abolitionist perspective, extractiveness is maximal (1.0) because animals are denied all moral and legal standing, and their entire existence is subject to human instrumental use. Suppression is extremely high (0.95) because animals are legally defined as property, with no means of consent or resistance, and legal systems actively enforce this status. Theater ratio is very low (0.05) because welfare reforms are viewed as superficial and performative, doing little to alter the fundamental property status or the scale of instrumental use; the core function is extraction, not genuine welfare. Resistance is high (0.9) due to active and growing abolitionist movements.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from the welfare and property readings. While welfare advocates might see some coordination in regulations that reduce suffering, abolitionists see only extraction. The property reading sees a natural order; the abolitionist reading sees a constructed snare. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are the full targets of this constraint (d=1.0), bearing all costs without benefit. Abolitionist advocates are agenda-setters, but from this reading's perspective, they are also targets of the system they seek to dismantle, as their efforts are suppressed by the entrenched property status. Animal use industries and consumers are beneficiaries, as they profit from or consume the products of instrumental use. Legal systems are agenda-setters and enforcers of the current snare.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_personhood_feasibility,
    'Is the legal recognition of animals as persons or rights-holders practically feasible within existing legal and economic systems, or would it require a complete societal overhaul?',
    'Analysis of legal precedents for non-human personhood (e.g., rivers, corporations) and economic impact studies of a transition away from animal agriculture and research.',
    'If feasible, the path to resolving the snare is clearer. If it requires a complete overhaul, the ''resistance'' metric might be understated, and the ''accessibility_collapse'' for alternatives to instrumental use is higher than currently estimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_personhood_feasibility, empirical, 'Practicality of granting legal personhood to animals.').

omega_variable(
    welfare_reform_legitimation,
    'Do animal welfare reforms genuinely reduce suffering and improve animal lives, or do they primarily serve to legitimize and perpetuate instrumental use by assuaging public conscience?',
    'Empirical studies on the actual impact of welfare regulations on animal well-being, compared with the growth rates of animal use industries in regulated vs. unregulated contexts.',
    'If reforms primarily legitimize, the ''theater_ratio'' for the current system is higher than stated, and the ''suppression'' of abolitionist alternatives is more insidious. If they genuinely improve lives, the ''welfare_reading'' gains more structural validity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_reform_legitimation, conceptual, 'Role of welfare reforms in legitimizing animal use.').

omega_variable(
    speciesism_as_structural_suppression,
    'To what extent is the suppression of animal rights rooted in explicit legal structures versus internalized speciesist attitudes that persist even if legal barriers were removed?',
    'Post-legal-reform social behavior: if instrumental use persists after legal barriers are removed, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the speciesist attitudes would carry the suppression with them after legal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speciesism_as_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism of speciesism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_status__abolitionist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(anim_tr_t1985, animal_status__abolitionist_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(anim_tr_t2000, animal_status__abolitionist_reading, theater_ratio, 2000, 0.07).
narrative_ontology:measurement(anim_tr_t2010, animal_status__abolitionist_reading, theater_ratio, 2010, 0.06).
narrative_ontology:measurement(anim_tr_t2024, animal_status__abolitionist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_status__abolitionist_reading, base_extractiveness, 1970, 0.95).
narrative_ontology:measurement(anim_be_t1985, animal_status__abolitionist_reading, base_extractiveness, 1985, 0.97).
narrative_ontology:measurement(anim_be_t2000, animal_status__abolitionist_reading, base_extractiveness, 2000, 0.98).
narrative_ontology:measurement(anim_be_t2010, animal_status__abolitionist_reading, base_extractiveness, 2010, 0.99).
narrative_ontology:measurement(anim_be_t2024, animal_status__abolitionist_reading, base_extractiveness, 2024, 1.0).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_status__abolitionist_reading, suppression_requirement, 1970, 0.85).
narrative_ontology:measurement(anim_su_t1985, animal_status__abolitionist_reading, suppression_requirement, 1985, 0.88).
narrative_ontology:measurement(anim_su_t2000, animal_status__abolitionist_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(anim_su_t2010, animal_status__abolitionist_reading, suppression_requirement, 2010, 0.92).
narrative_ontology:measurement(anim_su_t2024, animal_status__abolitionist_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% This constraint is the abolitionist reading of the 'animal_status' kernel. It fundamentally rejects the premises of the welfare and property readings, viewing them as mechanisms that legitimize exploitation. All three readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
