% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__functional_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Functional Capacity Reading of Legal Personhood Boundary
 *   domain: legal/philosophical/rights_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the functional_capacity_reading of the
 *   legal_personhood_boundary kernel. The constraint holds that legal
 *   personhood should follow demonstrable cognitive capacityârationality,
 *   sentience, self-awarenessâwithout regard to species membership. When
 *   adopted by courts or legislatures, it reallocates rights-bearing status
 *   from species-based boundaries to capacity-bearing entities. Non-human
 *   animals with demonstrated complex cognition are the primary
 *   beneficiaries, while animal exploitation industries and human supremacist
 *   legal traditions bear the costs of contested property rights and lost
 *   privilege. The reading logically forecloses both the restrictive
 *   anthropocentric reading (species membership as limiting condition) and
 *   the developmental potentiality reading (conception as threshold), because
 *   neither can coexist with a capacity-only criterion within a single legal
 *   framework.
 *
 * KEY AGENTS:
 *   - adjudicating_courts: agenda-setter (institutional/constrained) â administer capacity tests and grant standing
 *   - cognitively_complex_non_humans: beneficiary (powerless/trapped) â receive legal personhood status
 *   - animal_exploitation_industries: payer (organized/constrained) â bear costs of property-rights contestation
 *   - prospective_artificial_intelligences: excluded (powerless/trapped) â future entities affected but not present in discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.68).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.45).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Functional Capacity Reading of Legal Personhood Boundary").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal/philosophical/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, '89894275-f285-48e4-9b0c-9cce8ad4f962').
narrative_ontology:cs_kernel_codification('89894275-f285-48e4-9b0c-9cce8ad4f962', formalized).
narrative_ontology:cs_authority_grounding('89894275-f285-48e4-9b0c-9cce8ad4f962', lineage).
narrative_ontology:cs_interpretation_layer_present('89894275-f285-48e4-9b0c-9cce8ad4f962').
narrative_ontology:cs_reading_relation('89894275-f285-48e4-9b0c-9cce8ad4f962', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('89894275-f285-48e4-9b0c-9cce8ad4f962', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_axiom('89894275-f285-48e4-9b0c-9cce8ad4f962', foundational, moral_status_tracks_cognitive_capacity).
narrative_ontology:cs_axiom_status(moral_status_tracks_cognitive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('89894275-f285-48e4-9b0c-9cce8ad4f962', moral_status_tracks_cognitive_capacity, deontological).
narrative_ontology:cs_axiom('89894275-f285-48e4-9b0c-9cce8ad4f962', foundational, species_membership_irrelevant_to_personhood).
narrative_ontology:cs_axiom_status(species_membership_irrelevant_to_personhood, holdable).
narrative_ontology:cs_axiom_grounding('89894275-f285-48e4-9b0c-9cce8ad4f962', species_membership_irrelevant_to_personhood, deontological).
narrative_ontology:cs_reference_frame('89894275-f285-48e4-9b0c-9cce8ad4f962', cognitive_capacity_as_personhood_ground).
narrative_ontology:cs_drift_state('89894275-f285-48e4-9b0c-9cce8ad4f962', contemporary_legal_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('89894275-f285-48e4-9b0c-9cce8ad4f962', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, cognitively_complex_non_humans).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, animal_exploitation_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts and appellate bodies that must evaluate petitions for legal personhood using evidentiary standards for cognitive capacity rather than species classification. They issue rulings that determine whether non-human claimants receive standing and rights protections, bound by constitutional text, precedent, and the evidentiary record submitted by expert witnesses.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, adjudicating_courts, agenda_setter,
    institutional, generational, constrained, national).

% Great apes, cetaceans, elephants, and other non-human animals who demonstrate capacities such as self-awareness, problem-solving, and social complexity. They gain legal protections against confinement and instrumental use where courts recognize their personhood, but remain dependent on human proxies to initiate and prosecute legal claims on their behalf.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, cognitively_complex_non_humans, beneficiary,
    powerless, biographical, trapped, global).

% Agricultural, research, and entertainment enterprises that hold legal title to sentient non-human animals. Their property rights become contestable in jurisdictions adopting capacity-based personhood, exposing them to litigation, regulatory restriction, and the costs of restructuring practices around recognized rights-bearing entities.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_exploitation_industries, payer,
    organized, biographical, constrained, national).

% Advanced artificial systems that may in the future demonstrate cognitive capacities meeting the legal threshold for personhood. They are not currently present in legal discourse or recognized as potential rights-bearers in most jurisdictions, yet the functional capacity standard would structurally include them if their capacities were demonstrated.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, prospective_artificial_intelligences, excluded,
    powerless, civilizational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, empirically evaluable legal standard for adjudicating personhood claims across species boundaries, replacing arbitrary species membership with cognitive criteria that can be tested through comparative ethology and neuroscience.
% TRANSFER_FUNCTION: Transfers legal standing, rights-protection, and the burden of justification from species-based exclusion regimes to sentient beings demonstrating relevant cognitive capacities; imposes the cost of justification on property-holders seeking to deny personhood.
% ABSENT_VOICES: Non-human sentients themselves are structurally excluded from legal discourse and cannot testify to their own interests; future artificial intelligences and unrepresented ecosystems have no seat in the conversation; animal industry voices are present but are often outspent by institutional economic interests lobbying to maintain property status.
% DISAPPEARANCE_RATIONALE: If the capacity-based personhood standard vanished overnight, legal systems would revert to species-based or potentiality-based boundaries; standing for non-human animals would collapse, property rights over sentient beings would become absolute again, and the global legal landscape for animal rights would reorganize around anthropocentric defaults.
% FOUNDING_PROBLEM: The arbitrariness and moral inadequacy of using species membership alone to determine legal personhood and rights-bearing status, given mounting evidence of complex cognition in non-human animals.
% FOUNDING_PROBLEM_CORROBORATION: Animal behavior scientists and cognitive ethologists attest to the evidence of non-human cognition from outside the legal beneficiary set; philosophical advocates within rights theory corroborate the moral problem. However, legal positivists and anthropocentric tradition defenders dispute that this is a problem requiring legal solution, asserting that the species boundary is a legitimate legal convention.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is substantial (0.68) because the constraint actively reallocates property rights and legal standing from established holders to newly recognized non-human persons; suppression is moderate (0.45) because the doctrine displaces species-based alternatives through judicial precedent rather than raw coercion. Theater is low-moderate (0.28): capacity testing involves genuine evidentiary review, but ritualized legal process creates performative overhead. Resistance is significant (0.55) because animal industries and anthropocentric traditions contest the standard politically and legally. Accessibility collapse (0.60) reflects that once a jurisdiction adopts capacity-based personhood, species-based alternatives collapse locally, though cross-jurisdictional arbitrage remains. The measurement series run on a shared time grid to prevent misaligned temporal substitution.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (courts) experiences the constraint as a principled legal standard resolving indeterminate personhood claims; the payer seat (animal industries) experiences the same structure as an expropriative reordering of property rights. The beneficiary seat (non-human animals) experiences protection they cannot themselves request. The engine computes this divergence from the structural asymmetry in power, exit, and role.
 *
 * DIRECTIONALITY LOGIC:
 *   The adjudicating courts sit near symmetric (they neither collect nor pay, but administer the standard under institutional constraint). Cognitively complex non-humans sit at the beneficiary end: the constraint subsidizes them with legal standing they previously lacked. Animal exploitation industries sit at the target end: the constraint extracts their previously absolute property rights and subjects them to litigation and regulatory costs. Prospective artificial intelligences are excluded from present discourse but structurally trapped in a legal future shaped by this standard.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the coordination functionâa determinate standard for personhoodâthe constraint would be pure extraction (a snare). Without the asymmetric extraction falling on property holders, it would be pure coordination (a rope). The tangled_rope classification captures that both properties inhere in the same structure: the capacity standard genuinely solves a boundary-adjudication problem, but it does so by transferring costs to a concentrated payer group. Mandatrophy would occur if the legal system maintained capacity-testing rituals after abandoning the substantive rights allocation; the low and stable theater_ratio indicates this has not yet happened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_measurement_standard,
    'Which empirical tests and thresholds definitively demonstrate the cognitive capacities (rationality, sentience, self-awareness) that trigger personhood under this reading?',
    'Comparative cognitive ethology, neuroscientific evidence, and adversarial legal process establishing reproducible evidentiary standards.',
    'Determines the effective boundary of personhood; decides which non-human entities exit the victim set of species-based exclusion and which remain unprotected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_measurement_standard, empirical, 'Empirical ambiguity in measuring cognitive capacity for legal purposes.').

omega_variable(
    proxy_representation_dependency,
    'Does legal personhood for non-humans remain substantively meaningful if the rights must always be exercised through human proxies, or does this dependency recreate a species-based power asymmetry?',
    'Structural analysis of legal outcomes comparing proxy-controlled rights for non-humans with guardianship models for minors and incapacitated humans.',
    'Would reclassify the constraint''s beneficiary structure if the legal victory is purely formal and the protected entity''s interests are systematically overridden by human advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_representation_dependency, conceptual, 'Whether proxy representation undermines the species-neutrality claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lpb_funcap_tr_t0, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lpb_funcap_tr_t10, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(lpb_funcap_tr_t20, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(lpb_funcap_tr_t30, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(lpb_funcap_tr_t40, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(lpb_funcap_tr_t50, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(lpb_funcap_be_t0, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(lpb_funcap_be_t10, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(lpb_funcap_be_t20, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(lpb_funcap_be_t30, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(lpb_funcap_be_t40, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(lpb_funcap_be_t50, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 50, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(legal_personhood_boundary__functional_capacity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__developmental_potentiality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the legal_personhood_boundary kernel, instantiating the functional capacity standard. Sibling readings instantiate species-restrictive and developmental potentiality standards. The kernel decomposes into structurally distinct constraints because each reading produces a different beneficiary/victim structure and epsilon profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
