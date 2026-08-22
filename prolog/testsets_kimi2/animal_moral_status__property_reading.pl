% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Animal Moral Status â Property Reading
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the property reading of the
 *   animal_moral_status kernel: the claim that animals are legal resources
 *   without independent moral standing, their interests subordinate to human
 *   interests by definitional fiat. The reading presents itself as a natural
 *   legal baseline with negligible extraction, but it is sustained by active
 *   legal enforcement and benefits identifiable human parties. The authored
 *   metrics reflect the reading's own self-assessment of low extraction,
 *   while beneficiary declarations and omega variables flag the false-summit
 *   ambiguity for engine evaluation. Animals are deliberately excluded from
 *   the victim set per this reading's denial of moral patienthood.
 *
 * KEY AGENTS:
 *   - animal_property_owners: Primary beneficiary (organized/generational/constrained) â collects economic surplus and exclusive legal control under the property framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.06).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.12).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animal Moral Status â Property Reading").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, '0154db8b-72b9-4cd7-b763-bedd44cd3c54').
narrative_ontology:cs_kernel_codification('0154db8b-72b9-4cd7-b763-bedd44cd3c54', formalized).
narrative_ontology:cs_authority_grounding('0154db8b-72b9-4cd7-b763-bedd44cd3c54', lineage).
narrative_ontology:cs_interpretation_layer_present('0154db8b-72b9-4cd7-b763-bedd44cd3c54').
narrative_ontology:cs_reading_relation('0154db8b-72b9-4cd7-b763-bedd44cd3c54', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_reading_relation('0154db8b-72b9-4cd7-b763-bedd44cd3c54', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('0154db8b-72b9-4cd7-b763-bedd44cd3c54', foundational, animals_are_legal_property_without_moral_standing).
narrative_ontology:cs_axiom_status(animals_are_legal_property_without_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('0154db8b-72b9-4cd7-b763-bedd44cd3c54', animals_are_legal_property_without_moral_standing, conventional).
narrative_ontology:cs_reference_frame('0154db8b-72b9-4cd7-b763-bedd44cd3c54', classical_property_supremacy).
narrative_ontology:cs_drift_state('0154db8b-72b9-4cd7-b763-bedd44cd3c54', contemporary_welfare_regulation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0154db8b-72b9-4cd7-b763-bedd44cd3c54', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_property_owners).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, legal_anthropocentrism).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, property_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal title to animals as chattel property under statutory and common law regimes. Exercise rights of use, exclusion, and disposition limited chiefly by general prohibitions on waste, nuisance, or inefficiency. Receive the economic surplus generated by animal labor, reproduction, and body products within a framework that denies animals independent standing to object or bring claims.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_property_owners, beneficiary,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human use of animals by assigning exclusive, transferable control rights that prevent conflict over animal bodies, labor, and reproductive capacity; establishes a uniform legal category applicable across agriculture, research, companionship, and entertainment.
% TRANSFER_FUNCTION: Moves authority over animal disposition from the commons (or from animals themselves) to individual or corporate human owners; moves economic surplus from animal productivity to owners without countervailing moral or legal claim from the animals.
% ABSENT_VOICES: Abolitionist and animal-rights advocates challenge the property framework from outside the legal tradition, arguing that sentience generates standing independent of human assignment. Animals themselves are structurally voiceless within the framework, lacking procedural capacity to bring claims or be represented in adjudication.
% DISAPPEARANCE_RATIONALE: Legal title to animals underpins the entire animal-use economy; without it, ownership claims to livestock, research subjects, and companion animals would dissolve into custodianship or rights-based arrangements, forcing immediate restructuring of agriculture, biomedical research, and pet-keeping practices.
% FOUNDING_PROBLEM: How to prevent conflict over the control and use of non-human animals by establishing clear, enforceable human dominion and transferable rights of exclusion.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians trace the property classification to Roman law and early modern statutory codification. Animal ethicists and critical animal scholars outside the beneficiary class contest that the founding problem required a property solution, citing anthropocentric bias; no neutral party outside the benefiting tradition corroborates the property framing as the only or necessary solution.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__property_reading, 0.06, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.06 to reflect the property reading's self-assessment that arranging animals as property extracts almost nothing from humans and merely registers a natural hierarchy. Suppression is low (0.12) because the framework operates primarily through definitional fiat and bureaucratic legal enforcement rather than overt coercion. Accessibility collapse is high (0.88) because, once the property framework is accepted, alternative moral-legal arrangements collapse conceptually â animals are simply not candidates for rights. Theater ratio is modest (0.15) and rising slowly over the interval because the pure property reading increasingly performs legal maintenance rituals (formal title, registration, breed standards) as welfare and rights challenges erode its taken-for-granted status. Resistance is low-moderate (0.25) because animal advocacy exists but is structurally marginalized within property-law institutions.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (animal_property_owners) experiences the constraint as low-extraction coordination that prevents conflict over resource use. An observer seat sees that the same framework extracts bodies, labor, and lives from animals, but because the property reading denies animals standing, this extraction does not register in the victim-derived directionality chain. The divergence between the beneficiary seat and a hypothetical animal seat (which the reading renders non-existent) is extreme; the engine will compute near-zero effective extraction for the beneficiary and would compute near-total extraction for animals if they were admitted as payers.
 *
 * DIRECTIONALITY LOGIC:
 *   The only declared beneficiary is animal_property_owners, who receive legal title, economic surplus, and exclusionary control. No victims are declared because the property reading structurally denies that animals can be wronged or bear costs in a morally relevant sense. Directionality for the beneficiary seat is therefore near the full-beneficiary end (low d), dampening effective extraction toward zero. If animals were admitted as a stakeholder class, their directionality would sit at the full-target end (high d) and their effective extraction would be extreme; the reading prevents this by definitional exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by separating the reading's coordination function (clear resource allocation, conflict prevention over animal use) from its extraction function (appropriation of animal bodies without countervailing claim). Because the reading denies animals moral standing, it cannot be a snare or tangled_rope in the engine's victim-requiring gates; yet the presence of beneficiaries on a mountain claim triggers false-summit evaluation. If the founding problem (preventing human conflict over animals) is dead or contested while the arrangement persists, the mismatch flag fires, directing analysis toward piton or tangled_rope rather than mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_naturalness,
    'Is the classification of animals as property a natural-law feature of human-animal relations or a constructed legal regime that benefits identifiable human parties?',
    'Comparative legal history examining societies without formal animal property regimes, and analysis of whether property rights persist without active legal enforcement.',
    'If constructed, the constraint is a false summit (likely rope or tangled_rope) rather than a genuine mountain; if natural, the beneficiary presence requires explanation as non-extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_status_naturalness, conceptual, 'Natural law versus constructed legal regime ambiguity').

omega_variable(
    animal_agency_exclusion,
    'Does the exclusion of animals from the victim set reflect their genuine lack of moral patienthood, or does it function as a definitional move that prevents extraction from being registered?',
    'Ethical analysis of whether sentience alone is sufficient for moral standing, independent of legal categorization; cross-cultural comparison of animal moral status frameworks.',
    'If animals are genuine moral patients, effective extraction is vastly higher than the base metric suggests and the constraint is at minimum a tangled_rope with animals as hidden victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(animal_agency_exclusion, conceptual, 'Moral patienthood exclusion as definitional move or genuine absence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ams_property_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ams_property_tr_t20, animal_moral_status__property_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(ams_property_tr_t40, animal_moral_status__property_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(ams_property_tr_t60, animal_moral_status__property_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement(ams_property_tr_t80, animal_moral_status__property_reading, theater_ratio, 80, 0.13).
narrative_ontology:measurement(ams_property_tr_t100, animal_moral_status__property_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(ams_property_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(ams_property_be_t20, animal_moral_status__property_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(ams_property_be_t40, animal_moral_status__property_reading, base_extractiveness, 40, 0.06).
narrative_ontology:measurement(ams_property_be_t60, animal_moral_status__property_reading, base_extractiveness, 60, 0.06).
narrative_ontology:measurement(ams_property_be_t80, animal_moral_status__property_reading, base_extractiveness, 80, 0.07).
narrative_ontology:measurement(ams_property_be_t100, animal_moral_status__property_reading, base_extractiveness, 100, 0.07).

% Suppression requirement over time
narrative_ontology:measurement(ams_property_su_t0, animal_moral_status__property_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(ams_property_su_t20, animal_moral_status__property_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(ams_property_su_t40, animal_moral_status__property_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(ams_property_su_t60, animal_moral_status__property_reading, suppression_requirement, 60, 0.14).
narrative_ontology:measurement(ams_property_su_t80, animal_moral_status__property_reading, suppression_requirement, 80, 0.16).
narrative_ontology:measurement(ams_property_su_t100, animal_moral_status__property_reading, suppression_requirement, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__welfare_reading).

% DUAL FORMULATION NOTE:
% The animal_moral_status kernel decomposes into three structurally distinct constraints. The property reading (this file) asserts animals are resources without standing. The welfare reading layers sentience-based protections on top of property. The abolitionist reading rejects property status entirely. Each produces a different epsilon, victim set, and coordination/extraction balance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
