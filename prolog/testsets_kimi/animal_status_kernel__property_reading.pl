% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Animal Property Status â Unrestricted Use Reading
 *   domain: moral philosophy / animal ethics / legal theory
 *
 * SUMMARY:
 *   This constraint instantiates the property reading of the
 *   animal_status_kernel: the legal and moral framework under which nonhuman
 *   animals are classified as chattel property, moral considerability derives
 *   exclusively from ownership rights, and economic value is treated as the
 *   sole relevant normative consideration. Under this reading, animals are
 *   structurally excluded from the victim-setâthe framework cannot
 *   recognize them as bearing costs because it denies them independent moral
 *   standing. The constraint operates as a high-extraction regime enforced by
 *   property law, contract systems, and anti-cruelty statutes reinterpreted
 *   as protecting owner asset value. Key agents include the owner class and
 *   animal-use industries (beneficiaries), the legal system
 *   (agenda-setter/enforcer), and nonhuman animals (structural payers whose
 *   costs the reading renders invisible).
 *
 * KEY AGENTS:
 *   - property_owners: Primary beneficiary (powerful/mobile) â captures economic surplus from unrestricted use rights.
 *   - animal_use_industries: Secondary beneficiary (institutional/mobile) â sectoral lobbyists and corporations whose models depend on property status.
 *   - nonhuman_animals: Primary target (powerless/trapped) â bears the full cost of extraction but is denied standing.
 *   - legal_system: Agenda-setter (institutional/analytical) â codifies and enforces the property kernel.
 *   - animal_advocates: Excluded voice (moderate/constrained) â argues for standing but is filtered out by the framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.92).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.78).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal Property Status â Unrestricted Use Reading").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral philosophy / animal ethics / legal theory").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, 'f8d9cecb-9c65-470b-8107-c09179ee040f').
narrative_ontology:cs_kernel_codification('f8d9cecb-9c65-470b-8107-c09179ee040f', formalized).
narrative_ontology:cs_authority_grounding('f8d9cecb-9c65-470b-8107-c09179ee040f', lineage).
narrative_ontology:cs_interpretation_layer_present('f8d9cecb-9c65-470b-8107-c09179ee040f').
narrative_ontology:cs_reading_relation('f8d9cecb-9c65-470b-8107-c09179ee040f', animal_status_kernel__welfare_reading, influences).
narrative_ontology:cs_reading_relation('f8d9cecb-9c65-470b-8107-c09179ee040f', animal_status_kernel__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('f8d9cecb-9c65-470b-8107-c09179ee040f', foundational, ownership_confers_exclusive_moral_authority).
narrative_ontology:cs_axiom_status(ownership_confers_exclusive_moral_authority, holdable).
narrative_ontology:cs_axiom_grounding('f8d9cecb-9c65-470b-8107-c09179ee040f', ownership_confers_exclusive_moral_authority, conventional).
narrative_ontology:cs_axiom('f8d9cecb-9c65-470b-8107-c09179ee040f', foundational, sentience_lacks_independent_normative_weight).
narrative_ontology:cs_axiom_status(sentience_lacks_independent_normative_weight, holdable).
narrative_ontology:cs_axiom_grounding('f8d9cecb-9c65-470b-8107-c09179ee040f', sentience_lacks_independent_normative_weight, conventional).
narrative_ontology:cs_reference_frame('f8d9cecb-9c65-470b-8107-c09179ee040f', absolute_property_dominion).
narrative_ontology:cs_drift_state('f8d9cecb-9c65-470b-8107-c09179ee040f', contemporary_welfare_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f8d9cecb-9c65-470b-8107-c09179ee040f', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, property_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_use_industries).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, nonhuman_animals).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, ownership_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, economic_value_priority).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, legal_property_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal title to animals under statutes that classify sentient beings as chattel property. Exercise unrestricted use, sale, confinement, and destruction rights. Capture the full economic surplus from animal bodies, labor, and reproductive capacity. Can exit the constraint by divesting property, but the framework subsidizes their economic position.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, property_owners, beneficiary,
    powerful, biographical, mobile, national).

% Corporations and sectoral associations whose business models depend on the legal classification of animals as property. Lobby legislatures and shape regulatory interpretation to maintain unrestricted use rights. Capture economies of scale from intensive confinement and processing that would be impermissible under personhood or welfare-based frameworks.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_use_industries, beneficiary,
    institutional, generational, mobile, global).

% Legally categorized as movable property or chattels. Subject to sale, confinement, instrumental use, and killing without consent or compensation. Cannot exit the property relation because the framework denies them legal standing and personhood; their interests are structurally inadmissible in courts and contracts.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, nonhuman_animals, payer,
    powerless, immediate, trapped, universal).

% Codifies and enforces the property classification through civil codes, title systems, and criminal law. Anti-cruelty statutes are interpreted as protecting owner asset value and public order rather than animal interests. Courts dismiss claims that seek standing for animals, preserving the exclusivity of owner control.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, legal_system, agenda_setter,
    institutional, generational, analytical, national).

% Advance normative claims for animal personhood, welfare rights, or abolition of property status. Structurally excluded from legal standing on behalf of animals because the framework recognizes only owners as holders of relevant interests. Their arguments are treated as threats to property rights rather than admissible moral claims.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns exclusive, transferable control over sentient beings to identified owners, eliminating open-access conflicts and establishing a uniform legal basis for commercial exchange and agricultural production.
% TRANSFER_FUNCTION: Transfers moral and legal standing from nonhuman animals to owners; transfers the economic surplus of animal bodies, labor, and lives to the owner class and animal-use industries; transfers the costs of confinement, suffering, and death to the animals themselves.
% ABSENT_VOICES: Nonhuman animals are rendered legally voiceless by the property classification. Animal ethics advocates and abolitionist theorists are present in public discourse but structurally excluded from legal standing and normative weight because the framework recognizes only ownership rights as morally and legally relevant.
% DISAPPEARANCE_RATIONALE: If the property classification vanished overnight, title systems would collapse across agriculture, research, and entertainment sectors. Contract and tort law would require fundamental renegotiation. Industries dependent on unrestricted use would face existential restructuring, and legal systems would be forced to adopt personhood or constrained-welfare frameworks.
% FOUNDING_PROBLEM: How to establish stable, transferable title and exclusive use rights over sentient beings in agricultural, research, and companion contexts without perpetual conflict over their control and commercial exchange.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and critical animal scholars attest the founding problem was historically tied to expanding agricultural capitalism and colonial land-use regimes. Animal ethics theorists from outside the beneficiary set argue the problem has been superseded by scientific recognition of animal sentience, while property scholars and industry associations within the beneficiary set assert the coordination problem remains live.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status_kernel__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__property_reading, 0.92, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.92 because the reading removes any countervailing moral constraint on use, permitting total instrumentalization of sentient beings. Suppression is high (0.78) because the constraint's persistence depends on actively excluding rival normative frameworks (animal rights, personhood, substantive welfare) from legal standing and public moral consideration. Accessibility collapse is substantial (0.70) because, once the property frame is accepted, alternatives appear legally nonsensicalâone cannot 'steal' an animal in the same sense as harming a person, and personhood claims are filtered out as category errors. Resistance is moderate (0.45) because animal advocacy exists but lacks institutional power to alter the property kernel. Theater ratio is low-moderate (0.28) because most enforcement activity serves genuine economic functions (title protection, contract enforcement), though a growing share performs the ritual of humane concern while leaving property logic intact.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (nonhuman animals) and the beneficiary seats (owners, industries) experience radically different constraint types: from the owner seat, the arrangement is legitimate coordination of resource use; from the animal seat, it is total extraction with no exit. The engine computes this divergence from the structural dataâtrapped exit, powerless status, and universal scope amplify effective extraction for the payer, while mobile exit and powerful/institutional status damp it for beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (property_owners, animal_use_industries) receive low directionality: the constraint subsidizes their economic activity by externalizing all costs onto sentient beings. The legal_system sits near symmetric as the enforcement apparatus, though its maintenance of the frame is partly subsidy to the owner class. Nonhuman_animals bear the highest directionality: they are the direct targets of extraction, denied exit, identity-locked into the property relation by legal definition, and subject to universal scope. The directionality derivation from beneficiary/payer declarations plus exit modulation places animals at the full-target end of the spectrum.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this constraint as a Rope (pure coordination of resource use) by requiring victim identification and high extractiveness metrics. The reading's founding problemâstable title and conflict avoidanceâhas been superseded by industrial-scale extraction, but the constraint persists because its beneficiaries capture the rents. The divergence between the claimed coordination function and the authored metrics flags the extraction that the reading's exclusion of animals from the victim-set is designed to obscure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    animal_moral_status_ambiguity,
    'Is the exclusion of animals from the victim-set a genuine ontological absence of moral status, or a legal construct that suppresses pre-existing standing?',
    'Comparative legal analysis of jurisdictions granting habeas corpus or personhood to nonhuman animals; convergence of cognitive ethology on sentience in vertebrates and many invertebrates.',
    'If animals possess independent moral status, the constraint is a false summitâpresented as natural legal order but actually constructed extractionâand would reclassify toward tangled_rope or snare with heightened effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(animal_moral_status_ambiguity, conceptual, 'Whether animal exclusion from victimhood reflects natural absence or constructed suppression').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of animal interests structural (legal standing barriers) or internalized (pervasive human supremacy ideology)?',
    'Cross-cultural variation in animal legal status; historical shift in public attitudes where legal change lags or leads cultural change; post-exit trajectory analysis where jurisdictions alter property status.',
    'If internalized, effective suppression exceeds the structural measure because the framework is self-policing through cultural norms, raising the true extraction experienced by the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of animal interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_status_property_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(animal_status_property_tr_t10, animal_status_kernel__property_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(animal_status_property_tr_t20, animal_status_kernel__property_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(animal_status_property_tr_t30, animal_status_kernel__property_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(animal_status_property_tr_t40, animal_status_kernel__property_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(animal_status_property_tr_t50, animal_status_kernel__property_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(animal_status_property_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(animal_status_property_be_t10, animal_status_kernel__property_reading, base_extractiveness, 10, 0.84).
narrative_ontology:measurement(animal_status_property_be_t20, animal_status_kernel__property_reading, base_extractiveness, 20, 0.87).
narrative_ontology:measurement(animal_status_property_be_t30, animal_status_kernel__property_reading, base_extractiveness, 30, 0.89).
narrative_ontology:measurement(animal_status_property_be_t40, animal_status_kernel__property_reading, base_extractiveness, 40, 0.91).
narrative_ontology:measurement(animal_status_property_be_t50, animal_status_kernel__property_reading, base_extractiveness, 50, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(animal_status_property_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(animal_status_property_su_t10, animal_status_kernel__property_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(animal_status_property_su_t20, animal_status_kernel__property_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(animal_status_property_su_t30, animal_status_kernel__property_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(animal_status_property_su_t40, animal_status_kernel__property_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(animal_status_property_su_t50, animal_status_kernel__property_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_status_kernel decomposes into three structurally distinct readings: property (this constraint), welfare, and abolitionist. Each reading assigns a different moral-legal status to nonhuman animals and produces a different epsilon. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
