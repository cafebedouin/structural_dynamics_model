% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Animal Property Status Reading
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   The property reading of animal status holds that animals are chattel —
 *   legal things, not persons — whose moral considerability derives entirely
 *   from their economic value to owners. This reading instantiates the
 *   animal_status_kernel by fixing the kernel's reference frame in Roman
 *   property law and the agricultural economies that shaped Western legal
 *   tradition. Anti-cruelty statutes exist but protect only the owner's
 *   property interest (preventing 'wanton destruction of value'), not the
 *   animal's interest in not suffering. The constraint is a snare: its
 *   coordination story (clear ownership enables efficient use) is cover for a
 *   system that extracts the entirety of animals' lives for human benefit
 *   with no countervailing moral constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.85).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.9).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal Property Status Reading").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, 'e1c66178-a54d-4123-a2f4-a7c042fd8171').
narrative_ontology:cs_kernel_codification('e1c66178-a54d-4123-a2f4-a7c042fd8171', formalized).
narrative_ontology:cs_authority_grounding('e1c66178-a54d-4123-a2f4-a7c042fd8171', extraction).
narrative_ontology:cs_interpretation_layer_present('e1c66178-a54d-4123-a2f4-a7c042fd8171').
narrative_ontology:cs_reading_relation('e1c66178-a54d-4123-a2f4-a7c042fd8171', animal_status_kernel__welfare_reading, influences).
narrative_ontology:cs_reading_relation('e1c66178-a54d-4123-a2f4-a7c042fd8171', animal_status_kernel__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('e1c66178-a54d-4123-a2f4-a7c042fd8171', foundational, animals_are_mere_property).
narrative_ontology:cs_axiom_status(animals_are_mere_property, holdable).
narrative_ontology:cs_axiom_grounding('e1c66178-a54d-4123-a2f4-a7c042fd8171', animals_are_mere_property, conventional).
narrative_ontology:cs_axiom('e1c66178-a54d-4123-a2f4-a7c042fd8171', foundational, only_economic_value_matters).
narrative_ontology:cs_axiom_status(only_economic_value_matters, holdable).
narrative_ontology:cs_axiom_grounding('e1c66178-a54d-4123-a2f4-a7c042fd8171', only_economic_value_matters, instrumental).
narrative_ontology:cs_reference_frame('e1c66178-a54d-4123-a2f4-a7c042fd8171', roman_property_law_tradition).
narrative_ontology:cs_drift_state('e1c66178-a54d-4123-a2f4-a7c042fd8171', contemporary_animal_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e1c66178-a54d-4123-a2f4-a7c042fd8171', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, property_law_regime).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, animals).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, property_rights_absolutism).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, economic_value_monism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal title to animals; set terms of use, breeding, sale, and killing. Benefit from unrestricted use rights constrained only by anti-cruelty statutes that protect their property value. Can exit by selling animals or shifting to non-animal enterprises.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_owners, agenda_setter,
    institutional, generational, arbitrage, global).

% Industrial agriculture, biomedical research, entertainment, and pet trade sectors that depend on animals as property inputs. Collect economic surplus from animal use; lobby to maintain property framework. Exit requires business model transformation.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_use_industries, beneficiary,
    organized, biographical, constrained, global).

% The legal system that defines, enforces, and adjudicates animal property status. Courts, legislatures, and agencies that treat animals as chattel. Collects legitimacy and fee revenue from the property system. Could reform but has institutional inertia.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, property_law_regime, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__property_reading, property_law_regime, beneficiary).

% Sentient beings whose bodies, labor, and lives are the object of the property constraint. Bear all costs of use (confinement, suffering, killing) with zero legal standing to object. No exit possible within the property framework; escape is physically prevented.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animals, payer,
    powerless, immediate, trapped, local).

% Advocates and scholars who argue animals are moral persons with the right not to be property. Excluded from property-framework discourse because their position denies the framework's foundational premise. Seek to dismantle the constraint entirely.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, global).

% Advocates who accept property status but push for welfare constraints. Operate at the margin of the property framework; their gains are concessions within the system, not challenges to it. Excluded from core property-rights decision-making.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, welfare_advocates, excluded,
    organized, generational, constrained, global).

% Academic observers who analyze the property framework's coherence, history, and alternatives. Do not collect from or pay into the constraint; provide the analytical seat for classification.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human use of animals by establishing clear ownership rights and liability rules, enabling efficient markets in animal bodies and labor.
% TRANSFER_FUNCTION: Moves all moral considerability and legal protection from animals to owners; animals' interests count only insofar as they affect owner's property value.
% ABSENT_VOICES: Animals themselves (cannot speak in legal/political systems); abolitionist advocates (excluded from property-framework discourse because they deny its premise); future generations who inherit depleted animal populations and ecological consequences.
% DISAPPEARANCE_RATIONALE: Removing property status would collapse the legal framework enabling industrial animal use, requiring new regimes of care, rights, or stewardship. Animal agriculture, research, and pet trade would reorganize entirely.
% FOUNDING_PROBLEM: Need for clear, enforceable rules governing human control over animals for food, labor, research, and companionship in pre-modern and early modern societies.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians document the property framework's origins in Roman law and agricultural economies; animal ethicists outside the property framework (Regan, Singer, Francione) attest the founding problem is solved or obsolete; agricultural economists attest it remains live for current production systems.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status_kernel__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__property_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is very high (0.85) because the constraint permits any use that generates owner value, with no intrinsic limit. Suppression is near-total (0.9) because animals are physically confined and legally silenced; alternatives (rights, personhood, welfare-as-constraint) are structurally excluded from the property framework. Theater is low (0.1) — the property system functions exactly as designed for its beneficiaries. Accessibility collapse is high (0.8) because the legal categories make animal personhood conceptually incoherent within the framework. Resistance is moderate (0.4) — abolitionist and welfare movements exist but have not shifted the kernel's dominant reading.
 *
 * PERSPECTIVAL GAP:
 *   From the owner/industry seat, the arrangement is a rope: clear property rights solve the coordination problem of animal use efficiently. From the animal seat (if it could speak), it is a snare: total extraction with no voice. From the legal scholar seat, the divergence is visible — the same statute (anti-cruelty) reads as coordination (preventing value destruction) to owners and as theater (protecting only economic interest) to abolitionists. The engine computes this seat divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal owners and industries are structural beneficiaries (d near 0.0): they collect the full economic value of animal lives, control the rules, and have arbitrage-grade exit. The property law regime is both agenda-setter and beneficiary (d ~ 0.1): it administers the system and collects legitimacy rents. Animals are full targets (d = 1.0): they bear all costs, have zero exit, and are identity-locked into the property status by the very framework that denies them standing. Abolitionist and welfare advocates are excluded (d undefined): they are not seats within the constraint but external challengers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (clear rules for animal use in agrarian economies) is contested: property advocates say it remains live; abolitionists say it is obsolete; welfare advocates say it is partially solved but the solution (property) now causes more harm than the problem. The mandate has atrophied — the property framework persists not because it solves a live coordination problem better than alternatives, but because it extracts massive value for powerful beneficiaries. This is not a piton (theatrical maintenance of a dead function); the extraction is active and the function (enabling industrial animal use) is exactly what beneficiaries want.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the property reading a distinct constraint with its own ε, or merely a rhetorical framing of the same constraint that welfare and abolitionist readings evaluate differently?',
    'Apply ε-invariance test: if measuring extraction under property reading (animals excluded from victim-set) yields different ε than welfare reading (animals in victim-set with partial protection) or abolitionist reading (animals as full victims), they are distinct constraints. The structural delta (victim-set composition, beneficiary structure, enforcement target) confirms distinctness.',
    'If distinct, each reading gets its own constraint story with independent classification. If not distinct, the kernel would be a single constraint with observer-dependent classification — violating ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three readings of animal_status_kernel are structurally distinct constraints per ε-invariance.').

omega_variable(
    animal_victimhood_structural,
    'Do animals occupy the victim seat structurally, even though the property reading explicitly denies them victim status?',
    'Check whether the constraint''s operation meets the victim-seat criteria: (1) bears costs of the constraint, (2) has no exit, (3) has no voice in the constraint''s maintenance. Animals meet all three. The reading''s denial is a semantic move, not a structural one.',
    'If animals are structural victims, the constraint is a snare (pure extraction with suppressed victim-set). If the reading''s denial is structurally effective, the constraint might classify as rope (coordination with no victims). The engine''s directionality derivation from exit/power should place animals at d=1.0 regardless of reading''s semantics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(animal_victimhood_structural, conceptual, 'Whether structural victimhood exists independently of the reading''s explicit victim declarations.').

omega_variable(
    anti_cruelty_genuine_constraint,
    'Do anti-cruelty statutes impose any genuine constraint on owner use beyond protecting property value?',
    'Examine case law: are there prosecutions for cruelty that do not involve economic loss to owner? Are there welfare standards (space, enrichment, social needs) that reduce owner profit? If statutes only prevent ''wanton'' destruction without economic rationale, they are property-value protection, not animal welfare.',
    'If anti-cruelty laws are purely property-value protection, the constraint has zero internal constraint on extraction — pure snare. If they impose genuine welfare costs on owners, the constraint has a coordination/welfare hybrid element (tangled rope at the margins).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_cruelty_genuine_constraint, empirical, 'Whether anti-cruelty statutes function as genuine welfare constraints or pure property-value protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t25, animal_status_kernel__property_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__property_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(anim_tr_t75, animal_status_kernel__property_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(anim_tr_t100, animal_status_kernel__property_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(anim_be_t25, animal_status_kernel__property_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__property_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement(anim_be_t75, animal_status_kernel__property_reading, base_extractiveness, 75, 0.84).
narrative_ontology:measurement(anim_be_t100, animal_status_kernel__property_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(anim_su_t25, animal_status_kernel__property_reading, suppression_requirement, 25, 0.87).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__property_reading, suppression_requirement, 50, 0.88).
narrative_ontology:measurement(anim_su_t75, animal_status_kernel__property_reading, suppression_requirement, 75, 0.89).
narrative_ontology:measurement(anim_su_t100, animal_status_kernel__property_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__property_reading, 0.15).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_welfare_regulations).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_anticruelty_statutes).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, veterinary_practice_standards).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, wildlife_management_law).

% DUAL FORMULATION NOTE:
% This constraint is the property_reading of the animal_status_kernel. It differs from welfare_reading (which adds welfare constraints to property) and abolitionist_reading (which denies property status entirely). The property reading's ε is higher because it recognizes no countervailing moral claims; welfare_reading's ε is lower due to welfare constraints; abolitionist_reading's ε is near zero (no extraction if animals have rights not to be used). All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__property_reading, institutional, 0.1).
constraint_indexing:directionality_override(animal_status_kernel__property_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
