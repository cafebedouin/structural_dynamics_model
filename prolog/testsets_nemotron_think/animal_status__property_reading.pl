% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   This constraint story represents the property_reading of the contested
 *   animal_status kernel. From this reading's perspective, the legal
 *   classification of animals as property is a settled, functional framework
 *   — a mountain of legal ontology that enables human coordination around
 *   animal use with minimal friction. The reading acknowledges welfare
 *   statutes as the only legitimate constraints on ownership, but treats
 *   these as marginal adjustments to a stable baseline, not as evidence that
 *   the property classification itself is contested. Extraction is near-zero
 *   (ε=0.05) because the constraint operates as a coordination mechanism: it
 *   assigns decision rights clearly, reduces transaction costs, and requires
 *   little active enforcement beyond standard property law. Suppression is
 *   low (0.15) because the constraint's persistence does not depend on
 *   coercing animals (they have no legal agency to resist) but on maintaining
 *   a legal category that humans broadly accept. The slight rise in
 *   suppression_requirement over the interval reflects growing pressure from
 *   welfare_advocates and scientific evidence, which the property framework
 *   absorbs through statutory amendment rather than ontological revision.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.05).
domain_priors:suppression_score(animal_status__property_reading, 0.15).
domain_priors:theater_ratio(animal_status__property_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, mountain).
narrative_ontology:human_readable(animal_status__property_reading, "Animals as Legal Property (Property Reading)").
narrative_ontology:topic_domain(animal_status__property_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:emerges_naturally(animal_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, 'eeacbb79-1337-46b7-ba57-5eed92e18e5d').
narrative_ontology:cs_kernel_codification('eeacbb79-1337-46b7-ba57-5eed92e18e5d', formalized).
narrative_ontology:cs_authority_grounding('eeacbb79-1337-46b7-ba57-5eed92e18e5d', lineage).
narrative_ontology:cs_interpretation_layer_present('eeacbb79-1337-46b7-ba57-5eed92e18e5d').
narrative_ontology:cs_reading_relation('eeacbb79-1337-46b7-ba57-5eed92e18e5d', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_reading_relation('eeacbb79-1337-46b7-ba57-5eed92e18e5d', animal_status__welfare_reading, influences).
narrative_ontology:cs_axiom('eeacbb79-1337-46b7-ba57-5eed92e18e5d', foundational, animals_are_legal_property).
narrative_ontology:cs_axiom_status(animals_are_legal_property, holdable).
narrative_ontology:cs_axiom_grounding('eeacbb79-1337-46b7-ba57-5eed92e18e5d', animals_are_legal_property, conventional).
narrative_ontology:cs_axiom('eeacbb79-1337-46b7-ba57-5eed92e18e5d', secondary, human_dominion_absolute_except_welfare).
narrative_ontology:cs_axiom_status(human_dominion_absolute_except_welfare, holdable).
narrative_ontology:cs_axiom_grounding('eeacbb79-1337-46b7-ba57-5eed92e18e5d', human_dominion_absolute_except_welfare, conventional).
narrative_ontology:cs_reference_frame('eeacbb79-1337-46b7-ba57-5eed92e18e5d', classical_property_framework).
narrative_ontology:cs_drift_state('eeacbb79-1337-46b7-ba57-5eed92e18e5d', contemporary_animal_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eeacbb79-1337-46b7-ba57-5eed92e18e5d', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, human_owners).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_industries).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, legal_tradition_institutions).
narrative_ontology:constraint_vindicates(animal_status__property_reading, property_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_status__property_reading, human_dominion_framework).
narrative_ontology:constraint_vindicates(animal_status__property_reading, legal_personhood_boundary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal title to animals as property; set terms of use, transfer, and disposition. The legal framework recognizes their ownership rights as presumptively absolute, limited only by anti-cruelty statutes they influenced. Exit is trivial — they can sell, relinquish, or modify use at will.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, human_owners, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate at scale (agriculture, research, entertainment, companion animal trade) under the property framework. The legal status of animals as objects enables predictable commercial planning, liability limitation, and asset valuation. They lobby to maintain the property baseline and shape welfare statutes to minimize operational constraint.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_industries, beneficiary,
    organized, biographical, mobile, global).

% Courts, legislatures, and regulatory bodies that maintain and interpret the property classification. They benefit from the stability and clarity of a settled legal category — property law provides ready-made rules for transfer, liability, and remedy. Their authority rests partly on administering this stable classification.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, legal_tradition_institutions, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(animal_status__property_reading, legal_tradition_institutions, beneficiary).

% The entities whose legal status is defined by this constraint. As property, they have no standing to bring claims, no recognized interests that courts must weigh independently, and no exit from the classification. Their experiences of confinement, suffering, or flourishing are legally relevant only insofar as they affect human owners or violate welfare statutes.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animals, excluded,
    powerless, biographical, trapped, global).

% Seek to expand welfare protections within the property framework. They operate by lobbying for statutory amendments, not by challenging the property classification itself. Their exclusion from the foundational status question means they contest the boundary of permissible use, not the ontology that makes use permissible.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, welfare_advocates, excluded,
    organized, biographical, constrained, national).

% Analyze the property classification's coherence, history, and alternatives. They do not administer the constraint nor bear its costs directly. Their work maps the conceptual architecture and documents the pressure points where the property frame strains against empirical and moral developments.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, universal legal category for animals that resolves disputes between humans over possession, use, liability, and transfer without requiring case-by-case moral adjudication of each animal's interests.
% TRANSFER_FUNCTION: Moves decision-making authority over animal lives and bodies from any hypothetical animal interest to the human title-holder, subject only to legislated welfare floors. The transfer is from the excluded (animals) to the agenda-setters (owners), mediated by the state.
% ABSENT_VOICES: Animals themselves are structurally excluded — they cannot speak in legal proceedings, initiate legislation, or hold rights. Their interests enter only as filtered through human representatives (owners, advocates, regulators) who may or may not track animal welfare faithfully.
% DISAPPEARANCE_RATIONALE: If the property classification vanished overnight, the entire legal architecture governing human-animal relations — ownership transfer, liability for harm, regulatory permitting, veterinary authority, agricultural contracts, research protocols — would lose its foundational category. Courts would need a new ontology (guardianship? personhood? stewardship?) to resolve disputes, and industries would face immediate legal uncertainty.
% FOUNDING_PROBLEM: Pre-modern and early modern legal systems needed a workable category for living beings that humans routinely buy, sell, work, kill, and experiment on. The Roman law category of 'res' (thing) extended to animals provided a ready-made framework that avoided adjudicating animal interests in every transaction.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (outside the benefiting industries) confirm the property classification solved a genuine coordination problem for human commerce and dispute resolution. However, contemporary ethologists, cognitive scientists, and moral philosophers outside the property-rights coalition document that the founding assumption — that animals lack morally relevant interests — is empirically false and conceptually strained.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Claimed type is mountain because this reading sees the property classification as a natural-law-like feature of legal ontology — it 'just is' the framework within which human-animal relations are structured, analogous to how gravity structures physical relations. The metrics support this: near-zero extractiveness (the constraint coordinates rather than extracts), negligible theater (the legal category performs its function without performative maintenance), high accessibility_collapse (alternative ontologies are legally unintelligible within the framework), and low resistance (the framework persists without active defense because it is the default). The slight metric drift over 50 years (extraction 0.03→0.05, suppression 0.10→0.15) reflects welfare statutes expanding the 'except by welfare statutes' clause, which this reading treats as compatible adjustments, not fundamental challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the property_reading seat, this is a mountain — a coordination framework as natural as contract law. From the abolitionist_reading seat (a different constraint), the same legal structure is a snare — it extracts animal lives for human benefit under cover of legal ontology. From the welfare_reading seat, it is a tangled_rope — genuine coordination (clear property rules) hybridized with extraction (animals' interests systematically discounted). The engine computes these per-seat divergences from the structural data; this story authors only the property_reading's structural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Human owners and animal industries are structural beneficiaries (d near 0.0) — the constraint assigns them decision rights and asset value. Legal institutions are dual agenda_setter/beneficiary — they administer the framework and gain authority from its stability. Animals are excluded (not beneficiaries, not payers in this reading's ontology — they are the substrate). Welfare advocates are excluded from the foundational status question but operate within the framework. The engine will compute directionality from these structural declarations: beneficiaries have arbitrage-grade exit (they can sell animals, change industries, relocate), so d is very low; excluded animals have trapped exit by definition; observers have analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for a workable legal category for routinely-used living beings) is contested as live: property-rights proponents say it remains live; scientists and ethicists say the founding empirical premise (animals lack morally relevant interests) is dead. The constraint persists not because the founding problem is live, but because the property framework has become self-reinforcing — industries, legal doctrines, and regulatory structures are built on it. This is not mandatrophy in the pure sense (the arrangement still coordinates human affairs effectively) but shows scaffold-like drift: the original justification is contested while the structure remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the property_reading a genuine mountain of legal ontology, or a constructed constraint that benefits identifiable human industries by naturalizing animals as objects?',
    'Trace the historical contingency of the property classification: if Roman law''s extension of ''res'' to animals was a pragmatic choice among alternatives (guardianship, stewardship, personhood for some species), the mountain claim is constructed. If no coherent alternative framework existed for pre-modern legal systems, the mountain claim has stronger footing.',
    'If constructed, the false_summit_mountain signature would trigger (mountain with declared beneficiaries) and reclassify toward tangled_rope. If genuine natural-law-like, the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the property classification is a necessary legal ontology or a contingent historical choice that benefits human owners.').

omega_variable(
    welfare_statutes_as_coordination_or_extraction,
    'Do welfare statutes represent genuine coordination (protecting vulnerable beings within a property framework) or extraction (legitimizing the property framework by providing a moral pressure valve)?',
    'Compare welfare statute stringency and enforcement in jurisdictions with similar property frameworks but different industry power. If statutes track industry tolerance rather than animal welfare science, they function as extraction-legitimation.',
    'If extraction-legitimation, the property_reading''s low extraction claim is partial — the welfare subsystem masks the constraint''s true extractive character. If genuine coordination, the property_reading''s mountain claim is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_statutes_as_coordination_or_extraction, empirical, 'Whether welfare statutes are functional coordination or performative cover for the property framework.').

omega_variable(
    animal_sentience_legal_irrelevance,
    'Can a legal system maintain the property classification for animals while incorporating overwhelming scientific consensus on animal sentience, without the classification becoming a snare?',
    'Observe whether jurisdictions with strong animal sentience recognition (e.g., EU Treaty of Lisbon, New Zealand Animal Welfare Act) retain the property classification or shift toward guardianship/personhood models. Track whether sentience recognition changes legal outcomes for animals.',
    'If sentience recognition forces ontological revision, the property_reading''s mountain is brittle — its accessibility_collapse is maintained only by suppressing scientific knowledge. If property classification absorbs sentience without structural change, the mountain is more resilient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(animal_sentience_legal_irrelevance, empirical, 'Whether scientific consensus on animal sentience destabilizes the property classification''s mountain status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_status__property_reading_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(animal_status__property_reading_tr_t10, animal_status__property_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(animal_status__property_reading_tr_t20, animal_status__property_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(animal_status__property_reading_tr_t30, animal_status__property_reading, theater_ratio, 30, 0.075).
narrative_ontology:measurement(animal_status__property_reading_tr_t40, animal_status__property_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(animal_status__property_reading_tr_t50, animal_status__property_reading, theater_ratio, 50, 0.08).

% Extraction over time
narrative_ontology:measurement(animal_status__property_reading_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(animal_status__property_reading_be_t10, animal_status__property_reading, base_extractiveness, 10, 0.04).
narrative_ontology:measurement(animal_status__property_reading_be_t20, animal_status__property_reading, base_extractiveness, 20, 0.045).
narrative_ontology:measurement(animal_status__property_reading_be_t30, animal_status__property_reading, base_extractiveness, 30, 0.048).
narrative_ontology:measurement(animal_status__property_reading_be_t40, animal_status__property_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(animal_status__property_reading_be_t50, animal_status__property_reading, base_extractiveness, 50, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(animal_status__property_reading_su_t0, animal_status__property_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(animal_status__property_reading_su_t10, animal_status__property_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(animal_status__property_reading_su_t20, animal_status__property_reading, suppression_requirement, 20, 0.13).
narrative_ontology:measurement(animal_status__property_reading_su_t30, animal_status__property_reading, suppression_requirement, 30, 0.14).
narrative_ontology:measurement(animal_status__property_reading_su_t40, animal_status__property_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(animal_status__property_reading_su_t50, animal_status__property_reading, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, information_standard).
narrative_ontology:boltzmann_floor_override(animal_status__property_reading, 0.02).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_welfare_statutes).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_research_regulation).
narrative_ontology:affects_constraint(animal_status__property_reading, agricultural_property_law).

% DUAL FORMULATION NOTE:
% This constraint is the property_reading of the animal_status kernel. It decomposes the colloquial 'animals as property' claim from the welfare_reading (sentient beings with interests constraining use) and abolitionist_reading (rights-holders precluding instrumental use). The ε-invariance principle requires separate stories because the property_reading's ε (~0.05) differs radically from the abolitionist_reading's ε (high, from the abolitionist's lights on the standing arrangement) and the welfare_reading's ε (moderate). The three stories form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
