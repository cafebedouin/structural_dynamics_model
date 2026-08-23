% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Institutional Substitution of Dueling by Courts, Banking, and Libel Law
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This reading argues that dueling declined because state courts,
 *   commercial banking arbitration, and libel law offered a superior
 *   coordination mechanism for dispute resolution. The new institutions were
 *   not imposed by force alone; they outcompeted dueling by providing
 *   cheaper, more predictable, and more scalable outcomes. Dueling persisted
 *   only in institutional gaps (e.g., military honor, remote frontiers) where
 *   the new system had not yet reached. The constraint is the emergent
 *   institutional framework, which functions as a rope: it coordinates
 *   behavior without extracting from participants, and its adoption was
 *   largely voluntary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.1).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Institutional Substitution of Dueling by Courts, Banking, and Libel Law").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical_sociology/cultural_anthropology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, '50c09836-daac-456d-963d-e280d17401bf').
narrative_ontology:cs_kernel_codification('50c09836-daac-456d-963d-e280d17401bf', distributed).
narrative_ontology:cs_authority_grounding('50c09836-daac-456d-963d-e280d17401bf', practice).
narrative_ontology:cs_interpretation_layer_present('50c09836-daac-456d-963d-e280d17401bf').
narrative_ontology:cs_reading_relation('50c09836-daac-456d-963d-e280d17401bf', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('50c09836-daac-456d-963d-e280d17401bf', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('50c09836-daac-456d-963d-e280d17401bf', foundational, institutional_substitution_primary_driver).
narrative_ontology:cs_axiom_status(institutional_substitution_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('50c09836-daac-456d-963d-e280d17401bf', institutional_substitution_primary_driver, empirically_contingent).
narrative_ontology:cs_axiom('50c09836-daac-456d-963d-e280d17401bf', secondary, dueling_persistence_in_gaps).
narrative_ontology:cs_axiom_status(dueling_persistence_in_gaps, holdable).
narrative_ontology:cs_axiom_grounding('50c09836-daac-456d-963d-e280d17401bf', dueling_persistence_in_gaps, empirically_contingent).
narrative_ontology:cs_reference_frame('50c09836-daac-456d-963d-e280d17401bf', pre_institutional_dispute_resolution).
narrative_ontology:cs_drift_state('50c09836-daac-456d-963d-e280d17401bf', late_19th_century, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('50c09836-daac-456d-963d-e280d17401bf', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, state_courts).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, banking_institutions).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, libel_law_system).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, general_public).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, institutional_dispute_resolution_superiority).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, legal_modernization_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established and expanded formal judicial systems that offered binding, enforceable judgments for commercial, property, and personal disputes. They gained legitimacy and caseload as the primary dispute-resolution forum.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, state_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Developed commercial arbitration and credit-reporting mechanisms that resolved financial disputes without violence. They benefited from predictable, low-cost resolution that facilitated lending and trade.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, banking_institutions, beneficiary,
    organized, biographical, mobile, national).

% Provided a legal avenue for reputation defense, replacing the duel as the means of answering insult. Lawyers, publishers, and plaintiffs gained a structured process with damages rather than bloodshed.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, libel_law_system, beneficiary,
    organized, biographical, mobile, national).

% Gained access to reliable, non-violent dispute resolution for everyday conflicts. The shift reduced the risk of stray violence and made justice more accessible across classes.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, general_public, beneficiary,
    moderate, biographical, mobile, national).

% Aristocrats and military officers who viewed dueling as a necessary ritual of honor. They were not legally barred from courts but saw institutional resolution as incompatible with their code; their influence waned as the new system became normative.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, duelists_honor_culture_adherents, excluded,
    moderate, biographical, constrained, national).

% Analyze the long-term shift from private violence to state-mediated dispute resolution. They document the institutional, cultural, and economic drivers without participating in the historical process.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, legal_historians_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides reliable, standardized, and legally enforceable dispute resolution for commercial, reputational, and personal conflicts, replacing the ad-hoc and violent mechanism of dueling.
% TRANSFER_FUNCTION: Moves dispute resolution from private violence (dueling) to public institutions (courts, banking arbitration, libel courts), transferring the cost of conflict from bodily risk to legal fees and procedural time.
% ABSENT_VOICES: Duelists and honor-culture adherents who viewed dueling as a necessary aristocratic privilege; they were marginalized but not excluded from the new institutions.
% DISAPPEARANCE_RATIONALE: The institutional framework is the primary dispute-resolution mechanism for modern societies; its removal would create a vacuum that would be filled by either a return to private violence or new informal mechanisms.
% FOUNDING_PROBLEM: The need for a predictable, scalable, and non-violent method of resolving disputes in an increasingly complex commercial and social order.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Lawrence Friedman) and sociologists (e.g., Norbert Elias) document the long-term trend toward state monopolization of violence and formal dispute resolution; this is not solely attested by the benefiting institutions.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and suppression are low because the institutions did not forcibly extract resources from duelists; rather, individuals and groups chose the new forums because they worked better. Theater ratio is low because the institutions perform their stated function (resolving disputes) rather than maintaining a facade. Accessibility collapse is moderate: once the institutional option was understood and available, dueling became a fringe choice, but not because alternatives were actively suppressed. Resistance is low because the transition was driven by preference, not coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of honor-culture adherents, the constraint may feel like a snare (loss of meaning, forced assimilation). From the institutional perspective, it is a rope. The engine will compute per-seat types from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The agenda_setter (state courts) and beneficiaries (banking, libel law, public) all gain from the constraint: courts gain authority, banks gain predictable commerce, libel law gains a structured reputation market, and the public gains safety. The excluded group (duelists) loses status and a cultural practice but is not materially extracted from; they retain the option to use courts. No payer role exists because the substitution is voluntary and mutually beneficial.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for scalable non-violent dispute resolution) remains live. The constraint has not atrophied; it has expanded and adapted. No mandatrophy is present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this reading (institutional displacement) a distinct constraint from the contraction_reading and overdetermined_composite_reading, or are they facets of the same historical process?',
    'Compare the beneficiary/victim structures and coordination functions across readings. If each reading yields a different constraint type or different stakeholder roles, they are distinct constraints linked by network.affects_constraints.',
    'If distinct, each reading gets its own ε and classification; if not, they should be merged into a single constraint story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to ε-invariance: one reading, one constraint, one ε.').

omega_variable(
    voluntary_substitution_ambiguity,
    'Was the adoption of courts, banking arbitration, and libel law truly voluntary, or did state power coerce the transition (e.g., anti-dueling laws, monopoly on violence)?',
    'Examine historical enforcement: were anti-dueling statutes actively prosecuted, or did they merely codify a shift already underway? Compare jurisdictions with strong vs. weak state capacity.',
    'If coercion was primary, suppression and extractiveness would be higher, potentially reclassifying the constraint as tangled_rope or snare. If voluntary, rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_substitution_ambiguity, empirical, 'Whether the rope''s coordination function was imposed or adopted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t0, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(duel_tr_t30, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 30, 0.07).
narrative_ontology:measurement(duel_tr_t60, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement(duel_tr_t90, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 90, 0.09).
narrative_ontology:measurement(duel_tr_t120, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 120, 0.1).
narrative_ontology:measurement(duel_tr_t150, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 150, 0.1).

% Extraction over time
narrative_ontology:measurement(duel_be_t0, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(duel_be_t30, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(duel_be_t60, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 60, 0.14).
narrative_ontology:measurement(duel_be_t90, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 90, 0.15).
narrative_ontology:measurement(duel_be_t120, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 120, 0.15).
narrative_ontology:measurement(duel_be_t150, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 150, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t0, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(duel_su_t30, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 30, 0.07).
narrative_ontology:measurement(duel_su_t60, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 60, 0.09).
narrative_ontology:measurement(duel_su_t90, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 90, 0.1).
narrative_ontology:measurement(duel_su_t120, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 120, 0.1).
narrative_ontology:measurement(duel_su_t150, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 150, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__institutional_displacement_reading, 0.1).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings form a constraint family for the kernel dueling_disappearance_mechanism. Each reading instantiates a different constraint with its own ε: institutional displacement (rope), cultural contraction (mountain or snare depending on reading), overdetermined composite (tangled_rope). They are linked because they address the same historical phenomenon from different structural angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
