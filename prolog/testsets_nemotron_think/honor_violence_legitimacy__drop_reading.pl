% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Dueling Code Legitimacy (Drop Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   The drop_reading of the honor_violence_legitimacy kernel holds that
 *   dueling's structural legitimacy as an honor mechanism persisted as a
 *   cultural Mountain — thinkable, available, unchallenged in principle —
 *   while its practice collapsed under external costs: state prosecution,
 *   professional disciplinary codes, and the declining relevance of the armed
 *   aristocratic class. The constraint is the code's legitimacy itself, not
 *   the practice. Beneficiaries (aristocrats, officers) retain the code as
 *   identity capital; the few actual duelists pay all costs. The engine will
 *   compute per-seat types from this structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.15).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.25).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, mountain).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Dueling Code Legitimacy (Drop Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:emerges_naturally(honor_violence_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '8869bdb3-b31b-4a6e-b015-39eef8fc81ee').
narrative_ontology:cs_kernel_codification('8869bdb3-b31b-4a6e-b015-39eef8fc81ee', formalized).
narrative_ontology:cs_authority_grounding('8869bdb3-b31b-4a6e-b015-39eef8fc81ee', lineage).
narrative_ontology:cs_interpretation_layer_present('8869bdb3-b31b-4a6e-b015-39eef8fc81ee').
narrative_ontology:cs_reading_relation('8869bdb3-b31b-4a6e-b015-39eef8fc81ee', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8869bdb3-b31b-4a6e-b015-39eef8fc81ee', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('8869bdb3-b31b-4a6e-b015-39eef8fc81ee', foundational, honor_violence_legitimacy_endures).
narrative_ontology:cs_axiom_status(honor_violence_legitimacy_endures, holdable).
narrative_ontology:cs_axiom_grounding('8869bdb3-b31b-4a6e-b015-39eef8fc81ee', honor_violence_legitimacy_endures, conventional).
narrative_ontology:cs_axiom('8869bdb3-b31b-4a6e-b015-39eef8fc81ee', secondary, external_costs_suppress_practice_not_legitimacy).
narrative_ontology:cs_axiom_status(external_costs_suppress_practice_not_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8869bdb3-b31b-4a6e-b015-39eef8fc81ee', external_costs_suppress_practice_not_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('8869bdb3-b31b-4a6e-b015-39eef8fc81ee', chivalric_code_legitimacy).
narrative_ontology:cs_drift_state('8869bdb3-b31b-4a6e-b015-39eef8fc81ee', post_legal_proscription_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8869bdb3-b31b-4a6e-b015-39eef8fc81ee', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, aristocratic_military_elite).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, officer_corps).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, nobility).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, actual_duelists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, officer_corps).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__drop_reading, honor_requires_violent_defense).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__drop_reading, ritualized_violence_bounds_social_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain the conceptual availability of the dueling code as a status marker and honor reservoir. They rarely duel but the code's legitimacy structures their social identity and distinguishes them from commercial/bureaucratic elites. Exit means abandoning the honor idiom entirely — possible but identity-costly.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, aristocratic_military_elite, beneficiary,
    institutional, generational, arbitrage, continental).

% Military regulations increasingly prohibit dueling (career-ending), but the officer corps' collective identity still invokes the code. They benefit from the code's legitimating aura while bearing the cost of disciplinary risk if they actually fight. Exit is constrained by professional identity fusion.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, officer_corps, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__drop_reading, officer_corps, payer).

% The few who still duel face criminal prosecution, social ostracism, and physical danger. They are the ones who pay the external costs that make the practice rare. Their exit options are minimal — honor culture traps them in the performance.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, actual_duelists, payer,
    moderate, immediate, trapped, local).

% Prosecute dueling as homicide/manslaughter, progressively extending state monopoly on violence. They do not benefit from the code; they suppress its enactment while the code's structural legitimacy persists in culture. Their enforcement is the external cost.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, legal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Rising commercial/professional elites for whom honor disputes resolve through courts, reputation markets, and contract law. They are structurally excluded from the dueling code (not gentlemen) and would object to its legitimacy if consulted — but the code never claimed to include them.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, bourgeois_professional_classes, excluded,
    organized, biographical, mobile, national).

% Analyze the code's persistence as cultural structure after its practice collapses. They see the full field: the code's availability as a Mountain-like structure, the external costs as suppression, the beneficiaries as identity-locked. No material stake.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bounded, ritualized mechanism for resolving status and honor disputes among armed elites without escalating to uncontrolled feuding or destabilizing the social order — a coordination solution for a specific historical class structure.
% TRANSFER_FUNCTION: Moves the risk of legal prosecution, social ostracism, and physical harm onto the small number of actual duelists, while the legitimacy structure (the code's conceptual availability) remains a cost-free status resource for the broader elite.
% ABSENT_VOICES: Commoners, women, colonial subjects, and non-aristocratic men were never admitted to the duelist class and had no standing to contest the code's legitimacy. Their honor disputes were resolved through entirely different (and often more brutal) mechanisms — the code's exclusivity is part of its coordination function for the elite.
% DISAPPEARANCE_RATIONALE: If the dueling code's structural legitimacy vanished overnight, the practical situation would barely shift — dueling is already vanishingly rare. Honor disputes among elites have long since migrated to courts, press campaigns, and institutional rivalry. The code is a cultural fossil: its disappearance would rearrange nothing.
% FOUNDING_PROBLEM: Early modern European aristocracies and officer corps needed a mechanism to resolve status disputes among armed men without triggering uncontrolled violence that threatened the sovereign's monopoly on force or the social order.
% FOUNDING_PROBLEM_CORROBORATION: Norbert Elias (civilizing process), Robert Nye (masculinity studies), Victor Kiernan (dueling history) — all outside the benefiting parties — attest the founding problem was specific to a historical class configuration that dissolved in the 19th century. No non-beneficiary source corroborates the code as a live coordination necessity.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_violence_legitimacy__drop_reading),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Low extractiveness (0.15) because the constraint barely operates — few duel, fewer pay. Moderate suppression (0.25) because legal prohibition exists but is unevenly enforced and culturally contested. High theater_ratio (0.68) because the code's maintenance is largely performative: seconds still negotiate, codes are still published, but the mechanism is hollow. High accessibility_collapse (0.72) because once you understand the code, alternatives (courts, reputation) are structurally visible but culturally inaccessible to the identity-locked. Low resistance (0.18) because no organized force defends the practice — only the idea.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (aristocrats, officers), the code is a Mountain — natural, legitimate, coordination-preserving. From the payer seat (actual duelists), it is a Snare — coercive, extractive, suppressing exit. From the agenda_setter seat (legal authorities), it is a decaying Piton — enforced by inertia. The engine computes this divergence; the drop_reading's claim (Mountain) reflects the beneficiary/analytical view.
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocratic elites and officers are beneficiaries (d near 0.0) — they collect status/identity from the code's availability without paying practice costs. Actual duelists are full targets (d near 1.0) — trapped, identity-locked, bearing all legal/physical costs. Legal authorities are agenda_setters (d ~0.5) — they enforce external costs but don't extract from the constraint. Bourgeois classes are excluded (mobile exit). Sociologists are analytical observers (d=0.5). The engine derives directionality from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (armed elite dispute resolution) is dead — the class that needed it dissolved. The constraint persists as identity theater for successor elites (officers, aristocrats) who benefit from its legitimacy without practicing it. This is mandatrophy: the mandate (honor coordination) outlived its function, but the structure remains because beneficiaries are identity-locked and no one bears enough cost to dismantle it. The drop_reading captures this as 'legitimacy remains, practice drops' — a Piton masquerading as a Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the honor_violence_legitimacy kernel a single constraint with observer-dependent classification, or three structurally distinct constraints (one per reading)?',
    'Apply the ε-invariance test: if measuring the constraint via practice frequency yields low ε (drop_reading) but measuring via conceptual availability yields high ε (contraction_reading), they are distinct constraints. The corpus decomposition into three files with linked network.affects_constraints is the resolution.',
    'If one constraint, classification is observer-relative and the framework fails its core principle. If three constraints, each gets stable ε, stable stakeholders, and the kernel is a family — the intended design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel decomposes into three ε-invariant constraints per DP-001.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (legal prosecution, professional discipline) structural or internalized by the identity-locked duelists?',
    'Post-prohibition trajectory: if duelists continue to feel bound by the code after legal barriers relax (e.g., in jurisdictions where dueling was decriminalized but practice did not revive), suppression is partially internalized.',
    'If internalized, effective suppression is higher than structural measure — the constraint travels with the agent. This would increase χ for the payer seat and strengthen the Snare classification from that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the drop_reading''s payer seat.').

omega_variable(
    natural_law_vs_constructed_legitimacy,
    'Is the dueling code''s structural legitimacy a genuine Mountain (emergent from honor systems as such) or a constructed constraint benefiting identifiable elites?',
    'Cross-cultural comparison: if all honor systems generate dueling-like legitimacy structures independently, Mountain claim holds. If the code is historically specific to early modern Europe and serves elite differentiation, it is constructed — FSM triggers.',
    'FSM reclassification to Tangled Rope if constructed. The drop_reading''s claimed Mountain would be a false summit maintained by beneficiary identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_legitimacy, empirical, 'FSM candidate omega: natural-law vs. constructed ambiguity for Mountain with beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_violence_legitimacy__drop_reading, theater_ratio, 1700, 0.25).
narrative_ontology:measurement(hono_tr_t1750, honor_violence_legitimacy__drop_reading, theater_ratio, 1750, 0.38).
narrative_ontology:measurement(hono_tr_t1800, honor_violence_legitimacy__drop_reading, theater_ratio, 1800, 0.52).
narrative_ontology:measurement(hono_tr_t1850, honor_violence_legitimacy__drop_reading, theater_ratio, 1850, 0.63).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__drop_reading, theater_ratio, 1900, 0.68).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__drop_reading, base_extractiveness, 1700, 0.35).
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__drop_reading, base_extractiveness, 1750, 0.28).
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__drop_reading, base_extractiveness, 1800, 0.22).
narrative_ontology:measurement(hono_be_t1850, honor_violence_legitimacy__drop_reading, base_extractiveness, 1850, 0.18).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__drop_reading, base_extractiveness, 1900, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__drop_reading, suppression_requirement, 1700, 0.15).
narrative_ontology:measurement(hono_su_t1750, honor_violence_legitimacy__drop_reading, suppression_requirement, 1750, 0.22).
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__drop_reading, suppression_requirement, 1800, 0.35).
narrative_ontology:measurement(hono_su_t1850, honor_violence_legitimacy__drop_reading, suppression_requirement, 1850, 0.48).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__drop_reading, suppression_requirement, 1900, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__drop_reading, 0.08).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% The honor_violence_legitimacy kernel decomposes into three readings per ε-invariance: drop_reading (legitimacy Mountain, practice drops), contraction_reading (legitimacy collapses), composite_reading (both mechanisms operate). This reading emphasizes external costs as the driver; contraction_reading emphasizes conceptual redefinition; composite_reading claims overdetermination. All three share the kernel_id and are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
