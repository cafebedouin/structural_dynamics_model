% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: Eternal Marriage Covenant (Immutable Commandment Reading)
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'immutable commandment' reading of
 *   the eternal marriage covenant, specifically D&C 132's establishment of
 *   polygamy as an eternal, immutable divine law required for exaltation.
 *   From this perspective, the commandment itself is unchangeable, and any
 *   deviation from its practice, especially under external pressure, is a
 *   compromise of eternal salvation. The increasing extractiveness and
 *   suppression over the interval reflect the escalating federal pressure
 *   against polygamy, which transforms adherence into a 'martyrdom
 *   constraint' for believers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.85).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.9).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, snare).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "Eternal Marriage Covenant (Immutable Commandment Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, 'e0d54be6-8944-43a7-a79f-ceee18b593e1').
narrative_ontology:cs_kernel_codification('e0d54be6-8944-43a7-a79f-ceee18b593e1', fixed_text).
narrative_ontology:cs_authority_grounding('e0d54be6-8944-43a7-a79f-ceee18b593e1', lineage).
narrative_ontology:cs_interpretation_layer_present('e0d54be6-8944-43a7-a79f-ceee18b593e1').
narrative_ontology:cs_reading_relation('e0d54be6-8944-43a7-a79f-ceee18b593e1', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('e0d54be6-8944-43a7-a79f-ceee18b593e1', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('e0d54be6-8944-43a7-a79f-ceee18b593e1', foundational, polygamy_eternal_exaltation_requirement).
narrative_ontology:cs_axiom_status(polygamy_eternal_exaltation_requirement, holdable).
narrative_ontology:cs_axiom_grounding('e0d54be6-8944-43a7-a79f-ceee18b593e1', polygamy_eternal_exaltation_requirement, theological).
narrative_ontology:cs_axiom('e0d54be6-8944-43a7-a79f-ceee18b593e1', foundational, divine_law_immutable).
narrative_ontology:cs_axiom_status(divine_law_immutable, holdable).
narrative_ontology:cs_axiom_grounding('e0d54be6-8944-43a7-a79f-ceee18b593e1', divine_law_immutable, deontological).
narrative_ontology:cs_reference_frame('e0d54be6-8944-43a7-a79f-ceee18b593e1', eternal_divine_commandment).
narrative_ontology:cs_drift_state('e0d54be6-8944-43a7-a79f-ceee18b593e1', federal_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e0d54be6-8944-43a7-a79f-ceee18b593e1', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, ecclesiastical_leadership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, polygamous_adherents).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, women_in_polygamy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines, interprets, and enforces D&C 132 as an eternal, immutable divine law. Benefits from the authority and loyalty derived from being the sole arbiter of the path to exaltation. Faces internal pressure to maintain doctrinal consistency and external pressure from secular authorities.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, ecclesiastical_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Believe D&C 132 is a divine command essential for exaltation. Bear the severe costs of practicing polygamy, including legal penalties, social ostracization, and economic hardship, while facing internal pressure to comply for eternal salvation. Exit means apostasy and loss of eternal blessings.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, polygamous_adherents, payer,
    powerless, biographical, identity_locked, local).

% Are bound by the doctrine and social structure of polygamous families. Often face limited autonomy, economic dependency, and social isolation. Their situation is defined by the immutable command, with exit options severely constrained by belief, family ties, and lack of external support.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, women_in_polygamy, payer,
    powerless, biographical, trapped, local).

% Enforces secular laws against polygamy, viewing it as a criminal act. Its pressure directly creates the 'martyrdom constraint' for adherents of the immutable commandment reading, forcing a choice between divine law and civil law.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Observes the conflict between religious doctrine and civil law, generally condemning polygamy on grounds of human rights, gender equality, and social norms. Exerts social pressure against the practice.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, secular_society, observer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates family structure and social order according to divine revelation, ensuring adherence to eternal principles for the highest degree of exaltation and the continuation of family units into the eternities.
% TRANSFER_FUNCTION: Transfers absolute obedience, loyalty, and personal sacrifice from adherents to the ecclesiastical authority and the divine law, in exchange for the promise of eternal progression and family unity beyond this life.
% ABSENT_VOICES: Former members who left due to the doctrine, individuals excommunicated for non-compliance, and those who suffered abuse or coercion within polygamous structures. They would challenge the divine mandate and the immutability claim.
% DISAPPEARANCE_RATIONALE: If D&C 132 as an immutable command vanished, the entire theological foundation for eternal marriage and exaltation within this religious tradition would collapse. Family structures, ecclesiastical authority, and the path to salvation would be fundamentally re-evaluated, leading to a profound reorganization of belief and practice.
% FOUNDING_PROBLEM: To restore ancient biblical practices, fulfill a divine command for the 'fulness of the priesthood,' and provide the only path to the highest degree of exaltation and eternal increase.
% FOUNDING_PROBLEM_CORROBORATION: From the perspective of this reading, the problem is live, as exaltation remains the ultimate goal. Corroboration comes from within the ecclesiastical leadership and faithful adherents who believe in the literal interpretation of D&C 132. External corroboration for the 'live' status of the founding problem is absent; secular observers view it as a historical artifact or a means of control.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) and suppression (0.90) reflect the severe costs borne by adherents who believe in and attempt to practice this immutable command, especially under federal anti-polygamy laws. The 'required for exaltation' clause creates an existential trap for believers, making exit (apostasy) an unthinkable option. The low theater ratio (0.10) indicates the genuine and deeply held nature of the belief and its enforcement, rather than mere performance. The claimed type is 'snare' because the coordination story (exaltation) serves as cover for the immense extraction and suppression experienced by those caught in its grip, with no legitimate revision path from this reading's perspective.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ecclesiastical leadership and faithful adherents of this reading, the constraint is a divine imperative, a path to ultimate blessing. From the perspective of the federal government and secular society, it is a harmful practice requiring legal suppression. The engine's classification as a Snare highlights the coercive and extractive reality for those caught between these conflicting authorities, despite the internal framing as a divine path.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical leadership benefits from the authority derived from defining and enforcing this immutable path to exaltation. Polygamous adherents and women within these structures are the primary targets, bearing the direct costs of legal persecution, social ostracization, and limited autonomy. The federal government acts as an external agenda-setter, enforcing laws that directly suppress the practice, creating a conflict where compliance with divine law means apostasy from civil law.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_immutability_contestability,
    'Is the claim of D&C 132 as an ''eternal, immutable divine law'' empirically or conceptually contestable?',
    'Historical-critical analysis of scriptural interpretation, theological debate within the tradition, and comparative religious studies on the evolution of divine law claims.',
    'If contestable, the constraint''s ''naturalness'' (as divine law) is weakened, shifting it further from a Mountain toward a constructed Snare, and highlighting the role of human interpretation and enforcement in its persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_immutability_contestability, conceptual, 'Contestability of the ''immutable divine law'' claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal penalties, social ostracization) or internalized (belief in eternal consequences, identity fusion with the religious community)?',
    'Post-exit suppression trajectory: if individuals who leave the faith continue to experience significant internal distress or social isolation, it suggests a strong internalized component. Analysis of narratives from former adherents.',
    'If internalized suppression is dominant, the effective suppression is higher than external measures suggest, making exit even more difficult and amplifying the Snare-like qualities of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for adherents.').

omega_variable(
    exaltation_requirement_validity,
    'Is the ''required for exaltation'' claim a genuine theological necessity or an ecclesiastical enforcement mechanism?',
    'Theological review of alternative paths to exaltation within the broader religious tradition, and analysis of how this claim functions to secure obedience and loyalty.',
    'If primarily an enforcement mechanism, the coordination story is further exposed as cover for extraction, solidifying the Snare classification and highlighting the power dynamics within the ecclesiastical structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exaltation_requirement_validity, conceptual, 'Nature of the ''required for exaltation'' claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 1852, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1852, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1852, 0.1).
narrative_ontology:measurement(eter_tr_t1860, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1860, 0.1).
narrative_ontology:measurement(eter_tr_t1870, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1870, 0.1).
narrative_ontology:measurement(eter_tr_t1880, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1890, 0.1).

% Extraction over time
narrative_ontology:measurement(eter_be_t1852, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1852, 0.7).
narrative_ontology:measurement(eter_be_t1860, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1860, 0.75).
narrative_ontology:measurement(eter_be_t1870, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1870, 0.8).
narrative_ontology:measurement(eter_be_t1880, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1880, 0.85).
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1890, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1852, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1852, 0.75).
narrative_ontology:measurement(eter_su_t1860, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1860, 0.8).
narrative_ontology:measurement(eter_su_t1870, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1870, 0.85).
narrative_ontology:measurement(eter_su_t1880, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1880, 0.9).
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1890, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'eternal_marriage_covenant' kernel. Its ε value differs significantly from sibling readings due to its emphasis on immutability and the direct consequences for exaltation, which amplifies extraction and suppression for adherents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
