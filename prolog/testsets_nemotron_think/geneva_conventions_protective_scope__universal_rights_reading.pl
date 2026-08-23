% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Geneva Conventions Universal Protective Scope (Universal Rights Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint story captures the universal rights reading of the Geneva
 *   Conventions' protective scope. It asserts that Common Article 3, read
 *   together with international human rights law, creates a universal floor
 *   of protections applicable to all persons affected by armed conflict,
 *   irrespective of combatant status or conflict classification. This reading
 *   expands the victim set to include non-state actors and civilian
 *   populations, while raising the extractiveness on state military
 *   operations by restricting targeting, detention, and interrogation
 *   practices. The constraint operates as a tangled rope: it coordinates
 *   humanitarian protection across conflict types (genuine coordination
 *   function) while asymmetrically extracting operational flexibility from
 *   state military and intelligence agencies (asymmetric extraction).
 *   Enforcement depends on active monitoring by international courts, treaty
 *   bodies, and NGOs.
 *
 * KEY AGENTS:
 *   - state_military_forces: Primary target (institutional/constrained) — bears extraction through operational restrictions
 *   - civilian_populations: Primary beneficiary (organized/trapped) — receives protection floor
 *   - non_state_armed_groups: Beneficiary (organized/constrained) — gains protections but also obligations
 *   - international_courts_and_tribunals: Agenda setter (institutional/analytical) — interprets and enforces
 *   - state_intelligence_agencies: Payer (institutional/constrained) — bears extraction on interrogation/detention
 *   - human_rights_ngos: Observer (organized/mobile) — monitors and advocates
 *   - private_military_contractors: Excluded (moderate/mobile) — status ambiguous under the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.72).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.65).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Geneva Conventions Universal Protective Scope (Universal Rights Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, 'db9b4858-9d1f-4cf5-a333-e8050e12dbee').
narrative_ontology:cs_kernel_codification('db9b4858-9d1f-4cf5-a333-e8050e12dbee', fixed_text).
narrative_ontology:cs_authority_grounding('db9b4858-9d1f-4cf5-a333-e8050e12dbee', lineage).
narrative_ontology:cs_interpretation_layer_present('db9b4858-9d1f-4cf5-a333-e8050e12dbee').
narrative_ontology:cs_reading_relation('db9b4858-9d1f-4cf5-a333-e8050e12dbee', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('db9b4858-9d1f-4cf5-a333-e8050e12dbee', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('db9b4858-9d1f-4cf5-a333-e8050e12dbee', foundational, universal_humanitarian_floor).
narrative_ontology:cs_axiom_status(universal_humanitarian_floor, holdable).
narrative_ontology:cs_axiom_grounding('db9b4858-9d1f-4cf5-a333-e8050e12dbee', universal_humanitarian_floor, deontological).
narrative_ontology:cs_axiom('db9b4858-9d1f-4cf5-a333-e8050e12dbee', foundational, human_rights_law_applies_in_armed_conflict).
narrative_ontology:cs_axiom_status(human_rights_law_applies_in_armed_conflict, holdable).
narrative_ontology:cs_axiom_grounding('db9b4858-9d1f-4cf5-a333-e8050e12dbee', human_rights_law_applies_in_armed_conflict, conventional).
narrative_ontology:cs_reference_frame('db9b4858-9d1f-4cf5-a333-e8050e12dbee', common_article_3_minimum_standards).
narrative_ontology:cs_drift_state('db9b4858-9d1f-4cf5-a333-e8050e12dbee', post_icj_nuclear_weapons_opinion_1996, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('db9b4858-9d1f-4cf5-a333-e8050e12dbee', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, detainees).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, wounded_and_sick).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_forces).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_intelligence_agencies).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, common_article_3_universal_application).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, human_rights_law_applicability_in_armed_conflict).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, non_derogable_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State armed forces must extend Geneva protections to all persons in armed conflict, including non-state actors and civilians, restricting targeting, detention, and interrogation practices. Compliance requires legal review, training, and operational constraints. Non-compliance risks prosecution and reputational damage.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_forces, payer,
    institutional, biographical, constrained, global).

% Civilians in conflict zones receive expanded protections under Common Article 3 and human rights law, including humane treatment, fair trial guarantees, and protection from violence. They cannot exit the conflict zone easily and rely on the constraint for survival.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations, beneficiary,
    organized, biographical, trapped, global).

% Non-state armed groups gain legal protections (humane treatment if captured) but also incur obligations under Common Article 3. They are bound by the constraint without having consented to the treaty.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, beneficiary,
    organized, biographical, constrained, global).

% Persons detained in armed conflicts receive judicial guarantees, prohibition of torture, and humane treatment standards regardless of their status. They have no exit from detention and depend entirely on the constraint for protection.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, detainees, beneficiary,
    powerless, immediate, trapped, global).

% Intelligence agencies face restrictions on interrogation methods, detention practices, and targeted killings under the universal reading. They must adapt operational procedures to comply with human rights law standards.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_intelligence_agencies, payer,
    institutional, biographical, constrained, global).

% International courts (ICC, ICJ, regional human rights courts) adjudicate violations of the universal protective scope, creating jurisprudence that expands the constraint's reach. They do not bear operational costs but set the interpretive agenda.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_courts_and_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% NGOs document violations, advocate for enforcement, and provide legal analysis supporting the universal reading. They are not direct beneficiaries but their institutional mission aligns with the constraint.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, human_rights_ngos, observer,
    organized, generational, mobile, global).

% Private military contractors operate in armed conflicts but their status under the universal reading is ambiguous; they would claim combatant immunity but are often treated as civilians. They are excluded from clear protective status.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, private_military_contractors, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal humanitarian floor protecting all persons in armed conflict from inhumane treatment, regardless of the conflict's classification or the person's status.
% TRANSFER_FUNCTION: Transfers operational flexibility and freedom from legal constraint from state military and intelligence agencies to protected persons (civilians, detainees, non-state actors) in the form of legal protections and procedural guarantees.
% ABSENT_VOICES: Private military contractors, corporate actors in conflict zones, and states that reject human rights law applicability in armed conflict are structurally excluded from the interpretive community that defines the universal floor.
% DISAPPEARANCE_RATIONALE: Without the universal reading, states would apply Geneva protections only to privileged combatants, denying protections to non-state actors and civilians in non-international conflicts, fundamentally altering the legal framework of humanitarian protection.
% FOUNDING_PROBLEM: The original Geneva Conventions were designed for inter-state wars between regular armies; the founding problem was the lack of legal protections in non-international armed conflicts and for persons not meeting Article 4 combatant criteria.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's 2016 commentary on Common Article 3, the International Court of Justice's Nuclear Weapons advisory opinion, and UN Human Rights Committee general comments corroborate that the founding problem (protection gaps in non-international conflicts) remains live.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the reading substantially restricts state military and intelligence operations that would otherwise be permissible under narrower interpretations. Suppression (0.65) reflects the legal consequences for violations (prosecution, reputational costs) but acknowledges enforcement gaps. Theater ratio (0.42) indicates significant performative compliance: states ratify treaties and issue manuals but often violate in practice. Accessibility collapse (0.55) is moderate: the legal framework closes off certain alternatives (e.g., denying protections to 'unprivileged belligerents') but violations persist. Resistance (0.68) is high from states seeking operational flexibility. The claimed type is tangled_rope because the constraint both coordinates a universal humanitarian floor and extracts from state operational freedom, requiring active enforcement by international institutions.
 *
 * PERSPECTIVAL GAP:
 *   From the state military seat, the constraint appears as a snare-like extraction of operational autonomy imposed by external legal interpreters. From the civilian/detainee seat, it appears as a mountain-like protection floor that should be absolute. From the international court seat, it appears as a rope coordinating humanitarian standards across conflict types. The engine computes these divergent seat classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State military forces and intelligence agencies are declared victims (payers) — they bear the costs of compliance and lose operational flexibility, with constrained exit (cannot easily withdraw from legal obligations). Civilian populations, detainees, wounded/sick, and non-state armed groups are beneficiaries — they receive protections without bearing enforcement costs. International courts are agenda_setters — they administer the interpretive framework. Private military contractors are excluded — their status is unresolved. The directionality derivation from beneficiary/victim + exit options yields high d for state actors (targets) and low d for protected persons (beneficiaries).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protection gaps in non-international conflicts) remains live, so the constraint has not suffered mandatrophy. However, the expanding scope of the reading (from Common Article 3 to full human rights law applicability) has increased extractiveness over time, creating tension between the coordination function (universal floor) and the extraction function (state operational restrictions). This tension is the tangled rope dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the universal rights reading relate structurally to the other readings of the Geneva protective scope kernel?',
    'Track jurisprudential uptake: if international courts consistently apply the universal floor, the reading gains structural dominance; if states successfully resist in practice, the reading remains aspirational.',
    'If the universal reading becomes the dominant interpretive framework, the constraint''s extractiveness on state operations increases and its classification may shift toward snare for state actors; if it remains one of several competing readings, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Structural relationship between this reading and sibling readings of the same kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal consequences, enforcement mechanisms) or internalized (states genuinely believing the universal floor is binding)?',
    'Post-compliance trajectory analysis: if states comply only when monitored, suppression is structural; if compliance persists without monitoring, internalization has occurred.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them, reducing the need for active enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in state compliance').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the universal humanitarian floor a genuine natural law (mountain) or a constructed legal interpretation that benefits identifiable agents?',
    'Historical analysis of the reading''s emergence: if it tracks pre-existing moral intuitions universally recognized, it leans mountain; if it correlates with specific institutional advocacy (ICRC, human rights NGOs), it leans constructed.',
    'If constructed, the constraint is a false summit candidate (mountain claim with beneficiaries) and should be reclassified as tangled_rope or snare; if natural, the mountain classification would be warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether the universal floor reflects natural law or constructed legal interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1970, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(gene_tr_t1980, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(gene_tr_t2000, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2000, 0.36).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(gene_tr_t2020, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(gene_be_t1970, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(gene_be_t1980, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(gene_be_t2000, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(gene_be_t2020, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2020, 0.71).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1970, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(gene_su_t1980, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(gene_su_t2000, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(gene_su_t2020, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2020, 0.63).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__universal_rights_reading, 0.1).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, icc_jurisdiction).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, human_rights_law_in_armed_conflict).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, non_international_armed_conflict_regulation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the geneva_conventions_protective_scope kernel. The state_centric_reading and hybrid_proportionality_reading are sibling constraints. The universal reading forecloses the state_centric reading within a single framework but coexists with the hybrid reading. All three form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__universal_rights_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
