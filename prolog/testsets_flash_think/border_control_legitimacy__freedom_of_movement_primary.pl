% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Freedom of Movement as Primary Right (Border Control Legitimacy Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'freedom_of_movement_primary'
 *   reading of the 'border_control_legitimacy' kernel. It asserts that
 *   freedom of movement is a fundamental human right and that state
 *   territorial sovereignty does not inherently grant authority for border
 *   closure. From this perspective, state border control, when used for
 *   exclusion, is a coercive mechanism that extracts from those denied entry,
 *   rather than a legitimate coordination function. The metrics reflect this
 *   reading's view of the constraint as a Snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.85).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.9).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, snare).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Freedom of Movement as Primary Right (Border Control Legitimacy Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, '3a098969-edce-4432-a0b5-cc7ee2a94f0d').
narrative_ontology:cs_kernel_codification('3a098969-edce-4432-a0b5-cc7ee2a94f0d', formalized).
narrative_ontology:cs_authority_grounding('3a098969-edce-4432-a0b5-cc7ee2a94f0d', extraction).
narrative_ontology:cs_interpretation_layer_present('3a098969-edce-4432-a0b5-cc7ee2a94f0d').
narrative_ontology:cs_reading_relation('3a098969-edce-4432-a0b5-cc7ee2a94f0d', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('3a098969-edce-4432-a0b5-cc7ee2a94f0d', border_control_legitimacy__jurisdictional_sovereignty, influences).
narrative_ontology:cs_axiom('3a098969-edce-4432-a0b5-cc7ee2a94f0d', foundational, freedom_of_movement_is_fundamental_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('3a098969-edce-4432-a0b5-cc7ee2a94f0d', freedom_of_movement_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('3a098969-edce-4432-a0b5-cc7ee2a94f0d', foundational, state_sovereignty_is_limited_by_human_rights).
narrative_ontology:cs_axiom_status(state_sovereignty_is_limited_by_human_rights, holdable).
narrative_ontology:cs_axiom_grounding('3a098969-edce-4432-a0b5-cc7ee2a94f0d', state_sovereignty_is_limited_by_human_rights, deontological).
narrative_ontology:cs_reference_frame('3a098969-edce-4432-a0b5-cc7ee2a94f0d', universal_human_rights_framework).
narrative_ontology:cs_drift_state('3a098969-edce-4432-a0b5-cc7ee2a94f0d', contemporary_migration_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3a098969-edce-4432-a0b5-cc7ee2a94f0d', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, state_governments).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, citizens_of_receiving_states).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_agencies).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, migrants_and_asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, displaced_citizens_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Denied entry to desired territories, facing physical danger, economic exploitation, and prolonged displacement. Their fundamental right to movement is directly suppressed.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, migrants_and_asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Citizens of states with limited economic opportunities, seeking to exercise their right to work and live abroad, but denied entry based on nationality or origin. Their mobility is constrained by territorial borders.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, displaced_citizens_workers, payer,
    powerless, biographical, identity_locked, global).

% Claiming absolute territorial sovereignty and the right to control borders, they enforce exclusion policies. They benefit from perceived national security, control over labor markets, and political stability, often at the expense of human rights.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from perceived security, cultural homogeneity, and control over public resources. They often support restrictive border policies, though some also bear indirect costs of reduced labor supply or moral injury.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, citizens_of_receiving_states, beneficiary,
    organized, biographical, mobile, national).

% Tasked with physically enforcing border closures, they receive significant funding and exercise substantial power. Their existence and mandate are directly tied to the state's claim of border closure authority.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% Actively challenge state border policies, arguing for the primacy of freedom of movement as a human right. They document abuses, lobby international bodies, and advocate for policy changes.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% Analyze and debate the legal and philosophical foundations of state sovereignty versus human rights, often critiquing the traditional interpretation of absolute border control authority.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint *claims* to coordinate national security, resource allocation, and cultural preservation by controlling who enters a territory. However, this reading views these as pretexts for exclusion.
% TRANSFER_FUNCTION: Transfers perceived security, resource control, and political stability to receiving states and their citizens, by denying entry and mobility to non-citizens. This imposes immense costs of displacement, danger, economic exclusion, and human rights violations on migrants and asylum seekers.
% ABSENT_VOICES: Migrants and asylum seekers themselves are largely excluded from policy-making, as are those who would benefit from more open borders (e.g., industries with labor shortages, families seeking reunification).
% DISAPPEARANCE_RATIONALE: If border closure authority vanished overnight, global migration patterns would fundamentally shift, labor markets would rebalance, and the concept of national citizenship would be profoundly altered. This would lead to a massive reorganization of political, economic, and social structures worldwide.
% FOUNDING_PROBLEM: The perceived need for states to control their territory, manage populations, and ensure national security, often arising from historical conflicts, resource scarcity, or cultural preservation concerns.
% FOUNDING_PROBLEM_CORROBORATION: State governments and many citizens corroborate the founding problem as live and essential for national stability. However, international human rights bodies and migration scholars argue that the problem is often exaggerated or used as a pretext for exclusion, with corroboration coming from analyses of economic benefits of migration and the human cost of closure.
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__freedom_of_movement_primary, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.85) because the denial of a fundamental right imposes severe costs on individuals, including loss of life, economic opportunity, and family reunification. Suppression is very high (0.90) due to the physical and legal barriers, and the active enforcement by state apparatuses, which effectively trap individuals. Theater ratio is low (0.10) because the enforcement of border closure is a real, material activity, not primarily performative. Accessibility collapse is near total (0.95) for those targeted by exclusion, as legal and physical alternatives are almost entirely removed. Resistance is high (0.70) from migrants, advocates, and some international bodies, reflecting the ongoing contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments and many citizens, border control is a legitimate exercise of sovereignty and a necessary coordination function for national security and resource management. From the perspective of migrants, human rights advocates, and this reading, it is a coercive Snare that violates fundamental rights and extracts immense costs from vulnerable populations.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and their border enforcement agencies are the primary beneficiaries, gaining power, resources, and perceived security. Citizens of receiving states also benefit from perceived resource control and stability. Migrants, asylum seekers, and displaced citizens/workers are the clear targets, bearing the full costs of exclusion and rights denial. The engine will compute high effective extraction for these target groups.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading fundamentally challenges the legitimacy of the 'founding problem' as a justification for border closure. It argues that the mandate for border control has either atrophied (if the original security threats are exaggerated) or was never legitimate in the first place when it infringes on fundamental human rights. The classification as a Snare prevents mislabeling this as a legitimate coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_rights_ambiguity,
    'Is state territorial sovereignty an absolute right, or is it inherently limited by international human rights obligations, particularly the freedom of movement?',
    'International legal precedent from high courts or UN bodies explicitly adjudicating the hierarchy of state sovereignty versus human rights in border control cases.',
    'If human rights are deemed primary, the legitimacy of current border closure practices would collapse, reclassifying the constraint as a clear Snare. If sovereignty is absolute, the constraint might be re-framed as a Mountain or Tangled Rope from a different reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_rights_ambiguity, conceptual, 'The fundamental conceptual conflict between state sovereignty and individual human rights.').

omega_variable(
    security_threat_assessment_legitimacy,
    'Are the security threats cited by states as justification for border closure genuinely existential and unmitigable by less restrictive means, or are they exaggerated/manageable through alternative policies?',
    'Independent, peer-reviewed security analyses and risk assessments that compare the efficacy and human cost of border closure versus alternative security and migration management strategies.',
    'If threats are exaggerated or manageable otherwise, the ''coordination'' function of border control is further delegitimized, strengthening the Snare classification. If threats are genuinely existential, it might lend some (contested) weight to a coordination argument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_threat_assessment_legitimacy, empirical, 'Empirical validity of security justifications for border closure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1990, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(bord_tr_t1995, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(bord_tr_t2000, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(bord_tr_t2005, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(bord_tr_t2010, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(bord_tr_t2015, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(bord_tr_t2020, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1990, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(bord_be_t1995, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(bord_be_t2000, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(bord_be_t2005, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2005, 0.82).
narrative_ontology:measurement(bord_be_t2010, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(bord_be_t2015, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2015, 0.84).
narrative_ontology:measurement(bord_be_t2020, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1990, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(bord_su_t1995, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1995, 0.82).
narrative_ontology:measurement(bord_su_t2000, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(bord_su_t2005, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2005, 0.87).
narrative_ontology:measurement(bord_su_t2010, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(bord_su_t2015, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2015, 0.89).
narrative_ontology:measurement(bord_su_t2020, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, national_citizenship_rights).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, international_refugee_law).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, global_labor_mobility).

% DUAL FORMULATION NOTE:
% This constraint is the 'freedom_of_movement_primary' reading of the 'border_control_legitimacy' kernel. It is structurally distinct from the 'sovereignty_primary' and 'jurisdictional_sovereignty' readings, which offer different interpretations of state authority and human rights at borders.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
