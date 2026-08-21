% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation to Admit Refugees (Reading)
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint represents the reading of border legitimacy that
 *   emphasizes a state's humanitarian obligation to admit those fleeing
 *   persecution or disaster, while maintaining the right to exclude general
 *   economic migrants. It is a core tenet of international refugee law. The
 *   distinction creates a bifurcated victim set: those who qualify for
 *   protection (beneficiaries of the constraint) and those who do not
 *   (victims of its exclusion). The constraint is claimed as a 'tangled_rope'
 *   because it genuinely coordinates international protection while
 *   simultaneously extracting from those who do not meet its criteria through
 *   active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.45).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.7).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation to Admit Refugees (Reading)").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, '54d366e7-72b8-41d1-97d2-6a774484d99e').
narrative_ontology:cs_kernel_codification('54d366e7-72b8-41d1-97d2-6a774484d99e', formalized).
narrative_ontology:cs_authority_grounding('54d366e7-72b8-41d1-97d2-6a774484d99e', lineage).
narrative_ontology:cs_interpretation_layer_present('54d366e7-72b8-41d1-97d2-6a774484d99e').
narrative_ontology:cs_reading_relation('54d366e7-72b8-41d1-97d2-6a774484d99e', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('54d366e7-72b8-41d1-97d2-6a774484d99e', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_axiom('54d366e7-72b8-41d1-97d2-6a774484d99e', foundational, non_refoulement_principle).
narrative_ontology:cs_axiom_status(non_refoulement_principle, holdable).
narrative_ontology:cs_axiom_grounding('54d366e7-72b8-41d1-97d2-6a774484d99e', non_refoulement_principle, deontological).
narrative_ontology:cs_axiom('54d366e7-72b8-41d1-97d2-6a774484d99e', foundational, distinction_between_forced_and_voluntary_migration).
narrative_ontology:cs_axiom_status(distinction_between_forced_and_voluntary_migration, holdable).
narrative_ontology:cs_axiom_grounding('54d366e7-72b8-41d1-97d2-6a774484d99e', distinction_between_forced_and_voluntary_migration, conventional).
narrative_ontology:cs_reference_frame('54d366e7-72b8-41d1-97d2-6a774484d99e', post_wwii_refugee_convention_framework).
narrative_ontology:cs_drift_state('54d366e7-72b8-41d1-97d2-6a774484d99e', contemporary_global_migration_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('54d366e7-72b8-41d1-97d2-6a774484d99e', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, receiving_states_security).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, international_humanitarian_organizations).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_with_weak_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, refugees_and_asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States define and enforce criteria for admission, balancing humanitarian concerns with national security and economic interests. They benefit from perceived legitimacy and control over borders, but bear costs of processing and integration.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, receiving_states_security, agenda_setter,
    institutional, generational, constrained, national).

% Individuals fleeing persecution or disaster are granted protection and a pathway to legal residence, escaping immediate threats. Their lives depend on this distinction being upheld.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, refugees_and_asylum_seekers, beneficiary,
    powerless, immediate, trapped, global).

% Individuals seeking better economic opportunities are largely excluded by this framework, facing legal barriers, deportation, and often dangerous irregular migration routes. They bear the cost of exclusion.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, biographical, constrained, global).

% These organizations advocate for the rights of refugees and provide aid, operating within the framework of international law that this reading upholds. They benefit from the clear legal basis for their work.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, international_humanitarian_organizations, beneficiary,
    organized, generational, mobile, global).

% Monitor state compliance with international obligations, critique restrictive interpretations, and advocate for broader protections. They analyze the constraint's operation and its impact on vulnerable populations.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, human_rights_advocates, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to protect vulnerable populations fleeing persecution or disaster, establishing a shared legal and moral framework for state action and burden-sharing.
% TRANSFER_FUNCTION: Transfers the burden of protection and integration from individuals and crisis-affected regions to receiving states, while transferring the right to reside and seek safety to those meeting specific criteria.
% ABSENT_VOICES: Those advocating for a universal right to freedom of movement, who would challenge the legitimacy of any border restriction not based on direct harm, are largely excluded from the policy-making conversation that defines these distinctions.
% DISAPPEARANCE_RATIONALE: If the distinction between refugees and economic migrants vanished, states would either face an unmanageable influx or universally close borders, leading to a collapse of the international protection regime and a humanitarian crisis for those genuinely fleeing persecution.
% FOUNDING_PROBLEM: The post-WWII need to prevent statelessness and provide sanctuary for those fleeing persecution, recognizing that some individuals have no safe home to return to.
% FOUNDING_PROBLEM_CORROBORATION: International treaties (e.g., 1951 Refugee Convention), UN agencies (UNHCR), and numerous state laws corroborate the ongoing need for this framework, citing persistent conflicts and disasters globally.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).
:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while it provides vital protection, it also imposes significant costs on those deemed 'economic migrants' by denying them legal entry and often forcing them into precarious situations. Suppression is high (0.7) due to the active enforcement of border controls and legal distinctions. Theater ratio is low (0.2) as the humanitarian function is largely genuine, though some enforcement efforts may be performative to deter all irregular migration. The increasing extractiveness and suppression over time reflect the hardening of borders and more restrictive interpretations in response to rising global migration pressures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of refugees, this constraint is a lifeline (low extraction, high benefit). From the perspective of economic migrants, it is a barrier (high extraction, no benefit). Receiving states experience it as a complex coordination problem with significant costs and benefits. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving states are agenda-setters, balancing obligations with national interests. Refugees and asylum seekers are direct beneficiaries, gaining protection. Economic migrants are payers, bearing the cost of exclusion. International humanitarian organizations benefit from the legal framework that enables their work. Human rights advocates observe and critique the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    refugee_definition_ambiguity,
    'Is the distinction between ''refugee'' (fleeing persecution/disaster) and ''economic migrant'' (seeking economic opportunity) empirically clear or conceptually ambiguous in practice?',
    'Empirical analysis of mixed migration flows and individual motivations; legal challenges to narrow interpretations of ''persecution'' or ''disaster''.',
    'If ambiguous, the constraint''s effective extractiveness on ''economic migrants'' is higher, as many genuinely vulnerable individuals may be misclassified. If clear, the distinction holds its moral and legal force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_definition_ambiguity, empirical, 'Ambiguity in the core distinction driving the constraint''s application.').

omega_variable(
    state_capacity_vs_obligation,
    'To what extent does a state''s capacity to admit and integrate refugees legitimately limit its humanitarian obligation?',
    'International legal precedent on ''non-refoulement'' and ''burden-sharing''; economic and social impact studies of refugee integration in various contexts.',
    'If capacity is a strong limiting factor, the constraint''s ''tangled_rope'' nature leans more towards coordination (managing finite resources). If obligation is near-absolute, any capacity-based exclusion becomes more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_vs_obligation, conceptual, 'The interplay between state capacity and the scope of humanitarian obligation.').

omega_variable(
    mandatrophy_of_distinction,
    'Has the original problem of providing sanctuary for those fleeing persecution been superseded by a broader global migration challenge, rendering the strict refugee/economic migrant distinction an outdated tool for managing human mobility?',
    'Analysis of global migration patterns, climate change displacement, and economic disparities; international policy debates on new legal categories for migrants.',
    'If the distinction is mandatrohpic, the constraint functions more as a ''snare'' for a large class of vulnerable people, rather than a ''tangled_rope'' with a genuine coordination function. If still live, the distinction remains a necessary, albeit imperfect, tool.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_of_distinction, conceptual, 'Whether the founding problem for the refugee distinction is still the primary driver of migration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1951, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(bord_tr_t1970, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(bord_tr_t1990, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(bord_tr_t2010, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(bord_tr_t2024, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(bord_be_t1951, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1951, 0.3).
narrative_ontology:measurement(bord_be_t1970, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(bord_be_t1990, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(bord_be_t2010, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1951, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1951, 0.5).
narrative_ontology:measurement(bord_su_t1970, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(bord_su_t1990, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(bord_su_t2010, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, international_border_regimes).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, national_immigration_laws).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'border_legitimacy' kernel. It focuses on the humanitarian obligation, distinct from the 'sovereignty_reading' (state's right to exclude) and 'freedom_of_movement_reading' (borders as illegitimate restrictions). All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
