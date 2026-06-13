% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause: Substantial Effects Doctrine (Limited Reading)
 *   domain: constitutional_law/federalism/commerce_regulation
 *
 * SUMMARY:
 *   This constraint represents a specific, limited reading of the Commerce
 *   Clause, particularly as articulated in cases like United States v. Lopez
 *   (1995) and United States v. Morrison (2000). It acknowledges federal
 *   power over intrastate economic activity that substantially affects
 *   interstate commerce but imposes limits, requiring a clear jurisdictional
 *   nexus and preventing federal regulation of non-economic activity under
 *   the guise of commerce power. This reading aims to preserve a sphere of
 *   state autonomy while allowing federal action on national economic issues.
 *
 * KEY AGENTS:
 *   - federal_legislature: Agenda setter (institutional/constrained)
 *   - federal_judiciary: Agenda setter (institutional/analytical)
 *   - national_economic_actors: Beneficiary (organized/mobile)
 *   - states_seeking_police_power_autonomy: Payer (institutional/constrained)
 *   - intrastate_non_economic_actors: Payer (moderate/constrained)
 *   - originalist_legal_scholars: Excluded (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.3).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.4).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause: Substantial Effects Doctrine (Limited Reading)").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional_law/federalism/commerce_regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, '32caa5ce-3108-4751-a468-cbb25af3f5d3').
narrative_ontology:cs_kernel_codification('32caa5ce-3108-4751-a468-cbb25af3f5d3', fixed_text).
narrative_ontology:cs_authority_grounding('32caa5ce-3108-4751-a468-cbb25af3f5d3', lineage).
narrative_ontology:cs_interpretation_layer_present('32caa5ce-3108-4751-a468-cbb25af3f5d3').
narrative_ontology:cs_reading_relation('32caa5ce-3108-4751-a468-cbb25af3f5d3', commerce_clause_text__expansive_federal_reading, influences).
narrative_ontology:cs_reading_relation('32caa5ce-3108-4751-a468-cbb25af3f5d3', commerce_clause_text__originalist_narrow_reading, influences).
narrative_ontology:cs_axiom('32caa5ce-3108-4751-a468-cbb25af3f5d3', foundational, economic_activity_nexus_required).
narrative_ontology:cs_axiom_status(economic_activity_nexus_required, holdable).
narrative_ontology:cs_axiom_grounding('32caa5ce-3108-4751-a468-cbb25af3f5d3', economic_activity_nexus_required, conventional).
narrative_ontology:cs_axiom('32caa5ce-3108-4751-a468-cbb25af3f5d3', foundational, non_pretextual_regulation_mandated).
narrative_ontology:cs_axiom_status(non_pretextual_regulation_mandated, holdable).
narrative_ontology:cs_axiom_grounding('32caa5ce-3108-4751-a468-cbb25af3f5d3', non_pretextual_regulation_mandated, conventional).
narrative_ontology:cs_reference_frame('32caa5ce-3108-4751-a468-cbb25af3f5d3', post_lopez_morrison_framework).
narrative_ontology:cs_drift_state('32caa5ce-3108-4751-a468-cbb25af3f5d3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('32caa5ce-3108-4751-a468-cbb25af3f5d3', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_legislature).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, national_economic_actors).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, states_seeking_police_power_autonomy).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, intrastate_non_economic_actors).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, federal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, limited_government_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks to regulate national economic problems, using the Commerce Clause as a primary grant of power. This reading allows regulation of genuinely economic intrastate activity with substantial effects, but constrains attempts to use it as a general police power.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Interprets the scope of the Commerce Clause, policing the boundary between economic and non-economic activity and ensuring a jurisdictional nexus. This reading requires them to scrutinize federal laws to prevent overreach.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Benefit from uniform federal regulation of national markets, reducing compliance costs and market fragmentation. This reading provides a stable framework for their operations, preventing arbitrary state-level interference in genuinely economic matters.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, national_economic_actors, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of federal preemption in areas deemed to have substantial effects on interstate commerce. While acknowledging federal authority over genuine economic matters, they resist federal encroachment on traditional state police powers (e.g., criminal law, family law, education).
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, states_seeking_police_power_autonomy, payer,
    institutional, generational, constrained, national).

% Are protected from federal regulation under this reading if their activities are genuinely non-economic and lack a direct, substantial effect on interstate commerce. However, they bear the cost of litigation to establish this distinction and remain vulnerable to federal overreach if the 'economic' definition expands.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, intrastate_non_economic_actors, payer,
    moderate, biographical, constrained, local).

% Would argue that even this limited substantial effects reading is an illegitimate expansion beyond the original public meaning of the Commerce Clause, advocating for a much narrower interpretation. Their arguments are considered in judicial opinions but do not typically prevail in this reading's application.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, originalist_legal_scholars, excluded,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates federal and state regulatory authority by defining a sphere where federal power over intrastate economic activity is legitimate due to its aggregate effects on interstate commerce, while reserving non-economic police powers to the states.
% TRANSFER_FUNCTION: Transfers regulatory authority over certain intrastate economic activities from states to the federal government, and transfers the burden of proof to the federal government to demonstrate a jurisdictional nexus and non-pretextual economic purpose for its regulation.
% ABSENT_VOICES: Those advocating for a purely localist or states' rights view of all intrastate activity, regardless of economic impact, are largely excluded from the prevailing legal discourse that accepts some form of substantial effects. Also, those advocating for a purely expansive federal power are constrained by this reading's limits.
% DISAPPEARANCE_RATIONALE: If this specific reading of the Commerce Clause vanished, the balance of power between federal and state governments would be fundamentally altered. Either federal power would become virtually unlimited (if the expansive reading prevailed without limits) or severely curtailed (if the originalist reading became dominant), leading to massive shifts in regulatory authority, economic stability, and legal precedent.
% FOUNDING_PROBLEM: The original Commerce Clause was intended to prevent states from erecting trade barriers and to allow the federal government to manage a unified national economy, but its precise scope regarding intrastate activity was ambiguous, leading to disputes over federal power.
% FOUNDING_PROBLEM_CORROBORATION: The problem of defining the boundary between federal and state power in a complex national economy remains live, as evidenced by ongoing Supreme Court cases and legislative debates. Legal scholars across the ideological spectrum, state attorneys general, and federal agencies all attest to the persistent challenge of this jurisdictional boundary, corroborating the problem's ongoing relevance.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).
:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.3) as it transfers some state power to the federal level but also limits federal overreach, creating a more balanced distribution than an expansive reading. Suppression is moderate (0.4) as states and non-economic actors are constrained by federal preemption, but they retain avenues for legal challenge. Theater ratio is low (0.2) because the judicial scrutiny of federal laws is a genuine, functional check, not merely performative. The slight increase in extractiveness and suppression towards the end of the interval reflects ongoing tensions and occasional judicial shifts, but the core principles of this reading remain stable.
 *
 * PERSPECTIVAL GAP:
 *   The federal legislature and national economic actors generally experience this as a beneficial coordination mechanism, providing clarity and a stable regulatory environment. States and non-economic actors, however, experience it as a constraint that limits their autonomy, even if it prevents worse federal overreach. The federal judiciary's perspective is one of balancing competing claims and policing jurisdictional boundaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal legislature and national economic actors are beneficiaries (d closer to 0.0) as this reading grants federal power where needed for economic coordination. States and intrastate non-economic actors are payers (d closer to 1.0) as they lose some regulatory autonomy. The federal judiciary, while an agenda setter, also acts as a neutral arbiter, balancing these interests.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy by actively policing the boundaries of federal power. If the distinction between economic and non-economic activity were to collapse, or the jurisdictional nexus requirement became purely pretextual, the constraint would drift towards an 'expansive federal reading' (a Snare from the states' perspective), where the original mandate of limited federal power would be lost. The ongoing judicial enforcement prevents this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_vs_non_economic_distinction,
    'Is the distinction between ''economic'' and ''non-economic'' activity a stable, principled legal category, or is it inherently fluid and subject to judicial discretion?',
    'Longitudinal analysis of Supreme Court jurisprudence: if the distinction consistently shifts based on political composition or policy preferences rather than clear legal principles, it suggests fluidity.',
    'If fluid, the constraint''s limits are less robust, potentially allowing drift towards an ''expansive_federal_reading'' (Snare for states). If stable, the constraint effectively limits federal power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_vs_non_economic_distinction, conceptual, 'Ambiguity in the core definitional boundary of federal power.').

omega_variable(
    pretextual_regulation_detection,
    'How effectively can the federal judiciary detect and prevent ''pretextual'' federal regulation of non-economic activity disguised as commerce regulation?',
    'Empirical study of legislative intent and judicial outcomes: if federal laws consistently pass judicial review despite weak economic nexus claims, detection is ineffective.',
    'Ineffective detection means the constraint''s ''limited'' nature is largely theatrical, allowing federal power to expand (Snare for states). Effective detection maintains the constraint''s integrity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pretextual_regulation_detection, empirical, 'Effectiveness of judicial gatekeeping against federal overreach.').

omega_variable(
    reading_as_kernel_instantiation,
    'Is this ''substantial_effects_limited_reading'' a genuine, distinct instantiation of the Commerce Clause kernel, or is it merely a temporary phase in the oscillation between ''originalist_narrow_reading'' and ''expansive_federal_reading''?',
    'Analysis of legal scholarship and judicial opinions over several decades: if its core tenets persist and are actively defended as a distinct framework, it is a genuine reading. If it consistently collapses into one of the other two, it is a phase.',
    'If a distinct reading, it offers a stable, albeit contested, framework for federalism. If a phase, the underlying dynamic is one of continuous contestation between the other two readings, making this constraint''s stability illusory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_kernel_instantiation, conceptual, 'Whether this reading is a stable framework or a transient state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(comm_tr_t2015, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2005, 0.3).
narrative_ontology:measurement(comm_be_t2015, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(comm_su_t2015, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, federal_police_power_limits).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Commerce Clause text kernel. Each reading defines a different scope of federal power and has different beneficiaries/victims. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
