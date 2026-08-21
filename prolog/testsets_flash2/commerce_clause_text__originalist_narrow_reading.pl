% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Commerce Clause: Originalist Narrow Reading
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint represents an originalist narrow reading of the U.S.
 *   Constitution's Commerce Clause, limiting federal power to regulate
 *   commerce to only direct border-crossing trade and the instrumentalities
 *   of interstate movement. It emphasizes state sovereignty and local control
 *   over intrastate economic activity. This reading was dominant in early
 *   American jurisprudence but faced increasing challenges as the national
 *   economy integrated.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.25).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.4).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Commerce Clause: Originalist Narrow Reading").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, 'bf20854b-2173-44a0-a7a4-3db2e8db9611').
narrative_ontology:cs_kernel_codification('bf20854b-2173-44a0-a7a4-3db2e8db9611', fixed_text).
narrative_ontology:cs_authority_grounding('bf20854b-2173-44a0-a7a4-3db2e8db9611', lineage).
narrative_ontology:cs_interpretation_layer_present('bf20854b-2173-44a0-a7a4-3db2e8db9611').
narrative_ontology:cs_reading_relation('bf20854b-2173-44a0-a7a4-3db2e8db9611', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('bf20854b-2173-44a0-a7a4-3db2e8db9611', commerce_clause_text__substantial_effects_limited_reading, coexists_with).
narrative_ontology:cs_axiom('bf20854b-2173-44a0-a7a4-3db2e8db9611', foundational, original_public_meaning_supremacy).
narrative_ontology:cs_axiom_status(original_public_meaning_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('bf20854b-2173-44a0-a7a4-3db2e8db9611', original_public_meaning_supremacy, deontological).
narrative_ontology:cs_axiom('bf20854b-2173-44a0-a7a4-3db2e8db9611', foundational, intrastate_commerce_is_local).
narrative_ontology:cs_axiom_status(intrastate_commerce_is_local, holdable).
narrative_ontology:cs_axiom_grounding('bf20854b-2173-44a0-a7a4-3db2e8db9611', intrastate_commerce_is_local, conventional).
narrative_ontology:cs_reference_frame('bf20854b-2173-44a0-a7a4-3db2e8db9611', founding_era_jurisprudence).
narrative_ontology:cs_drift_state('bf20854b-2173-44a0-a7a4-3db2e8db9611', early_20th_century_industrialization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bf20854b-2173-44a0-a7a4-3db2e8db9611', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, uniform_national_standards).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, externality_management).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, businesses_operating_nationally).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, environmental_advocates).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, states_rights_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, limited_federal_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain broad police powers over intrastate economic activity, free from federal interference, allowing for diverse state-level policy choices and regulatory environments. They benefit from the preservation of their sovereignty.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Advocate for a strict interpretation of federal power, seeing this reading as essential to preventing an overreaching national government and preserving individual liberties and local control. They benefit ideologically and politically from this interpretation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates, beneficiary,
    organized, generational, mobile, national).

% Its power to regulate commerce is strictly limited to direct border-crossing trade and instrumentalities. It must justify any regulation by demonstrating a direct connection to interstate movement, which constrains its ability to address national economic issues comprehensively.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% Face a patchwork of state regulations rather than a single federal standard for intrastate activities, increasing compliance costs and complexity. They bear the burden of navigating diverse legal frameworks.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, businesses_operating_nationally, payer,
    powerful, biographical, constrained, national).

% Struggle to implement uniform national standards for pollution or resource management when impacts are primarily intrastate, even if they have aggregate interstate effects. They bear the cost of fragmented regulatory authority.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, environmental_advocates, payer,
    organized, generational, constrained, national).

% Analyze the Commerce Clause through the lens of its original public meaning at the time of ratification, seeking to apply the framers' intent. They provide intellectual grounding for this narrow reading.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, constitutional_scholars_originalist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear division of regulatory authority between federal and state governments, preventing federal overreach into purely local matters and preserving state autonomy in areas not directly involving interstate trade.
% TRANSFER_FUNCTION: Transfers regulatory power over intrastate economic activity from the federal government to state governments, and from national uniformity to state-level diversity. It also transfers the burden of managing interstate externalities to individual states or leaves them unaddressed.
% ABSENT_VOICES: Advocates for a strong federal role in addressing national economic and social problems (e.g., civil rights, environmental protection, labor standards) are marginalized, as their preferred solutions often require a broader interpretation of federal commerce power. They would argue for the necessity of national solutions to national problems.
% DISAPPEARANCE_RATIONALE: If this narrow reading vanished, the federal government would likely assert broader authority over economic activities, leading to more uniform national regulations, potentially reducing compliance costs for national businesses, but also diminishing state sovereignty and policy diversity. The balance of power in the federal system would fundamentally shift.
% FOUNDING_PROBLEM: The Articles of Confederation failed to provide a strong central government capable of regulating interstate trade, leading to economic balkanization and disputes among states. The Commerce Clause was intended to grant Congress power to prevent these issues.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and states' rights advocates attest that the problem of federal overreach remains live, and that a narrow reading is essential to prevent the federal government from becoming a general police power. Critics, including many economists and legal scholars, argue that the original problem of balkanization has been replaced by the problem of unaddressed interstate externalities, which a narrow reading exacerbates.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__originalist_narrow_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).
:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because this reading primarily limits federal power rather than actively extracting from parties, though it imposes costs on businesses and limits federal capacity to address externalities. Suppression (0.4) reflects the active judicial enforcement required to maintain this boundary against federal legislative attempts. Theater ratio is low (0.1) as the interpretation was genuinely applied during this period, not merely performed. Accessibility collapse is high (0.7) because the legal interpretation significantly restricts alternative federal regulatory approaches. Resistance (0.3) is moderate, reflecting ongoing debates and occasional challenges to federal authority, but not widespread defiance.
 *
 * PERSPECTIVAL GAP:
 *   State governments and anti-federal consolidation advocates experience this as a beneficial constraint, preserving their power and ideological commitments. Businesses operating nationally and environmental advocates, however, experience it as a costly limitation, forcing them to navigate fragmented regulations or leaving national problems unaddressed. The federal government, as the agenda-setter, is constrained by this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and anti-federal consolidation advocates are beneficiaries (d near 0.0) as this reading protects their interests. The federal government, while an agenda-setter, is also a target of this constraint (d near 0.6) as its power is limited. Businesses operating nationally and environmental advocates are payers/victims (d near 0.8-0.9) due to the costs and limitations imposed on them.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading, while rooted in a genuine founding problem (preventing state balkanization), faces mandatrophy analysis when confronted with modern economic realities and national problems. Its persistence, despite the emergence of national markets and interstate externalities, raises questions about whether its original mandate is still 'live' or if it has become a mechanism for preserving state power at the expense of national coordination. The engine's classification will highlight whether it functions as a genuine 'rope' for federalism or a 'snare' for national problem-solving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_modern_economy,
    'Does the original public meaning of ''commerce among the several states'' adequately address the complexities of a modern, integrated national economy with pervasive interstate effects?',
    'Historical analysis of economic conditions and legal interpretations at the time of ratification compared with contemporary economic structures and judicial precedents. Legal scholarship and empirical economic studies on the scope of interstate commerce.',
    'If the original meaning is deemed insufficient, it strengthens arguments for more expansive readings of the Commerce Clause, potentially reclassifying this constraint from a ''rope'' to a ''piton'' or ''snare'' that hinders necessary federal action. If deemed sufficient, it reinforces the ''rope'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_modern_economy, conceptual, 'The applicability of 18th-century legal definitions to 21st-century economic realities.').

omega_variable(
    federalism_balance_point,
    'Is the balance of power between federal and state governments, as defined by this narrow reading, optimal for addressing contemporary challenges that often transcend state borders?',
    'Comparative analysis of federal systems in other countries, empirical studies on the effectiveness of state-level versus federal-level regulation for issues like environmental protection or public health, and policy debates on the appropriate scope of federal action.',
    'If the balance is found to be suboptimal, it would suggest that the constraint, while preserving state autonomy, imposes significant costs on collective action, pushing its classification towards ''tangled_rope'' or ''snare''. If optimal, it reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_balance_point, preference, 'The normative question of the ideal federal-state power distribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 1789, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1789, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(comm_tr_t1824, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1824, 0.07).
narrative_ontology:measurement(comm_tr_t1850, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1850, 0.09).
narrative_ontology:measurement(comm_tr_t1890, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1890, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1789, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1789, 0.2).
narrative_ontology:measurement(comm_be_t1824, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1824, 0.22).
narrative_ontology:measurement(comm_be_t1850, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1850, 0.23).
narrative_ontology:measurement(comm_be_t1890, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1890, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1789, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1789, 0.35).
narrative_ontology:measurement(comm_su_t1824, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1824, 0.37).
narrative_ontology:measurement(comm_su_t1850, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1850, 0.38).
narrative_ontology:measurement(comm_su_t1890, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1890, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__substantial_effects_limited_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, federal_environmental_regulation).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, national_labor_standards).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
