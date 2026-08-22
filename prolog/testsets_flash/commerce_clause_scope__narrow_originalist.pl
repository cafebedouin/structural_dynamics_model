% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Commerce Clause Scope (Narrow Originalist Reading)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint represents a narrow originalist reading of the U.S.
 *   Constitution's Commerce Clause, asserting that 'commerce' refers strictly
 *   to trade crossing state lines, and 'regulate' means to make regular or
 *   facilitate, not to restrict or prohibit. Federal power is thus limited to
 *   removing state-imposed barriers to interstate trade and ensuring uniform
 *   commercial rules. This reading emphasizes state sovereignty over
 *   intrastate economic activity and limits the scope of federal intervention
 *   in areas like environmental protection, labor law, and civil rights when
 *   they do not directly involve interstate trade. The constraint is claimed
 *   as a Mountain because its proponents view it as an unchangeable,
 *   foundational principle of constitutional structure, inherent to the
 *   original meaning of the text.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.2).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.1).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.2).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, mountain).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Commerce Clause Scope (Narrow Originalist Reading)").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional_law/federalism").

domain_priors:emerges_naturally(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, 'beb3f647-ce94-41fa-b09c-5f311663eac2').
narrative_ontology:cs_kernel_codification('beb3f647-ce94-41fa-b09c-5f311663eac2', fixed_text).
narrative_ontology:cs_authority_grounding('beb3f647-ce94-41fa-b09c-5f311663eac2', lineage).
narrative_ontology:cs_interpretation_layer_present('beb3f647-ce94-41fa-b09c-5f311663eac2').
narrative_ontology:cs_reading_relation('beb3f647-ce94-41fa-b09c-5f311663eac2', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('beb3f647-ce94-41fa-b09c-5f311663eac2', commerce_clause_scope__intermediate_channels, forecloses).
narrative_ontology:cs_axiom('beb3f647-ce94-41fa-b09c-5f311663eac2', foundational, commerce_is_trade_only).
narrative_ontology:cs_axiom_status(commerce_is_trade_only, holdable).
narrative_ontology:cs_axiom_grounding('beb3f647-ce94-41fa-b09c-5f311663eac2', commerce_is_trade_only, conventional).
narrative_ontology:cs_axiom('beb3f647-ce94-41fa-b09c-5f311663eac2', foundational, regulate_is_facilitate_not_prohibit).
narrative_ontology:cs_axiom_status(regulate_is_facilitate_not_prohibit, holdable).
narrative_ontology:cs_axiom_grounding('beb3f647-ce94-41fa-b09c-5f311663eac2', regulate_is_facilitate_not_prohibit, conventional).
narrative_ontology:cs_reference_frame('beb3f647-ce94-41fa-b09c-5f311663eac2', original_constitutional_compact).
narrative_ontology:cs_drift_state('beb3f647-ce94-41fa-b09c-5f311663eac2', contemporary_judicial_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('beb3f647-ce94-41fa-b09c-5f311663eac2', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from retained sovereignty over intrastate economic activity, allowing for diverse state-level regulatory approaches without federal preemption. They are constrained by the need to avoid direct barriers to interstate trade.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Benefit from reduced federal oversight on purely local economic activities, allowing them to operate under state-specific regulations that may be less burdensome than national standards. They are mobile within their state but face federal limits if they engage in interstate commerce.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_businesses, beneficiary,
    moderate, biographical, mobile, local).

% Its power to regulate commerce is limited to removing state-imposed barriers and ensuring uniform rules for trade crossing state lines. This reading constrains its ability to enact broad national economic or social legislation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% Bears the cost of a fragmented regulatory landscape where federal power cannot easily impose uniform standards across states, even for issues with national implications. This is a conceptual victim, representing the ideal of consistent national policy.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity).

% Bears the cost of federal inability to use the Commerce Clause to enforce civil rights in areas deemed purely local or non-commercial, particularly in states resistant to such enforcement. This is a conceptual victim.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, civil_rights_enforcement, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__narrow_originalist, civil_rights_enforcement).

% Advocate for a broader interpretation of the Commerce Clause, believing that federal power should extend to any economic activity with a substantial aggregate effect on interstate commerce. They are excluded from this narrow originalist framework.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, broad_effects_proponents, excluded,
    powerful, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates interstate trade by empowering the federal government to remove state-imposed barriers and ensure a level playing field for commerce crossing state lines, preventing protectionist state policies.
% TRANSFER_FUNCTION: Transfers regulatory authority over intrastate economic activity from the federal government to state governments, and limits the scope of federal economic and social legislation.
% ABSENT_VOICES: Proponents of a broader Commerce Clause interpretation, including civil rights advocates and environmental protection groups, would argue that this narrow reading hobbles federal capacity to address national problems. They are excluded by the interpretive framework itself.
% DISAPPEARANCE_RATIONALE: If this narrow originalist reading vanished, federal power would expand significantly, potentially leading to a more centralized regulatory state, with federal laws preempting many areas currently reserved for states. State autonomy over economic matters would diminish, and national uniformity would increase.
% FOUNDING_PROBLEM: The Articles of Confederation failed to prevent states from erecting trade barriers against each other, leading to economic balkanization and hindering national prosperity. The Commerce Clause was designed to remedy this by granting federal power to ensure free trade among states.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars widely corroborate the problem of interstate trade barriers under the Articles of Confederation. Debates continue regarding whether the original solution remains adequate for a modern, integrated economy, but the core problem of state protectionism is still relevant.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, ExtMetricName, E),
    domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(commerce_clause_scope__narrow_originalist),
    narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.2) reflects that this reading primarily limits federal power, rather than extracting from citizens directly. Its 'extraction' is from the potential for federal regulatory reach. Suppression (0.1) is low because it's a structural interpretation, not actively enforced coercion against individuals, but rather a limit on governmental action. Theater ratio (0.05) is minimal as the interpretation is largely consistent and not performative. Accessibility collapse (0.85) is high because, within this interpretive framework, alternatives for federal action are largely foreclosed. Resistance (0.15) is low from within this framework, though it faces significant external resistance from other interpretive schools.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments and local businesses, this reading is a protective barrier, a 'Rope' or even a 'Mountain' ensuring their autonomy. From the perspective of those advocating for national solutions to social or economic problems, it acts as a 'Snare' or 'Tangled Rope' that prevents necessary federal action. The engine's classification will highlight this divergence from the claimed Mountain type for those seats experiencing its restrictive effects.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and local businesses are beneficiaries as they retain greater autonomy. The federal government, while an agenda-setter, is constrained by this reading. 'National regulatory uniformity' and 'civil rights enforcement' are conceptual victims, representing the policy goals that are curtailed by this narrow interpretation. Proponents of broader interpretations are 'excluded' from this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_ambiguity,
    'Is the ''original intent'' regarding ''commerce'' and ''regulate'' truly as narrow as this reading asserts, or did the framers envision a more dynamic or expansive federal role?',
    'Further historical and linguistic analysis of founding-era documents, debates, and contemporary understandings of the terms ''commerce'' and ''regulate''.',
    'If original intent is found to be broader, the ''emerges_naturally'' claim for this Mountain would be weakened, potentially reclassifying it as a constructed constraint (e.g., a Snare or Tangled Rope for federal power).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_ambiguity, empirical, 'Ambiguity regarding the historical meaning of key terms in the Commerce Clause.').

omega_variable(
    federalism_vs_national_problems,
    'Does this narrow originalist reading of federal power adequately address the challenges of a modern, integrated national economy and society, or does it create a structural impediment to solving national problems?',
    'Empirical analysis of policy outcomes in areas where federal action is constrained by this reading (e.g., environmental protection, labor standards, civil rights) compared to outcomes under broader interpretations.',
    'If it demonstrably impedes solutions to national problems, the ''beneficiary'' status of state governments might be re-evaluated as a ''payer'' of collective action problems, shifting the constraint towards a Snare or Tangled Rope for the nation as a whole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_vs_national_problems, preference, 'Whether the benefits of state autonomy outweigh the costs of limited federal capacity for national problem-solving.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the ''commerce_clause_scope'' kernel. What would a ''broad_effects_test'' or ''intermediate_channels'' reading change structurally?',
    'Comparative legal analysis of judicial decisions and legislative outcomes under each reading.',
    'A ''broad_effects_test'' reading would expand federal power, making federal government a beneficiary and state governments payers. An ''intermediate_channels'' reading would offer a more nuanced expansion, balancing federal and state interests differently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural differences between this narrow originalist reading and sibling interpretations of the Commerce Clause.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1787, commerce_clause_scope__narrow_originalist, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(comm_tr_t1850, commerce_clause_scope__narrow_originalist, theater_ratio, 1850, 0.05).
narrative_ontology:measurement(comm_tr_t1900, commerce_clause_scope__narrow_originalist, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(comm_tr_t1950, commerce_clause_scope__narrow_originalist, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_scope__narrow_originalist, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_scope__narrow_originalist, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(comm_be_t1787, commerce_clause_scope__narrow_originalist, base_extractiveness, 1787, 0.2).
narrative_ontology:measurement(comm_be_t1850, commerce_clause_scope__narrow_originalist, base_extractiveness, 1850, 0.2).
narrative_ontology:measurement(comm_be_t1900, commerce_clause_scope__narrow_originalist, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(comm_be_t1950, commerce_clause_scope__narrow_originalist, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_scope__narrow_originalist, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_scope__narrow_originalist, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1787, commerce_clause_scope__narrow_originalist, suppression_requirement, 1787, 0.1).
narrative_ontology:measurement(comm_su_t1850, commerce_clause_scope__narrow_originalist, suppression_requirement, 1850, 0.1).
narrative_ontology:measurement(comm_su_t1900, commerce_clause_scope__narrow_originalist, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(comm_su_t1950, commerce_clause_scope__narrow_originalist, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_scope__narrow_originalist, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_scope__narrow_originalist, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Commerce Clause's scope. Each reading has a different structural impact and thus constitutes a separate constraint. They are linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
