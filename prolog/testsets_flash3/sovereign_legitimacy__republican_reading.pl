% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Reading of Popular Sovereignty
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint represents the 'republican reading' of legitimate
 *   authority, where power originates from the people and is delegated
 *   through consent. It is a foundational principle for democratic states.
 *   While primarily a coordination mechanism (a 'rope'), it exhibits moderate
 *   extractiveness due to the inherent costs of collective action, the
 *   potential for majoritarian tyranny, and the exclusion of certain
 *   populations from the 'people' whose consent is sought. The metrics
 *   reflect the ongoing tension between ideal and practice in democratic
 *   systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.45).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.3).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Reading of Popular Sovereignty").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, '60e8cc9c-985c-4bb7-ac42-e861524b26a4').
narrative_ontology:cs_kernel_codification('60e8cc9c-985c-4bb7-ac42-e861524b26a4', formalized).
narrative_ontology:cs_authority_grounding('60e8cc9c-985c-4bb7-ac42-e861524b26a4', lineage).
narrative_ontology:cs_interpretation_layer_present('60e8cc9c-985c-4bb7-ac42-e861524b26a4').
narrative_ontology:cs_reading_relation('60e8cc9c-985c-4bb7-ac42-e861524b26a4', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('60e8cc9c-985c-4bb7-ac42-e861524b26a4', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('60e8cc9c-985c-4bb7-ac42-e861524b26a4', foundational, authority_derives_from_consent).
narrative_ontology:cs_axiom_status(authority_derives_from_consent, holdable).
narrative_ontology:cs_axiom_grounding('60e8cc9c-985c-4bb7-ac42-e861524b26a4', authority_derives_from_consent, deontological).
narrative_ontology:cs_axiom('60e8cc9c-985c-4bb7-ac42-e861524b26a4', foundational, popular_sovereignty_is_supreme).
narrative_ontology:cs_axiom_status(popular_sovereignty_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('60e8cc9c-985c-4bb7-ac42-e861524b26a4', popular_sovereignty_is_supreme, deontological).
narrative_ontology:cs_reference_frame('60e8cc9c-985c-4bb7-ac42-e861524b26a4', enlightenment_social_contract).
narrative_ontology:cs_drift_state('60e8cc9c-985c-4bb7-ac42-e861524b26a4', contemporary_democratic_backsliding_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('60e8cc9c-985c-4bb7-ac42-e861524b26a4', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, citizenry_with_franchise).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, elected_representatives).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, excluded_populations).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, minority_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate source of legitimate authority, delegating power through elections and participatory mechanisms. They benefit from self-governance but are constrained by the need for collective action and the potential for majoritarian overreach.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, citizenry_with_franchise, agenda_setter,
    organized, generational, constrained, national).

% Receive delegated authority from the citizenry to govern. Their legitimacy depends on ongoing consent and adherence to constitutional frameworks. They benefit from holding power but are accountable to the electorate.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_representatives, beneficiary,
    institutional, biographical, constrained, national).

% Those denied voting rights or effective participatory mechanisms. They bear the costs of governance without having a direct voice in delegated consent, experiencing the constraint as a form of extraction or suppression.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, excluded_populations, payer,
    powerless, generational, trapped, national).

% While possessing franchise, their interests may be systematically underrepresented or overridden by majoritarian rule. They pay the cost of policies they did not consent to, experiencing a form of extraction through the democratic process itself.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, minority_groups, payer,
    moderate, biographical, constrained, national).

% Analyze the theoretical and practical application of popular sovereignty, examining the mechanisms of consent, delegation, and accountability. They assess the fidelity of the political system to its republican ideals.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable and legitimate framework for collective decision-making and governance by channeling popular will into delegated authority, preventing arbitrary rule and civil strife.
% TRANSFER_FUNCTION: Transfers the right to govern from the collective 'people' to specific elected officials, in exchange for accountability and the promise of governance in the public interest. It also transfers compliance obligations from the governed to the governing.
% ABSENT_VOICES: Historically, women, enslaved people, indigenous populations, and non-propertied citizens were excluded from the 'people' whose consent legitimized authority. Today, non-citizens, incarcerated individuals, and those disenfranchised by systemic barriers remain excluded, and would challenge the universality of 'the people'.
% DISAPPEARANCE_RATIONALE: If the principle of legitimate authority flowing upward from the people vanished, the entire structure of modern democratic states would collapse. Governments would lose their moral and legal basis, leading to widespread civil disobedience, challenges to state power, and a fundamental reordering of political systems.
% FOUNDING_PROBLEM: To establish a stable and just form of government that avoids both tyranny and anarchy, by grounding political power in the consent of the governed rather than divine right or brute force.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists, historians, and contemporary social movements attest that the problem of legitimate governance remains live, as evidenced by ongoing debates about electoral integrity, democratic backsliding, and the rights of marginalized groups. This corroboration comes from outside the immediate beneficiaries of the current system.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the costs of maintaining a system of delegated consent, including the potential for majoritarian decisions to impose costs on minorities. Suppression (0.3) is low but present, as the system requires active enforcement of electoral rules and constitutional adherence, and can suppress dissent that challenges the legitimacy framework itself. Theater ratio (0.1) is low, indicating that the mechanisms of consent (elections, public debate) are largely functional, though performative aspects exist. The values fluctuate slightly over time, reflecting periods of increased or decreased democratic participation and contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the citizenry with franchise, this constraint is a pure rope, enabling self-governance. From the perspective of excluded populations, it operates as a snare, legitimizing their marginalization through a system from which they are excluded. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The citizenry with franchise and elected representatives are beneficiaries, as they directly participate in and benefit from the system of self-governance. Excluded populations and minority groups are victims, bearing the costs of governance without full participation or protection from majoritarian decisions. Constitutional scholars act as observers, analyzing the system's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_the_people,
    'Who constitutes ''the people'' whose consent legitimizes authority, and how is this boundary determined?',
    'Historical analysis of franchise expansion, legal challenges to voting rights, and philosophical arguments for universal political inclusion. Empirical study of the effects of exclusion on political stability.',
    'If ''the people'' is narrowly defined, the constraint''s extractiveness and suppression for excluded groups are higher, pushing it towards a Snare for those seats. If broadly defined, it moves closer to a Rope for all citizens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_the_people, conceptual, 'Ambiguity in the definition of ''the people'' and its impact on legitimacy and exclusion.').

omega_variable(
    majoritarian_tyranny_risk,
    'To what extent does delegated consent, without robust minority protections, enable majoritarian tyranny, thereby increasing extraction for minority groups?',
    'Comparative political science studies on the impact of constitutional design (e.g., bill of rights, judicial review) on minority rights and political stability. Analysis of historical instances of majoritarian overreach.',
    'If majoritarian tyranny is a significant risk, the constraint''s extractiveness for minority groups is higher than currently estimated, potentially reclassifying their seat as a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_risk, empirical, 'The tension between popular sovereignty and minority rights.').

omega_variable(
    reading_distinction_monarchical,
    'Is the ''republican_reading'' truly distinct from the ''monarchical_reading'', or do elements of inherited authority persist in practice?',
    'Analysis of constitutional texts, historical political transitions, and contemporary political rituals. Examination of the role of unelected institutions (e.g., supreme courts, hereditary chambers) in republican systems.',
    'If significant elements of inherited authority persist, the ''republican_reading'' is less pure, and its claimed upward flow of legitimacy is partially theatrical, increasing its theater_ratio and potentially shifting its classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_distinction_monarchical, conceptual, 'Distinction between republican and monarchical legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__republican_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(sove_tr_t10, sovereign_legitimacy__republican_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__republican_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(sove_tr_t30, sovereign_legitimacy__republican_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__republican_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(sove_tr_t50, sovereign_legitimacy__republican_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__republican_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sove_be_t10, sovereign_legitimacy__republican_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__republican_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(sove_be_t30, sovereign_legitimacy__republican_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__republican_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(sove_be_t50, sovereign_legitimacy__republican_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__republican_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(sove_su_t10, sovereign_legitimacy__republican_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__republican_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(sove_su_t30, sovereign_legitimacy__republican_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__republican_reading, suppression_requirement, 40, 0.31).
narrative_ontology:measurement(sove_su_t50, sovereign_legitimacy__republican_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'sovereign_legitimacy' kernel. This 'republican_reading' emphasizes upward flow of authority from the people, contrasting with monarchical and constitutional hybrid readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
