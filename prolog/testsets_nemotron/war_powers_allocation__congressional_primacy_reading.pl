% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: Congressional Primacy in War Powers Allocation
 *   domain: constitutional/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint story instantiates the congressional primacy reading of
 *   the war powers allocation kernel. The reading holds that Article I,
 *   Section 8's Declare War Clause, combined with the Necessary and Proper
 *   Clause and Congress's power of the purse, requires explicit congressional
 *   authorization for any sustained military force beyond immediate defense
 *   of the United States. Executive unilateral action beyond immediate
 *   defense constitutes extraction from Congress's constitutional war power.
 *   The constraint operates as a tangled rope: it coordinates democratic
 *   accountability and legal clarity (genuine coordination function) while
 *   extracting operational flexibility from the executive branch and military
 *   command (asymmetric extraction). Active enforcement is required through
 *   judicial review, congressional oversight, appropriations control, and
 *   political accountability mechanisms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.35).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.65).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional Primacy in War Powers Allocation").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, '25a0650d-bb24-4e6e-b3bc-f5598c688d57').
narrative_ontology:cs_kernel_codification('25a0650d-bb24-4e6e-b3bc-f5598c688d57', fixed_text).
narrative_ontology:cs_authority_grounding('25a0650d-bb24-4e6e-b3bc-f5598c688d57', lineage).
narrative_ontology:cs_interpretation_layer_present('25a0650d-bb24-4e6e-b3bc-f5598c688d57').
narrative_ontology:cs_reading_relation('25a0650d-bb24-4e6e-b3bc-f5598c688d57', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('25a0650d-bb24-4e6e-b3bc-f5598c688d57', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('25a0650d-bb24-4e6e-b3bc-f5598c688d57', foundational, declare_war_clause_exclusive).
narrative_ontology:cs_axiom_status(declare_war_clause_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('25a0650d-bb24-4e6e-b3bc-f5598c688d57', declare_war_clause_exclusive, deontological).
narrative_ontology:cs_axiom('25a0650d-bb24-4e6e-b3bc-f5598c688d57', foundational, purse_power_conditions_force).
narrative_ontology:cs_axiom_status(purse_power_conditions_force, holdable).
narrative_ontology:cs_axiom_grounding('25a0650d-bb24-4e6e-b3bc-f5598c688d57', purse_power_conditions_force, conventional).
narrative_ontology:cs_reference_frame('25a0650d-bb24-4e6e-b3bc-f5598c688d57', constitutional_convention_allocation).
narrative_ontology:cs_drift_state('25a0650d-bb24-4e6e-b3bc-f5598c688d57', post_911_perpetual_authorization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('25a0650d-bb24-4e6e-b3bc-f5598c688d57', '2026-08-04T14:30:00Z').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, judicial_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, state_governments_militia).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, international_allies_treaty_partners).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, military_command_structure).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, intelligence_community).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, separation_of_powers_principle).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, democratic_accountability_in_war).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, republican_government_guarantee).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, legislative_supremacy_in_fiscal_matters).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, non_delegation_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, checks_and_balances_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the constitutional power to declare war and authorize military force. Sets the agenda for war authorization through committee hearings, authorization bills, and appropriations. Bears political accountability for war decisions. When bypassed, loses its primary constitutional check on executive power and its role in democratic accountability for war. Exit is constrained by constitutional design — cannot easily cede or reclaim this power without structural change.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, legislative_branch, agenda_setter,
    institutional, generational, constrained, national).

% Commands the military as Commander-in-Chief but requires congressional authorization for sustained force beyond immediate defense. Bears the operational burden of seeking authorization, managing congressional relations, and accepting legislative constraints on military strategy. When authorization is denied or delayed, loses operational flexibility and speed. Exit is constrained by constitutional structure — cannot unilaterally rewrite the allocation without constitutional crisis.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Executes military operations under civilian control. Requires clear legal authority and funding streams from congressional authorization. Faces operational uncertainty when authorization is ambiguous or contested. Bears the risk of operating without clear legal cover. Exit is constrained by chain of command and constitutional subordination to civilian authority.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, military_command_structure, payer,
    organized, biographical, constrained, national).

% Provides intelligence support for military operations. Requires legal authority for covert action and surveillance tied to authorized military force. Faces legal exposure when operations exceed congressional authorization. Exit is constrained by statutory framework and executive oversight structures.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, intelligence_community, payer,
    organized, biographical, constrained, national).

% Adjudicates war powers disputes when cases arise. Gains institutional legitimacy and relevance from being the arbiter of constitutional allocation. Benefits from clear constitutional text and precedent that enables judicial review. Exit is analytical — observes and rules but does not participate in the operational dynamic.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, judicial_branch, beneficiary,
    institutional, generational, analytical, national).

% Retains constitutional role in militia and state defense forces. Benefits from congressional control over federal military deployment, which preserves state autonomy and the militia clause. Bears costs when federal mobilization overrides state control. Exit is constrained by federal supremacy and constitutional structure.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, state_governments_militia, beneficiary,
    organized, generational, constrained, regional).

% Relies on U.S. commitments made through treaty and congressional authorization. Gains predictability and democratic legitimacy from congressional war powers. Loses confidence when executive acts unilaterally without legislative backing. Exit is mobile — can adjust alliance commitments but faces high switching costs.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, international_allies_treaty_partners, beneficiary,
    organized, generational, mobile, global).

% Analyzes, interprets, and litigates the constitutional allocation. Provides the interpretive framework that shapes how the constraint is understood and enforced. No direct stake in operational outcomes but shapes the legitimacy of competing readings. Exit is analytical — the discourse continues regardless of any single participant.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, constitutional_scholars_courts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the constitutional allocation of war powers between legislative and executive branches, ensuring democratic accountability, deliberative decision-making, and legal clarity for military operations. Solves the collective action problem of committing the nation to war by requiring broad political consent through elected representatives.
% TRANSFER_FUNCTION: Transfers the authority to initiate sustained military force from the executive branch (which would exercise it unilaterally) to the legislative branch (which must deliberate and authorize). The executive bears the cost of seeking permission and accepting constraints; the legislature gains the power to condition, limit, or deny authorization. The military and intelligence communities bear the operational costs of authorization delays and political constraints.
% ABSENT_VOICES: The public at large, whose lives and resources are committed to war, has no direct institutional voice in the authorization process — only indirect representation through Congress. Future generations who bear the long-term consequences of war decisions are entirely excluded. Allied populations affected by U.S. military action have no voice in the authorizing legislature. Anti-war movements and pacifist constituencies are structurally excluded from the formal war powers machinery.
% DISAPPEARANCE_RATIONALE: If the congressional authorization requirement vanished overnight, the executive branch would immediately expand unilateral military deployments. The constitutional separation of powers would collapse into executive supremacy in war. Democratic accountability for war would evaporate. International allies would lose confidence in U.S. commitments. The legal framework for military operations would shift from statutory authority to inherent executive power, fundamentally altering civil-military relations and the constitutional order.
% FOUNDING_PROBLEM: The Constitution's framers sought to prevent the concentration of war power in a single executive, having experienced monarchical war-making. They allocated the power to declare war to Congress to ensure democratic deliberation, political accountability, and a check on executive ambition. The founding problem was how to enable effective defense while preventing tyranny and unilateral war-making.
% FOUNDING_PROBLEM_CORROBORATION: The congressional primacy reading is corroborated by the Constitutional Convention records (Madison's notes), the Federalist Papers (particularly Federalist 69), early congressional practice (declarations of war in 1812, 1846, 1898, 1917, 1941), and Supreme Court dicta (Youngstown Sheet & Tube, Prize Cases). The inherent executive reading claims corroboration from early presidential practice (Jefferson's Barbary actions, Polk's Mexican War maneuvers) and Cold War precedents. The functional accommodation reading cites the War Powers Resolution (1973) and post-9/11 practice as evidence of a settled middle ground. No single reading commands consensus among constitutional scholars, political branches, or the courts.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).
:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) reflects the executive's loss of unilateral operational freedom — a real cost but not total deprivation, as immediate defense remains executive prerogative. Suppression (0.65) is high because the constraint's persistence depends on active political, legal, and institutional enforcement against inherent executive authority claims. Theater ratio (0.45) is moderate: congressional authorizations often involve performative deliberation (post-9/11 AUMFs passed with minimal debate, War Powers Resolution routinely contested), but the coordination function (democratic legitimacy, legal clarity, alliance credibility) remains real. Accessibility collapse (0.4) is moderate: alternatives (executive unilateralism, functional accommodation) remain live and practiced. Resistance (0.55) is substantial: every president since Truman has contested congressional primacy in practice.
 *
 * PERSPECTIVAL GAP:
 *   From the congressional seat, the constraint is a rope: genuine coordination of democratic war-making. From the executive seat, it is a snare: extraction of operational flexibility under cover of coordination. From the military seat, it is a tangled rope: real coordination (legal authority, funding) mixed with extraction (operational constraints, political risk). From the judicial seat, it is a mountain-ish coordination structure: constitutional text provides relatively stable interpretive anchor. The engine computes these per-seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative branch is the primary agenda-setter and beneficiary: it sets the authorization agenda and gains democratic accountability and institutional relevance. The executive branch, military command, and intelligence community are payers: they bear the costs of seeking authorization, accepting constraints, and facing legal exposure. The judicial branch, states, and allies are beneficiaries: they gain institutional role, autonomy protection, and commitment credibility. The analytical observer sees the full structural asymmetry. Directionality derives from beneficiary/victim declarations plus institutional power and exit constraints: Congress is institutionally powerful but exit-constrained (cannot cede power); the executive is institutionally powerful but exit-constrained (cannot rewrite Constitution); the military is organized but exit-constrained (chain of command).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing executive tyranny in war) remains contested: the threat of unilateral executive war-making persists, but the nature of war has transformed (non-state actors, cyber, drones, gray zone). The coordination function (democratic authorization) is eroded by functional accommodation in practice. The extraction function (constraining executive) is real but unevenly enforced. The constraint is not a piton — it remains actively contested and enforced, not merely performative. It is not a scaffold — no sunset clause exists, and the founding problem is not universally agreed as resolved. Tangled rope best captures the hybrid: coordination function under active pressure from extraction dynamics on both sides.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immediate_defense_boundary,
    'What constitutes ''immediate defense'' sufficient to trigger inherent executive authority without congressional authorization?',
    'Judicial decisions in concrete cases, congressional-executive branch negotiations over specific operations, or scholarly consensus on the temporal and spatial limits of ''immediate.''',
    'A narrow definition (only response to actual attack on U.S. territory) strengthens congressional primacy; a broad definition (anticipatory self-defense, protection of citizens abroad, vital interests) expands executive unilateralism and weakens the constraint''s coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immediate_defense_boundary, conceptual, 'The boundary between congressional and inherent executive war power').

omega_variable(
    authorization_form_threshold,
    'What forms of congressional action satisfy the authorization requirement — formal declaration, AUMF, appropriations, tacit acquiescence?',
    'Supreme Court decision on the constitutionality of specific authorization forms, or constitutional amendment clarifying the requirement.',
    'If only formal declarations count, most post-1941 wars were unconstitutional; if AUMFs and appropriations suffice, the constraint''s extraction from the executive is substantially lower. The threshold determines whether the constraint is a meaningful check or a procedural formality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorization_form_threshold, empirical, 'The threshold of congressional action that constitutes valid authorization').

omega_variable(
    kernel_framing_underdetermination,
    'Does the war_powers_allocation kernel admit a single correct reading, or is the constitutional design inherently indeterminate between the three declared readings?',
    'Originalist historical analysis, structural constitutional theory, or political settlement over time. No purely legal resolution is available.',
    'If the kernel is determinate, one reading is constitutionally correct and the others are errors. If indeterminate, the constraint''s classification depends on which reading prevails politically — making the ''tangled rope'' classification itself a contingent political fact rather than a structural necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the constitutional text and structure determine a unique war powers allocation').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (constitutional text, judicial review, appropriations power) or internalized (executive branch self-restraint, institutional norms, professional military ethos)?',
    'Compare suppression levels during periods of unified vs. divided government; measure executive compliance when judicial enforcement is absent vs. present; analyze military officer corps attitudes toward unilateral vs. authorized operations.',
    'If suppression is primarily internalized, the constraint is more fragile — a president willing to break norms faces lower effective suppression. If structural, the constraint persists even against norm-breaking executives. This affects whether the constraint is a rope (self-sustaining coordination) or a snare/tangled_rope (requiring active enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_tr_t1789, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1789, 0.1).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_tr_t1812, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1812, 0.12).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_tr_t1846, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1846, 0.25).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_tr_t1898, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1898, 0.3).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_tr_t1917, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_tr_t1941, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1941, 0.1).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_tr_t1950, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1950, 0.4).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_tr_t1964, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1964, 0.5).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_tr_t1973, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1973, 0.45).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_tr_t2001, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2001, 0.55).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_tr_t2024, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_be_t1789, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1789, 0.15).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_be_t1812, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1812, 0.18).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_be_t1846, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1846, 0.22).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_be_t1898, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1898, 0.25).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_be_t1917, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1917, 0.2).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_be_t1941, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1941, 0.15).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_be_t1950, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_be_t1964, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1964, 0.45).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_be_t1973, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1973, 0.4).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_be_t2001, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_be_t2024, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_su_t1789, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1789, 0.2).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_su_t1812, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1812, 0.25).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_su_t1846, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1846, 0.4).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_su_t1898, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1898, 0.45).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_su_t1917, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1917, 0.3).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_su_t1941, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1941, 0.25).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_su_t1950, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_su_t1964, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1964, 0.7).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_su_t1973, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1973, 0.65).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_su_t2001, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2001, 0.75).
narrative_ontology:measurement(war_powers_allocation__congressional_primacy_reading_su_t2024, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__congressional_primacy_reading, 0.12).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_resolution_1973).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, aumf_2001_2002).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, national_emergencies_act).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, posse_comitatus_act).

% DUAL FORMULATION NOTE:
% This constraint is one member of the war_powers_allocation constraint family. The kernel decomposes into three structurally distinct readings with different ε values, beneficiary/victim structures, and classifications. Congressional primacy (this story): ε=0.35, tangled_rope. Functional accommodation: lower ε, rope or scaffold. Inherent executive: higher ε, snare or mountain (from executive seat). All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__congressional_primacy_reading, institutional, 0.3).
constraint_indexing:directionality_override(war_powers_allocation__congressional_primacy_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
