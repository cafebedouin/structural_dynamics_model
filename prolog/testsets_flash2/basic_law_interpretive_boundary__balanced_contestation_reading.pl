% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Basic Law Interpretive Boundary (Balanced Contestation Reading)
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint describes the 'balanced contestation' reading of the
 *   Basic Law interpretive boundary, where both the legislature and judiciary
 *   hold legitimate but bounded authority. Courts interpret within their
 *   jurisdictional domain, while the legislature retains ultimate sovereign
 *   power, constrained by international obligations and norms of judicial
 *   independence. This reading emphasizes institutional dialogue and
 *   negotiation, with neither branch being fully dominant. This is one
 *   reading of the 'basic_law_interpretive_boundary' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.45).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.3).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Basic Law Interpretive Boundary (Balanced Contestation Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, '212e1d6d-125c-4c22-a47a-a17a563640ff').
narrative_ontology:cs_kernel_codification('212e1d6d-125c-4c22-a47a-a17a563640ff', formalized).
narrative_ontology:cs_authority_grounding('212e1d6d-125c-4c22-a47a-a17a563640ff', lineage).
narrative_ontology:cs_interpretation_layer_present('212e1d6d-125c-4c22-a47a-a17a563640ff').
narrative_ontology:cs_reading_relation('212e1d6d-125c-4c22-a47a-a17a563640ff', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('212e1d6d-125c-4c22-a47a-a17a563640ff', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('212e1d6d-125c-4c22-a47a-a17a563640ff', foundational, institutional_dialogue_as_legitimacy).
narrative_ontology:cs_axiom_status(institutional_dialogue_as_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('212e1d6d-125c-4c22-a47a-a17a563640ff', institutional_dialogue_as_legitimacy, conventional).
narrative_ontology:cs_axiom('212e1d6d-125c-4c22-a47a-a17a563640ff', foundational, bounded_sovereignty_of_legislature).
narrative_ontology:cs_axiom_status(bounded_sovereignty_of_legislature, holdable).
narrative_ontology:cs_axiom_grounding('212e1d6d-125c-4c22-a47a-a17a563640ff', bounded_sovereignty_of_legislature, deontological).
narrative_ontology:cs_reference_frame('212e1d6d-125c-4c22-a47a-a17a563640ff', dynamic_constitutional_equilibrium).
narrative_ontology:cs_drift_state('212e1d6d-125c-4c22-a47a-a17a563640ff', contemporary_political_polarization, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('212e1d6d-125c-4c22-a47a-a17a563640ff', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, judiciary).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, civil_society_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate sovereign power but is constrained by international obligations and norms of judicial independence. Engages in dialogue with the judiciary, sometimes adjusting legislation in response to judicial signals, sometimes reasserting its authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, legislature, agenda_setter,
    institutional, generational, constrained, national).

% Interprets Basic Laws within its jurisdictional domain, engaging in judicial review. Its authority is respected but not absolute, leading to a dynamic of dialogue and negotiation with the legislature and executive.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Operates within the framework established by the Basic Laws and interpreted by both the legislature and judiciary. Benefits from the stability of a system where no single branch is fully dominant, allowing for policy flexibility within established boundaries.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch, beneficiary,
    institutional, biographical, constrained, national).

% Advocate for specific interpretations of Basic Laws and constitutional principles. Bear the costs of engaging in a complex, multi-institutional contestation process, where outcomes are uncertain and require sustained effort across different branches of government.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, civil_society_organizations, payer,
    organized, biographical, constrained, national).

% Monitors the adherence of national institutions to international obligations and norms of judicial independence. Its influence is primarily through soft power, reputation, and diplomatic pressure, shaping the context of domestic interpretive debates.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_legal_community, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of sovereign power among the legislative, judicial, and executive branches, ensuring that each operates within its legitimate sphere while contributing to a stable constitutional order.
% TRANSFER_FUNCTION: Transfers legitimacy and authority between branches, allowing for checks and balances. It also transfers the burden of final interpretation and policy adjustment across institutions, preventing any single branch from bearing it exclusively.
% ABSENT_VOICES: Radical constitutional reformers who advocate for a single, unequivocally supreme branch (either judicial or parliamentary) are marginalized, as the system's stability relies on maintaining a dynamic balance. Their proposals are often dismissed as destabilizing.
% DISAPPEARANCE_RATIONALE: If the interpretive boundary and the norms of balanced contestation vanished, the system would likely collapse into either judicial supremacy or parliamentary sovereignty, leading to a fundamental restructuring of governmental power and potentially severe constitutional crises.
% FOUNDING_PROBLEM: The challenge of establishing a stable constitutional order that balances democratic accountability (legislature) with the protection of fundamental rights and rule of law (judiciary), preventing unchecked power in any single branch.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, political scientists, and international legal bodies consistently attest to the ongoing challenge of maintaining this balance in democratic systems, citing historical and contemporary examples of institutional overreach and conflict. This corroboration comes from outside the direct beneficiaries of the specific national arrangement.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).
:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the costs of ongoing institutional negotiation and the occasional policy compromises required by this dynamic balance. Suppression (0.30) is relatively low, as neither institution can fully suppress the other's legitimate claims, leading to a more open, albeit sometimes tense, dialogue. Theater ratio (0.10) is low, indicating that the contestation is genuine and functional, not merely performative. The accessibility collapse (0.40) is moderate, as alternatives to this balanced contestation (e.g., full judicial supremacy or parliamentary sovereignty) are structurally difficult to achieve and maintain. Resistance (0.50) is moderate, reflecting the ongoing, legitimate disagreements between institutions and civil society over the precise boundaries of authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legislature and judiciary, the constraint is a necessary framework for legitimate governance, ensuring checks and balances. However, for civil society organizations, the ongoing contestation can be seen as a costly and uncertain process, where their advocacy efforts are diffused across multiple institutional arenas. The executive branch benefits from the stability but must navigate the interpretive outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature and judiciary are both agenda-setters and beneficiaries of this reading, as it legitimizes their respective roles and ensures their continued influence. The executive branch benefits from the stable framework. Civil society organizations are payers, bearing the costs of advocacy in a complex, contested environment. International legal community acts as an observer, influencing norms rather than directly participating in the domestic power dynamic.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Rope (claimed) with moderate extractiveness and low theater ratio suggests that the constraint's coordination function (balancing institutional power) remains largely live. The ongoing contestation, while costly, prevents the constraint from becoming a Piton (inertial) or a Snare (pure extraction), as the active engagement of multiple institutions ensures its mandate is continually re-evaluated and defended. The 'contested' status of the founding problem further reinforces that the constraint is actively managed, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_domain_variation_in_extraction,
    'Does the effective extraction (χ) of this interpretive boundary vary significantly across different policy domains (e.g., security vs. social welfare vs. economic regulation)?',
    'Empirical analysis of judicial review outcomes and legislative responses across a range of policy areas, quantifying the costs and compromises imposed on each branch.',
    'If extraction varies significantly, the ''balanced contestation'' reading might itself be a family of constraints, with some domains exhibiting more Snare-like characteristics (higher extraction for one branch) and others more Rope-like (lower, more symmetric costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_domain_variation_in_extraction, empirical, 'Whether the balance of power and associated costs are uniform across all policy domains.').

omega_variable(
    normative_vs_empirical_balance,
    'Is the ''balanced contestation'' primarily a normative ideal that guides institutional behavior, or an empirically observable equilibrium that emerges from power dynamics?',
    'Comparative constitutional studies analyzing the gap between stated constitutional theory and actual institutional practice in similar systems. If the gap is wide, it''s more normative; if narrow, more empirical.',
    'If primarily normative, the constraint''s persistence relies more on shared commitment to an ideal, making it vulnerable to shifts in political culture. If primarily empirical, it''s more robust to ideological challenges but sensitive to changes in institutional power resources.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_vs_empirical_balance, conceptual, 'The underlying nature of the ''balance'' – ideal or emergent.').

omega_variable(
    international_law_influence_strength,
    'How strongly do international obligations and norms of judicial independence actually constrain the legislature, and is this constraint increasing or decreasing over time?',
    'Analysis of legislative compliance with international court rulings, treaty obligations, and the frequency/impact of international legal community interventions. Trend analysis of legislative overrides or disregard for international norms.',
    'If international constraints are weak or weakening, the legislature''s effective power increases, potentially shifting the balance towards parliamentary sovereignty. If strong or strengthening, it reinforces the ''bounded authority'' aspect of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_law_influence_strength, empirical, 'The actual binding force of international legal norms on domestic legislative power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 1992, 0.08).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(basi_tr_t2008, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(basi_tr_t2016, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2016, 0.1).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(basi_be_t2008, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2008, 0.42).
narrative_ontology:measurement(basi_be_t2016, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2016, 0.43).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 1992, 0.25).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(basi_su_t2008, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2008, 0.29).
narrative_ontology:measurement(basi_su_t2016, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2016, 0.3).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'balanced_contestation_reading' of the 'basic_law_interpretive_boundary' kernel. It is one of three sibling readings, alongside 'judicial_supremacy_reading' and 'parliamentary_sovereignty_reading', each representing a distinct structural interpretation of the same constitutional kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
