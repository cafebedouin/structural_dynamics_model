% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment Arms Right (Civic Republican Reading)
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint represents the civic republican reading of the Second
 *   Amendment, which views the right to bear arms as intrinsically linked to
 *   the duty of citizens to participate in a well-regulated militia for the
 *   common defense and republican self-governance. It is neither a purely
 *   individual right nor solely a state prerogative, but a civic right-duty.
 *   This reading emphasizes the 'well-regulated militia' clause as central to
 *   the amendment's purpose, allowing for moderate regulation to ensure
 *   competence and civic responsibility. The metrics reflect a constraint
 *   that coordinates civic participation with a right, with moderate
 *   extractiveness from the duties it imposes and low suppression, as it is
 *   largely self-enforcing through civic norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.35).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.2).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment Arms Right (Civic Republican Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, '33ec48ed-6dbc-46e2-82f6-43ca9f38ba3d').
narrative_ontology:cs_kernel_codification('33ec48ed-6dbc-46e2-82f6-43ca9f38ba3d', fixed_text).
narrative_ontology:cs_authority_grounding('33ec48ed-6dbc-46e2-82f6-43ca9f38ba3d', lineage).
narrative_ontology:cs_interpretation_layer_present('33ec48ed-6dbc-46e2-82f6-43ca9f38ba3d').
narrative_ontology:cs_reading_relation('33ec48ed-6dbc-46e2-82f6-43ca9f38ba3d', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('33ec48ed-6dbc-46e2-82f6-43ca9f38ba3d', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('33ec48ed-6dbc-46e2-82f6-43ca9f38ba3d', foundational, armed_citizenry_essential_for_republic).
narrative_ontology:cs_axiom_status(armed_citizenry_essential_for_republic, holdable).
narrative_ontology:cs_axiom_grounding('33ec48ed-6dbc-46e2-82f6-43ca9f38ba3d', armed_citizenry_essential_for_republic, deontological).
narrative_ontology:cs_axiom('33ec48ed-6dbc-46e2-82f6-43ca9f38ba3d', foundational, well_regulated_implies_civic_duty).
narrative_ontology:cs_axiom_status(well_regulated_implies_civic_duty, holdable).
narrative_ontology:cs_axiom_grounding('33ec48ed-6dbc-46e2-82f6-43ca9f38ba3d', well_regulated_implies_civic_duty, conventional).
narrative_ontology:cs_reference_frame('33ec48ed-6dbc-46e2-82f6-43ca9f38ba3d', founding_era_republican_ideal).
narrative_ontology:cs_drift_state('33ec48ed-6dbc-46e2-82f6-43ca9f38ba3d', contemporary_political_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('33ec48ed-6dbc-46e2-82f6-43ca9f38ba3d', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, civic_militia_members).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_governance_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, civic_militia_members).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, civic_virtue_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, popular_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These citizens are empowered to bear arms as part of a well-regulated militia, fulfilling a civic duty to defend the republic. They benefit from the right but also bear the costs of training, qualification, and adherence to regulations. Exit means abandoning a core civic identity.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, civic_militia_members, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, civic_militia_members, payer).

% Benefit from the constitutional framework that links armed citizenry to the health of the republic, seeing it as a check on tyranny and a foundation for self-governance. Their benefit is ideological and structural, not direct material gain.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, republican_governance_advocates, beneficiary,
    institutional, generational, analytical, national).

% Are tasked with regulating the militia and armed citizenry to ensure a 'well-regulated' status, balancing individual rights with public safety. They set training standards, licensing requirements, and permissible arms, constrained by the civic republican ideal.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, state_regulatory_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Would argue for a broader, less regulated individual right to bear arms, independent of militia service. Their perspective is marginalized by this reading's emphasis on civic duty and collective defense, seeing it as an undue burden on individual freedom.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, individual_liberty_advocates, excluded,
    organized, biographical, constrained, national).

% Would argue for stricter regulations on firearms, prioritizing public safety over armed citizenship. This reading's emphasis on the civic role of arms bearing limits the scope for comprehensive gun control measures, seeing them as undermining republican self-governance.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, gun_control_advocates, excluded,
    organized, biographical, constrained, national).

% Analyze the historical and philosophical underpinnings of the Second Amendment, interpreting its meaning within the broader context of republican political theory. They assess the coherence and implications of this reading.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the right of citizens to bear arms with the civic duty to participate in a well-regulated militia, ensuring a collective capacity for self-defense and a check on potential tyranny, while allowing for necessary regulation.
% TRANSFER_FUNCTION: Transfers the responsibility for collective security and civic participation to armed citizens, in exchange for the right to bear arms, with regulatory authority exercised by the state.
% ABSENT_VOICES: Pure individual-rights advocates and strict gun-control advocates are largely excluded from the core interpretive framework of this reading, as their positions are seen as undermining either the civic duty or the regulatory necessity inherent in the republican model.
% DISAPPEARANCE_RATIONALE: If this civic republican understanding of the Second Amendment vanished, the legal and political landscape around gun rights would fundamentally shift. The justification for armed citizenry would lose its civic grounding, potentially leading to either a purely individualistic, unregulated right or a state-controlled monopoly on force, both of which would alter the balance of power in the republic.
% FOUNDING_PROBLEM: The problem of ensuring a free state's security against both internal usurpation and external threats, without relying on a standing army that could itself become tyrannical, by empowering a citizen militia.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political theorists outside of direct advocacy groups corroborate the founding problem's historical context and its continued relevance to debates about civil-military relations and popular sovereignty. While the specific form of 'militia' has evolved, the underlying tension between state power and citizen capacity remains live.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because it imposes duties (training, qualification) on citizens who wish to exercise the right, which can be seen as a cost. Suppression is low (0.20) as the constraint is primarily maintained through civic norms and the perceived benefit of participation, rather than overt coercion. Theater ratio is low (0.10) as the civic function is genuinely pursued, not merely performed. Accessibility collapse is moderate (0.40) because while the right is available, it comes with conditions that might deter some. Resistance is moderate (0.30) from those who prefer either a purely individualistic or a more restrictive interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civic militia members, this is a beneficial coordination mechanism that empowers them while ensuring order. From the perspective of individual liberty advocates, it is an extractive constraint that burdens a fundamental right. The engine will compute these different classifications based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Civic militia members are dual beneficiaries/payers: they benefit from the right but bear the costs of regulation and duty. Republican governance advocates are beneficiaries of the structural outcome. State regulatory authorities are agenda-setters, balancing rights and duties. Individual liberty advocates and gun control advocates are excluded, as their positions are outside this reading's core framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_duty_vs_individual_burden,
    'To what extent do the ''duties'' imposed by ''well-regulated'' status (training, qualification) genuinely serve a civic purpose, versus acting as an undue burden on individual citizens?',
    'Empirical study of militia effectiveness and civic participation rates under varying regulatory regimes, compared to the burden on individual citizens.',
    'If the burden is disproportionate to the civic benefit, the extractiveness of this reading would be higher, potentially shifting its classification towards a Tangled Rope for individual citizens. If the civic benefit is clear and widely shared, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_duty_vs_individual_burden, empirical, 'Assessing the balance between civic duty and individual burden in the ''well-regulated'' clause.').

omega_variable(
    republican_self_governance_definition,
    'Is the concept of ''republican self-governance'' sufficiently stable and universally understood to ground a constitutional right, or is it subject to interpretive drift that could alter the constraint''s meaning?',
    'Conceptual analysis of historical and contemporary political philosophy, and legal scholarship on the evolution of republican ideals in constitutional law.',
    'If ''republican self-governance'' is highly contested or unstable, the foundational legitimacy of this reading could be undermined, increasing its conceptual extractiveness and potentially shifting it towards a Snare if the concept is used to justify extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(republican_self_governance_definition, conceptual, 'Ambiguity in the foundational concept of republican self-governance.').

omega_variable(
    civic_militia_relevance,
    'Is the ''well-regulated militia'' concept still relevant in modern defense and security contexts, or has its function atrophied, making the civic republican reading a form of theatrical maintenance?',
    'Comparative analysis of national defense strategies, the role of civilian reserves, and the actual deployment of non-state armed groups in contemporary security challenges.',
    'If the militia concept is largely obsolete, the theater_ratio of this reading would increase, potentially pushing it towards a Piton, as its original function would have atrophied while the constraint persists due to inertia or performative maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_militia_relevance, empirical, 'Relevance of the ''well-regulated militia'' in modern contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__civic_republican_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(seco_tr_t10, second_amendment_arms_right__civic_republican_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__civic_republican_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(seco_tr_t30, second_amendment_arms_right__civic_republican_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(seco_tr_t40, second_amendment_arms_right__civic_republican_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(seco_tr_t50, second_amendment_arms_right__civic_republican_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(seco_be_t10, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(seco_be_t30, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement(seco_be_t40, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(seco_be_t50, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(seco_su_t10, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(seco_su_t30, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 30, 0.21).
narrative_ontology:measurement(seco_su_t40, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(seco_su_t50, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
