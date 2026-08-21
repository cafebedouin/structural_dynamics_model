% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: Paris Agreement Article 4 NDC Equity Reading (CBDR-RC)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'equity reading' of the Paris Agreement's
 *   Article 4 on Nationally Determined Contributions (NDCs), emphasizing
 *   Common But Differentiated Responsibilities and Respective Capabilities
 *   (CBDR-RC). Under this reading, developed states bear greater obligations
 *   for emissions reductions and financial transfers, while developing states
 *   retain policy space. It is a contested interpretation of the Paris
 *   Agreement, which itself is a kernel. The metrics reflect a moderately
 *   extractive constraint, primarily from developed states, with active
 *   enforcement by equity coalitions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.45).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.3).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "Paris Agreement Article 4 NDC Equity Reading (CBDR-RC)").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, '772242bb-ad66-4ce7-a8f5-6c64267924be').
narrative_ontology:cs_kernel_codification('772242bb-ad66-4ce7-a8f5-6c64267924be', fixed_text).
narrative_ontology:cs_authority_grounding('772242bb-ad66-4ce7-a8f5-6c64267924be', lineage).
narrative_ontology:cs_interpretation_layer_present('772242bb-ad66-4ce7-a8f5-6c64267924be').
narrative_ontology:cs_reading_relation('772242bb-ad66-4ce7-a8f5-6c64267924be', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('772242bb-ad66-4ce7-a8f5-6c64267924be', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_axiom('772242bb-ad66-4ce7-a8f5-6c64267924be', foundational, historical_responsibility_for_emissions).
narrative_ontology:cs_axiom_status(historical_responsibility_for_emissions, holdable).
narrative_ontology:cs_axiom_grounding('772242bb-ad66-4ce7-a8f5-6c64267924be', historical_responsibility_for_emissions, deontological).
narrative_ontology:cs_axiom('772242bb-ad66-4ce7-a8f5-6c64267924be', foundational, differentiated_capabilities_for_action).
narrative_ontology:cs_axiom_status(differentiated_capabilities_for_action, holdable).
narrative_ontology:cs_axiom_grounding('772242bb-ad66-4ce7-a8f5-6c64267924be', differentiated_capabilities_for_action, empirically_contingent).
narrative_ontology:cs_reference_frame('772242bb-ad66-4ce7-a8f5-6c64267924be', unfccc_cbdr_rc_framework).
narrative_ontology:cs_drift_state('772242bb-ad66-4ce7-a8f5-6c64267924be', post_paris_agreement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('772242bb-ad66-4ce7-a8f5-6c64267924be', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, equity_coalitions).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, high_emitting_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Expected to bear a disproportionate burden of emissions reductions and provide financial/technological support to developing states, reflecting historical responsibility. Their policy space for economic development is constrained by these obligations.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_states, payer,
    institutional, generational, constrained, global).

% Retain significant policy space for economic development, with less stringent emissions reduction targets and a right to receive support. They benefit from the principle of differentiated responsibilities, allowing them to prioritize poverty eradication and economic growth.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_states, beneficiary,
    organized, generational, mobile, global).

% Advocate for the full implementation of CBDR-RC, ensuring climate action addresses historical injustices and supports sustainable development in the Global South. They gain significant influence over the interpretation and implementation of NDCs under this reading.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, equity_coalitions, agenda_setter,
    organized, civilizational, constrained, global).

% Face increased pressure for decarbonization and potential financial transfers under this equity-focused interpretation, particularly if located in developed states. Their operational models are directly challenged by the structural distinctions required.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, high_emitting_industries, payer,
    powerful, biographical, constrained, global).

% Their ability to impose uniform, binding targets or enforce compliance across all states is limited by the emphasis on differentiated responsibilities and national circumstances. They would advocate for stronger, more centralized enforcement mechanisms.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, supranational_climate_institutions, excluded,
    institutional, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global climate action by acknowledging historical responsibility and differing capacities, aiming to build trust and facilitate participation from all states by distributing burdens equitably.
% TRANSFER_FUNCTION: Mandates financial and technological transfers from developed to developing states, and shifts the burden of emissions reductions disproportionately onto developed states and their industries.
% ABSENT_VOICES: Supranational climate institutions and those advocating for uniform, legally binding targets across all states are marginalized, as their proposals for stronger enforcement are often vetoed by equity coalitions prioritizing national policy space for developing countries.
% DISAPPEARANCE_RATIONALE: If this equity reading vanished, the foundational principle of CBDR-RC would be undermined, leading to a breakdown in trust between developed and developing nations. Developing states would likely withdraw from ambitious climate commitments without assurances of support and differentiated responsibilities, and the global climate regime would struggle to achieve consensus or legitimacy.
% FOUNDING_PROBLEM: The historical disproportionate contribution of developed nations to climate change and the unequal capacity of nations to address it, leading to a need for equitable burden-sharing in global climate action.
% FOUNDING_PROBLEM_CORROBORATION: Developing states and numerous academic analyses corroborate that the founding problem of climate injustice and unequal capacity remains live. Developed states often acknowledge historical emissions but contest the extent of ongoing responsibility or the specific mechanisms for transfer, leading to a 'contested' status for the problem's resolution, but not its existence.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).
:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while developed states bear significant burdens, the overall framework aims for global coordination. Suppression (0.30) is relatively low, as this reading relies on political pressure and coalition-building rather than direct coercive enforcement. Theater ratio (0.20) is present due to some states making commitments that are not fully backed by domestic policy, but the core principle of differentiated responsibility is genuinely pursued by its proponents. The slight increase and then stabilization in extractiveness and suppression reflect the ongoing political contestation and the difficulty of enforcing these differentiated responsibilities.
 *
 * PERSPECTIVAL GAP:
 *   Developed states experience this as a moderately extractive constraint, imposing costs and limiting sovereignty. Developing states experience it as a beneficial coordination mechanism that rectifies historical injustices. Equity coalitions see it as a necessary framework for legitimate global climate action. The engine's per-seat classification will reflect these divergent experiences based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed states and high-emitting industries are targets (high d) due to their increased obligations. Developing states and equity coalitions are beneficiaries (low d) as they gain policy space and influence. Supranational climate institutions are excluded, as this reading limits their enforcement power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cbdr_rc_operationalization_ambiguity,
    'How precisely can ''Common But Differentiated Responsibilities and Respective Capabilities'' be operationalized into concrete, measurable obligations without becoming a source of perpetual negotiation deadlock?',
    'Development and adoption of a universally agreed-upon, transparent framework for assessing historical responsibility and current capabilities, leading to quantifiable, differentiated targets and support mechanisms.',
    'If operationalized effectively, the constraint''s legitimacy and effectiveness would increase, potentially reducing resistance and increasing compliance. If it remains ambiguous, it risks becoming a source of theater and contested claims, increasing extractiveness for those who bear the burden without clear justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cbdr_rc_operationalization_ambiguity, conceptual, 'Ambiguity in translating CBDR-RC into actionable policy.').

omega_variable(
    developed_state_compliance_enforcement,
    'What mechanisms exist to genuinely enforce the transfer obligations and higher emissions reduction burdens on developed states, given their sovereign power and potential resistance?',
    'Establishment of a robust international accountability framework with clear penalties for non-compliance, or the emergence of strong domestic political will in developed states to meet these obligations.',
    'If enforcement is weak, the constraint''s extractiveness from developed states will remain theoretical, shifting it towards a more ''theater'' or ''piton'' classification. Stronger enforcement would solidify its ''tangled_rope'' nature, with real transfers and burdens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(developed_state_compliance_enforcement, empirical, 'Effectiveness of enforcement mechanisms for developed state obligations.').

omega_variable(
    equity_vs_ambition_tradeoff,
    'Does prioritizing equity and differentiated responsibilities inherently limit the overall ambition and speed of global climate action, or can both be simultaneously maximized?',
    'Empirical observation of climate outcomes in regimes that prioritize equity versus those that prioritize ambition, alongside theoretical modeling of optimal pathways. This is a long-term, complex empirical and conceptual question.',
    'If equity consistently leads to lower ambition, the constraint might be re-evaluated as a ''snare'' for global climate goals, even if beneficial for some states. If both can be maximized, it reinforces the ''tangled_rope'' classification as a legitimate, albeit complex, coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equity_vs_ambition_tradeoff, preference, 'Trade-off between equity and overall climate ambition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__equity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pari_tr_t5, paris_article_4_ndc__equity_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(pari_tr_t10, paris_article_4_ndc__equity_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(pari_tr_t15, paris_article_4_ndc__equity_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(pari_tr_t20, paris_article_4_ndc__equity_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__equity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(pari_be_t5, paris_article_4_ndc__equity_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(pari_be_t10, paris_article_4_ndc__equity_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(pari_be_t15, paris_article_4_ndc__equity_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(pari_be_t20, paris_article_4_ndc__equity_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__equity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(pari_su_t5, paris_article_4_ndc__equity_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(pari_su_t10, paris_article_4_ndc__equity_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(pari_su_t15, paris_article_4_ndc__equity_reading, suppression_requirement, 15, 0.31).
narrative_ontology:measurement(pari_su_t20, paris_article_4_ndc__equity_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_agreement_ratcheting_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, green_climate_fund_allocation).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'paris_article_4_ndc' kernel. This 'equity_reading' emphasizes CBDR-RC, contrasting with the 'sovereigntist_reading' (voluntary pledges) and the 'supranational_reading' (binding commitments).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
