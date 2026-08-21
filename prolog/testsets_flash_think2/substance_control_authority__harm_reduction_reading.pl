% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__harm_reduction_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: substance_control_authority__harm_reduction_reading
 *   human_readable: State Harm Reduction Policy for Substance Use
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint describes the state's authority to implement harm
 *   reduction policies for substance use, which involves decriminalization,
 *   provision of public health services (e.g., needle exchanges, supervised
 *   consumption sites), and a focus on minimizing health harms rather than
 *   criminalizing users. It represents a middle ground between full
 *   prohibition and full legalization, aiming to coordinate public health and
 *   safety while acknowledging the persistence of substance use. This story
 *   is one reading of the broader 'substance_control_authority' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.35).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.45).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "State Harm Reduction Policy for Substance Use").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, '4215b0b1-ff83-4ae3-bf36-1e6a78647019').
narrative_ontology:cs_kernel_codification('4215b0b1-ff83-4ae3-bf36-1e6a78647019', formalized).
narrative_ontology:cs_authority_grounding('4215b0b1-ff83-4ae3-bf36-1e6a78647019', practice).
narrative_ontology:cs_interpretation_layer_present('4215b0b1-ff83-4ae3-bf36-1e6a78647019').
narrative_ontology:cs_reading_relation('4215b0b1-ff83-4ae3-bf36-1e6a78647019', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('4215b0b1-ff83-4ae3-bf36-1e6a78647019', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('4215b0b1-ff83-4ae3-bf36-1e6a78647019', foundational, public_health_first).
narrative_ontology:cs_axiom_status(public_health_first, holdable).
narrative_ontology:cs_axiom_grounding('4215b0b1-ff83-4ae3-bf36-1e6a78647019', public_health_first, empirically_contingent).
narrative_ontology:cs_axiom('4215b0b1-ff83-4ae3-bf36-1e6a78647019', foundational, human_dignity_in_addiction).
narrative_ontology:cs_axiom_status(human_dignity_in_addiction, holdable).
narrative_ontology:cs_axiom_grounding('4215b0b1-ff83-4ae3-bf36-1e6a78647019', human_dignity_in_addiction, deontological).
narrative_ontology:cs_reference_frame('4215b0b1-ff83-4ae3-bf36-1e6a78647019', public_health_paradigm).
narrative_ontology:cs_drift_state('4215b0b1-ff83-4ae3-bf36-1e6a78647019', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4215b0b1-ff83-4ae3-bf36-1e6a78647019', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, substance_users).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, communities).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, law_enforcement).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, healthcare_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from decriminalization and access to health services, reducing criminal justice harms. However, they still bear the health harms of substance use and must comply with public health interventions, which can feel coercive.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, substance_users, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, substance_users, payer).

% Are mandated to design and implement harm reduction services, receiving funding and legitimacy for this work. They set guidelines and coordinate interventions, but operate under political and budgetary constraints.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, public_health_agencies, beneficiary).

% Benefit from reduced disease transmission, overdose deaths, and some forms of drug-related crime. However, they may still bear residual social disorder, public health costs, and perceived risks associated with visible drug use or service provision.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, communities, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, communities, payer).

% Shifts focus from prosecuting minor drug offenses to addressing violent crime and drug trafficking, potentially reducing their workload and improving community relations. They enforce decriminalization policies and support public health initiatives.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, law_enforcement, agenda_setter,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, law_enforcement, beneficiary).

% Are ideologically committed to criminalizing drug use and view harm reduction as condoning harmful behavior. Their perspective is marginalized in this policy framework, though they continue to exert political pressure.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, prohibition_advocates, excluded,
    organized, generational, identity_locked, national).

% Believe that full regulation of drug markets is the most effective way to minimize harms and maximize individual liberty. They see harm reduction as an insufficient, intermediate step, and their calls for full legalization are not central to this policy.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, legalization_advocates, excluded,
    organized, generational, identity_locked, national).

% Experience increased access to patients who might otherwise avoid care due to fear of criminalization. They receive funding and training to deliver harm reduction services, integrating substance use treatment into mainstream healthcare.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, healthcare_providers, beneficiary,
    organized, biographical, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__harm_reduction_reading, diffuse).
narrative_ontology:fixing_cost_class(substance_control_authority__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health services, law enforcement, and community efforts to manage substance use, focusing on health outcomes and reducing criminal justice involvement, rather than criminal punishment.
% TRANSFER_FUNCTION: Transfers resources from the state (taxpayers) to public health services and substance users (via services), and transfers the burden of criminalization away from users. Residual health harms and social risks are borne by users and communities.
% ABSENT_VOICES: Advocates for full prohibition (who would argue for stricter criminalization) and advocates for full legalization (who would argue for regulated markets) are structurally excluded from the core framing of this policy, though they remain active in public discourse.
% DISAPPEARANCE_RATIONALE: If this authority vanished, the policy vacuum would likely lead to either a return to full criminalization (increasing harms and social costs) or a chaotic, unregulated market, both of which would drastically alter public health and safety landscapes, requiring a complete reorganization of societal responses to substance use.
% FOUNDING_PROBLEM: The failure of punitive prohibition to reduce drug use or related harms, leading to mass incarceration, disease transmission, social marginalization, and an overburdened criminal justice system.
% FOUNDING_PROBLEM_CORROBORATION: Public health data, medical professionals, international health organizations (e.g., WHO), and social science research consistently corroborate the ineffectiveness and harms of prohibition, supporting the need for alternative, health-focused approaches. This corroboration comes from sources independent of the direct beneficiaries of harm reduction policies.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__harm_reduction_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__harm_reduction_reading_tests).
:- end_tests(substance_control_authority__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) is moderate: while criminal penalties are reduced, substance users still bear significant health harms, and communities bear residual social costs. Suppression (0.45) is also moderate: criminal suppression is reduced, but public health interventions still involve state control, guidance, and enforcement of certain boundaries. Theater ratio (0.15) is low, reflecting a largely functional approach, though some performative aspects exist in managing public perception. Accessibility collapse (0.5) indicates that while alternatives to harmful use are offered, full legal markets are not, limiting choices. Resistance (0.4) comes from both prohibitionist and legalization advocates who see this approach as either too lenient or too restrictive. The slight increase in extractiveness and suppression over time reflects the ongoing challenges of managing public health crises and the potential for bureaucratic creep or increased enforcement in response to public pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health agencies, this is a functional, evidence-based approach to a complex problem. From the perspective of substance users, it offers vital support but can still feel paternalistic or coercive. From the perspective of prohibition advocates, it's a dangerous capitulation, while legalization advocates see it as an insufficient compromise. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Substance users are beneficiaries through decriminalization and services, but also payers through continued health harms and compliance. Public health agencies and law enforcement are agenda-setters and beneficiaries, gaining a clearer mandate and reduced burdens, respectively. Communities are beneficiaries of reduced harms but payers of residual risks and service costs. Prohibition and legalization advocates are excluded, as their core positions are not adopted by this policy. Healthcare providers are beneficiaries through increased patient access and funding.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (minimizing harms from substance use) remains live, as evidenced by ongoing public health crises. The 'tangled_rope' classification prevents mislabeling it as pure coordination (a 'rope') by acknowledging the persistent health harms and social costs borne by users and communities, and the active enforcement required to maintain the policy's boundaries. It also prevents mislabeling as pure extraction (a 'snare') by recognizing the genuine coordination function and benefits provided.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_of_interventions,
    'To what extent do public health interventions effectively reduce health harms and social costs, and what proportion of measured extraction is irreducible harm versus policy inefficiency?',
    'Longitudinal epidemiological studies, cost-benefit analyses of specific programs, and comparative studies with jurisdictions employing different approaches.',
    'If interventions are highly effective, the remaining extraction is closer to an irreducible cost of managing substance use. If they are inefficient, the policy itself contributes to extraction, suggesting a need for reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_interventions, empirical, 'Measures the actual impact of harm reduction policies on health and social outcomes.').

omega_variable(
    boundary_with_legalization,
    'Is the distinction between decriminalization (harm reduction) and de facto legalization (unregulated market) or full legalization (regulated market) clear and consistently maintained in practice?',
    'Legal analysis of enforcement patterns, ethnographic studies of drug markets, and public perception surveys. If the lines blur, the policy''s intended function may drift.',
    'If the boundary is consistently blurred, the ''harm_reduction_reading'' may effectively drift towards an ''implicit_legalization'' state, altering its classification and stakeholder dynamics. If the boundary is clear, the policy''s distinct nature is preserved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_with_legalization, conceptual, 'Examines the practical distinction between harm reduction and legalization.').

omega_variable(
    community_risk_tolerance,
    'What is the actual level of risk (e.g., public disorder, perceived safety) communities are willing to tolerate in exchange for reduced overdose deaths and disease transmission, and how does this influence policy enforcement?',
    'Community surveys, public forums, and analysis of local political responses to harm reduction initiatives. This is a preference-based question with empirical consequences.',
    'If community tolerance for residual risks is low, political pressure may increase suppression requirements or shift policy back towards more punitive measures, increasing extraction for users. If tolerance is high, the policy may become more expansive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(community_risk_tolerance, preference, 'Assesses community willingness to accept residual risks from harm reduction policies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(subs_tr_t6, substance_control_authority__harm_reduction_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__harm_reduction_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(subs_tr_t18, substance_control_authority__harm_reduction_reading, theater_ratio, 18, 0.14).
narrative_ontology:measurement(subs_tr_t24, substance_control_authority__harm_reduction_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(subs_tr_t30, substance_control_authority__harm_reduction_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t6, substance_control_authority__harm_reduction_reading, base_extractiveness, 6, 0.32).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__harm_reduction_reading, base_extractiveness, 12, 0.33).
narrative_ontology:measurement(subs_be_t18, substance_control_authority__harm_reduction_reading, base_extractiveness, 18, 0.34).
narrative_ontology:measurement(subs_be_t24, substance_control_authority__harm_reduction_reading, base_extractiveness, 24, 0.35).
narrative_ontology:measurement(subs_be_t30, substance_control_authority__harm_reduction_reading, base_extractiveness, 30, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(subs_su_t6, substance_control_authority__harm_reduction_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__harm_reduction_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(subs_su_t18, substance_control_authority__harm_reduction_reading, suppression_requirement, 18, 0.44).
narrative_ontology:measurement(subs_su_t24, substance_control_authority__harm_reduction_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(subs_su_t30, substance_control_authority__harm_reduction_reading, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'substance_control_authority' kernel. The 'harm_reduction_reading' focuses on public health and decriminalization, distinct from the 'prohibition_reading' (criminalization) and 'legalization_reading' (regulated markets). Each reading represents a different structural approach to substance control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
