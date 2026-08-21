% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__legalization_reading, []).

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
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Substance Control Legitimacy: Legalization Reading
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'legalization reading' of the broader
 *   'substance_control_legitimacy' kernel. It posits that competent adults
 *   possess autonomy over their substance use, and state authority is
 *   legitimately limited to preventing demonstrable third-party harm. While
 *   intended to reduce state extraction and suppression, the implementation
 *   of legalization often introduces new forms of extraction through legal
 *   markets and regulatory burdens, leading to its classification as a
 *   Tangled Rope. The core principle of autonomy is coordinated, but the
 *   structure also facilitates asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.65).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.4).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Substance Control Legitimacy: Legalization Reading").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, '797e2737-fa90-47d9-8aa1-c6fbba814acb').
narrative_ontology:cs_kernel_codification('797e2737-fa90-47d9-8aa1-c6fbba814acb', formalized).
narrative_ontology:cs_authority_grounding('797e2737-fa90-47d9-8aa1-c6fbba814acb', practice).
narrative_ontology:cs_interpretation_layer_present('797e2737-fa90-47d9-8aa1-c6fbba814acb').
narrative_ontology:cs_reading_relation('797e2737-fa90-47d9-8aa1-c6fbba814acb', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('797e2737-fa90-47d9-8aa1-c6fbba814acb', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('797e2737-fa90-47d9-8aa1-c6fbba814acb', foundational, adult_autonomy_over_body).
narrative_ontology:cs_axiom_status(adult_autonomy_over_body, holdable).
narrative_ontology:cs_axiom_grounding('797e2737-fa90-47d9-8aa1-c6fbba814acb', adult_autonomy_over_body, deontological).
narrative_ontology:cs_axiom('797e2737-fa90-47d9-8aa1-c6fbba814acb', foundational, state_authority_limited_to_externalities).
narrative_ontology:cs_axiom_status(state_authority_limited_to_externalities, holdable).
narrative_ontology:cs_axiom_grounding('797e2737-fa90-47d9-8aa1-c6fbba814acb', state_authority_limited_to_externalities, conventional).
narrative_ontology:cs_reference_frame('797e2737-fa90-47d9-8aa1-c6fbba814acb', individual_liberty_with_social_contract).
narrative_ontology:cs_drift_state('797e2737-fa90-47d9-8aa1-c6fbba814acb', contemporary_policy_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('797e2737-fa90-47d9-8aa1-c6fbba814acb', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, competent_adults).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, legal_substance_industries).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, tax_authorities).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, consumers_of_legal_substances).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, small_scale_producers).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, victims_of_unmitigated_third_party_harm).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, competent_adults).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain autonomy over personal substance use choices, free from criminalization. They bear the costs of legal market prices, taxes, and compliance with regulations designed to prevent third-party harm.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, competent_adults, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, competent_adults, payer).

% Profit from the legal production, distribution, and sale of substances. They actively lobby for favorable regulatory environments and market access, shaping the implementation of the constraint.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, legal_substance_industries, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, legal_substance_industries, agenda_setter).

% Collect significant tax revenue from the legal substance market, which can be allocated to public services, including harm prevention and treatment programs.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, tax_authorities, beneficiary,
    institutional, generational, analytical, national).

% Access substances legally but pay market prices, taxes, and may be subject to corporate extraction through branding and pricing strategies. Their choices are shaped by the legal market's offerings.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, consumers_of_legal_substances, payer,
    moderate, immediate, constrained, local).

% Face high regulatory barriers, licensing costs, and intense competition from larger, established industries in the legal market, making it difficult to participate or thrive.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, small_scale_producers, payer,
    powerless, biographical, constrained, local).

% Bear the direct and indirect costs of harms (e.g., impaired driving accidents, secondhand exposure) that the state's limited authority or regulatory framework fails to adequately prevent or mitigate.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, victims_of_unmitigated_third_party_harm, payer,
    powerless, immediate, trapped, local).

% Monitor public health outcomes related to substance use, advocate for evidence-based harm prevention strategies, and assess the effectiveness of regulatory measures.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, public_health_advocates, observer,
    organized, generational, analytical, national).

% Oppose the legalization framework, believing substance use is inherently harmful and should be criminalized. Their moral and public safety arguments are marginalized within this reading's policy discourse.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, prohibition_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual liberty with public safety by establishing a legal framework for substance use that respects adult autonomy while limiting state intervention to preventing demonstrable third-party harm.
% TRANSFER_FUNCTION: Transfers control over personal substance use from the criminal justice system to individuals. It transfers regulatory burden from criminal enforcement to public health and market regulation, and generates tax revenue from legal sales.
% ABSENT_VOICES: Prohibition advocates are structurally excluded from the core premise of this reading. Additionally, the voices of those disproportionately affected by unmitigated third-party harms may be underrepresented if policy focuses too heavily on individual liberty without robust harm prevention.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the legal substance market would collapse, individual autonomy over substance use would be re-criminalized or revert to an unregulated black market, and the state would either re-impose prohibition or face chaotic, unmanaged public health and safety challenges. The entire social and economic infrastructure around substance control would reorganize.
% FOUNDING_PROBLEM: The over-criminalization of individuals for personal substance use, the disproportionate enforcement against marginalized communities, the proliferation of dangerous black markets, and the infringement of individual liberty regarding personal choices.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations, economic studies on the failures of prohibition, and social justice advocates consistently corroborate the ongoing nature of these problems, arguing that legalization is a necessary step to address them. This corroboration comes from outside the direct beneficiaries of the legal substance industry.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) reflects the costs imposed on consumers and small producers by the legal market (e.g., high prices, taxes, regulatory compliance, corporate consolidation) and the potential for unmitigated third-party harms. Suppression (0.40) is lower than under prohibition but still present due to active regulation and enforcement against third-party harms. The theater ratio is low (0.10) as the constraint is functionally implemented. Resistance (0.50) comes from both prohibition advocates and those who feel the legalization framework does not adequately address public health or social equity concerns. The slight increase in extractiveness over time reflects the tendency for legal markets to consolidate and impose higher costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of competent adults, this constraint is a liberating Rope, restoring autonomy. From the perspective of small-scale producers or consumers facing high market prices, it can feel like a Snare, replacing state criminalization with corporate extraction. The engine's classification as Tangled Rope captures this hybrid nature, acknowledging both the coordination of autonomy and the asymmetric extraction within the legal market.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent adults and legal substance industries are primary beneficiaries, gaining autonomy and profit respectively. Tax authorities also benefit from revenue. Consumers and small-scale producers are targets, bearing the costs of market extraction and regulatory hurdles. Victims of unmitigated third-party harm are also targets, as the constraint's limited state authority may not fully protect them. Prohibition advocates are excluded, as their core premise is rejected by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_market_extraction_vs_autonomy_coordination,
    'Is the extraction observed in the legal substance market an inherent feature of this ''legalization reading'' constraint, or a separate, emergent constraint (e.g., ''corporate_monopoly_in_legal_substances'')?',
    'Analysis of market structure and regulatory capture: if market concentration and pricing power are direct consequences of the legalization framework''s design, it''s inherent; if they arise from independent market dynamics, it''s a separate constraint.',
    'If inherent, the ''legalization reading'' is more strongly a Tangled Rope or Snare. If separate, the core ''legalization reading'' might be closer to a Rope, with market extraction being a distinct, linked Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_market_extraction_vs_autonomy_coordination, conceptual, 'Distinguishing extraction from the legalization framework itself versus emergent market dynamics.').

omega_variable(
    definition_and_measurement_of_third_party_harm,
    'How are ''third-party harms'' defined and measured, and is the state''s limited authority sufficient to prevent them without infringing on autonomy?',
    'Empirical studies on public health and safety outcomes post-legalization, coupled with legal challenges to the scope of state intervention. Consensus on harm metrics and effective interventions would clarify the boundary.',
    'If harms are broadly defined or poorly mitigated, the constraint''s legitimacy is challenged, and the ''victims_of_unmitigated_third_party_harm'' become more central. If harms are narrowly defined and effectively mitigated, the autonomy aspect is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_and_measurement_of_third_party_harm, empirical, 'Ambiguity in defining and mitigating third-party harms under limited state authority.').

omega_variable(
    prohibition_reading_structural_delta,
    'What would be the structural changes if the ''prohibition_reading'' of the substance_control_legitimacy kernel were adopted instead of this ''legalization_reading''?',
    'Comparative policy analysis of jurisdictions with prohibitionist frameworks, focusing on changes in victim sets, enforcement mechanisms, and extractiveness.',
    'Adoption of ''prohibition_reading'' would re-criminalize substance use, making ''competent_adults'' and ''consumers_of_legal_substances'' primary victims, significantly increasing state suppression and extractiveness through criminal penalties and black markets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_reading_structural_delta, conceptual, 'Structural impact of adopting the prohibitionist reading.').

omega_variable(
    harm_reduction_reading_structural_delta,
    'What would be the structural changes if the ''harm_reduction_reading'' of the substance_control_legitimacy kernel were adopted instead of this ''legalization_reading''?',
    'Comparative policy analysis of jurisdictions with harm reduction frameworks, focusing on public health outcomes, state intervention types, and market regulation.',
    'Adoption of ''harm_reduction_reading'' would prioritize public health interventions (e.g., safe supply, treatment access) over market-driven legalization, potentially leading to different regulatory structures, less corporate extraction, and a shift in the primary beneficiaries and victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_reduction_reading_structural_delta, conceptual, 'Structural impact of adopting the harm reduction reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(subs_tr_t5, substance_control_legitimacy__legalization_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__legalization_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__legalization_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(subs_be_t5, substance_control_legitimacy__legalization_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__legalization_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__legalization_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(subs_su_t5, substance_control_legitimacy__legalization_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__legalization_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__legalization_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is the 'legalization_reading' of the 'substance_control_legitimacy' kernel, distinct from 'prohibition_reading' and 'harm_reduction_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
