% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__public_health_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: Public Health Compulsion for Collective Harm Prevention
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'public_health_primary' reading of the
 *   'coercion_legitimacy_boundary' kernel. This reading asserts that
 *   collective harm-prevention can legitimately outweigh individual autonomy,
 *   particularly in public health crises. The constraint is framed as a
 *   necessary coordination mechanism to protect the vulnerable, but its
 *   operation involves significant extraction from those whose autonomy is
 *   overridden. Sibling readings include 'bodily_autonomy_primary' (which
 *   holds individual consent as paramount) and 'proportionality_reading'
 *   (which conditions coercion on disease severity and transmission
 *   dynamics).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.78).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.85).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "Public Health Compulsion for Collective Harm Prevention").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, 'f3960b4a-079d-4069-bec3-6c634c921218').
narrative_ontology:cs_kernel_codification('f3960b4a-079d-4069-bec3-6c634c921218', formalized).
narrative_ontology:cs_authority_grounding('f3960b4a-079d-4069-bec3-6c634c921218', lineage).
narrative_ontology:cs_interpretation_layer_present('f3960b4a-079d-4069-bec3-6c634c921218').
narrative_ontology:cs_reading_relation('f3960b4a-079d-4069-bec3-6c634c921218', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('f3960b4a-079d-4069-bec3-6c634c921218', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('f3960b4a-079d-4069-bec3-6c634c921218', foundational, collective_welfare_supremacy).
narrative_ontology:cs_axiom_status(collective_welfare_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('f3960b4a-079d-4069-bec3-6c634c921218', collective_welfare_supremacy, deontological).
narrative_ontology:cs_reference_frame('f3960b4a-079d-4069-bec3-6c634c921218', state_police_power_doctrine).
narrative_ontology:cs_drift_state('f3960b4a-079d-4069-bec3-6c634c921218', contemporary_pandemic_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f3960b4a-079d-4069-bec3-6c634c921218', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, healthcare_system).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, individual_autonomy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for safeguarding public health, they interpret and enforce policies that may include compelling medical interventions. They benefit from the ability to implement broad harm-prevention strategies.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Highly vulnerable to infectious diseases, they directly benefit from widespread vaccination and other collective health measures that reduce pathogen circulation. Their autonomy is not directly targeted, but their safety depends on others' compliance.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, local).

% Are the direct targets of compelled medical interventions (e.g., mandatory vaccination). They bear the cost of lost bodily autonomy and may face social or economic penalties for non-compliance. Their options are compliance or facing legal/social repercussions.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals, payer,
    powerless, immediate, constrained, local).

% Represent individuals whose bodily autonomy is infringed by state mandates. They bear the cost of defending individual rights against collective claims and experience the constraint as an erosion of fundamental liberties. Their exit options involve legal challenges and political organizing.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, individual_autonomy_advocates, payer,
    organized, generational, constrained, national).

% Benefits from reduced burden during public health crises due to effective collective interventions. It provides the infrastructure for interventions and is less likely to be overwhelmed when compliance is high.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, healthcare_system, beneficiary,
    institutional, biographical, mobile, national).

% Analyze the legal boundaries of state power versus individual rights, often litigating cases related to public health mandates. They observe the tension between collective welfare and individual liberties.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, constitutional_lawyers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__public_health_primary, public_health_authorities).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent the spread of infectious diseases and protect vulnerable populations by ensuring a high level of collective immunity or adherence to public health measures, thereby avoiding overwhelming the healthcare system.
% TRANSFER_FUNCTION: Transfers the burden of individual risk and autonomy from the collective (especially vulnerable groups) to individuals who are compelled to undergo medical interventions. It also transfers decision-making power from individuals to public health authorities.
% ABSENT_VOICES: Individuals with deeply held religious or philosophical objections to specific medical interventions, who are often marginalized or legally overridden in this framework. Their perspectives on bodily integrity and conscience are not fully accommodated.
% DISAPPEARANCE_RATIONALE: If the state's power to compel medical intervention for public health vanished, the ability to control epidemics would be severely hampered. Vulnerable populations would face increased risk, and the healthcare system could be overwhelmed during outbreaks, leading to a significant reorganization of public health strategies and societal norms around collective responsibility.
% FOUNDING_PROBLEM: The historical problem of widespread infectious disease outbreaks causing mass mortality and societal disruption, necessitating collective action beyond individual choice to protect the population.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations (e.g., WHO, CDC) and medical professionals consistently attest to the ongoing threat of infectious diseases and the necessity of collective measures. Historical records of epidemics and contemporary epidemiological data from outside the direct beneficiaries corroborate the problem's persistence.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because individuals are compelled to act against their will, bearing the cost of lost autonomy. Suppression is very high (0.85) as the state uses its coercive power (laws, penalties) to enforce compliance, limiting alternatives. Theater ratio is low (0.10) because the state's actions are direct and functional, aimed at achieving public health outcomes, with little performative maintenance. Accessibility collapse is high (0.70) as legal and social alternatives to compliance are significantly reduced. Resistance is high (0.75) due to strong counter-claims from individual rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities, this is a necessary and legitimate coordination mechanism for collective survival. From the perspective of compelled individuals, it is a coercive imposition and a violation of fundamental rights. The engine's classification will reflect this divergence, likely showing a 'tangled_rope' for the system as a whole, but a 'snare' from the individual's seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and the healthcare system are beneficiaries, gaining the ability to manage population health and avoid system overload. Immunocompromised individuals are also clear beneficiaries, as their safety is directly enhanced. Unvaccinated individuals and individual autonomy advocates are the primary targets/victims, bearing the costs of compulsion and the erosion of rights. The state's directionality is toward benefiting the collective, while individuals subject to compulsion are at the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing collective harm) is still live, especially in the context of ongoing infectious disease threats. The challenge is not that the problem has disappeared, but whether the *means* (compulsion) remain proportional or necessary, which is addressed by the 'proportionality_reading' sibling. This constraint, by prioritizing public health, asserts the mandate's continued relevance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_harm_threshold_ambiguity,
    'What specific empirical threshold of collective harm (e.g., R0 value, hospitalization rate, mortality rate) justifies overriding individual autonomy?',
    'Consensus among epidemiologists and public health ethicists on a quantitative framework for ''collective harm'' that triggers compulsion, or legislative codification of such thresholds.',
    'If a clear, empirically grounded threshold is established, the constraint''s application becomes less arbitrary, potentially reducing perceived extractiveness for some. If no such threshold exists, the constraint''s legitimacy remains contested, amplifying perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_harm_threshold_ambiguity, empirical, 'Ambiguity in the empirical basis for ''collective harm-prevention''.').

omega_variable(
    proportionality_vs_primacy_framing,
    'Is the ''public_health_primary'' reading truly a categorical primacy, or is it implicitly subject to proportionality considerations that are merely less stringent than the ''proportionality_reading''?',
    'Analysis of judicial rulings and public health policy documents to determine if any level of ''minor'' harm would *never* justify compulsion, even under this reading, or if the ''primacy'' is absolute.',
    'If implicit proportionality is found, the conceptual gap between this reading and the ''proportionality_reading'' narrows, potentially leading to a re-evaluation of their relationship from ''forecloses'' to ''influences'' or ''coexists_with'' under certain conditions. If absolute primacy, the conceptual distinction remains sharp.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_vs_primacy_framing, conceptual, 'Conceptual ambiguity between categorical primacy and implicit proportionality.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal penalties, social exclusion) or internalized (fear of social stigma, moral obligation to the collective)?',
    'Post-mandate compliance trajectory: if compliance persists after legal mandates are lifted, reclassify as partially internalized. Surveys on motivations for compliance.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — individuals carry the suppression with them. If purely structural, removing mandates would lead to rapid non-compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in public health mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(coer_tr_t6, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 6, 0.11).
narrative_ontology:measurement(coer_tr_t12, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 12, 0.1).
narrative_ontology:measurement(coer_tr_t18, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 18, 0.09).
narrative_ontology:measurement(coer_tr_t24, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 24, 0.09).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(coer_be_t6, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(coer_be_t12, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(coer_be_t18, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(coer_be_t24, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 24, 0.77).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(coer_su_t6, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(coer_su_t12, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(coer_su_t18, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 18, 0.82).
narrative_ontology:measurement(coer_su_t24, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 24, 0.84).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
