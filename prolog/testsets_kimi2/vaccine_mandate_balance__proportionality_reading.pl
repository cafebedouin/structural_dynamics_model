% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Proportionality-Limited Vaccine Mandate Framework
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   A legal-ethical framework holding that state-compelled vaccination is
 *   permissible solely when disease severity, transmission risk, and vaccine
 *   safety satisfy strict proportionality thresholds, and where exemption
 *   pathways remain robust. This framework functions as a conditional
 *   override of individual bodily autonomy, coordinated through public health
 *   institutions and subject to judicial review. It is one reading of the
 *   vaccine_mandate_balance kernel; sibling readings differ on whether
 *   individual consent is inviolable or categorically subordinate to
 *   collective protection.
 *
 * KEY AGENTS:
 *   - public_health_authorities: Primary agenda_setter (institutional/analytical) â designs epidemiological thresholds, enforces mandates, and defends the proportionality framework.
 *   - mandated_individuals: Primary target (moderate/constrained) â bear the direct cost of compelled vaccination and autonomy override when thresholds are met.
 *   - vulnerable_populations: Primary beneficiary (powerless/constrained) â receive reduced exposure risk without administering the constraint.
 *   - judicial_review_bodies: Analytical observer (institutional/analytical) â adjudicates whether threshold applications satisfy constitutional proportionality.
 *   - bodily_autonomy_advocates: Excluded voice (organized/mobile) â reject the proportionality premise entirely and are structurally absent from threshold-setting.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.55).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.45).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Proportionality-Limited Vaccine Mandate Framework").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, '0fc2936e-942f-4ff6-a2f0-4863deb27ecb').
narrative_ontology:cs_kernel_codification('0fc2936e-942f-4ff6-a2f0-4863deb27ecb', formalized).
narrative_ontology:cs_authority_grounding('0fc2936e-942f-4ff6-a2f0-4863deb27ecb', lineage).
narrative_ontology:cs_interpretation_layer_present('0fc2936e-942f-4ff6-a2f0-4863deb27ecb').
narrative_ontology:cs_reading_relation('0fc2936e-942f-4ff6-a2f0-4863deb27ecb', vaccine_mandate_balance__public_health_primary, influences).
narrative_ontology:cs_reading_relation('0fc2936e-942f-4ff6-a2f0-4863deb27ecb', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_axiom('0fc2936e-942f-4ff6-a2f0-4863deb27ecb', foundational, proportionality_governs_autonomy_limits).
narrative_ontology:cs_axiom_status(proportionality_governs_autonomy_limits, holdable).
narrative_ontology:cs_axiom_grounding('0fc2936e-942f-4ff6-a2f0-4863deb27ecb', proportionality_governs_autonomy_limits, conventional).
narrative_ontology:cs_axiom('0fc2936e-942f-4ff6-a2f0-4863deb27ecb', foundational, robust_exemptions_mandatory).
narrative_ontology:cs_axiom_status(robust_exemptions_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('0fc2936e-942f-4ff6-a2f0-4863deb27ecb', robust_exemptions_mandatory, conventional).
narrative_ontology:cs_reference_frame('0fc2936e-942f-4ff6-a2f0-4863deb27ecb', strict_proportionality_framework).
narrative_ontology:cs_drift_state('0fc2936e-942f-4ff6-a2f0-4863deb27ecb', contemporary_pandemic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0fc2936e-942f-4ff6-a2f0-4863deb27ecb', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, mandated_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the epidemiological thresholds for mandate activation and design exemption criteria. They collect compliance data, enforce penalties for non-compliance, and justify the framework as minimizing harm while respecting autonomy through strict proportionality. Their authority derives from constitutional and statutory public health powers.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Must vaccinate or secure a recognized exemption when thresholds are met. They bear the direct bodily intrusion and autonomy loss, plus any social or professional costs of non-compliance. Exit is limited to navigating exemption bureaucracies or accepting penalties.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, mandated_individuals, payer,
    moderate, biographical, constrained, national).

% Benefit from reduced community transmission when mandates raise coverage above voluntary levels. They depend on the proportionality framework to activate protection during high-risk periods but do not administer it.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, constrained, national).

% Review whether threshold determinations and exemption regimes meet constitutional proportionality standards. They do not collect or pay but adjudicate the boundary between valid public health coercion and rights violation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, judicial_review_bodies, observer,
    institutional, generational, analytical, national).

% Assert that no pathogen severity justifies state-compelled medical intervention. They are structurally absent from the proportionality threshold-setting process because their position rejects the framework's foundational premise entirely.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, bodily_autonomy_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinating population-level immunity when voluntary uptake is insufficient to protect vulnerable groups from severe outcomes, by temporarily overriding individual opt-out under strictly defined epidemiological and safety conditions.
% TRANSFER_FUNCTION: Transfers bodily autonomy decision-making from the individual to public health authority when proportionality thresholds are met; transfers risk reduction from the general population to the vulnerable.
% ABSENT_VOICES: Individuals who reject the proportionality calculus entirely (absolutist bodily autonomy advocates) and community representatives who dispute the threshold-setting process are present in public discourse but structurally excluded from the technical committees that set activation criteria.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished overnight, public health authorities would lose the legal tool to compel vaccination during emergencies, vulnerable populations would face higher exposure when voluntary uptake is inadequate, and the legal balance between autonomy and coercion would revert to either categorical prohibition or categorical override depending on jurisdiction.
% FOUNDING_PROBLEM: Endemic and epidemic infectious disease causing preventable mortality and morbidity in populations where voluntary protective behavior and vaccine uptake are insufficient to prevent community transmission to vulnerable groups.
% FOUNDING_PROBLEM_CORROBORATION: Medical historians and epidemiologists outside the direct public health enforcement apparatus document recurrent pandemic mortality and the historical insufficiency of purely voluntary measures during acute outbreaks; these sources attest the problem remains live.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 because the compelled medical intrusion represents a serious extraction from bodily autonomy, but it is bounded by proportionality thresholds, necessity requirements, and robust exemptions. Suppression is 0.45 because enforcement involves real legal penalties yet remains constrained by judicial review and exemption bureaucracies rather than absolute. Theater ratio is 0.25 because the framework is largely functional, though some performative threshold-setting occurs to legitimize pre-determined policy. Accessibility collapse is 0.40 because alternatives such as targeted protection and purely voluntary campaigns remain visible but are politically marginalized once proportionality doctrine is accepted. Resistance is 0.50 because the framework faces sustained legal and social challenge from both absolutist autonomy advocates and public-health-expansionist critics.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat experiences the constraint as necessary, bounded coordination that saves lives while preserving legal legitimacy; the payer seat experiences it as conditional state intrusion on bodily integrity that is only as robust as the exemption bureaucracy allows; the beneficiary seat experiences it as passive protection from risk generated by others. The engine computes these directionalities independently from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations are declared beneficiaries and therefore receive low directionality (d near 0.0), damping their effective extraction toward subsidy. Mandated individuals are declared victims (role: payer) and therefore receive high directionality (d near 1.0), amplifying their effective extraction. Public health authorities sit outside the beneficiary/victim lists as agenda_setter; their directionality reverts to the canonical fallback for institutional power, yielding a moderate d that reflects their structural position as coordinators rather than collectors. Judicial review bodies are analytical observers with minimal positional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than rope or snare requires demonstrating both a genuine coordination function (protecting vulnerable groups from preventable mortality) and asymmetric extraction (compelled bodily intervention). The proportionality framework resists mislabeling because it explicitly names both. Mandatrophy is further resisted by the built-in threshold logic: mandates deactivate when epidemiological conditions fall below proportionality, functioning as a conditional rather than permanent constraint. Should the framework persist after the founding problem disappears or should thresholds be manipulated to always trigger, it would drift toward snare or piton; the authored metrics and temporal series capture the pre-drift state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pathogen_dependent_epsilon,
    'Does the proportionality framework''s extraction remain bounded across all pathogen severities, or does it collapse into categorical coercion at high severity and into ineffectual theater at low severity?',
    'Comparative case-law analysis across pathogens with different severity profiles (smallpox, polio, measles, COVID-19, seasonal influenza) measuring actual mandate enforcement rates and exemption grant rates.',
    'If epsilon is pathogen-invariant in practice, the proportionality reading is a cover story; if epsilon tracks severity as claimed, the reading is structurally valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathogen_dependent_epsilon, empirical, 'Whether extraction tracks the proportionality thresholds or is invariant to pathogen severity.').

omega_variable(
    kernel_reading_boundary,
    'Is the proportionality reading structurally distinct from the public_health_primary reading, or does it function as a downstream legitimization mechanism that collapses into public_health_primary during emergencies?',
    'Examining threshold-setting behavior during declared emergencies: do proportionality constraints bind, or are they suspended?',
    'If suspended during emergencies, the proportionality reading is not an independent constraint but a peacetime veneer over a public_health_primary kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether proportionality remains operative or collapses into public health primacy under emergency conditions.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties for non-compliance) or internalized (social stigma, professional ostracism)?',
    'Post-exit suppression trajectory: compare social and professional outcomes for exempted versus non-compliant individuals after mandate expiration.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests; if purely structural, expiration of legal threat ends extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_balance__proportionality_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_balance__proportionality_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__proportionality_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_balance__proportionality_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__proportionality_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__proportionality_reading, theater_ratio, 24, 0.25).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 4, 0.35).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 24, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 24, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
