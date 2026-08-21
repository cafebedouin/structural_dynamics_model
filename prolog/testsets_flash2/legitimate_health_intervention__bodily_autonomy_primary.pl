% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary in Health Interventions
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents a reading of 'legitimate_health_intervention'
 *   that prioritizes individual bodily autonomy and informed consent above
 *   all other considerations, including public health benefits. It asserts
 *   that state coercion in medical matters constitutes a violation of
 *   fundamental rights, regardless of the perceived collective good. The
 *   constraint is classified as a Snare from the perspective of
 *   mandate-coerced individuals, as it highlights the extractive nature of
 *   state power when it infringes on bodily integrity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.65).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.7).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "Bodily Autonomy as Primary in Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '943125cf-bf07-45f6-889e-9a2be1bafe5c').
narrative_ontology:cs_kernel_codification('943125cf-bf07-45f6-889e-9a2be1bafe5c', formalized).
narrative_ontology:cs_authority_grounding('943125cf-bf07-45f6-889e-9a2be1bafe5c', lineage).
narrative_ontology:cs_interpretation_layer_present('943125cf-bf07-45f6-889e-9a2be1bafe5c').
narrative_ontology:cs_reading_relation('943125cf-bf07-45f6-889e-9a2be1bafe5c', legitimate_health_intervention__proportionality_reading, coexists_with).
narrative_ontology:cs_reading_relation('943125cf-bf07-45f6-889e-9a2be1bafe5c', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('943125cf-bf07-45f6-889e-9a2be1bafe5c', foundational, bodily_integrity_is_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('943125cf-bf07-45f6-889e-9a2be1bafe5c', bodily_integrity_is_absolute, deontological).
narrative_ontology:cs_axiom('943125cf-bf07-45f6-889e-9a2be1bafe5c', foundational, informed_consent_is_non_derogable).
narrative_ontology:cs_axiom_status(informed_consent_is_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('943125cf-bf07-45f6-889e-9a2be1bafe5c', informed_consent_is_non_derogable, deontological).
narrative_ontology:cs_reference_frame('943125cf-bf07-45f6-889e-9a2be1bafe5c', post_nuremberg_code_ethics).
narrative_ontology:cs_drift_state('943125cf-bf07-45f6-889e-9a2be1bafe5c', contemporary_pandemic_response, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('943125cf-bf07-45f6-889e-9a2be1bafe5c', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, individual_rights_advocates).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who face loss of employment, access to services, or social participation due to state-imposed health mandates. They bear the direct cost of coercion, experiencing a violation of bodily integrity and autonomy. Their 'exit' often means severe personal and economic hardship.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals, payer,
    powerless, immediate, identity_locked, national).

% Government bodies responsible for public health, which, under this reading, are seen as overstepping their legitimate authority when imposing mandates without individual consent. They are the source of the coercive pressure, leveraging employment or access to services.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Organizations and legal experts who champion individual liberties and bodily autonomy. This reading aligns with their core mission, and they benefit from its legal and ethical reinforcement, even if they do not directly 'collect' from its operation.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, individual_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Officials who believe in balancing individual autonomy with public health needs, advocating for interventions proportional to the threat. Under this reading, their perspective is excluded as it prioritizes collective benefit over absolute individual consent.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, public_health_officials_proportionality_reading, excluded,
    institutional, biographical, constrained, national).

% Officials who prioritize population-level health outcomes, viewing individual refusal as an externality. Their perspective is fundamentally at odds with the bodily autonomy primary reading and is excluded from its framework.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, public_health_officials_public_health_primary, excluded,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates individual actions by establishing a clear boundary for state power, ensuring that medical interventions are undertaken only with informed consent, thereby fostering trust in medical institutions and protecting individual liberty.
% TRANSFER_FUNCTION: It transfers the locus of decision-making power for medical interventions from the state to the individual, preventing the state from extracting compliance through coercion. The 'cost' of this transfer is borne by public health efforts that might otherwise rely on mandates.
% ABSENT_VOICES: Public health officials operating under the 'public_health_primary' or 'proportionality_reading' are excluded. They would argue that this reading unduly restricts the state's ability to protect the population from communicable diseases, leading to greater societal harm.
% DISAPPEARANCE_RATIONALE: If this constraint (bodily autonomy as primary) disappeared, state public health authorities would likely expand their coercive powers, leading to more widespread mandates for interventions. This would fundamentally alter the relationship between citizens and the state regarding health decisions, with significant societal and ethical repercussions.
% FOUNDING_PROBLEM: The historical problem of state-sanctioned medical experimentation, forced sterilization, and other violations of individual bodily integrity, particularly against vulnerable populations, which led to the development of informed consent principles.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, bioethicists, and human rights organizations consistently corroborate that the threat to bodily autonomy from state overreach remains live, citing historical abuses and contemporary debates over mandates. This corroboration comes from sources outside state public health agencies.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant cost borne by individuals forced to comply with mandates (e.g., job loss, social exclusion). Suppression (0.7) is high because the state actively enforces these mandates through legal and economic leverage, limiting individual exit options. The theater ratio is low (0.1) as the constraint's operation is direct and functional in its impact on individuals, with little performative overhead. Resistance (0.8) is high, reflecting significant public and legal challenges to mandates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of mandate-coerced individuals, the constraint is a Snare, as it extracts compliance through coercion. From the perspective of individual rights advocates, it is a Mountain or Rope, representing an unchangeable principle or a necessary coordination mechanism for liberty. The engine's classification will reflect the victim's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Mandate-coerced individuals are the primary victims/payers, experiencing direct extraction and suppression (high d). State public health authorities are the agenda-setters, wielding the coercive power (low d, as they benefit from compliance). Individual rights advocates are beneficiaries, as this reading reinforces their core principles (low d). Sibling readings' proponents are excluded, as their frameworks are incompatible with this reading's foundational premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_coercion,
    'What constitutes ''state coercion'' in medical interventions? Does it include indirect pressures (e.g., social stigma, employer policies influenced by state guidance) or only direct legal mandates?',
    'Legal precedent and legislative definitions clarifying the boundaries of state action and its indirect effects on individual choice.',
    'A broader definition of coercion would increase the measured extractiveness and suppression, potentially strengthening the Snare classification. A narrower definition would reduce these metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_coercion, conceptual, 'Definitional ambiguity of ''state coercion'' in health policy.').

omega_variable(
    identity_lock_mechanism,
    'For ''mandate_coerced_individuals'', is the ''identity_locked'' exit option primarily due to professional identity (e.g., healthcare workers losing licenses) or social identity (e.g., exclusion from public life)?',
    'Qualitative sociological studies and legal analysis of specific mandate impacts, distinguishing between professional and social consequences.',
    'If primarily professional, the identity lock is more structural and tied to specific career paths. If primarily social, it highlights broader societal pressures that may persist even if formal mandates are lifted, suggesting a deeper, internalized suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Distinguishing professional vs. social identity lock for coerced individuals.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, job loss) or internalized (fear of social ostracization, self-censorship)?',
    'Post-exit suppression trajectory: if suppression persists after formal mandates are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for mandate compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legi_tr_t5, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(legi_tr_t10, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(legi_tr_t15, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 15, 0.1).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(legi_be_t5, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(legi_be_t10, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(legi_be_t15, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(legi_su_t5, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(legi_su_t10, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(legi_su_t15, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimate_health_intervention' kernel, focusing on bodily autonomy. It is distinct from the 'public_health_primary' and 'proportionality_reading' siblings, which offer alternative frameworks for legitimate state action in health.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
