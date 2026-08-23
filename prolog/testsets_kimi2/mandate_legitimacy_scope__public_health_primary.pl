% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: Vaccine Refusal Accommodation in Public Health Emergencies
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the public_health_primary reading of
 *   the mandate_legitimacy_scope kernel. The standing arrangement under
 *   contest is the institutionalized accommodation of vaccine refusal during
 *   public health emergencies â the legal and constitutional barriers that
 *   prevent state authorities from compelling vaccination even when
 *   epidemiological necessity is established. From this reading, the
 *   arrangement is a tangled rope: it genuinely coordinates a socially valued
 *   settlement around bodily autonomy and limits on state medical power, but
 *   it asymmetrically extracts health security from immunocompromised
 *   populations who cannot protect themselves through personal vaccination.
 *   The high extractiveness (0.78) reflects the reading's assessment that
 *   refusal accommodation in the presence of vulnerable populations
 *   constitutes serious harm externalization.
 *
 * KEY AGENTS:
 *   - immunocompromised_populations: Primary target (powerless/trapped) â bear epidemiological extraction
 *   - vaccine_refusers: Primary beneficiary (moderate/mobile) â autonomy protected at collective cost
 *   - judicial_authorities: Agenda-setter (institutional/analytical) â interpret and enforce autonomy limits
 *   - public_health_authorities: Secondary payer (institutional/constrained) â mission frustrated by legal barriers
 *   - civil_liberties_advocates: Secondary beneficiary (organized/mobile) â autonomy norms vindicated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.78).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.72).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "Vaccine Refusal Accommodation in Public Health Emergencies").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, 'f3ad286c-ef34-4315-91c7-4da150d18a3d').
narrative_ontology:cs_kernel_codification('f3ad286c-ef34-4315-91c7-4da150d18a3d', formalized).
narrative_ontology:cs_authority_grounding('f3ad286c-ef34-4315-91c7-4da150d18a3d', lineage).
narrative_ontology:cs_interpretation_layer_present('f3ad286c-ef34-4315-91c7-4da150d18a3d').
narrative_ontology:cs_reading_relation('f3ad286c-ef34-4315-91c7-4da150d18a3d', mandate_legitimacy_scope__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('f3ad286c-ef34-4315-91c7-4da150d18a3d', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('f3ad286c-ef34-4315-91c7-4da150d18a3d', foundational, collective_protection_overrides_bodily_autonomy).
narrative_ontology:cs_axiom_status(collective_protection_overrides_bodily_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('f3ad286c-ef34-4315-91c7-4da150d18a3d', collective_protection_overrides_bodily_autonomy, deontological).
narrative_ontology:cs_axiom('f3ad286c-ef34-4315-91c7-4da150d18a3d', foundational, state_duty_to_protect_vulnerable_populations).
narrative_ontology:cs_axiom_status(state_duty_to_protect_vulnerable_populations, holdable).
narrative_ontology:cs_axiom_grounding('f3ad286c-ef34-4315-91c7-4da150d18a3d', state_duty_to_protect_vulnerable_populations, deontological).
narrative_ontology:cs_reference_frame('f3ad286c-ef34-4315-91c7-4da150d18a3d', state_medical_compulsion_for_collective_protection).
narrative_ontology:cs_drift_state('f3ad286c-ef34-4315-91c7-4da150d18a3d', contemporary_autonomy_precedence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f3ad286c-ef34-4315-91c7-4da150d18a3d', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, vaccine_refusers).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, immunocompromised_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, civil_liberties_advocates).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, public_health_authorities).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, bodily_autonomy_doctrine).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, informed_consent_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in shared epidemiological environments where herd immunity is their primary protection; cannot mount effective immune responses to many vaccine-preventable diseases regardless of personal vaccination status; bear elevated infection risk when community vaccination rates fall below threshold.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, immunocompromised_populations, payer,
    powerless, immediate, trapped, national).

% Decline vaccination for personal, religious, or political reasons; are legally and socially permitted to refuse in jurisdictions where this constraint operates; benefit from autonomy protections while sharing air and infrastructure with vulnerable populations.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vaccine_refusers, beneficiary,
    moderate, biographical, mobile, national).

% Interpret constitutional provisions and human rights frameworks to set the boundary of legitimate state medical compulsion; their rulings determine whether public health mandates survive legal challenge.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, judicial_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Responsible for disease surveillance, outbreak response, and population health protection; are blocked by legal and judicial constraints from mandating vaccination in many contexts where epidemiological models indicate necessity; bear operational and reputational costs when preventable outbreaks occur.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, public_health_authorities, payer,
    institutional, generational, constrained, national).

% Organize legal and political opposition to vaccine mandates; frame compulsory vaccination as state overreach and bodily autonomy violation; benefit from institutional and cultural validation of their position when refusal accommodation is upheld.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, civil_liberties_advocates, beneficiary,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of defining the boundary between individual medical autonomy and state police power; establishes a predictable legal settlement limiting government compulsion of medical interventions.
% TRANSFER_FUNCTION: Transfers epidemiological risk from the general population and vaccine-refusing individuals to immunocompromised and medically vulnerable populations who cannot achieve protective immunity regardless of vaccination status.
% ABSENT_VOICES: Immunocompromised individuals and their caregivers are frequently absent from legislative and constitutional debates over mandate boundaries; their testimony is often solicited only after outbreaks occur rather than during norm-setting.
% DISAPPEARANCE_RATIONALE: If the constraint on state compulsion vanished, public health authorities would deploy vaccine mandates in outbreak settings, the legal status of refusal would shift from protected right to sanctionable conduct, and vulnerable populations would gain structural protection at the expense of expanded state medical power.
% FOUNDING_PROBLEM: Historical state abuse of medical power, including coerced sterilization, non-consensual experimentation, and unchecked police power over bodily integrity, created legitimate demand for constitutional and ethical limits on government medical compulsion.
% FOUNDING_PROBLEM_CORROBORATION: Medical historians and human rights institutions attest the founding problem was real; however, public health ethicists and immunocompromised patient advocates attest that modern regulatory safeguards and tested vaccines have substantially addressed the founding problem, and the constraint now operates beyond its original protective purpose.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint permits vaccine refusers to externalize infection risk onto immunocompromised populations who cannot achieve immunity. Suppression is substantial (0.72) because the constraint's persistence depends on active judicial enforcement of autonomy limits against public health mandates, not merely on spontaneous coordination. Theater ratio is moderate-high (0.48): while the autonomy settlement has genuine normative weight, a growing share of public discourse defends refusal through performative freedom rhetoric that obscures epidemiological externalities. Accessibility collapse is high (0.75) because vulnerable populations have no practical alternative to sharing air and infrastructure with unvaccinated individuals. Resistance is significant (0.62) from public health authorities, patient advocates, and epidemiologists who contest the current balance.
 *
 * PERSPECTIVAL GAP:
 *   The immunocompromised seat and the vaccine-refuser seat compute radically different constraint types from identical structural data: the refuser experiences low directionality (d near 0.2) because the constraint subsidizes their autonomy preference, yielding low effective extraction or even negative extraction (subsidy); the immunocompromised seat experiences high directionality (d near 0.9) because the constraint traps them in a shared epidemiological environment they cannot exit, amplifying effective extraction to near the ceiling. Judicial authorities sit near symmetric (d ~0.5) because they are neither net beneficiaries nor net targets â they administer the constraint through interpretive labor.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to vaccine_refusers (autonomy protection) and civil_liberties_advocates (norm vindication); victim declarations map to immunocompromised_populations (health security extraction). The public_health_authorities are declared as payers because they bear the operational and political costs of failed outbreak control, though they are not in the base victims array. Exit options differentiate the seats sharply: vaccine_refusers are mobile (can relocate, can choose to vaccinate if preferences change), while immunocompromised populations are trapped (cannot exit the epidemiological commons). This exit asymmetry drives the engine's per-seat classification divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mandatrophic mislabeling because its founding problem â state medical abuse â is contested rather than dead. A genuine piton would show no concentrated beneficiary and no live founding narrative; here, vaccine_refusers actively benefit and civil liberties advocates maintain the normative story. A snare reading would require that the autonomy coordination be pure cover; the authored theater_ratio (0.48) and the honest coordination_function (legal settlement of state power boundaries) prevent that reduction. The constraint is therefore a tangled rope: real coordination genuinely provided, real extraction asymmetrically borne.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the public_health_primary reading of kernel mandate_legitimacy_scope. Would classification change if the bodily_autonomy_primary or proportionality_reading were adopted instead?',
    'Compare the resulting constraint families: bodily_autonomy_primary would likely classify mandate presence as snare; proportionality_reading would yield context-dependent scaffold or tangled_rope assessments.',
    'If the kernel is read through bodily_autonomy_primary, the victim and beneficiary sets invert; the same institutional arrangement emits opposite directionalities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Sibling reading structural divergence for mandate legitimacy kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal and constitutional barriers to mandates) or internalized (professional identity among clinicians prioritizing autonomy over collective protection)?',
    'Track whether mandate authority expands when legal barriers are removed (structural) or whether medical professionals resist exercising expanded authority despite legal permission (internalized).',
    'If internalized, the constraint''s effective suppression exceeds the structural measure â the autonomy norm persists even when legal barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in public health autonomy norms').

omega_variable(
    coordination_extraction_separability,
    'Is the bodily autonomy coordination produced by this constraint separable from its epidemiological extraction, or are they structurally fused?',
    'Natural experiment from jurisdictions with strict mandates plus robust informed-consent processes: if autonomy is respected through procedural means while compulsion is exercised, the functions are separable.',
    'If separable, the constraint is tangled rope; if fused, the autonomy story is cover for a snare that extracts health from vulnerable populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether autonomy coordination and epidemiological extraction are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mlphp_tr_t0, mandate_legitimacy_scope__public_health_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mlphp_tr_t4, mandate_legitimacy_scope__public_health_primary, theater_ratio, 4, 0.25).
narrative_ontology:measurement(mlphp_tr_t8, mandate_legitimacy_scope__public_health_primary, theater_ratio, 8, 0.32).
narrative_ontology:measurement(mlphp_tr_t12, mandate_legitimacy_scope__public_health_primary, theater_ratio, 12, 0.38).
narrative_ontology:measurement(mlphp_tr_t16, mandate_legitimacy_scope__public_health_primary, theater_ratio, 16, 0.42).
narrative_ontology:measurement(mlphp_tr_t20, mandate_legitimacy_scope__public_health_primary, theater_ratio, 20, 0.46).
narrative_ontology:measurement(mlphp_tr_t24, mandate_legitimacy_scope__public_health_primary, theater_ratio, 24, 0.48).

% Extraction over time
narrative_ontology:measurement(mlphp_be_t0, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(mlphp_be_t4, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(mlphp_be_t8, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(mlphp_be_t12, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(mlphp_be_t16, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 16, 0.72).
narrative_ontology:measurement(mlphp_be_t20, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(mlphp_be_t24, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(mlphp_su_t0, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(mlphp_su_t4, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(mlphp_su_t8, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(mlphp_su_t12, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(mlphp_su_t16, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(mlphp_su_t20, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(mlphp_su_t24, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, identity_coordination).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is the public_health_primary reading of the mandate_legitimacy_scope kernel, focusing on the extractive impact of refusal-accommodation norms when state compulsion would protect vulnerable populations. Sibling readings instantiate structurally distinct constraints from the same legal-constitutional kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
