% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__public_health_primary, []).

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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public Health Mandate Authority â Public Health Primary Reading
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the public_health_primary reading of
 *   the public_health_mandate_authority kernel. The standing arrangement
 *   under contest is the exercise of state public health authority to impose
 *   medical or protective mandates justified as protecting immunocompromised
 *   populations and healthcare infrastructure. In this reading,
 *   immunocompromised populations are beneficiaries of collective action,
 *   while mandate-resistant individuals who face employment exclusion and
 *   service loss are the structural payers. The unvaccinated are excluded
 *   from the victim set and reframed as free-riders imposing externalized
 *   risk. Sibling readings (bodily_autonomy_primary, proportionality_reading)
 *   would reseat victims and beneficiaries or condition the constraint on
 *   contextual threat severity.
 *
 * KEY AGENTS:
 *   - public_health_authority: Primary agenda-setter (institutional/analytical) â administers mandate policy and enforcement machinery.
 *   - immunocompromised_population: Primary beneficiary (powerless/trapped) â cannot exit medical vulnerability, benefits from reduced transmission when compliance is high.
 *   - healthcare_system: Secondary beneficiary (institutional/constrained) â receives reduced surge and preserved capacity.
 *   - mandate_resistant_individuals: Primary target (moderate/constrained) â bear extraction via employment loss and service exclusion.
 *   - bodily_autonomy_advocates: Excluded voice (organized/constrained) â contest the framing but are backgrounded in the collective-obligation calculus.
 *   - constitutional_courts: Analytical observer (institutional/analytical) â review proportionality and police-power limits without bearing costs directly.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.72).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.76).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate Authority â Public Health Primary Reading").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, '1b0dde37-b3f9-4091-955b-f9800a676e5e').
narrative_ontology:cs_kernel_codification('1b0dde37-b3f9-4091-955b-f9800a676e5e', formalized).
narrative_ontology:cs_authority_grounding('1b0dde37-b3f9-4091-955b-f9800a676e5e', expertise).
narrative_ontology:cs_interpretation_layer_present('1b0dde37-b3f9-4091-955b-f9800a676e5e').
narrative_ontology:cs_reading_relation('1b0dde37-b3f9-4091-955b-f9800a676e5e', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('1b0dde37-b3f9-4091-955b-f9800a676e5e', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('1b0dde37-b3f9-4091-955b-f9800a676e5e', foundational, vulnerable_commons_protection_obligation).
narrative_ontology:cs_axiom_status(vulnerable_commons_protection_obligation, holdable).
narrative_ontology:cs_axiom_grounding('1b0dde37-b3f9-4091-955b-f9800a676e5e', vulnerable_commons_protection_obligation, deontological).
narrative_ontology:cs_axiom('1b0dde37-b3f9-4091-955b-f9800a676e5e', foundational, individual_noncompliance_as_externality).
narrative_ontology:cs_axiom_status(individual_noncompliance_as_externality, holdable).
narrative_ontology:cs_axiom_grounding('1b0dde37-b3f9-4091-955b-f9800a676e5e', individual_noncompliance_as_externality, conventional).
narrative_ontology:cs_reference_frame('1b0dde37-b3f9-4091-955b-f9800a676e5e', vulnerable_commons_protection_frame).
narrative_ontology:cs_drift_state('1b0dde37-b3f9-4091-955b-f9800a676e5e', post_emergency_transition, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1b0dde37-b3f9-4091-955b-f9800a676e5e', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_population).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_system).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces public health mandates (vaccination, masking, testing) through employment restrictions and service exclusions. Justifies coercion as necessary to protect immunocompromised populations and healthcare infrastructure from communicable disease transmission. Administers compliance infrastructure and penalty frameworks.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Cannot exit medical vulnerability. Benefits from reduced community transmission when mandate compliance is high, which lowers personal exposure risk and preserves access to healthcare services that would be rationed during surges. Dependent on enforcement continuity for protection.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_population, beneficiary,
    powerless, biographical, trapped, national).

% Receives reduced patient volume and acuity during outbreaks when mandates are effective, preserving staffing and capacity. Operates within the policy framework and depends on collective compliance to avoid collapse. Does not set the mandate but benefits from its enforcement.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_system, beneficiary,
    institutional, biographical, constrained, national).

% Face termination of employment, exclusion from public accommodations, and loss of social participation due to non-compliance with medical mandates. Experience the constraint as coercive extraction of bodily autonomy and livelihood. Limited exit options because geographic relocation or sector switching carries comparable compliance costs.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals, payer,
    moderate, immediate, constrained, national).

% Contest the legitimacy of mandates on sovereignty and informed-consent grounds. Within the public_health_primary reading, their objections are backgrounded and the unvaccinated are reframed as free-riders imposing externalized risk rather than as rights-bearers. They are not seated in the beneficiary-victim calculus.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, bodily_autonomy_advocates, excluded,
    organized, generational, constrained, national).

% Review mandate proportionality, constitutional limits on police power, and rights-balancing tests. They take testimony from affected seats and can impose narrowing remedies, but do not themselves bear mandate costs or benefits.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__public_health_primary, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects the vulnerable commons â immunocompromised populations and healthcare infrastructure â from communicable disease transmission by solving the collective-action problem of individual vaccination and protective behavior through centralized enforcement.
% TRANSFER_FUNCTION: Moves compliance burden (medical intervention acceptance, documentation, exclusion from non-compliant spaces) and risk of livelihood loss from mandate-resistant individuals toward immunocompromised populations and healthcare infrastructure in the form of reduced transmission exposure and preserved capacity.
% ABSENT_VOICES: Bodily autonomy advocates and mandate-resistant individuals contesting the externality framing are structurally excluded from the victim set; their objections are treated as individual preference rather than legitimate rights-claims within the collective-obligation frame.
% DISAPPEARANCE_RATIONALE: If the mandate authority vanished overnight, immunocompromised populations would face elevated exposure risk, healthcare systems would lose the collective compliance mechanism that flattens epidemic curves, and the governance of infectious disease would revert to individual voluntarism â the vulnerable-commons protection function would unwind rapidly during outbreaks.
% FOUNDING_PROBLEM: Communicable disease outbreaks that exploit dense social interaction to overwhelm healthcare infrastructure and disproportionately kill or harm immunocompromised and vulnerable populations who cannot protect themselves through individual action alone.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological surveillance institutions and immunocompromised patient-advocacy groups outside the direct mandate-beneficiary relationship independently attest to ongoing vulnerability. Constitutional courts sometimes corroborate the live threat while contesting the mandate mechanism, confirming the problem persists even when the solution is disputed.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint imposes severe, concentrated costs (livelihood loss, exclusion) on a discrete population to generate diffuse health benefits. Suppression (0.76) is high because persistence depends on active enforcement â employer mandates, service exclusions, and legal penalties. Theater ratio (0.36) is moderate: a growing share of compliance activity becomes performative (credential-checking theater, ritual masking) rather than epidemiologically targeted. Accessibility collapse (0.70) is high because once the mandate framework is institutionalized, alternatives (voluntary harm reduction without employment loss) collapse as policy options. Resistance (0.60) reflects organized legal and political opposition. The claim/metric independence is maintained: the reading claims a coordination function (protecting vulnerable commons) while the metrics describe the substantial extraction required to maintain it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (public_health_authority) experiences the constraint as legitimate expertise-based coordination; the payer seat (mandate_resistant_individuals) experiences it as coercive extraction of bodily autonomy and livelihood. The beneficiary seats (immunocompromised, healthcare_system) experience reduced risk but also dependence on enforcement continuity. The engine computes these divergences from structural data: same constraint, different computed classifications per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (immunocompromised_population, healthcare_system) derive low directionality â the constraint subsidizes their safety and system stability. Payers (mandate_resistant_individuals) derive high directionality â the constraint extracts compliance and livelihood. The agenda_setter (public_health_authority) derives low directionality as the orchestrating beneficiary. Excluded observers (bodily_autonomy_advocates) would face high directionality if seated inside the constraint. Exit modulation separates the immunocompromised (trapped in biological vulnerability) from the mandate-resistant (constrained by policy but geographically mobile if they accept compliance).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by declaring both the coordination function (vulnerable commons protection) and the extraction mechanism (employment/service exclusion of resisters). Without this dual declaration, the constraint could be misread as pure coordination (rope) by beneficiaries or pure extraction (snare) by resisters. The active enforcement requirement and presence of both beneficiaries and victims structurally identify it as tangled_rope rather than scaffold or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is the public_health_primary reading of the public_health_mandate_authority kernel. Sibling readings (bodily_autonomy_primary, proportionality_reading) reseat victims and beneficiaries. Does the structural classification survive if the kernel is read through bodily autonomy or proportionality lenses?',
    'Cross-jurisdictional comparison where different readings dominate policy and jurisprudence.',
    'Under bodily_autonomy_primary, the same enforcement structure computes as a snare with public_health_authority as beneficiary and mandate_resistant_individuals as sovereignty victims; under proportionality_reading, classification becomes context-dependent and potentially scaffold-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Committer-frame uncertainty: sibling reading effects on classification').

omega_variable(
    externality_quantification,
    'What is the marginal contribution of mandate-resistant individuals to transmission risk for immunocompromised populations relative to other vectors (waning immunity, novel variants, behavioral relaxation)?',
    'High-resolution contact-tracing, seroprevalence studies, and comparative epidemiological analysis across mandate and non-mandate jurisdictions.',
    'If marginal contribution is negligible, the coordination story weakens and the extraction story strengthens, pushing effective extraction upward; if substantial, the coordination function is empirically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_quantification, empirical, 'Empirical grounding of the free-rider externality claim').

omega_variable(
    enforcement_threat_tracking,
    'Does enforcement intensity track epidemiological threat levels, or does it persist by institutional inertia after threat subsidence?',
    'Time-series regression of enforcement actions, employer mandate audits, and service-exclusion citations against contemporaneous case rates and hospitalization metrics.',
    'If enforcement persists independent of threat, the constraint is drifting toward piton (inertial enforcement) or snare (extractive persistence), raising theater_ratio and mandatrophy flags.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_threat_tracking, empirical, 'Whether enforcement tracks threat or institutional inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__public_health_primary, theater_ratio, 6, 0.18).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__public_health_primary, theater_ratio, 12, 0.25).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__public_health_primary, theater_ratio, 18, 0.3).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__public_health_primary, theater_ratio, 24, 0.33).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__public_health_primary, theater_ratio, 30, 0.35).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__public_health_primary, theater_ratio, 36, 0.36).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__public_health_primary, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__public_health_primary, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__public_health_primary, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__public_health_primary, base_extractiveness, 24, 0.69).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__public_health_primary, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__public_health_primary, base_extractiveness, 36, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__public_health_primary, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__public_health_primary, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__public_health_primary, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__public_health_primary, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__public_health_primary, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__public_health_primary, suppression_requirement, 36, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
