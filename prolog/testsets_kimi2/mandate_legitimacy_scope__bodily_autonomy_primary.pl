% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__bodily_autonomy_primary, []).

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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Mandatory Medical Intervention Regime â Bodily Autonomy Reading
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story models the mandatory medical intervention regime
 *   through the bodily_autonomy_primary reading of the
 *   mandate_legitimacy_scope kernel. The arrangement requires individuals to
 *   undergo medical intervention regardless of informed refusal, enforced by
 *   the state through legal penalties and exclusion. This reading treats the
 *   mandate as a structural violation of bodily integrity: the
 *   unvaccinated-coerced are victims of state power, and the public health
 *   justification, while positing a genuine coordination function, is
 *   normatively and structurally insufficient to justify the extraction. The
 *   reading asserts that the constraint is a tangled ropeâgenuine
 *   disease-prevention coordination braided with asymmetric bodily
 *   extractionârather than a legitimate rope or a pure snare.
 *
 * KEY AGENTS:
 *   - public_health_authority: Agenda-setter and beneficiary (institutional power, mobile exit) â administers and gains institutional scope from enforcement.
 *   - unvaccinated_coerced: Primary target and victim (powerless, trapped exit) â bears the direct extraction of compelled bodily intrusion.
 *   - compliant_citizenry: Beneficiary (organized, constrained exit) â receives perceived protection and retains civic access without direct extraction.
 *   - civil_liberties_observers: Analytical observer (organized, analytical exit) â challenges the mandate from outside the benefiting parties.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.83).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.78).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.83).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Mandatory Medical Intervention Regime â Bodily Autonomy Reading").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, '30b65d11-4da2-4239-94de-f5658da5000e').
narrative_ontology:cs_kernel_codification('30b65d11-4da2-4239-94de-f5658da5000e', formalized).
narrative_ontology:cs_authority_grounding('30b65d11-4da2-4239-94de-f5658da5000e', lineage).
narrative_ontology:cs_interpretation_layer_present('30b65d11-4da2-4239-94de-f5658da5000e').
narrative_ontology:cs_reading_relation('30b65d11-4da2-4239-94de-f5658da5000e', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('30b65d11-4da2-4239-94de-f5658da5000e', mandate_legitimacy_scope__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('30b65d11-4da2-4239-94de-f5658da5000e', foundational, bodily_integrity_non_derogable).
narrative_ontology:cs_axiom_status(bodily_integrity_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('30b65d11-4da2-4239-94de-f5658da5000e', bodily_integrity_non_derogable, deontological).
narrative_ontology:cs_reference_frame('30b65d11-4da2-4239-94de-f5658da5000e', bodily_autonomy_sovereignty).
narrative_ontology:cs_drift_state('30b65d11-4da2-4239-94de-f5658da5000e', contemporary_mandate_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('30b65d11-4da2-4239-94de-f5658da5000e', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_authority).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, compliant_citizenry).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, informed_consent_doctrine).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, bodily_integrity_absolutism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers compulsory medical intervention programs under statutory and police power. Expands regulatory precedent through enforcement mechanisms including employment exclusion, fines, and movement restrictions. Collects compliance data and institutional mandate expansion.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_authority, agenda_setter,
    institutional, generational, mobile, national).

% Individuals refusing medical intervention on conscience, medical, or religious grounds who are compelled by law. Face loss of livelihood, exclusion from public accommodations, social ostracism, or physical restraint. Their refusal is overridden by state power; exit requires geographic flight or submission to unwanted bodily intrusion.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced, payer,
    powerless, biographical, trapped, national).

% Individuals who accept the intervention voluntarily or acquiesce under social pressure. They experience reduced perceived disease risk and retain full civic access. They do not bear the direct bodily extraction but may incur indirect costs if the intervention carries undisclosed risks.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, compliant_citizenry, beneficiary,
    organized, biographical, constrained, national).

% Legal advocacy groups and constitutional scholars who challenge mandate legitimacy on substantive due process and informed consent grounds. They litigate on behalf of the coerced, document enforcement overreach, and sit outside the benefit stream of the mandate.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, civil_liberties_observers, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_authority).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__bodily_autonomy_primary, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves high population coverage of a medical intervention to reduce infectious disease transmission and protect those who cannot be immunized, centralizing compliance under a single authority rather than diffuse voluntary action.
% TRANSFER_FUNCTION: Moves bodily autonomy and informed consent from the refusing individual to the state's public health authority, substituting state compulsion for voluntary medical decision-making in exchange for claimed collective risk reduction.
% ABSENT_VOICES: Individuals with medical contraindications poorly served by one-size-fits-all mandates, religious communities facing narrow exemption windows, and dissenting clinicians who perceive risk-benefit asymmetry but are excluded from policy advisory roles.
% DISAPPEARANCE_RATIONALE: If the mandate regime vanished, the unvaccinated would regain unrestricted employment and movement; the public health authority would lose a primary enforcement tool and institutional precedent; the compliant citizenry's risk environment would shift toward voluntary uptake models; civil liberties frameworks would re-center around informed consent rather than exemption petitions.
% FOUNDING_PROBLEM: Controlling epidemic infectious disease when voluntary medical intervention uptake is insufficient to prevent serious harm to vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and medical historians attest to the historical severity of epidemic disease. Constitutional scholars and international human rights monitors from outside the public health authority attest that the mandate scope and coercion level exceed what the founding problem proportionally justifies, and that less restrictive alternatives were available.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.83, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.83) because the constraint overrides informed consent and compels bodily intrusion, a severe extraction of autonomy. Suppression is high (0.78) because persistence depends on active legal and social enforcement: employment termination, movement restrictions, and exclusion. Theater ratio is moderate (0.45) because while the disease-prevention function is real, an increasing share of enforcement activity defends the mandate's legitimacy against dissent rather than addressing emergent disease dynamics. Accessibility collapse is substantial (0.75) because refusal is heavily penalized and alternatives such as targeted protection or voluntary education are structurally marginalized. Resistance is significant (0.70) owing to sustained legal challenges, protest, and noncompliance. The metric trajectory shows extraction and suppression rising as the mandate expands from targeted groups to universal application, then plateauing as enforcement capacity peaks and partial rollback begins.
 *
 * PERSPECTIVAL GAP:
 *   The public_health_authority seat experiences the constraint as necessary coordination protecting the vulnerable; its computed classification will trend toward rope or tangled-rope with low directionality. The unvaccinated_coerced seat experiences the identical arrangement as direct bodily violation; its computed classification will trend toward snare with high directionality. The engine derives this divergence from the structural asymmetry in power, exit options, and beneficiary-victim declarationsânot from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The public_health_authority is declared as beneficiary (low directionality, near 0.0) because it collects institutional scope and compliance. The compliant_citizenry is also a beneficiary (low-moderate directionality) because it receives the coordination good without bearing the coerced extraction. The unvaccinated_coerced is declared as victim (high directionality, near 1.0) because the constraint extracts bodily autonomy directly from them through legal compulsion. No override is needed: the structural derivation chain produces the correct directionalities from the combination of role declarations and exit options (trapped vs. mobile/constrained).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâepidemic infectious disease threatening vulnerable populationsâmay resolve or attenuate while the mandate apparatus persists. The T17 abductive trigger would fire if base_extractiveness continues to rise after the epidemiological threat declines, signaling extraction accumulation. Under this reading, the mandate is illegitimate from inception, but the framework still distinguishes a live founding problem (legitimate emergency, even if overridden by rights claims) from a dead one (theater-maintained coercion after the emergency passes). The measurement series is designed to capture this lifecycle drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_mandate_legitimacy_scope_reading,
    'This constraint is the bodily_autonomy_primary reading of the mandate_legitimacy_scope kernel. How would the structural classification change if the proportionality_reading or public_health_primary reading were adopted instead?',
    'Evaluate the sibling constraint stories in the same kernel family; the engine''s cross-reading divergence metric measures the structural gap between the absolute-rights framing and the conditional-legitimacy framings.',
    'If the proportionality reading is more accurate, effective extraction may be lower and directionality more symmetric. If the public_health_primary reading is more accurate, the constraint may compute as rope or scaffold with victim set empty, altering the entire classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_mandate_legitimacy_scope_reading, conceptual, 'Kernel reading location and sibling structural delta for mandate legitimacy scope').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of bodily refusal structural (legal penalties and exclusion) or internalized (medical paternalism, social stigma, and trust in authority)?',
    'Post-mandate trajectory analysis: if refusal rates and autonomy assertions remain suppressed after legal penalties are removed, the suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint functions partly through identity_coordination rather than mere enforcement mechanism, amplifying the theater_ratio and altering the drift path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in medical mandate context').

omega_variable(
    public_health_benefit_genuineness,
    'Does the mandate regime produce a genuine coordination benefit (disease reduction) that is structurally inseparable from the coercive mechanism, or is the benefit achievable by less restrictive means?',
    'Comparative epidemiological and economic analysis of jurisdictions with voluntary versus mandatory regimes, controlling for baseline health infrastructure and population risk profiles.',
    'If the benefit is genuine and inseparable, the constraint remains tangled_rope; if achievable without coercion, the coordination story is cover and reclassification toward snare is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_health_benefit_genuineness, empirical, 'Whether public health benefit is genuine and inseparable from coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mand_tr_t5, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 5, 0.25).
narrative_ontology:measurement(mand_tr_t10, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 10, 0.33).
narrative_ontology:measurement(mand_tr_t15, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 15, 0.4).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 20, 0.42).
narrative_ontology:measurement(mand_tr_t25, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mand_be_t5, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(mand_be_t10, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(mand_be_t15, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 15, 0.8).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(mand_be_t25, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 25, 0.83).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(mand_su_t5, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(mand_su_t10, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(mand_su_t15, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(mand_su_t25, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 25, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel mandate_legitimacy_scope. The kernel decomposes into three structurally distinct constraints because each reading assigns a different epsilon, beneficiary/victim structure, and classification to the same mandate regime. This reading (bodily_autonomy_primary) treats the mandate as asymmetric extraction violating an absolute right; the sibling readings treat it as conditional coordination or legitimate public health authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
