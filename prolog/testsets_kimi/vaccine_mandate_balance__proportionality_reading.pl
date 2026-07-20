% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Proportionality-Conditioned Vaccine Mandate Regime
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the proportionality reading of the
 *   vaccine_mandate_balance kernel. It governs state vaccination mandates
 *   that are permissible only when disease severity, transmission risk, and
 *   vaccine safety meet strict proportionality thresholds and when exemptions
 *   are robust. Unlike the categorical public_health_primary reading, it
 *   conditions compulsion on contextual evidence; unlike the categorical
 *   bodily_autonomy_primary reading, it accepts that bodily integrity can be
 *   overridden by sufficient state interest. The expected structural delta is
 *   that both beneficiary and victim sets are conditional on disease
 *   parameters, so effective extraction varies sharply by pathogen (e.g.,
 *   smallpox versus seasonal influenza). The authored metrics and claimed
 *   type are independent: the constraint is structurally a tangled rope
 *   because it combines genuine coordination (herd protection) with
 *   asymmetric extraction (compelled medical intervention and exemption
 *   burdens) and requires active enforcement.
 *
 * KEY AGENTS:
 *   - public_health_authorities: Agenda-setter (institutional/constrained) â sets proportionality thresholds and enforces mandates
 *   - compelled_individuals: Primary target (moderate/constrained) â bears compelled medical intervention
 *   - exemption_burdened: Secondary target (moderate/constrained) â bears administrative and social costs of exemption
 *   - vulnerable_populations: Primary beneficiary (powerless/trapped) â receives protection from community coverage
 *   - community_herd_beneficiaries: Secondary beneficiary (moderate/mobile) â receives reduced outbreak risk
 *   - constitutional_judiciary: Analytical observer (institutional/analytical) â adjudicates proportionality
 *   - civil_libertarian_dissidents: Excluded voice (organized/constrained) â rejects framework's foundational premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.48).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.45).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Proportionality-Conditioned Vaccine Mandate Regime").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, '5f6c37e4-4e3c-4ead-9326-cedaf32ce5aa').
narrative_ontology:cs_kernel_codification('5f6c37e4-4e3c-4ead-9326-cedaf32ce5aa', formalized).
narrative_ontology:cs_authority_grounding('5f6c37e4-4e3c-4ead-9326-cedaf32ce5aa', lineage).
narrative_ontology:cs_interpretation_layer_present('5f6c37e4-4e3c-4ead-9326-cedaf32ce5aa').
narrative_ontology:cs_reading_relation('5f6c37e4-4e3c-4ead-9326-cedaf32ce5aa', vaccine_mandate_balance__public_health_primary, influences).
narrative_ontology:cs_reading_relation('5f6c37e4-4e3c-4ead-9326-cedaf32ce5aa', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_axiom('5f6c37e4-4e3c-4ead-9326-cedaf32ce5aa', foundational, autonomy_as_qualified_right).
narrative_ontology:cs_axiom_status(autonomy_as_qualified_right, holdable).
narrative_ontology:cs_axiom_grounding('5f6c37e4-4e3c-4ead-9326-cedaf32ce5aa', autonomy_as_qualified_right, conventional).
narrative_ontology:cs_axiom('5f6c37e4-4e3c-4ead-9326-cedaf32ce5aa', foundational, mandate_requires_strict_proportionality).
narrative_ontology:cs_axiom_status(mandate_requires_strict_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('5f6c37e4-4e3c-4ead-9326-cedaf32ce5aa', mandate_requires_strict_proportionality, conventional).
narrative_ontology:cs_reference_frame('5f6c37e4-4e3c-4ead-9326-cedaf32ce5aa', constitutional_proportionality_framework).
narrative_ontology:cs_drift_state('5f6c37e4-4e3c-4ead-9326-cedaf32ce5aa', post_emergency_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5f6c37e4-4e3c-4ead-9326-cedaf32ce5aa', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, community_herd_beneficiaries).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, compelled_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, exemption_burdened).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, constitutional_proportionality_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, state_police_power_limits).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the epidemiological and safety thresholds that activate mandates, administer enforcement mechanisms, and defend proportionality in court. Their authority is bounded by judicial review and political accountability; they cannot exit the legal framework but can adjust thresholds within it.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Subject to compulsory vaccination when proportionality thresholds are declared met. Must comply or pursue costly legal challenges; noncompliance triggers fines, employment exclusion, or social sanctions. Exit is constrained by legal and economic barriers.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, compelled_individuals, payer,
    moderate, biographical, constrained, national).

% Individuals who seek robust exemptions on medical, religious, or philosophical grounds. Bear administrative costs of documentation, face discretionary denial, and may encounter professional or social stigma even when exempted.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, exemption_burdened, payer,
    moderate, biographical, constrained, national).

% Immunocompromised or elderly persons who cannot mount full vaccine responses and depend on community coverage for protection. They benefit from reduced exposure risk but have no exit from their medical vulnerability or from the public health system.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% General population members who receive reduced outbreak risk from higher vaccination coverage without directly paying compliance costs. Their benefit is diffuse and they can often exit geographically or socially.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, community_herd_beneficiaries, beneficiary,
    moderate, biographical, mobile, national).

% Reviews whether mandates and their enforcement meet strict proportionality requirements. Strikes down or upholds mandates based on evidence of severity, transmission risk, and safety. Occupies an analytical seat independent of health outcomes.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, constitutional_judiciary, observer,
    institutional, generational, analytical, national).

% Advocate for categorical bodily autonomy and reject any state medical compulsion. Structurally excluded from proportionality frameworks because the framework itself presumes conditional state authority over bodily integrity; their objections are heard in dissent but not in threshold-setting.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, civil_libertarian_dissidents, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__proportionality_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents infectious disease outbreaks from exceeding healthcare capacity and protects those who cannot be immunized, by achieving vaccination coverage sufficient for community protection when voluntary uptake falls below threshold, while attempting to limit state overreach through evidence-based proportionality tests.
% TRANSFER_FUNCTION: Transfers compelled medical compliance and administrative exemption burdens from individuals to state public health objectives; transfers reduced infection risk and healthcare system stabilization to vulnerable and general populations.
% ABSENT_VOICES: Civil libertarians who reject any state bodily compulsion; individuals with prior adverse vaccine reactions who distrust the robustness of exemptions; and future populations who may face normalized mandate precedent. These voices are structurally underrepresented because the proportionality framework presumes the state's conditional authority to compel.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished overnight, jurisdictions would polarize toward categorical prohibition of mandates (bodily_autonomy_primary) or categorical public-health supremacy (public_health_primary). The current conditional equilibrium would dissolve, and constitutional public health law would reorganize around one of the two sibling readings.
% FOUNDING_PROBLEM: Infectious disease outbreaks that exceed healthcare capacity and expose vulnerable populations to lethal risk when voluntary vaccination uptake fails to achieve community protection.
% FOUNDING_PROBLEM_CORROBORATION: Public health historians and epidemiologists attest to historical severity; however, civil liberties organizations and medical ethicists outside the direct beneficiary set contest whether current pathogens meet the threshold. The strict proportionality requirement is corroborated by constitutional jurisprudence (e.g., German Federal Constitutional Court, ECHR) rather than by public health beneficiaries alone.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.48, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.48) is moderate because strict proportionality thresholds and robust exemptions limit the scope of compulsion, though they do not eliminate it. Suppression (0.45) is moderate: enforcement is real (fines, exclusions) but bounded by judicial review and exemption pathways. Theater ratio (0.38) reflects that as emergencies fade, a growing share of enforcement activity becomes performative compliance rather than responsive to live epidemiological data. Accessibility collapse (0.40) is moderate because legal alternatives (exemption, litigation) remain open but are costly. Resistance (0.55) is substantial because anti-mandate movements and civil liberties litigation are persistent. The temporal arc shows extraction and suppression rising during emergency peak (T=12) and partially normalizing afterward, while theater rises as justification thins.
 *
 * PERSPECTIVAL GAP:
 *   The compelled individual experiences the constraint as state extraction of bodily autonomy (high directionality, high extraction). The vulnerable patient experiences it as life-protecting coordination (low directionality, negative effective extraction). The constitutional judiciary experiences it as a doctrinal balancing test. The engine computes these divergent per-seat classifications from the same structural data; the story does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations and community herd beneficiaries occupy the beneficiary side: they receive protective externalities without paying the direct cost of compliance, yielding low directionality. Compelled individuals and exemption-burdened parties occupy the victim side: they bear the direct costs of bodily compulsion and administrative burden, yielding high directionality. Public health authorities are agenda-setters who gain institutional authority and budget but also bear political and legal liability; their structural position is mixed and closer to symmetric. The constitutional judiciary is an analytical observer with no extractive stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâinsufficient voluntary vaccination during severe outbreaksâmay be live for high-severity pathogens but dead for low-severity ones. The constraint risks mandatrophy if proportionality thresholds are applied mechanically to seasonal pathogens or if robust exemptions are bureaucratically eroded. The T17 theater trajectory and the R5 founding-problem/dead+disappearance-verdict mismatch flags would trigger if the framework outlives its contextual justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pathogen_conditional_epsilon,
    'Does the proportionality threshold genuinely modulate extraction by pathogen severity, or does institutional momentum sustain mandates even when disease parameters are low?',
    'Cross-pathogen comparison of mandate persistence against contemporaneous severity and transmission metrics; judicial review outcomes distinguishing high-severity from low-severity scenarios.',
    'If extraction persists independent of severity, the proportionality framework functions as theater and the constraint migrates toward snare or piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathogen_conditional_epsilon, empirical, 'Whether extractiveness is truly pathogen-conditional').

omega_variable(
    exemption_robustness_reality,
    'Are declared robust exemptions substantively accessible or procedural theater?',
    'Exemption grant rates, administrative burden measurements, and post-exemption retaliation tracking across jurisdictions.',
    'If exemptions are illusory, effective extraction rises and the constraint collapses toward the public_health_primary sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_robustness_reality, empirical, 'Substantive versus illusory exemption robustness').

omega_variable(
    proportionality_as_kernel_synthesis,
    'Is the proportionality reading a stable synthesis or an unstable compromise between categorical bodily autonomy and categorical public health supremacy?',
    'Longitudinal observation of jurisdictions: convergence toward one sibling reading indicates instability; persistence of three-way contestation indicates stability.',
    'Drift toward public_health_primary increases extraction and weakens the proportionality axiom; drift toward bodily_autonomy_primary dissolves the constraint entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_as_kernel_synthesis, conceptual, 'Stability of the proportionality reading as a kernel compromise').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmb_proport_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vmb_proport_tr_t6, vaccine_mandate_balance__proportionality_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(vmb_proport_tr_t12, vaccine_mandate_balance__proportionality_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(vmb_proport_tr_t18, vaccine_mandate_balance__proportionality_reading, theater_ratio, 18, 0.45).
narrative_ontology:measurement(vmb_proport_tr_t24, vaccine_mandate_balance__proportionality_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(vmb_proport_tr_t30, vaccine_mandate_balance__proportionality_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(vmb_proport_tr_t36, vaccine_mandate_balance__proportionality_reading, theater_ratio, 36, 0.38).

% Extraction over time
narrative_ontology:measurement(vmb_proport_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vmb_proport_be_t6, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(vmb_proport_be_t12, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(vmb_proport_be_t18, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(vmb_proport_be_t24, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(vmb_proport_be_t30, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(vmb_proport_be_t36, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 36, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(vmb_proport_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(vmb_proport_su_t6, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(vmb_proport_su_t12, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(vmb_proport_su_t18, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 18, 0.75).
narrative_ontology:measurement(vmb_proport_su_t24, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(vmb_proport_su_t30, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(vmb_proport_su_t36, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 36, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint decomposes the colloquial 'vaccine mandate' debate into three structurally distinct constraints. The proportionality reading introduces threshold-dependence that the other two reject; its epsilon varies by pathogen, whereas the sibling readings have categorical victim and beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
