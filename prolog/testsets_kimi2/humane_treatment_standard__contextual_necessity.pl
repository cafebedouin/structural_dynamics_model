% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Common Article 3 Contextual Necessity Reading
 *   domain: legal/political/security
 *
 * SUMMARY:
 *   This constraint story instantiates the contextual_necessity reading of
 *   the humane_treatment_standard kernel. Under this reading, Common Article
 *   3 of the Geneva Conventions provides a baseline of humane treatment for
 *   detainees in non-international armed conflict, but permits states to
 *   override that baseline through enhanced interrogation when national
 *   security imperatives are invoked. The reading gives security agencies
 *   discretion to define the content of humane treatment and shrinks the
 *   protected victim set by excluding high-value detainees in necessity
 *   scenarios. It is contested by the absolute_prohibition reading (which
 *   treats CA3 as non-derogable) and the proportionality_balancing reading
 *   (which demands structured weighing rather than executive discretion).
 *
 * KEY AGENTS:
 *   - security_agencies: Primary agenda-setter (institutional/constrained) â defines necessity and conducts enhanced interrogation
 *   - state_executive: Co-agenda-setter (institutional/constrained) â sets policy interpreting CA3 as conditional
 *   - detainees: Primary payer (powerless/trapped) â bear conditional protections and enhanced interrogation
 *   - human_rights_organizations: Excluded voice (organized/constrained) â advocate absolute prohibition but overridden
 *   - international_courts: Analytical observer (institutional/analytical) â adjudicate without enforcement
 *   - national_judiciary: Captured observer (institutional/constrained) â review under deference doctrines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.78).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.76).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.78).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Common Article 3 Contextual Necessity Reading").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "legal/political/security").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, '16438f2e-5c67-4460-90d7-488b99f40136').
narrative_ontology:cs_kernel_codification('16438f2e-5c67-4460-90d7-488b99f40136', fixed_text).
narrative_ontology:cs_authority_grounding('16438f2e-5c67-4460-90d7-488b99f40136', extraction).
narrative_ontology:cs_interpretation_layer_present('16438f2e-5c67-4460-90d7-488b99f40136').
narrative_ontology:cs_reading_relation('16438f2e-5c67-4460-90d7-488b99f40136', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('16438f2e-5c67-4460-90d7-488b99f40136', humane_treatment_standard__proportionality_balancing, coexists_with).
narrative_ontology:cs_axiom('16438f2e-5c67-4460-90d7-488b99f40136', foundational, necessity_overrides_humane_treatment).
narrative_ontology:cs_axiom_status(necessity_overrides_humane_treatment, holdable).
narrative_ontology:cs_axiom_grounding('16438f2e-5c67-4460-90d7-488b99f40136', necessity_overrides_humane_treatment, conventional).
narrative_ontology:cs_axiom('16438f2e-5c67-4460-90d7-488b99f40136', foundational, state_discretion_defines_humane).
narrative_ontology:cs_axiom_status(state_discretion_defines_humane, holdable).
narrative_ontology:cs_axiom_grounding('16438f2e-5c67-4460-90d7-488b99f40136', state_discretion_defines_humane, conventional).
narrative_ontology:cs_reference_frame('16438f2e-5c67-4460-90d7-488b99f40136', state_security_necessity_framework).
narrative_ontology:cs_drift_state('16438f2e-5c67-4460-90d7-488b99f40136', contemporary_accountability_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('16438f2e-5c67-4460-90d7-488b99f40136', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, state_executive).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, detainees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise discretion to determine when national security imperatives override humane treatment standards; conduct enhanced interrogation under legal cover provided by the necessity reading; are shielded by state secrecy and immunity doctrines.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, security_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Sets policy and legal interpretation that Common Article 3 permits enhanced interrogation in necessity scenarios; benefits from intelligence extracted and from flexibility in detention policy; politically accountable but structurally insulated from detainee redress.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, state_executive, agenda_setter,
    institutional, generational, constrained, national).

% Subject to detention and interrogation under a standard where humane treatment is defined by the detaining authority; cannot challenge the necessity determination; lack effective legal remedy due to secrecy, immunity, and conditional protections.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, detainees, payer,
    powerless, immediate, trapped, local).

% Advocate for absolute prohibition of torture and degrading treatment; are consulted in treaty processes but structurally excluded from national security necessity determinations; their legal challenges are often dismissed on state-secrecy or standing grounds.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, human_rights_organizations, excluded,
    organized, generational, constrained, global).

% Adjudicate complaints and issue opinions holding that CA3 and CAT prohibit necessity overrides for torture; lack enforcement mechanisms against non-compliant states; observe the divergence between international standard and state practice.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, international_courts, observer,
    institutional, civilizational, analytical, global).

% Review detention and interrogation policies under deference doctrines; often accept executive claims of necessity and state secrecy; constrained by political pressure and institutional capture to avoid blocking security operations.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, national_judiciary, observer,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__contextual_necessity, security_agencies).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared legal baseline for the minimum treatment of detainees in non-international armed conflict, while coordinating state security actors around an interpretive framework that permits override when national security is deemed at risk.
% TRANSFER_FUNCTION: Transfers definitional authority over humane treatment from an absolute, detainee-protective standard to state security agencies and executive government; transfers the physical and psychological costs of interrogation onto detainees designated as high-value targets.
% ABSENT_VOICES: Detainees are excluded from the rooms where necessity is determined; absolute-prohibition advocates and international monitors are formally heard in treaty bodies but structurally overridden in national security deliberations.
% DISAPPEARANCE_RATIONALE: If the contextual-necessity interpretation vanished, security agencies would lose legal authorization for enhanced interrogation, forcing reliance on stricter baseline standards or covert operation without legal cover; state-executive flexibility would contract and detainee protective expectations would expand.
% FOUNDING_PROBLEM: How to prevent atrocities in internal armed conflict while preserving state capacity to obtain intelligence from detainees believed to pose severe security threats.
% FOUNDING_PROBLEM_CORROBORATION: International legal historians and the ICRC attest the original 1949 problem was unregulated internal conflict atrocities; security-studies scholars outside the executive beneficiary set attest that the necessity override reading emerged later as a doctrinal innovation rather than the original drafters' solution.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the reading converts an absolute humanitarian baseline into a conditional standard that permits physical and psychological coercion when the detaining power unilaterally declares necessity. Suppression (0.76) is high because the constraint depends on excluding alternative absolute-prohibition frameworks, classifying interrogation evidence, and immunizing agents from accountability. Theater ratio (0.42) reflects the presence of oversight boards and judicial review that rarely block necessity claims, producing performative legitimacy without substantive constraint. Accessibility collapse (0.68) captures the partial suppression of the absolute-prohibition alternative within state legal systems, even though it persists in international discourse. Resistance (0.72) reflects sustained opposition from human rights institutions and some international tribunals. The claim of tangled_rope is authored independently: the reading retains a nominal coordination function (a baseline still exists, and states share a framework) but the asymmetric extraction is structurally dominant. The engine will compute the per-seat divergence: the security-agency seat should compute toward extraction, while the detainee seat computes toward severe target status.
 *
 * PERSPECTIVAL GAP:
 *   From the security-agency seat, the constraint is necessary coordination that prevents total legal paralysis while allowing intelligence collection; from the detainee seat, the same text operates as a conditional waiver of bodily integrity, with the necessity determination made by their captor. The engine captures this divergence through the same structural data: identical text, opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Security agencies and state executive are structural beneficiaries: they gain discretion, legal cover, and intelligence product (d near 0.0). Detainees are structural targets: they lose absolute protection and bear the physical costs of interrogation (d near 1.0). Human rights organizations are excluded from the necessity determination (d indeterminate or high due to futility). International courts and national judiciary occupy analytical or captured observer positions with intermediate d, though national judicial deference pushes their effective directionality toward the beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â regulating internal armed conflict â remains live, preventing automatic piton classification. However, the contextual-necessity reading has drifted from the original humanitarian purpose toward an extraction function: the baseline exists in text but is routinely overridden. The mandatrophy is contested because the beneficiary parties (security agencies) claim the necessity override is still solving the founding problem (preventing attacks), while external observers argue the problem has shifted to one of accountability. The Tangled Rope classification captures this ambiguity rather than collapsing it to Snare or Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Does the contextual-necessity reading represent a genuine interpretation of Common Article 3, or a post-hoc legal rationalization for practices that violate the absolute-prohibition reading?',
    'Historical treaty-tracing and travaux prÃ©paratoires analysis to determine if the drafters contemplated necessity overrides; comparison with sibling readings'' textual grounding.',
    'If the necessity reading is textually unsupported, it functions as extraction riding on a coordination text (Snare); if supported, it may be a contested Tangled Rope or Rope with high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the necessity override is an authentic legal interpretation or a cover story.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of detainee remedies structural (immunity doctrines, state secrecy, classification) or internalized (detainees believe they have no rights)?',
    'Post-release testimony and legal clinic data: if released detainees pursue remedies when channels open, suppression is structural; if they do not pursue even when channels exist, internalization is present.',
    'Structural suppression supports the authored high suppression metric; internalized suppression suggests effective extraction exceeds the structural measure because the target carries the suppression after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism for detainees.').

omega_variable(
    necessity_authenticity,
    'Are national security imperatives invoked to justify enhanced interrogation assessed by independent adjudicators, or self-certified by the extracting agencies?',
    'Comparative analysis of oversight regimes: independent judicial review versus executive self-certification in necessity determinations.',
    'If self-certified, the constraint is closer to pure extraction (Snare); if independently reviewed, the coordination function is more credible (Tangled Rope or Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_authenticity, empirical, 'Whether necessity determinations are independently verified or self-certified.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.25).
narrative_ontology:measurement(huma_tr_t5, humane_treatment_standard__contextual_necessity, theater_ratio, 5, 0.35).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__contextual_necessity, theater_ratio, 10, 0.4).
narrative_ontology:measurement(huma_tr_t15, humane_treatment_standard__contextual_necessity, theater_ratio, 15, 0.45).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__contextual_necessity, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(huma_be_t5, humane_treatment_standard__contextual_necessity, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__contextual_necessity, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(huma_be_t15, humane_treatment_standard__contextual_necessity, base_extractiveness, 15, 0.74).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__contextual_necessity, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(huma_su_t5, humane_treatment_standard__contextual_necessity, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__contextual_necessity, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(huma_su_t15, humane_treatment_standard__contextual_necessity, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__contextual_necessity, suppression_requirement, 20, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
