% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Bodily-Autonomy-Primary Reading of Medical Intervention Legitimacy
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the bodily-autonomy-primary reading of the
 *   legitimate-health-intervention kernel: legitimacy requires informed
 *   consent for medical interventions, and state coercion — including
 *   coercion mediated through employment or access conditions — violates
 *   bodily integrity regardless of the public-health benefit claimed. This is
 *   a single reading among three siblings of the same kernel
 *   (public_health_primary, proportionality_reading), each instantiated as a
 *   separate constraint with its own ε and stakeholder structure per the
 *   ε-invariance principle. Under this reading's premises, the exemption
 *   bureaucracy and employer-mediated mandates are themselves evidence of the
 *   constraint's tangled character: a genuine coordination function (disease
 *   suppression) is bundled with an enforcement mechanism that treats
 *   economic coercion as functionally equivalent to physical coercion of the
 *   body.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.61).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.58).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.61).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "Bodily-Autonomy-Primary Reading of Medical Intervention Legitimacy").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '19a34b92-5f94-4860-943f-f2fde573e85f').
narrative_ontology:cs_kernel_codification('19a34b92-5f94-4860-943f-f2fde573e85f', distributed).
narrative_ontology:cs_authority_grounding('19a34b92-5f94-4860-943f-f2fde573e85f', distributed).
narrative_ontology:cs_reading_relation('19a34b92-5f94-4860-943f-f2fde573e85f', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('19a34b92-5f94-4860-943f-f2fde573e85f', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('19a34b92-5f94-4860-943f-f2fde573e85f', foundational, consent_is_freestanding_precondition).
narrative_ontology:cs_axiom_status(consent_is_freestanding_precondition, holdable).
narrative_ontology:cs_axiom_grounding('19a34b92-5f94-4860-943f-f2fde573e85f', consent_is_freestanding_precondition, deontological).
narrative_ontology:cs_axiom('19a34b92-5f94-4860-943f-f2fde573e85f', secondary, economic_leverage_constitutes_coercion).
narrative_ontology:cs_axiom_status(economic_leverage_constitutes_coercion, holdable).
narrative_ontology:cs_axiom_grounding('19a34b92-5f94-4860-943f-f2fde573e85f', economic_leverage_constitutes_coercion, conventional).
narrative_ontology:cs_reference_frame('19a34b92-5f94-4860-943f-f2fde573e85f', post_nuremberg_consent_primacy).
narrative_ontology:cs_drift_state('19a34b92-5f94-4860-943f-f2fde573e85f', pandemic_era_mandate_escalation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('19a34b92-5f94-4860-943f-f2fde573e85f', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, employers_administering_mandates).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, compliant_population_cohort).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, medical_exemption_seekers).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, religious_and_conscience_objectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers vaccination/intervention mandates, sets exemption criteria, and enforces compliance through licensing, employment conditions, and access restrictions. Frames its authority in terms of population health outcomes rather than individual consent, and treats the bodily-autonomy objection as a cost of doing business rather than a binding constraint.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Implements mandates as a condition of employment because public health guidance and liability exposure push them to. Gains legal cover and reduced workplace-outbreak liability by requiring compliance, but exercises real coercive leverage over workers who have no comparable employer to move to without losing income.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, employers_administering_mandates, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, employers_administering_mandates, beneficiary).

% Complies with the intervention, retains employment and access to public spaces, and experiences the mandate as a minor inconvenience rather than a rights violation. From this reading's own premises, their compliance does not resolve the underlying legitimacy question because consent, not outcome, is what this reading measures.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, compliant_population_cohort, beneficiary,
    moderate, biographical, mobile, national).

% Faces termination, loss of licensure, or exclusion from public accommodations for declining the intervention. Under this reading, the 'choice' to comply is not meaningfully consensual because the alternative is loss of livelihood — the coercion operates through economic leverage rather than direct physical force, but the bodily-integrity violation is treated as occurring regardless of the vector.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_workers, payer,
    powerless, biographical, trapped, national).

% Has a documented medical contraindication but must navigate an exemption bureaucracy that this reading holds should not exist at all — under bodily-autonomy-primary premises, no one should need to justify a bodily refusal to a state or employer panel. The exemption process itself is read as an admission that consent is conditional, not primary.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, medical_exemption_seekers, payer,
    powerless, biographical, trapped, national).

% Objects on grounds the exemption process does not weight equally with medical grounds, or that are denied entirely depending on jurisdiction. Their objection is rarely engaged on its own terms in policy debate, which centers on epidemiological modeling and employer liability rather than the coercion the objector experiences directly.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, religious_and_conscience_objectors, excluded,
    powerless, biographical, trapped, national).

% Adjudicates challenges to mandates under bodily integrity, due process, and religious liberty doctrines. Produces the record from which this reading draws its strongest institutional corroboration, though rulings have been inconsistent across jurisdictions and time periods.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, courts_and_constitutional_review, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level disease suppression by raising vaccination/intervention uptake past a threshold that reduces transmission and healthcare-system strain — a genuine collective-action problem where individual non-participation imposes costs on others.
% TRANSFER_FUNCTION: Moves bodily autonomy and consent-authority from the individual to the state/employer, and moves the material costs of refusal (income, employment, access) from the collective onto specific non-compliant individuals, in exchange for reduced transmission risk borne collectively.
% ABSENT_VOICES: Conscience and religious objectors whose refusal is not medically documentable are structurally disadvantaged relative to those with a diagnosable contraindication — the exemption architecture privileges the public-health-primary reading's own categories even while nominally accommodating objection.
% DISAPPEARANCE_RATIONALE: If bodily-autonomy-primary reasoning were fully adopted and mandates were struck down or made unenforceable, mandate-coerced workers and objectors would immediately regain employment/access options — a real rearrangement for them. But public health agencies dispute that population outcomes would meaningfully worsen at typical uptake levels, and employers dispute that liability exposure would change; the disagreement over what 'the world' actually depends on this constraint is itself the live dispute between readings.
% FOUNDING_PROBLEM: Historical abuses of medical authority without consent (forced sterilization, non-consensual experimentation, coercive quarantine used punitively) established that individual consent must be a binding precondition for legitimate medical intervention, independent of the intervention's claimed benefit.
% FOUNDING_PROBLEM_CORROBORATION: Bioethics scholarship and post-war human-subjects protections (Nuremberg Code, Belmont Report) corroborate the founding problem from outside current public-health-agency interests, establishing consent as a freestanding requirement rather than a derivative of outcome. Public health agencies and employers, who are direct beneficiaries under this reading, contest that the founding problem still constrains mandate design, arguing informed consent doctrine was never intended to bar population-level conditions on employment or access.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, contested).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises over the interval (0.22 to 0.61) as mandate enforcement mechanisms matured from voluntary guidance to employment-conditioned and access-conditioned requirements — a real historical pattern in pandemic-era policy escalation. Suppression tracks similarly but plateaus and slightly recedes at the tail (0.60 to 0.58) as some jurisdictions rolled back mandates under litigation and political pressure, while others held enforcement steady; the flat final segment reflects genuine post-peak stabilization rather than continued escalation. Theater ratio stays comparatively low (0.28 ceiling) because the enforcement machinery is doing real coercive work, not merely performing — this is not a piton, it is an active, contested tangled rope.
 *
 * PERSPECTIVAL GAP:
 *   From the public-health-agency seat, the mandate is coordination solving a genuine collective-action problem — this is the seat from which the sibling public_health_primary reading is written. From the mandate-coerced-worker seat, under bodily-autonomy-primary premises, the same structure is extraction of consent-authority mediated through employment leverage, regardless of whether the underlying intervention was medically sound. The engine computes these as structurally different seat classifications from the same base data; that divergence is the analytical payload of decomposing the kernel into separate readings rather than averaging across them.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and employers sit near the beneficiary end: they set the terms, bear little of the cost of non-compliance, and derive institutional or liability benefit from high compliance rates. Mandate-coerced workers and medical exemption seekers sit near the full-target end: trapped exit options (loss of livelihood is not a meaningful alternative under this reading's own premises), and the extraction is the loss of bodily-integrity-grounded consent itself, independent of any material harm from the intervention. The compliant cohort is a genuine beneficiary under most readings but under THIS reading their compliance does not validate the arrangement — the reading measures consent, not uptake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unconsented medical intervention as documented historical harm) remains live in the sense that the doctrinal protections it produced (Nuremberg Code, informed consent law) are still cited and litigated. What is contested is whether CURRENT mandate architectures are a continuation of that founding problem's remedy or a drift away from it — proponents of this reading argue the exemption bureaucracy itself demonstrates that consent has been made conditional rather than primary, which is precisely the pattern the founding doctrine was built to foreclose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_bodily_autonomy,
    'Is bodily-autonomy-primary the correct reading of the legitimate_health_intervention kernel, or is it one of three defensible framings (alongside public_health_primary and proportionality_reading) that produce structurally different constraints from the same underlying legitimacy claim?',
    'No empirical resolution exists — this is a normative/conceptual disagreement about which value (individual consent vs. population outcome vs. proportional weighting) grounds legitimacy. Constitutional courts across jurisdictions have split, which is itself evidence the kernel is genuinely contested rather than settled.',
    'Adopting public_health_primary instead would remove mandate_coerced_workers and medical_exemption_seekers from the victim set entirely and reclassify uptake-driven metrics as the primary measure of legitimacy rather than consent; adopting proportionality_reading would make ε a function of disease severity rather than a comparatively stable enforcement-driven trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_bodily_autonomy, conceptual, 'Which of three sibling readings of the kernel is the operative legitimacy framework — genuinely contested, not resolvable by more data.').

omega_variable(
    economic_coercion_equivalence,
    'Does employment-conditioned or access-conditioned pressure to accept a medical intervention constitute ''coercion'' violating bodily integrity in the same sense as direct physical force, or is it a lesser category that this reading over-extends?',
    'Comparative doctrinal analysis of how courts have treated economic duress versus physical duress in consent law generally (contract law, criminal law) applied by analogy to medical consent; also survey of whether affected workers report equivalent psychological harm to direct coercion.',
    'If economic coercion is doctrinally treated as equivalent, the victim set and extraction magnitude for mandate_coerced_workers stands as authored. If courts and bioethics treat it as meaningfully lesser, the extractiveness trajectory for this reading is overstated relative to a narrower reading limited to direct-force mandates only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_coercion_equivalence, conceptual, 'Whether employment/access leverage counts as bodily-integrity-violating coercion under this reading''s own premises.').

omega_variable(
    exemption_bureaucracy_as_evidence,
    'Does the existence of a medical/religious exemption process demonstrate that consent has been made conditional (supporting this reading''s claim of drift from the founding problem), or is a bounded exemption process compatible with genuine bodily-autonomy-primary legitimacy?',
    'Compare jurisdictions with narrow versus broad exemption criteria and assess whether narrow-exemption regimes are judged less legitimate by courts applying bodily-integrity doctrine, holding population outcomes roughly constant.',
    'If narrow exemptions are treated as evidence of illegitimate conditionality, this strengthens classification toward tangled_rope with high suppression; if bounded exemptions are treated as a reasonable accommodation compatible with autonomy primacy, the suppression metric may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_bureaucracy_as_evidence, conceptual, 'Whether the exemption architecture is evidence of consent-conditionality or a legitimate accommodation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legi_tr_t4, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 4, 0.13).
narrative_ontology:measurement(legi_tr_t8, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 8, 0.18).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 12, 0.24).
narrative_ontology:measurement(legi_tr_t16, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 16, 0.27).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 20, 0.28).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(legi_be_t4, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(legi_be_t8, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(legi_be_t16, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 24, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(legi_su_t4, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(legi_su_t8, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(legi_su_t16, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language 'legitimate medical intervention' concept, per the ε-invariance principle: bodily_autonomy_primary (this file, tangled_rope, ε~0.61, victims = mandate-coerced individuals), public_health_primary (expected rope or tangled_rope with different victim framing, ε driven by uptake/outcome metrics), and proportionality_reading (expected variable ε keyed to disease severity, likely the most context-dependent of the three). All three share the same underlying kernel text but instantiate structurally distinct constraints with different beneficiary/victim sets and different classification-relevant metrics. They are linked via affects_constraints rather than merged because merging would violate ε-invariance — averaging or parameterizing a single ε across readings is exactly the anti-pattern the framework prohibits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
