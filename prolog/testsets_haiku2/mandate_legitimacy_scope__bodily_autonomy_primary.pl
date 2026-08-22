% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Mandate Legitimacy Scope — Bodily Autonomy Primary Reading
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This is the bodily_autonomy_primary READING of the
 *   mandate_legitimacy_scope kernel—one instantiation of a contested
 *   constitutional claim about whether state authority to mandate medical
 *   intervention may override informed consent and bodily autonomy when
 *   justified by public health necessity. This reading asserts that bodily
 *   autonomy is a non-waivable right: medical intervention without informed
 *   consent is a rights violation regardless of collective benefit. The state
 *   becomes a coercive actor violating bodily integrity; individuals who
 *   refuse become victims of state power, not non-compliant citizens. The
 *   claim stands independent of whether the underlying disease is real or
 *   whether vaccination is medically effective—the focal claim is structural:
 *   CAN the state mandate without consent? This reading answers NO. The
 *   measurement series trace extractiveness and suppression rising as mandate
 *   enforcement intensifies, then stabilizing as the constraint becomes
 *   normalized in institutional practice.
 *
 * KEY AGENTS:
 *   - Unvaccinated-coerced individuals: powerless, trapped, identity-locked — targeted by mandate enforcement; no exit except compliance or acceptance of exclusion
 *   - Medical conscience objectors: moderate power, identity-locked — forced choice between conscience and livelihood/standing
 *   - State health authority: institutional power, analytical exit — sets and enforces the mandate; this reading characterizes it as rights violator
 *   - Vulnerable populations: powerless, trapped — excluded from mandate negotiation; protected at cost of others' autonomy
 *   - Courts/constitutional review: institutional power, analytical seat — determine whether mandate meets constitutional constraints
 *   - Advocacy coalitions: organized power, mobile exit — excluded from authority table; represent bodily autonomy position in public sphere
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.81).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.77).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.81).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Mandate Legitimacy Scope — Bodily Autonomy Primary Reading").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, 'f5af66f5-dd59-409e-82f5-3ec901a8298f').
narrative_ontology:cs_kernel_codification('f5af66f5-dd59-409e-82f5-3ec901a8298f', formalized).
narrative_ontology:cs_authority_grounding('f5af66f5-dd59-409e-82f5-3ec901a8298f', extraction).
narrative_ontology:cs_reading_relation('f5af66f5-dd59-409e-82f5-3ec901a8298f', mandate_legitimacy_scope__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('f5af66f5-dd59-409e-82f5-3ec901a8298f', mandate_legitimacy_scope__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('f5af66f5-dd59-409e-82f5-3ec901a8298f', foundational, bodily_autonomy_non_waivable).
narrative_ontology:cs_axiom_status(bodily_autonomy_non_waivable, holdable).
narrative_ontology:cs_axiom_grounding('f5af66f5-dd59-409e-82f5-3ec901a8298f', bodily_autonomy_non_waivable, deontological).
narrative_ontology:cs_axiom('f5af66f5-dd59-409e-82f5-3ec901a8298f', foundational, informed_consent_prerequisite_to_medical_intervention).
narrative_ontology:cs_axiom_status(informed_consent_prerequisite_to_medical_intervention, holdable).
narrative_ontology:cs_axiom_grounding('f5af66f5-dd59-409e-82f5-3ec901a8298f', informed_consent_prerequisite_to_medical_intervention, deontological).
narrative_ontology:cs_reference_frame('f5af66f5-dd59-409e-82f5-3ec901a8298f', liberal_constitutional_bodily_autonomy_protection).
narrative_ontology:cs_drift_state('f5af66f5-dd59-409e-82f5-3ec901a8298f', post_pandemic_mandate_emergence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f5af66f5-dd59-409e-82f5-3ec901a8298f', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, medical_conscience_objectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face coerced medical intervention (vaccination) under threat of employment loss, exclusion from essential services, or legal penalty. Their bodily autonomy is overridden by state mandate regardless of their assessment of personal risk or their conscientious objection. Under this reading, they are rights-violated targets of the state apparatus, not uncooperative citizens.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, biographical, trapped, national).

% Healthcare providers, religious adherents, or individuals with sincere conscientious beliefs against vaccination are forced to choose between compliance and career/community standing. Their identity-constituted refusal to participate in the intervention is treated as mere preference rather than protected conscience.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medical_conscience_objectors, payer,
    moderate, biographical, identity_locked, national).

% Imposes the vaccination mandate as a condition of public participation, justified by collective health benefit. This reading characterizes the state's authority as a rights violator: it asserts state power to override individual bodily autonomy without informed consent, using public health necessity as the warrant.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, state_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Those who cannot receive certain vaccines due to medical contraindication are excluded from the negotiation. Under this reading, the mandate is structured to benefit their protection at the expense of others' bodily autonomy, but they have no voice in whether that trade is acceptable.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, vulnerable_populations, excluded,
    powerless, biographical, trapped, national).

% Evaluate whether state coercion of bodily medical intervention meets constitutional constraints on bodily autonomy and informed consent. Their scrutiny determines whether the mandate's authority survives doctrinal challenge.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, courts_constitutional_review, observer,
    institutional, generational, analytical, national).

% Organizations opposing medical coercion and defending bodily autonomy are excluded from the mandate-setting process. Their voice — that informed consent is non-waivable and that population benefit does not override individual bodily integrity — is not present at the decision table.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, advocacy_coalitions, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None under this reading. The mandate is not presented as solving a coordination problem; it is presented as enforcing an obligation on individuals to accept a medical intervention against their will for collective benefit. No genuine collective-action problem is solved — the constraint is pure coercive transfer.
% TRANSFER_FUNCTION: Bodily autonomy and informed consent are transferred from individuals to the state. Individuals lose the right to refuse medical intervention based on their own assessment of risk and benefit. The state gains authority to mandate medical procedures and override individual choice when it claims public health necessity.
% ABSENT_VOICES: Medical conscience objectors and unvaccinated individuals are excluded from mandate deliberation; their objections are treated as mere compliance resistance, not as principled positions worthy of negotiation. Those who would argue that bodily autonomy is a non-negotiable constitutional constraint are not present at the authority table.
% DISAPPEARANCE_RATIONALE: If the mandate disappeared, individuals would regain the right to refuse vaccination and to choose based on informed consent. Employment, education, and service access would not depend on compliance with the medical intervention. The restructuring would be immediate and substantial—a return to consensual medical practice.
% FOUNDING_PROBLEM: Disease threat to population health. The founding problem is that an infectious disease poses risk to vulnerable populations who cannot protect themselves through their own medical choices.
% FOUNDING_PROBLEM_CORROBORATION: Public health epidemiologists attest the disease threat is real and ongoing. However, this reading's contested claim is NOT about disease existence—it is about whether the founding problem justifies overriding bodily autonomy. Courts, constitutional scholars, and medical ethicists outside the public health establishment attest that the founding problem, even if live, does not warrant suspension of informed consent and bodily autonomy rights.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.81 at interval end) is high because the constraint transfers bodily autonomy and informed consent from individuals to the state without compensation or choice. Under this reading, the transfer is a pure rights loss—individuals lose the capacity to refuse medical intervention based on their own assessment. Suppression (0.77 stable) is sustained by employment exclusion, service denial, and legal penalty—coercive machinery that persists at high intensity throughout the interval. Theater (0.22 moderate) reflects that the mandate includes genuine public health justification (disease risk, vulnerable protection), but an increasing share of enforcement energy goes to excluding and coercing refusers rather than to the coordination function itself. The measurement trajectory shows extractiveness rising as the mandate's scope expands (enforcement against more refusers) and stabilizing as institutional practice normalizes the constraint. Suppression requirement stays elevated because sustained enforcement is necessary—without active coercion, refusal rates would re-emerge. This is NOT a rope: no participant is a net beneficiary; no genuine collective-action problem is solved (disease protection could be achieved through less coercive means). This IS a snare: persistence depends on coercion and on suppressing the alternative (refusal/exemption/exit).
 *
 * PERSPECTIVAL GAP:
 *   The state health authority and the unvaccinated-coerced individuals compute this constraint RADICALLY differently. From the authority's seat (public_health_primary reading), the mandate solves a coordination problem: individuals fail to vaccinate at sufficiently high rates to protect vulnerable populations; the state compels participation for collective good. The mandate is legitimate, even if costly to individual preference. From the coerced individual's seat (bodily_autonomy_primary reading—THIS reading), the state is a rights violator: it claims authority to override bodily autonomy without consent, turning medical practice into a domain of coercive state power. The engine computes each seat's type from power, exit, and beneficiary/victim status: the authority sits as agenda-setter with institutional power and arbitrage exit (can change the mandate); coerced individuals sit as powerless payers with trapped exit (compliance or exclusion). These structural differences drive the per-seat classification divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated-coerced individuals and medical conscience objectors are structural victims: they bear the cost (bodily autonomy overridden, forced medical intervention), they have no say in the arrangement, and their exit options are trapped or identity-locked (leaving the jurisdiction, complying, or accepting exclusion). Their directionality d approaches 1.0 (full targets). The state health authority is the agenda-setter with institutional power and high exit optionality (analytical/arbitrage—can revise the mandate). Its directionality d approaches 0.0 relative to the extraction it enforces, though this reading characterizes it as extracting from its citizens rather than benefiting from legitimate coordination. Under this reading, there are NO beneficiaries—no actor collects from the mandate and is left better off net. The vulnerable populations are excluded rather than beneficiaries: they are invoked as justification but have no agency in the mandate structure. No override is needed; the structural data produces the correct directionality automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows signs of early-stage mandatrophy: theater_ratio rises from 0.15 to 0.22 over the interval, indicating performative justification content increasing as substantive coordination function remains opaque or contested. The suppression requirement stays high and stable (0.68→0.77), suggesting the mandate does not achieve compliance through voluntary acceptance of its legitimacy frame; active enforcement machinery is required. If the theater ratio continues to rise above 0.35-0.40 in subsequent measurement intervals, the constraint risks classification as piton (justified by diminished function, maintained by inertia). Currently, it sits as snare: coercive, extractive, with legitimate-sounding justification (public health), but the mounting theater and stable high suppression indicate the foundation is contested and active enforcement carries the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bodily_autonomy_vs_collective_benefit,
    'Is bodily autonomy a non-waivable constitutional right, or is it a right that may be suspended when collective health benefit is sufficiently high?',
    'Constitutional doctrine evolution: courts rule on whether public health necessity permits override of informed consent. The resolution lies in jurisprudence (whether courts adopt or reject the bodily autonomy primary position), not in empirics.',
    'If bodily autonomy is non-waivable, the mandate is a constitutional violation and ε approaches 1.0 (pure rights violation). If bodily autonomy may be suspended, the mandate''s legitimacy depends on whether the collective benefit meets the proportionality threshold (public_health_primary vs. proportionality_reading). This omega is the reading''s core contested claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bodily_autonomy_vs_collective_benefit, conceptual, 'Whether bodily autonomy is a constraining right or a balanceable interest.').

omega_variable(
    alternatives_to_coercion,
    'Are less coercive alternatives (targeted protection of vulnerable populations, voluntary incentives, transparent risk communication) available and equally effective?',
    'Empirical evaluation: did jurisdictions using less coercive approaches achieve comparable protection of vulnerable populations? Did voluntary uptake + targeted protection suffice, or was coercion necessary for sufficient coverage?',
    'If effective alternatives existed and were rejected in favor of blanket coercion, the mandate exhibits unnecessary violation of bodily autonomy (supporting the bodily autonomy primary reading and increasing ε). If coercion was the only mechanism available, the mandate moves toward the proportionality_reading (coercion justified by necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternatives_to_coercion, empirical, 'Whether coercive mandates were necessary or whether less restrictive means existed.').

omega_variable(
    consent_capacity_and_paternalism,
    'Does the reading permit paternalistic override of consent when individuals make medically risky choices, or is informed consent binding regardless of the decision''s medical advisability?',
    'Doctrine and case law: courts clarify whether bodily autonomy includes the right to refuse medically beneficial interventions, or whether state may override refusal to ''protect'' individuals from their own poor medical judgment.',
    'Pure bodily autonomy reading: individuals retain right to refuse even medically beneficial interventions. Public health reading: state may override when individual choice endangers others (externality justification). This omega distinguishes the three readings'' core philosophical commitments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_capacity_and_paternalism, conceptual, 'Whether bodily autonomy includes right to refuse beneficial medical intervention.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of refusal and objection structural (employment loss, service exclusion, legal penalty) or internalized (individuals adopt the mandate''s framing as legitimate)?',
    'Post-mandate trajectory: if penalties are removed, do refusals re-emerge at pre-mandate rates (structural suppression), or do individuals remain compliant (internalized acceptance)? Long-term attitude surveys and subsequent voluntary uptake patterns.',
    'Structural suppression: high ε because coercion does real work. Internalized acceptance: lower ε because the mandate achieves compliance through adoption of its legitimacy frame, not through active suppression. This affects whether the constraint persists through enforcement machinery or through belief alignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of objection is sustained by enforcement or by internalized legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mand_tr_t6, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 6, 0.17).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 12, 0.19).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 24, 0.22).
narrative_ontology:measurement(mand_tr_t36, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 36, 0.21).
narrative_ontology:measurement(mand_tr_t48, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 48, 0.22).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(mand_be_t6, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 12, 0.78).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 24, 0.81).
narrative_ontology:measurement(mand_be_t36, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 36, 0.8).
narrative_ontology:measurement(mand_be_t48, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 48, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(mand_su_t6, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 6, 0.71).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 24, 0.77).
narrative_ontology:measurement(mand_su_t36, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 36, 0.78).
narrative_ontology:measurement(mand_su_t48, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 48, 0.77).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (bodily_autonomy_primary) of the mandate_legitimacy_scope kernel. The sibling reading mandate_legitimacy_scope__public_health_primary asserts state authority as legitimate when necessary to protect vulnerable populations; this reading forecloses that authority claim on constitutional grounds. The proportionality_reading attempts to negotiate between the two by requiring balancing; bodily_autonomy_primary argues that autonomy cannot be balanced away. Each reading has its own constraint_id, its own ε (though derived from the same domain), its own stakeholder configuration, and its own classification. They share the kernel (the contested claim about mandate legitimacy) but differ in the answer and in which seats bear costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
