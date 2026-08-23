% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__autonomy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: Sovereign Authority Over Own Death (Autonomy Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the autonomy_reading of the contested
 *   kernel 'end_of_life_decision_authority'. The reading asserts that
 *   competent individuals possess sovereign authority over their own death —
 *   a claim grounded in bodily autonomy and self-ownership. The constraint is
 *   the legal-institutional arrangement (statutes, clinical protocols,
 *   regulatory oversight) that recognizes and operationalizes this authority.
 *   The reading claims this is a fundamental right (Mountain), but the
 *   constraint requires active enforcement (safeguards, assessments,
 *   reporting) and produces asymmetric extraction: healthcare professionals
 *   are compelled to facilitate, and vulnerable populations bear externalized
 *   slippery-slope risk. The sibling readings — sanctity_reading (life as
 *   intrinsic value) and vulnerability_protection_reading (distributed
 *   authority with checkpoints) — contest both the natural-law status and the
 *   beneficiary/victim structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.35).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.25).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, mountain).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Sovereign Authority Over Own Death (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).
domain_priors:emerges_naturally(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, '8cbf27fb-9d4d-45fd-b9d4-bcc43d0da76c').
narrative_ontology:cs_kernel_codification('8cbf27fb-9d4d-45fd-b9d4-bcc43d0da76c', formalized).
narrative_ontology:cs_authority_grounding('8cbf27fb-9d4d-45fd-b9d4-bcc43d0da76c', lineage).
narrative_ontology:cs_interpretation_layer_present('8cbf27fb-9d4d-45fd-b9d4-bcc43d0da76c').
narrative_ontology:cs_reading_relation('8cbf27fb-9d4d-45fd-b9d4-bcc43d0da76c', end_of_life_decision_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('8cbf27fb-9d4d-45fd-b9d4-bcc43d0da76c', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('8cbf27fb-9d4d-45fd-b9d4-bcc43d0da76c', foundational, bodily_autonomy_includes_death_authority).
narrative_ontology:cs_axiom_status(bodily_autonomy_includes_death_authority, holdable).
narrative_ontology:cs_axiom_grounding('8cbf27fb-9d4d-45fd-b9d4-bcc43d0da76c', bodily_autonomy_includes_death_authority, deontological).
narrative_ontology:cs_axiom('8cbf27fb-9d4d-45fd-b9d4-bcc43d0da76c', foundational, competence_sufficient_for_death_authority).
narrative_ontology:cs_axiom_status(competence_sufficient_for_death_authority, holdable).
narrative_ontology:cs_axiom_grounding('8cbf27fb-9d4d-45fd-b9d4-bcc43d0da76c', competence_sufficient_for_death_authority, conventional).
narrative_ontology:cs_reference_frame('8cbf27fb-9d4d-45fd-b9d4-bcc43d0da76c', liberal_autonomy_framework).
narrative_ontology:cs_drift_state('8cbf27fb-9d4d-45fd-b9d4-bcc43d0da76c', contemporary_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8cbf27fb-9d4d-45fd-b9d4-bcc43d0da76c', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_individuals).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_competent).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, vulnerable_populations).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_with_conscience_objections).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_willing).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_competent).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_willing).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, bodily_autonomy_principle).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, self_ownership_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the legally recognized authority to request and receive assistance in dying when they judge their suffering intolerable. They benefit from a regulated pathway that replaces clandestine or violent self-harm. Exit from the constraint means choosing not to exercise the right; the constraint does not compel use.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_individuals, beneficiary,
    moderate, biographical, mobile, national).

% A subset of competent individuals whose suffering is severe and prolonged; they are the primary intended beneficiaries of the access right. They bear the procedural burden of multiple assessments, waiting periods, and eligibility scrutiny — costs imposed by the safeguards that legitimize the right.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_competent, beneficiary,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_competent, payer).

% Bear the externalized slippery-slope risk: the constraint's expansion of death-authority creates structural pressure (cost-containment incentives, familial burden narratives, ableist assumptions) that makes some vulnerable people feel compelled to choose death. They did not consent to this risk transfer and have no effective exit from the social context that generates it.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, vulnerable_populations, payer,
    powerless, biographical, trapped, national).

% Are legally required to provide effective referral or transfer when a patient requests assisted dying, even if they morally object. They bear the cost of complicity in what they regard as killing. Exit means leaving the profession or jurisdiction — a high-cost exit that makes their position constrained rather than trapped.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_with_conscience_objections, payer,
    organized, biographical, constrained, national).

% Gain professional recognition and legal protection for a practice some view as core to palliative care. They also bear the emotional and procedural burden of being the state's designated facilitators of death — a role that concentrates moral weight on a small cadre.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_willing, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_willing, payer).

% Enacts and amends the statute that creates the right, defines competence, and structures the safeguards. It bears the political cost of legitimacy contests but controls the legislative framework. Its exit is analytical — it can repeal or amend, but only through democratic process.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, state_legal_system, agenda_setter,
    institutional, generational, analytical, national).

% Design and enforce the clinical standards (assessment protocols, waiting periods, reporting) that operationalize the right. They are the day-to-day enforcers of the constraint's coordination function. Exit is analytical — they interpret and adjust within statutory authority.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, medical_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Argue that the constraint encodes ableist assumptions (that life with disability is a fate worse than death) and that safeguards are inadequate to prevent coercion. They are structurally excluded from the constraint's core bargain — their objection is treated as external to the autonomy framework.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Experience the death of a loved one through the regulated pathway; some report relief at witnessing a peaceful chosen death, others report trauma from feeling excluded from the decision or pressured to support it. They have no formal standing in the eligibility assessment.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, family_members, excluded,
    moderate, biographical, constrained, local).

% Analyze the constraint's coherence, its safeguards' efficacy, and its effects on trust in medicine. They do not bear the constraint's costs or collect its benefits directly; their seat is the analytical perch from which the engine computes per-seat classifications.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, bioethicists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transition from life to death for competent individuals who judge their suffering intolerable, providing a legal and medical pathway that replaces clandestine or violent self-harm with regulated, witnessed, and supported dying.
% TRANSFER_FUNCTION: Moves the authority to determine the timing and manner of death from state prohibition and medical paternalism to the competent individual; moves the burden of facilitation onto healthcare professionals; moves the risk of coercion onto vulnerable populations as externalized cost.
% ABSENT_VOICES: Future incompetent selves (who cannot consent to the constraint's terms); those who would choose life but face subtle pressure to choose death (the internalized slippery slope); religious communities for whom the constraint violates cosmological order.
% DISAPPEARANCE_RATIONALE: The legal right structures the entire medical-legal pathway for assisted dying; its removal would force return to prohibition, criminalize facilitation, eliminate the regulated pathway, and return suffering-prolonged competent individuals to the pre-legalization condition of clandestine or violent self-harm.
% FOUNDING_PROBLEM: The prohibition on assisted dying forced competent suffering individuals into clandestine, violent, or isolated deaths, denied them the ability to orchestrate a peaceful transition, and subjected healthcare professionals to legal jeopardy for compassionate action.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of pre-legalization deaths (e.g., Sue Rodriguez, Tony Nicklinson, Brittany Maynard) attested by independent journalists and courts; medical surveys showing clinician support for legalization; parliamentary committee reports from jurisdictions that legalized (Oregon, Netherlands, Canada, Belgium).
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, ExtMetricName, E),
    domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(end_of_life_decision_authority__autonomy_reading),
    narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) reflects the compelled facilitation of healthcare professionals and the externalized risk to vulnerable populations — real costs transferred through the constraint. Suppression (0.25) is modest: the constraint grants freedom, but its safeguards (waiting periods, multiple assessments) suppress some autonomous choices. Theater_ratio (0.15) is low because safeguards are functional, not performative. Accessibility_collapse (0.7) is high: once the right exists, the alternative of 'no legal pathway' collapses for competent individuals — but not completely, as one may still choose not to exercise it. Resistance (0.65) is high because the sanctity and vulnerability readings actively contest the constraint's legitimacy and seek to restrict or reverse it.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (competent individuals), the constraint appears as a Mountain — a natural right finally recognized, with safeguards as reasonable coordination. From the victim seats (vulnerable populations, conscience objectors), it appears as a Snare — an arrangement that extracts from them to subsidize autonomy. The engine computes this divergence from the declared structural data; the autonomy reading's claimed_type (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent individuals and suffering-prolonged competent are structural beneficiaries (d near 0.0): the constraint subsidizes their sovereign choice. Vulnerable populations and conscience-objecting professionals are structural targets (d near 1.0): they bear extracted costs (externalized risk, compelled action) without collecting the autonomy benefit. Willing professionals sit near symmetric (d ~ 0.5): they gain professional recognition but bear concentrated moral burden. The state and regulators are agenda_setters with analytical exit — they administer but do not personally collect or pay. Disability advocates and family members are excluded: they experience effects but have no structural seat in the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prohibition forcing clandestine deaths) remains contested: the autonomy reading says it persists; the vulnerability reading says the problem was misdiagnosed (the real problem is inadequate palliative care and social support); the sanctity reading says the problem was never legitimate. The constraint has not atrophied — its scope is expanding (Canada's MAID expansion, Netherlands' dementia directives). Mandatrophy is not resolved; the constraint's mandate is actively contested and expanding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_right,
    'Is the sovereign authority over one''s death a genuine natural law (Mountain) or a constructed legal right that benefits identifiable agents (competent individuals, professional guilds)?',
    'Cross-cultural and historical analysis: if the claim appears only in specific liberal-legal traditions and not as a cross-cultural constant, it is constructed. Convergent evolution of assisted-dying laws in jurisdictions with similar liberal frameworks would support constructedness.',
    'If constructed, the Mountain claim fails FSM (false_summit_mountain) and the constraint reclassifies as Tangled Rope (coordination + asymmetric extraction). If natural, the Mountain claim holds and beneficiaries do not trigger reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_right, conceptual, 'Whether the autonomy right is a natural law or a liberal-legal construction.').

omega_variable(
    slippery_slope_externalization,
    'Is the externalized slippery-slope risk a real victimization of vulnerable populations (empirically observed coercion, internalized duty-to-die) or a speculative fear not borne out in jurisdictional data?',
    'Longitudinal data from legalized jurisdictions: rates of assisted dying among disabled/poor/isolated populations vs. general population; qualitative studies of pressure narratives; safeguard failure audits.',
    'If real, vulnerable_populations are confirmed victims and the constraint''s extraction is higher than the autonomy reading acknowledges — strengthening the Tangled Rope classification. If speculative, the victim declaration may be overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(slippery_slope_externalization, empirical, 'Whether slippery-slope risk materializes as actual victimization.').

omega_variable(
    competence_boundary_stability,
    'Is the ''competent individual'' boundary a stable natural category or a constructed legal fiction that shifts under pressure (e.g., expanding to mature minors, dementia advance directives, mental illness)?',
    'Track legislative and judicial expansions of eligibility criteria over time; assess whether each expansion follows a principled competence standard or responds to political pressure.',
    'If the boundary is unstable and expanding, the constraint''s coordination function is not fixed — it is a moving target that progressively extracts from new populations (e.g., non-terminal conditions), shifting toward Snare. If stable, the coordination function is bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_boundary_stability, empirical, 'Stability of the competence boundary that defines the constraint''s subject.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__autonomy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(end__tr_t6, end_of_life_decision_authority__autonomy_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(end__tr_t12, end_of_life_decision_authority__autonomy_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(end__tr_t18, end_of_life_decision_authority__autonomy_reading, theater_ratio, 18, 0.14).
narrative_ontology:measurement(end__tr_t24, end_of_life_decision_authority__autonomy_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(end__tr_t30, end_of_life_decision_authority__autonomy_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(end__be_t6, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(end__be_t12, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 12, 0.31).
narrative_ontology:measurement(end__be_t18, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 18, 0.33).
narrative_ontology:measurement(end__be_t24, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 24, 0.34).
narrative_ontology:measurement(end__be_t30, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 30, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(end__su_t6, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 6, 0.37).
narrative_ontology:measurement(end__su_t12, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(end__su_t18, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 18, 0.3).
narrative_ontology:measurement(end__su_t24, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 24, 0.27).
narrative_ontology:measurement(end__su_t30, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 30, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_decision_authority__autonomy_reading, 0.08).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This autonomy_reading decomposes the kernel 'end_of_life_decision_authority' from the sanctity_reading (life as intrinsic value, constraint = prohibition) and vulnerability_protection_reading (distributed authority with checkpoints). The autonomy reading's ε (0.35) reflects extraction on professionals/vulnerable; the sanctity reading would assess ε on life itself (high); the vulnerability reading would assess higher ε on vulnerable populations. All three stories link via affects_constraints to form the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_decision_authority__autonomy_reading, organized, 0.55).
constraint_indexing:directionality_override(end_of_life_decision_authority__autonomy_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
