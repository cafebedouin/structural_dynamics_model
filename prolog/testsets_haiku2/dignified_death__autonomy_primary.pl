% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__autonomy_primary, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: dignified_death__autonomy_primary
 *   human_readable: Autonomy-Primary Dignified Death (Self-Determination Reading)
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   The autonomy-primary reading of dignified death asserts that dignity
 *   resides in self-determination: the suffering individual retains final
 *   authority over the timing and method of their own death. This is ONE
 *   reading of a contested kernel (the kernel of dignified death itself).
 *   Other readings — sanctity-primary (life's intrinsic value overrides
 *   consent) and relational-autonomy (decision authority distributed across
 *   patient-family-clinician triad) — instantiate different constraints with
 *   different victim sets and beneficiaries. This story models the
 *   autonomy-primary reading only: its structural claim (dignity = autonomous
 *   choice), its beneficiary (the autonomous agent), its victim (those denied
 *   exit in prohibitionist regimes), and the tangled coordination/extraction
 *   dynamic it creates when medical gatekeeping entangles with autonomy
 *   norms. The claim/metric gap is intentional: the constraint is CLAIMED as
 *   tangled_rope (coordination norm for decision authority + asymmetric
 *   extraction via gatekeeping) while metrics describe moderate-high
 *   extractiveness (0.52) and suppression (0.71) in prohibitionist contexts.
 *   The engine measures this reading's structural fit independent of the
 *   sibling readings.
 *
 * KEY AGENTS:
 *   - autonomous_dying_agent: the person in terminal or unbearable suffering, seeking final authority over death's timing and method. High beneficiary seat.
 *   - suffering_individuals_denied_exit: terminal/severely suffering persons in prohibitionist jurisdictions denied the autonomy-primary choice. Powerless, trapped, bearing the extraction cost.
 *   - medical_profession: agenda-setter administering gatekeeping criteria (diagnosis, prognosis, capacity, eligibility). Institutional power, analytical horizon.
 *   - state_legal_authority: agenda-setter establishing legal framework for or against autonomy-primary access. Dual-positioned (agenda-setter + payer in prohibitionist regimes bearing political cost).
 *   - religious_and_sanctity_tradition_holders: excluded from decision authority; their sanctity-primary objection is not weighted. Organized power, civilizational time horizon.
 *   - families_and_relational_circles: excluded from decision authority but relationally entangled; identity-locked (the dying person's death is not separable from relational meaning). Moderate power, biographical horizon.
 *   - vulnerable_disabled_populations: non-terminal disabled persons bearing risk from scope creep toward death-as-reasonable-choice. Powerless, identity-locked (disability is not separable from self).
 *   - jurisdictions_implementing_autonomy_primary: legal systems that have institutionalized the reading gain legitimacy and manage political pressure. Institutional beneficiary.
 *   - bioethicists_and_philosophical_analysts: observer seat examining coherence, scope, and implications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.52).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.71).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Autonomy-Primary Dignified Death (Self-Determination Reading)").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, 'bb68f3ca-54ae-458c-91d7-8bb84723a396').
narrative_ontology:cs_kernel_codification('bb68f3ca-54ae-458c-91d7-8bb84723a396', formalized).
narrative_ontology:cs_authority_grounding('bb68f3ca-54ae-458c-91d7-8bb84723a396', lineage).
narrative_ontology:cs_interpretation_layer_present('bb68f3ca-54ae-458c-91d7-8bb84723a396').
narrative_ontology:cs_reading_relation('bb68f3ca-54ae-458c-91d7-8bb84723a396', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_reading_relation('bb68f3ca-54ae-458c-91d7-8bb84723a396', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('bb68f3ca-54ae-458c-91d7-8bb84723a396', foundational, dignity_constituted_by_self_determination).
narrative_ontology:cs_axiom_status(dignity_constituted_by_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('bb68f3ca-54ae-458c-91d7-8bb84723a396', dignity_constituted_by_self_determination, deontological).
narrative_ontology:cs_axiom('bb68f3ca-54ae-458c-91d7-8bb84723a396', foundational, individual_retains_final_authority_over_death).
narrative_ontology:cs_axiom_status(individual_retains_final_authority_over_death, holdable).
narrative_ontology:cs_axiom_grounding('bb68f3ca-54ae-458c-91d7-8bb84723a396', individual_retains_final_authority_over_death, deontological).
narrative_ontology:cs_reference_frame('bb68f3ca-54ae-458c-91d7-8bb84723a396', individual_autonomous_dignity).
narrative_ontology:cs_drift_state('bb68f3ca-54ae-458c-91d7-8bb84723a396', contemporary_jurisdictional_divergence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bb68f3ca-54ae-458c-91d7-8bb84723a396', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, autonomous_dying_agent).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, suffering_individuals_denied_exit).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, families_and_relational_circles).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, jurisdictions_implementing_autonomy_primary).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, state_legal_authority).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, vulnerable_disabled_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A person in terminal illness or unbearable suffering who seeks to exercise self-determination over the timing and method of their death. Under this reading, dignity is constituted by their capacity to author the final chapter of their own life. They experience the constraint as affirmative permission to choose, though availability depends on jurisdiction and eligibility criteria. Their 'exit' is the choice itself — it must not be blocked.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, autonomous_dying_agent, beneficiary,
    moderate, immediate, arbitrage, national).

% Persons in terminal illness or unbearable suffering living in jurisdictions where the autonomy-primary reading is NOT adopted — where state law or medical gatekeeping prohibits or severely restricts self-determined death. They pay the cost of prolonged suffering against their will, denied the authority to choose. Their exit is blocked by legal prohibition and/or medical refusal.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, suffering_individuals_denied_exit, payer,
    powerless, immediate, trapped, national).

% Clinicians and institutional medical bodies that administer gatekeeping criteria: determining diagnosis, prognosis, eligibility, mental capacity, timing of access. They interpret and enforce the autonomy norm against countervailing medical ethics (do no harm, primacy of life preservation). Their role is to coordinate the norm's application and manage institutional liability.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_profession, agenda_setter,
    institutional, generational, analytical, national).

% Legislatures and courts that establish the legal framework for or against self-determined death. Where the autonomy-primary reading is institutionalized, the state enforces access; where it is rejected or restricted, the state enforces prohibition. The state is both setter of the norm and, in prohibition regimes, a payer — it bears the political and moral cost of denying citizens this choice.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_legal_authority, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, state_legal_authority, payer).

% Communities and institutions whose moral frameworks hold life's intrinsic sanctity as paramount, independent of consent or suffering. They are excluded from the decision-making process when autonomy-primary norms are applied; their objections — that intentional death-hastening violates transcendent moral order — are not given weight in the coordination mechanism. They would argue for a different distribution of authority.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, religious_and_sanctity_tradition_holders, excluded,
    organized, civilizational, constrained, national).

% Loved ones and kin of the dying person. Under the autonomy-primary reading, they are structurally excluded from decision authority, though they may be consulted. They bear the relational cost of the dying person's choice and the loss that follows, but their consent is not required. Some experience this as respectful (honoring the loved one's autonomy), others as exclusion from a fundamentally relational event.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, families_and_relational_circles, excluded,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, families_and_relational_circles, beneficiary).

% Disabled persons living with chronic, non-terminal conditions who may fear that expansive autonomy-primary framing will create pressure (explicit or internalized) toward death as a 'reasonable choice' given disability. They do not have terminal illness but bear risk from the norm's scope creep. Their exit is constrained by identity fusion — disability is not separable from self.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, vulnerable_disabled_populations, payer,
    powerless, biographical, identity_locked, national).

% Legal systems (e.g., Netherlands, Belgium, Switzerland, some Canadian provinces) that have institutionalized the autonomy-primary reading. They benefit by aligning law with an influential ethical doctrine, managing political pressure from advocates, and gaining legitimacy as 'humane' systems. They also bear enforcement cost and liability risk.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, jurisdictions_implementing_autonomy_primary, beneficiary,
    institutional, generational, analytical, global).

% Academics and professional ethicists examining the foundations and implications of the autonomy-primary reading. They do not make binding decisions but their analysis shapes legal doctrine, professional guidelines, and public understanding. They occupy a seat to examine coherence, scope, and boundary problems.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, bioethicists_and_philosophical_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the coordination problem of how to respect individual agency in end-of-life contexts when medical gatekeeping (diagnosis, prognosis, access) is unavoidable. The autonomy-primary reading coordinates around the principle that the dying person retains final decision authority, subject to procedural checks for capacity and informed consent — moving the locus of legitimate authority from medical paternalism to patient self-determination.
% TRANSFER_FUNCTION: Transfers decision authority and moral legitimacy from medical and state institutions to the suffering individual. In prohibitionist regimes, the constraint extracts obedience and suffering continuation from those who would choose death, transferring authority to state/medical gatekeepers. In permissive regimes, it transfers authority to the dying person and extracts procedural compliance from medical providers (documentation, eligibility assessment, waiting periods).
% ABSENT_VOICES: Religious and sanctity-tradition communities are structurally excluded — their objection that life-termination violates transcendent moral order is not given weight in autonomy-primary decision-making. Families and relational circles are excluded from decision authority, though they may be consulted. Disabled persons living with non-terminal chronic conditions are not at the table but bear risk from scope creep. Jurisdictions not implementing the autonomy-primary reading are not represented in forums where it is advanced.
% DISAPPEARANCE_RATIONALE: If the autonomy-primary norm vanished overnight in permissive jurisdictions, medical authority would revert to paternalistic gating (physician discretion on access), end-of-life timing would shift to covert practices or medical neglect, and legal systems would reorganize around life-preservation primacy. If it disappeared in prohibitionist jurisdictions, no immediate rearrangement would occur — the constraint is already the status quo there. Its disappearance elsewhere would represent a convergence toward restriction.
% FOUNDING_PROBLEM: Historical medical paternalism (physicians determining end-of-life timing and method without patient input) left terminal and severely suffering patients without recourse or voice in their own death. The founding problem is: how can individual agency be preserved in contexts where suffering is unbearable and death is imminent or inevitable?
% FOUNDING_PROBLEM_CORROBORATION: Advocates (patient-autonomy movements, some bioethicists, permissive jurisdictions) attest the founding problem is live and the autonomy-primary reading solves it. Sanctity-tradition holders and relational-autonomy advocates attest the founding problem is misdescribed — that the real problem is ensuring dignified death through relational wisdom and life-preserving default, not individual override. Empirical evidence from permissive jurisdictions shows the problem was real (covert practices were common); contested is whether autonomy-primary is the right solution or whether it creates new harms (scope creep, subtle coercion of disabled persons, exclusion of relational wisdom).
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__autonomy_primary, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) and rising through the period because the constraint entangles two functions: (1) genuine coordination — resolving the problem of decision authority in end-of-life contexts; (2) asymmetric extraction — in prohibitionist regimes, denying suffering individuals the choice they seek, transferring authority to state/medical gatekeepers. The time series shows extractiveness rising from 0.38 to 0.52 over 25 years, stabilizing thereafter, tracking the institutionalization of autonomy-primary reading in some jurisdictions while prohibition persists in others. Suppression is higher (0.71) because the constraint's persistence in prohibitionist regimes depends on active enforcement: legal prohibition, medical refusal, institutional gatekeeping. The reading must be defended against advocates and against individuals' own stated wishes. Theater is moderate-low (0.28): the procedural legitimacy of capacity assessment and informed consent is real, but a growing share of enforcement energy (especially in prohibitionist contexts) is spent defending the life-preservation default itself against challenge, not implementing transparent assessment. Accessibility collapse is moderate (0.62): alternatives (covert assistance, medical neglect, migration to permissive jurisdictions, non-compliant exit) exist but carry legal, relational, and existential costs. Resistance is high (0.73): both advocates (patient-autonomy movements) and opponents (sanctity traditions, relational-autonomy camps) mount sustained resistance; the constraint is actively contested.
 *
 * PERSPECTIVAL GAP:
 *   This reading's greatest perspectival divergence is between the beneficiary seat (autonomous agent) and the victim seat (suffering person denied exit). From the beneficiary seat, the autonomy-primary norm is liberatory: dignity emerges from choice, from authorship of one's final chapter. From the victim seat (in prohibitionist regimes), the same norm is experienced as a right that is denied, a betrayal of dignity through enforced passivity. Medical professionals in permissive jurisdictions experience gatekeeping as legitimate implementation of a sound principle; medical professionals in prohibitionist regimes experience gatekeeping as defending a life-preservation default that patients increasingly reject. The excluded seats (religious traditions, families) experience the norm very differently: traditions experience it as a categorical rejection of their moral framework; families experience it as honoring autonomy but sometimes as cold individualism. Disabled persons experience it as neutral in theory but threatening in practice (scope creep). The engine's per-seat computation should capture these divergences through power, exit_options, and role asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seat (autonomous_dying_agent): low d (near 0.1-0.2) — this agent benefits from the norm without running it; the choice itself is the benefit, and it requires no extraction from others to realize (though it does require medical cooperation). Victim seat (suffering_individuals_denied_exit) in prohibitionist regimes: high d (0.75-0.85) — these agents are trapped (identity_locked exit option), their suffering is prolonged against will, and they pay the cost of enforcement. Medical profession (agenda_setter): moderate-high d (0.55-0.65) — institutional power, they administer gatekeeping, they bear institutional liability, they are partially captured by the norm (must explain refusal) but retain discretionary authority. State authority (dual agenda_setter/payer): in permissive regimes, moderate d (0.45-0.55, beneficiary of norm legitimacy); in prohibitionist regimes, higher d (0.65-0.75, bearing political cost of denial). Religious traditions (excluded): moderate d (0.50-0.60) — they hold organized power but are systematically excluded from decision weight, so the norm extracts their forced non-participation. Families (excluded/beneficiary): moderate d (0.50) — they benefit from clarity (the dying person's wish is honored, relational ambiguity reduced) but pay through loss and exclusion from authority. Disabled populations (payer): high d (0.70-0.80) — they bear risk from scope creep; disability is identity-locked; alternatives (proving terminal status) don't exit them; subtle pressures accumulate.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (the founding mandate has outlived its function) does NOT apply to this reading at the interval's end. The founding mandate is 'preserve individual agency in end-of-life contexts against medical paternalism.' In permissive jurisdictions, the mandate is live and the constraint carries it out. In prohibitionist jurisdictions, the mandate is contested but not dead — advocates argue it is precisely the paternalism the norm opposes; opponents argue the mandate is misdescribed (the real problem is relational wisdom, not individual override). The constraint is not zombie-like; it is actively contested along its founding problem. Where the constraint does show traces of theater is in the gatekeeping layer: capacity assessment procedures have become increasingly formalized and ritualized, sometimes detached from genuine deliberation about whether the person's choice reflects authentic suffering or coerced resignation. This theater signal (0.28, moderate-low) suggests some degradation at the edges, but the core function (deciding who has authority) is still operative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_coercion_boundary,
    'Where is the boundary between respecting autonomy and detecting subtle coercion (economic desperation, family pressure, internalized ableist expectations) that makes the ''autonomous'' choice unfree?',
    'Longitudinal studies of persons who access autonomy-primary options vs. those who pursue life-extension after expressing initial interest in death; post-access interviews with families about relational pressures; analysis of how socioeconomic status and disability status correlate with access.',
    'If subtle coercion is pervasive, the beneficiary seat (autonomous agent) is partially illusory — the constraint is extracting choices from vulnerable populations rather than respecting them. If coercion is minimal, the constraint''s beneficiary framing is sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_coercion_boundary, empirical, 'Whether autonomy-primary access reflects free choice or accumulation of subtle pressures.').

omega_variable(
    scope_creep_disabled_populations,
    'Will the autonomy-primary reading''s reach expand from terminal illness and unbearable acute suffering to chronic disability and persistent suffering unrelated to death-imminence?',
    'Longitudinal tracking of scope changes in permissive jurisdictions: do eligibility criteria expand? Do disability-advocacy organizations report increased patient pressure or decreased institutional resistance to disabled persons'' end-of-life requests?',
    'If scope creep is substantial, disabled persons (currently excluded but identity-locked) become payers: they bear risk that their disability will be reframed as a candidate for death-as-reasonable-choice. The constraint transforms from benefiting terminal patients to extracting from disabled populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_creep_disabled_populations, empirical, 'Whether scope expansion will shift the victim set toward non-terminal disabled populations.').

omega_variable(
    reading_coexistence_or_foreclosure,
    'Are the autonomy-primary and sanctity-primary readings logically foreclosed from coexisting in a single legal framework, or can they coexist as competing live positions held by different parties?',
    'Formal logical analysis of the core premises: if autonomy-primary asserts the individual has FINAL authority and sanctity-primary asserts the individual does NOT (life-termination is categorically impermissible), they cannot coexist in the same decision rule. If they can coexist (e.g., pluralistic frameworks that permit individual choice while protecting conscientious objectors), they are live alternatives in social pluralism.',
    'If foreclosed, the kernel contest is zero-sum — one reading''s institutionalization excludes the other. If they coexist, the constraint''s type may shift toward Piton (performative pluralism hiding zero-sum power dynamics) or Tangled Rope (genuine pluralistic coordination carrying asymmetric costs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_or_foreclosure, conceptual, 'Whether the autonomy-primary reading logically forecloses the sanctity-primary reading in any single framework.').

omega_variable(
    reading_commission_vs_omission,
    'Does dignity in death require active authorization for a chosen death (commission: ''I decide to end my life''), or is it sufficient that the state does not prevent natural death-hastening through withdrawal of treatment (omission: ''I decide to stop fighting'')?',
    'Comparative institutional analysis of how permissive jurisdictions frame access: is it actively-chosen death (commission) or protected refusal of treatment (omission)? Do they expand to active medical assistance (euthanasia) or remain at treatment withdrawal?',
    'If the autonomy-primary reading requires commission, it makes a stronger claim about the individual''s authority and carries higher moral conflict with sanctity traditions. If it permits omission only, it may converge with relational-autonomy reading on procedural safeguards and relational witness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_commission_vs_omission, conceptual, 'Whether autonomy-primary dignity requires active chosen death or can be satisfied by protected refusal.').

omega_variable(
    sibling_reading_context_dependence,
    'Is the choice between autonomy-primary and relational-autonomy readings culturally contingent (Western individualism vs. relational ethics in other traditions) or do they represent genuinely distinct ethical structures applicable across cultural contexts?',
    'Cross-cultural analysis of how different philosophical and religious traditions conceptualize death-authority (e.g., Confucian relational duty, Islamic trust in divine will, Ubuntu personhood-through-relation). Do they map to relational-autonomy or produce a third reading?',
    'If culturally contingent, the autonomy-primary reading''s universalization may constitute structural imperialism. If genuinely distinct ethical structures, they are live alternatives across contexts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_context_dependence, conceptual, 'Whether the autonomy-primary vs. relational-autonomy distinction is culturally universal or context-contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__autonomy_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(dign_tr_t0, observed).
narrative_ontology:measurement(dign_tr_t5, dignified_death__autonomy_primary, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(dign_tr_t5, observed).
narrative_ontology:measurement(dign_tr_t10, dignified_death__autonomy_primary, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(dign_tr_t10, observed).
narrative_ontology:measurement(dign_tr_t15, dignified_death__autonomy_primary, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(dign_tr_t15, observed).
narrative_ontology:measurement(dign_tr_t20, dignified_death__autonomy_primary, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(dign_tr_t20, observed).
narrative_ontology:measurement(dign_tr_t25, dignified_death__autonomy_primary, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(dign_tr_t25, observed).
narrative_ontology:measurement(dign_tr_t30, dignified_death__autonomy_primary, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(dign_tr_t30, observed).
narrative_ontology:measurement(dign_tr_t35, dignified_death__autonomy_primary, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(dign_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__autonomy_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(dign_be_t0, observed).
narrative_ontology:measurement(dign_be_t5, dignified_death__autonomy_primary, base_extractiveness, 5, 0.41).
narrative_ontology:measurement_basis(dign_be_t5, observed).
narrative_ontology:measurement(dign_be_t10, dignified_death__autonomy_primary, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(dign_be_t10, observed).
narrative_ontology:measurement(dign_be_t15, dignified_death__autonomy_primary, base_extractiveness, 15, 0.48).
narrative_ontology:measurement_basis(dign_be_t15, observed).
narrative_ontology:measurement(dign_be_t20, dignified_death__autonomy_primary, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(dign_be_t20, observed).
narrative_ontology:measurement(dign_be_t25, dignified_death__autonomy_primary, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(dign_be_t25, observed).
narrative_ontology:measurement(dign_be_t30, dignified_death__autonomy_primary, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(dign_be_t30, observed).
narrative_ontology:measurement(dign_be_t35, dignified_death__autonomy_primary, base_extractiveness, 35, 0.52).
narrative_ontology:measurement_basis(dign_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__autonomy_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(dign_su_t0, observed).
narrative_ontology:measurement(dign_su_t5, dignified_death__autonomy_primary, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(dign_su_t5, observed).
narrative_ontology:measurement(dign_su_t10, dignified_death__autonomy_primary, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(dign_su_t10, observed).
narrative_ontology:measurement(dign_su_t15, dignified_death__autonomy_primary, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(dign_su_t15, observed).
narrative_ontology:measurement(dign_su_t20, dignified_death__autonomy_primary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(dign_su_t20, observed).
narrative_ontology:measurement(dign_su_t25, dignified_death__autonomy_primary, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(dign_su_t25, observed).
narrative_ontology:measurement(dign_su_t30, dignified_death__autonomy_primary, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(dign_su_t30, observed).
narrative_ontology:measurement(dign_su_t35, dignified_death__autonomy_primary, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(dign_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dignified_death__autonomy_primary, 0.12).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__sanctity_primary).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% The kernel 'dignified_death' decomposes into three distinct constraints, each instantiating a different reading. Autonomy-primary asserts individual final authority; sanctity-primary asserts life's intrinsic value independent of consent; relational-autonomy distributes authority across the patient-family-clinician triad. These are NOT the same constraint viewed from different angles. Each reading has its own beneficiary structure, victim set, and epsilon value. The family is linked via network.affects_constraints; each member affects the others through institutional competition, legal precedent, and theological debate. Autonomy-primary influences (and may eventually foreclose) sanctity-primary in jurisdictions adopting it; relational-autonomy influences both by offering a compromise path. Sanctity-primary forecloses autonomy-primary in theocratic frameworks. These are the living alternatives in the kernel contest, modeled as separate constraint stories with explicit reading_relations in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
