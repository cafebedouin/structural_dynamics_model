% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__absolutist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Absolutist Reading: Categorical Speech Protection
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The absolutist reading of the First Amendment holds that the
 *   constitutional text 'Congress shall make no law...abridging the freedom
 *   of speech' means categorical protection for all speech except narrow,
 *   historically established exceptions (true threats, incitement to imminent
 *   lawless action, fighting words). Under this reading, harm to minorities,
 *   harassment victims, and vulnerable communities is externalized as a cost
 *   of protecting speech — speakers and majorities benefit from unrestricted
 *   expression; targeted minorities bear unconsented-to harm costs with no
 *   legal remedy. The claim is 'rope' (genuine coordination solving a real
 *   problem: preventing government suppression of dissent). The metrics are
 *   substantially extractive (0.68) because the constraint's effect is to
 *   concentrate speech benefits in empowered speakers while distributing harm
 *   costs to vulnerable targets, and active enforcement is required to block
 *   legislative harm-based exceptions. This divergence (claimed rope,
 *   measured extractive) is the central measurement the constraint story
 *   exists to expose.
 *
 * KEY AGENTS:
 *   - Speakers holding majority viewpoints or institutional power: free to amplify, organize, no suppression risk
 *   - Political dissenters with legal/organizational resources: protected from silencing, can leverage rule for movement speech
 *   - Targeted minorities and harassment victims: no remedy for speech-based harm, trapped in the constraint
 *   - Marginalized speech communities: structurally excluded from rule's baseline assumptions
 *   - Constitutional courts: enforce the reading through doctrine, block legislative carve-outs
 *   - Legislative coalitions: periodically attempt harm-based exceptions, blocked by institutional enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.68).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.45).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, rope).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Reading: Categorical Speech Protection").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, '69ab6904-09b5-4ed5-b22c-91f6a1fc49a2').
narrative_ontology:cs_kernel_codification('69ab6904-09b5-4ed5-b22c-91f6a1fc49a2', fixed_text).
narrative_ontology:cs_authority_grounding('69ab6904-09b5-4ed5-b22c-91f6a1fc49a2', lineage).
narrative_ontology:cs_interpretation_layer_present('69ab6904-09b5-4ed5-b22c-91f6a1fc49a2').
narrative_ontology:cs_reading_relation('69ab6904-09b5-4ed5-b22c-91f6a1fc49a2', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('69ab6904-09b5-4ed5-b22c-91f6a1fc49a2', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('69ab6904-09b5-4ed5-b22c-91f6a1fc49a2', foundational, speech_protection_categorical_no_balancing).
narrative_ontology:cs_axiom_status(speech_protection_categorical_no_balancing, holdable).
narrative_ontology:cs_axiom_grounding('69ab6904-09b5-4ed5-b22c-91f6a1fc49a2', speech_protection_categorical_no_balancing, deontological).
narrative_ontology:cs_axiom('69ab6904-09b5-4ed5-b22c-91f6a1fc49a2', foundational, textual_originalism_speech_no_law).
narrative_ontology:cs_axiom_status(textual_originalism_speech_no_law, holdable).
narrative_ontology:cs_axiom_grounding('69ab6904-09b5-4ed5-b22c-91f6a1fc49a2', textual_originalism_speech_no_law, empirically_contingent).
narrative_ontology:cs_reference_frame('69ab6904-09b5-4ed5-b22c-91f6a1fc49a2', original_public_meaning_no_law_categorical).
narrative_ontology:cs_drift_state('69ab6904-09b5-4ed5-b22c-91f6a1fc49a2', contemporary_networked_speech_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('69ab6904-09b5-4ed5-b22c-91f6a1fc49a2', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, speakers_majority_viewpoint).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, political_dissenters_empowered_position).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_minorities_systemic_oppression).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, harassment_victims_no_remedy).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, textual_originalism).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, categorical_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Speakers holding majority or powerful viewpoints (commercial speech, political speech aligned with institutional power, religious speech from numerically dominant tradition) face minimal risk of suppression under absolutist reading. They can amplify, organize, and spread their message without strategic concern about harm-based restrictions. Exit from the constraint is unnecessary because the constraint protects their speech.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, speakers_majority_viewpoint, beneficiary,
    organized, generational, mobile, national).

% Political and social dissenters with institutional backing, legal resources, and organizational capacity benefit from categorical protection — their radical speech, even when highly offensive or destabilizing, is sheltered from suppression or liability. They can leverage the absolutist rule to maximize protected speech space for their movement.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, political_dissenters_empowered_position, beneficiary,
    powerful, generational, arbitrage, national).

% Minorities targeted by slurs, incitement, harassment, and dehumanizing speech campaigns have no remedy within the absolutist framework — speech calling for their exclusion, removal, or harm is constitutionally protected. Their recourse is silence, withdrawal from public spaces, or accepting the psychological/material costs of exposure to speech designed to intimidate them. They cannot exit the constraint; it governs the public square they need to participate in.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_minorities_systemic_oppression, payer,
    powerless, biographical, trapped, national).

% Individuals subjected to targeted harassment, doxing, coordinated abuse, or defamation campaigns cannot seek legal remedy under absolutist reading if the speech is deemed 'political' or 'expressive' — they bear the material and psychological costs (safety threats, economic harm, reputational damage) without legal recourse. Their constraint options are limited to private platform moderation or geographic relocation.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, harassment_victims_no_remedy, payer,
    moderate, biographical, constrained, national).

% Speech communities organized around marginalized identities (historically enslaved, colonized, or persecuted groups) are excluded from the speech protection calculus — the absolutist rule was authored before their participation in public discourse was theoretically possible, and the rule does not account for their structural vulnerability to speech-based harm. Their exclusion is not formal but structural: the baseline of 'free speech' assumes speakers from the historically empowered groups.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, marginalized_speech_communities, excluded,
    powerless, biographical, identity_locked, national).

% Courts administering First Amendment doctrine enforce the absolutist reading by striking down harm-based restrictions and setting precedent for categorical protection. They maintain the constraint through doctrine and refuse exceptions even when harm is demonstrable. Their institutional position allows them to hold the line against legislative or executive pressure to narrow protection.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Legislative bodies and civil-rights coalitions periodically attempt to carve out exceptions for speech that causes demonstrable harm (hate speech, incitement to violence, coordinated harassment), but the absolutist reading's institutional enforcement blocks these attempts. They observe the constraint and seek to revise it, but lack the power to override judicial doctrine without constitutional amendment.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, legislative_pressure_coalitions, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__absolutist_reading, speakers_majority_viewpoint).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a categorical, judicially enforceable rule: speech receives protection unless it falls into pre-existing narrow exceptions (true threats, incitement to imminent lawless action, fighting words). Solves the coordination problem of preventing government suppression of disfavored political speech — a speaker cannot be silenced for the ideological content of their expression.
% TRANSFER_FUNCTION: Transfers the cost of expressive freedom to targets of speech: minorities, harassment victims, and marginalized communities bear the unconsented-to harm from protected speech (slurs, incitement, dehumanization, coordinated campaigns) without legal remedy. The speaker and broader speech-protection beneficiaries gain unrestricted expression; the targeted communities externalize the harm cost.
% ABSENT_VOICES: Marginalized speech communities whose existence postdates the rule's authoring; individuals bearing coordinated harassment costs; groups targeted by incitement-adjacent speech; victims of speech-enabled violence who cannot establish direct causation. These communities would argue for speech restrictions that account for systemic vulnerability and compound harm, but their voices are structurally excluded from the rule's baseline assumptions.
% DISAPPEARANCE_RATIONALE: If the absolutist reading disappeared and harm-based restrictions became permitted (even in narrow form), the speech landscape would reorganize: minority protection might increase, coordinated harassment campaigns would face legal liability, incitement-adjacent speech would face scrutiny. Speakers and institutions currently benefiting from categorical protection would face new constraint costs. The public square would restructure around different protection boundaries.
% FOUNDING_PROBLEM: Prevent government silencing of political dissent and suppression of unpopular ideology. The founding problem is explicitly about protecting speech from democratic majoritarian suppression, not about protecting speakers from each other.
% FOUNDING_PROBLEM_CORROBORATION: Government suppression of dissent remains a persistent risk, attested by comparative constitutional law (other democracies' censorship patterns), historical examples (McCarthy era, protest suppression), and judicial reasoning in major cases (New York Times v. Sullivan, Brandenburg v. Ohio). The founding problem is alive. Whether the absolutist reading is the optimal solution to that problem is contested (see sibling readings and harm_limited concerns), but the problem itself is live. Independent sources outside the benefiting parties (civil-liberties organizations, international human-rights bodies, comparative constitutional scholars) corroborate that government suppression of dissent is a genuine problem.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__absolutist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the reading's effect is demonstrably asymmetric: speakers (especially empowered ones) extract freedom from the constraint; targeted communities extract costs (harm, no remedy, participation barriers). Extractiveness is not at maximum (0.85+) because the founding problem is real and the constraint does solve it — preventing government suppression of dissent is a genuine coordination function. The metric reflects the mixed character: real coordination value + substantial harm externalization = extractive but not pure snare. Suppression is lower (0.45) because the absolutist reading does NOT suppress speech — it protects it even when that protection is used to harm others. The suppression applies to harm-based restrictions (courts actively suppress legislative attempts to narrow protection), not to speakers. Theater ratio is low (0.22) because the functional activity (blocking government censorship) is real; the performative activity (courts insisting the rule is natural law when it is institutional choice) is limited. Resistance is high (0.71) because legislative bodies, civil-rights coalitions, marginalized communities, and harassment victims all actively resist the absolutist reading and propose alternatives, and that resistance is meeting persistent institutional enforcement (courts hold the line against harm-based exceptions). The measurement series shows extractiveness and suppression requirements rising slightly (enforcement hardening as resistance grows) and stabilizing, while theater ratio plateaus — consistent with a constraint that is actively maintained against mounting pressure but not yet theatricalized.
 *
 * PERSPECTIVAL GAP:
 *   Speakers and courts holding the absolutist reading perceive this as pure rope: a genuine coordination rule preventing oppressive government power, with no victims — only free agents making speech choices and others responding. Targeted minorities, harassment victims, and legislative coalitions perceive this as snare-like extraction: the rule is authored to protect the historically empowered, enforced against democratic attempts to protect vulnerable speakers from speech-based harm, and produces a systematic cost transfer from majorities to minorities. The engine computes this divergence from structural data: speakers have d near 0.0 (full beneficiaries), minorities have d near 1.0 (full targets), and the computed type differs between seats. The authored claim (rope) reflects the agenda-setter seat's frame; the measured extraction reflects the victim seat's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are speakers holding majority viewpoints or institutional resources (organized power, mobile exit, no suppression risk — low directionality) and political dissenters with legal backing (powerful, arbitrage-capable, leveraging the rule for movement speech — low directionality). Victims are targeted minorities (powerless, identity-locked, trapped in the constraint, bearing unconsented-to harm — high directionality) and harassment victims (moderate power, constrained exit, no legal remedy — high directionality). The excluded stakeholder (marginalized speech communities) has zero power and identity-locked exit, but is excluded from the rule's baseline assumption — they are not yet participants in the public square the rule governs, so the rule was authored without accounting for their structure. Courts enforce the rule actively, holding the beneficiary seats' position against legislative pressure to narrow protection.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading exhibits mandatrophy — the founding problem (preventing government suppression of dissent) remains live, but the institutional response (categorical protection without harm mitigation) has outlived optimal fit. The rule solves the founding problem but externalizes costs to minorities in a way that was not necessary at the founding (when minorities were systematically excluded from public discourse) and is increasingly problematic as minorities participate and become targets. The rule persists because (1) courts have institutional power to enforce it and face no strong political cost for doing so, (2) empowered speakers benefit and have resources to defend it, and (3) harm to minorities is distributed and lacks a legal remedy pathway. Mandatrophy resolution would require either (a) revising the rule to permit harm-based exceptions while preserving core anti-suppression function, (b) creating parallel structures (safety infrastructure, counter-speech funding, restorative justice) that mitigate harm without touching speech law, or (c) accepting the harm cost as necessary to the rule and developing ways to support targets outside constitutional law. The constraint story documents this trajectory in the measurement series: extractiveness plateaus as the rule stabilizes and political pressure mounts, resistance grows, but the institutional position holds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_baseline_ambiguity,
    'Is the baseline harm cost externalized to minorities a feature of the absolutist reading or a contingent outcome that could be mitigated within the same reading?',
    'Distinguish between (a) harm that is logically entailed by categorical protection (if speech is always protected, speakers targeting minorities must be protected) versus (b) harm that arises from the reading''s application context (coordinated harassment, algorithmic amplification, structural inequality) and might be addressed through remedies outside speech law (safety infrastructure, counter-speech funding, restorative justice). The distinction clarifies whether harm externalization is constitutive or contextual.',
    'If harm is constitutive, the absolutist reading inherently trades off majority speech freedom against minority safety — mandatrophy resolved. If contextual, the reading could be held with parallel harm-mitigation structures — mandatrophy unresolved. The characterization affects whether this is a pure rope or a Tangled Rope with uncompensated victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_baseline_ambiguity, conceptual, 'Whether externalized harm to minorities is constitutive of absolutist protection or contingent.').

omega_variable(
    historical_exclusion_scope,
    'Are the ''narrow historical exclusions'' (true threats, incitement to imminent lawless action, fighting words) genuinely exhaustive, or does the absolutist reading systematically exclude speech forms that were not invented when those categories were established?',
    'Examine how courts have handled new speech categories developed after the narrow exceptions were set (coordinated online harassment, synthetic media, algorithmic incitement, speech-enabled violence in networked contexts). If new categories require exception-creation, the historical closure is not really historical; if they are absorbed into old categories without exception, the reading is internally consistent but potentially overgeneralizing.',
    'If the reading must expand exceptions to accommodate modern harm, it is not truly absolutist — the narrow exceptions are a function of historical technology, not principle. If the reading absorbs new harms into old exceptions, it is absolutist but increasingly strained. Either outcome suggests the reading''s claim/metric gap (claimed rope, measured extractive) has a deeper structural source.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_exclusion_scope, empirical, 'Whether the ''narrow historical exclusions'' remain exhaustive as speech technologies evolve.').

omega_variable(
    coordinated_harassment_causation,
    'In cases of coordinated harassment campaigns (doxing, targeted abuse), how does the absolutist reading treat the causation chain: is the protected speech itself the harm, or does the harm arise from coordination/amplification mechanisms outside the speech act?',
    'Distinguish between (a) individual speech acts protected under the reading (each person''s slur or threat is protected because it does not meet imminent-lawless-action threshold) versus (b) the aggregate harmful outcome from coordination (the same speech, repeated by 10,000 people, causes demonstrable safety/economic harm). If harm emerges at the coordination level, not the individual-speech level, the absolutist reading protects the components while remaining agnostic about their aggregated impact.',
    'If the reading treats coordination as a separate mechanism, harassment victims have no speech-law remedy but might have alternatives (organizational liability, platform responsibility, civil conspiracy law). If the reading treats coordinated speech as atomized protected expression, victims have no remedy anywhere. The distinction clarifies the reading''s scope and whether alternative legal structures could mitigate harm without revising First Amendment doctrine itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinated_harassment_causation, conceptual, 'Whether coordinated harassment is a speech-law problem or a coordination-liability problem.').

omega_variable(
    kernel_reading_contestation,
    'This constraint is the absolutist reading of the First Amendment kernel. What is the structural relationship between this reading and the sibling readings (harm_limited_reading, categorical_balancing_reading)?',
    'Consult cs_structure.reading_relations and cs_structure.axioms for the formal specification: the relation to each sibling (forecloses/coexists_with/influences) and the foundational axioms that distinguish this reading. The omega documents committer structure per Rule 2.',
    'The reading relations determine how readings interact in institutional practice: if this reading forecloses the siblings, only one can be authoritative at a time (institutional contest, one wins). If they coexist, multiple readings are live in different courts/jurisdictions (fragmentation). If this reading influences the siblings, it sets the baseline others must work within or against.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Formal specification of this reading''s relationship to sibling readings in the First Amendment kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__absolutist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(firs_tr_t0, observed).
narrative_ontology:measurement(firs_tr_t5, first_amendment_speech_protection__absolutist_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement_basis(firs_tr_t5, observed).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__absolutist_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(firs_tr_t10, observed).
narrative_ontology:measurement(firs_tr_t15, first_amendment_speech_protection__absolutist_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(firs_tr_t15, observed).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__absolutist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(firs_tr_t20, observed).
narrative_ontology:measurement(firs_tr_t25, first_amendment_speech_protection__absolutist_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(firs_tr_t25, observed).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__absolutist_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(firs_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(firs_be_t0, observed).
narrative_ontology:measurement(firs_be_t5, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(firs_be_t5, observed).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(firs_be_t10, observed).
narrative_ontology:measurement(firs_be_t15, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(firs_be_t15, observed).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(firs_be_t20, observed).
narrative_ontology:measurement(firs_be_t25, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(firs_be_t25, observed).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(firs_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(firs_su_t0, observed).
narrative_ontology:measurement(firs_su_t5, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(firs_su_t5, observed).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 10, 0.43).
narrative_ontology:measurement_basis(firs_su_t10, observed).
narrative_ontology:measurement(firs_su_t15, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement_basis(firs_su_t15, observed).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(firs_su_t20, observed).
narrative_ontology:measurement(firs_su_t25, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 25, 0.45).
narrative_ontology:measurement_basis(firs_su_t25, observed).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(firs_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__absolutist_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% The First Amendment kernel decomposes into three structurally distinct constraints corresponding to three live readings of the constitutional text. The absolutist reading (this story) instantiates a maximum-protection constraint that externalizes harm to minorities; the harm_limited reading instantiates a harm-balanced constraint that restricts categorical protection; the categorical_balancing reading instantiates a case-by-case doctrinal constraint. Each reading has a different ε (measured extractiveness), different beneficiary/victim set, and different type. They are linked via the kernel network because each reading affects the institutional and doctrinal landscape for the others: the absolutist reading's institutional entrenchment constrains the practical applicability of the harm_limited reading, and both influence the balancing reading's case outcomes. This decomposition follows from DP-001 (ε-invariance): the three readings would produce substantially different ε values (absolutist ~0.68, harm_limited ~0.25, balancing ~0.45) if measured under their respective reading's lights, and the structural beneficiary/victim sets differ, so they are three constraints, not three framings of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__absolutist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
