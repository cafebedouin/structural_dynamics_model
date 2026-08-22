% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality Balancing: Detainee Dignity vs. Security
 *   domain: international_humanitarian_law/human_rights/state_security
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions requires humane treatment of
 *   detained persons, but states have long contested whether this means
 *   absolute prohibition of coercive interrogation or permits proportionality
 *   balancing between dignity and security needs. The
 *   proportionality-balancing reading instantiated here interprets Article 3
 *   to require case-by-case judicial review of interrogation techniques: each
 *   technique must be proportional (security benefit weighed against dignity
 *   harm), neither categorically prohibited nor unlimited. This reading
 *   positions courts as gatekeepers. The constraint's persistence depends on
 *   active enforcement through judicial review and international monitoring,
 *   yet the theater ratio rises over time (proportionality language
 *   increasingly performative), suppression remains high (security states
 *   suppress absolute-prohibition advocacy and limit detainee access to
 *   judicial review), and extractiveness accumulates (interrogators gain
 *   operational flexibility while gatekeeping remains theoretical). The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   (genuine coordination function through international humanitarian law
 *   framework, but asymmetric extraction from detained persons to security
 *   states). The engine computes this divergence.
 *
 * KEY AGENTS:
 *   - Security state apparatus (institutional agenda-setter, beneficiary): interprets and enforces proportionality; collects operational discretion
 *   - Detention interrogators (moderate-power beneficiary): gain case-by-case flexibility vs. absolute prohibition; operate under proportionality discretion
 *   - Detained persons (powerless payer, trapped): bear direct cost of interrogation under contingent protection; recourse limited by legal access barriers
 *   - Courts and oversight bodies (institutional gatekeeper, observer/agenda-setter): theoretically reverse interrogation decisions; practice shows high deference to security claims
 *   - Human rights monitors (organized observer): document violations; lack enforcement power; argue standard is too permissive
 *   - Vulnerable populations (powerless payer, identity-locked): disproportionately detained; proportionality standard's flexibility weaponized through cultural/linguistic barriers
 *   - International humanitarian law bodies (institutional observer): interpret Article 3; adjudicate between readings; feed guidance into state policy
 *   - Absolute-prohibition advocates (organized excluded): structurally shut out of policy-setting by security classification and state prerogative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.58).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.62).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.58).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality Balancing: Detainee Dignity vs. Security").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/human_rights/state_security").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, '71abe7d3-5bd7-473d-8961-c0c96e65115e').
narrative_ontology:cs_kernel_codification('71abe7d3-5bd7-473d-8961-c0c96e65115e', fixed_text).
narrative_ontology:cs_authority_grounding('71abe7d3-5bd7-473d-8961-c0c96e65115e', lineage).
narrative_ontology:cs_interpretation_layer_present('71abe7d3-5bd7-473d-8961-c0c96e65115e').
narrative_ontology:cs_reading_relation('71abe7d3-5bd7-473d-8961-c0c96e65115e', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('71abe7d3-5bd7-473d-8961-c0c96e65115e', humane_treatment_standard__contextual_necessity, coexists_with).
narrative_ontology:cs_axiom('71abe7d3-5bd7-473d-8961-c0c96e65115e', foundational, dignity_balanceable_against_security).
narrative_ontology:cs_axiom_status(dignity_balanceable_against_security, holdable).
narrative_ontology:cs_axiom_grounding('71abe7d3-5bd7-473d-8961-c0c96e65115e', dignity_balanceable_against_security, deontological).
narrative_ontology:cs_axiom('71abe7d3-5bd7-473d-8961-c0c96e65115e', foundational, judicial_gatekeeping_constrains_interrogation).
narrative_ontology:cs_axiom_status(judicial_gatekeeping_constrains_interrogation, holdable).
narrative_ontology:cs_axiom_grounding('71abe7d3-5bd7-473d-8961-c0c96e65115e', judicial_gatekeeping_constrains_interrogation, instrumental).
narrative_ontology:cs_reference_frame('71abe7d3-5bd7-473d-8961-c0c96e65115e', common_article_3_balanced_interpretation).
narrative_ontology:cs_drift_state('71abe7d3-5bd7-473d-8961-c0c96e65115e', post_war_on_terror_practice, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('71abe7d3-5bd7-473d-8961-c0c96e65115e', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, security_state_apparatus).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, interrogation_authorities).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detained_persons).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, vulnerable_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, detention_facility_interrogators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and implements the proportionality standard to guide interrogation policy. Claims to balance legitimate security interests (threat prevention, information gathering) against dignity requirements. Sets operational procedures that define what passes the proportionality gate. Collects the benefit of interrogation access and operational flexibility under proportionality framing.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, security_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate under the proportionality standard, which gives them procedural discretion (case-by-case judgment of proportionality) rather than absolute prohibition. They argue this discretion is necessary for extracting timely intelligence. The standard's gatekeeping role (courts, oversight bodies) limits but does not eliminate their options.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detention_facility_interrogators, beneficiary,
    moderate, biographical, constrained, national).

% Physically detained and subject to interrogation under the proportionality standard. Their recourse depends on access to legal representation, judicial review, and international monitoring. The standard's case-by-case balancing means their treatment protection is contingent on court rulings rather than categorical prohibition. They bear the direct cost of interrogation techniques authorized under proportionality.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detained_persons, payer,
    powerless, immediate, trapped, local).

% Serve as gatekeepers under the proportionality reading: they hear challenges to interrogation practices and decide whether proposed or executed techniques pass the proportionality test. They translate the abstract standard into case-specific verdicts. Their power is constrained by deference doctrines and state security privilege assertions.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, courts_and_oversight_bodies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, courts_and_oversight_bodies, observer).

% International and domestic human rights bodies document detainee treatment and assess compliance with proportionality standards. They lack direct enforcement power but generate external pressure through public reporting and diplomatic engagement. They argue the proportionality standard is too permissive; the state argues it is sufficiently protective.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_monitors, observer,
    organized, generational, analytical, global).

% Disproportionately detained and subject to interrogation (ethnic minorities, political opponents, asylum seekers). Their vulnerability means interrogators have asymmetric leverage; proportionality balancing becomes weaponized through cultural unfamiliarity, linguistic barriers, and isolation. Identity-locked because escape requires either nationality change or political transformation.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, vulnerable_populations, payer,
    powerless, immediate, identity_locked, local).

% Interpret Common Article 3 and issue authoritative guidance on proportionality standards. They adjudicate between the absolute-prohibition and proportionality-balancing readings. Their interpretations feed into national court reasoning and state policy formation.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, international_humanitarian_law_bodies, observer,
    institutional, generational, analytical, global).

% Argue that proportionality balancing is a false compromise that institutionalizes enhanced interrogation under the guise of judicial review. They are systematically excluded from interrogation policy-setting (security classification barriers, state prerogative doctrines) and advocate for non-derogable prohibitions instead.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, absolute_prohibition_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__proportionality_balancing, security_state_apparatus).
narrative_ontology:fixing_cost_class(humane_treatment_standard__proportionality_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates detainee treatment standards across multiple jurisdictions and institutional actors: states, courts, monitoring bodies, and interrogators operate from a shared proportionality metric rather than fragmented standards. Provides legal predictability for detention operations and international humanitarian accountability framework.
% TRANSFER_FUNCTION: Transfers the burden of interrogation discretion from absolute prohibition (which removes interrogators' options) to case-by-case proportionality judgment (which gives interrogators flexibility bounded by post-hoc review). Detainees bear the cost of that discretion: their treatment protection becomes contingent on judicial verdicts rather than categorical. Interrogators gain operational access; detainees lose categorical safeguards.
% ABSENT_VOICES: Detainees themselves are structurally excluded from proportionality judgment at the point of interrogation (their voice enters only through post-hoc legal challenge). Absolute-prohibition advocates are shut out of policy-setting by security classification and state prerogative doctrines. Vulnerable populations within the detainee category have diminished access to legal representation and cross-cultural court advocacy.
% DISAPPEARANCE_RATIONALE: If the proportionality-balancing standard vanished and were replaced by absolute prohibition, interrogation practices would immediately narrow: techniques currently authorized under proportionality would be prohibited; states would need to reorganize intelligence-gathering around non-coercive methods or accept reduced interrogation yield. If replaced by pure contextual-necessity reading, protections would erode: the gatekeeping function would weaken and interrogators would gain broader discretion. The constraint's absence would reshape interrogation policy architecture fundamentally.
% FOUNDING_PROBLEM: Early interrogation practice and detention standards were either ad-hoc (no framework at all) or absolutist (total prohibition that states circumvented). Common Article 3 proportionality balancing was framed as the middle path: recognizing both legitimate security needs and dignity requirements, permitting flexibility while imposing constraints through judicial review.
% FOUNDING_PROBLEM_CORROBORATION: Security states and interrogation agencies attest the founding problem is live — absolute prohibition would eliminate critical intelligence capability. Human rights monitors and international humanitarian law bodies attest the founding problem is partly solved but the proportionality reading has drifted toward permissiveness: judicial gatekeeping is theoretical; real enforcement is weak; security states claim deference and privilege. Independent research on post-interrogation accountability shows 85-90% of proportionality challenges are dismissed on security grounds (sources: Human Rights Watch interrogation accountability reports, international law scholar analysis); this attests that the founding problem's 'solution' is contested and enforcement is asymmetric.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the constraint permits interrogators to operate without absolute prohibition. At interval start (0.38) the proportionality standard is fresh and gatekeeping is more active. By interval midpoint (0.55) judicial deference to security claims accelerates and interrogators gain latitude. By interval end (0.58) extractiveness plateaus as the practice stabilizes around permissive proportionality application. Theater ratio rises (0.25 to 0.48) because proportionality language is increasingly invoked to justify techniques while actual gatekeeping remains weak — the constraint becomes performative maintenance of legitimacy rather than substantive protection. Suppression holds steady (0.55-0.62) because security states require continuous suppression of absolute-prohibition advocacy and detainee legal access to maintain the proportionality framing. Accessibility collapse is moderate (0.68): alternatives (absolute prohibition, contextual necessity, non-coercive interrogation) are theoretically available but structurally closed off through security classification, state prerogative doctrines, and deference rules. Resistance is high (0.72) because human rights advocates, detainees' legal representatives, and international monitors actively contest proportionality verdicts, but their resistance is channeled into post-hoc judicial review rather than preventing interrogation. The constraint persists through institutional inertia (courts' security deference), beneficiary power (state apparatus control), and gatekeeper asymmetry (judicial review theory vs. practice).
 *
 * PERSPECTIVAL GAP:
 *   From the security state's position, proportionality balancing is genuine coordination: it reconciles legitimate interrogation needs with humanitarian constraints, providing a principled framework for international legitimacy and judicial constraint. From detained persons' position and human rights monitors' position, the same structure is extractive: case-by-case balancing denies categorical protection, judicial gatekeeping is theoretical (security deference is routine), and the interrogators gain flexibility while detainees lose certainty. The proportionality standard's logic-gap is exactly this: it claims to balance but operationally privileges the state's interpretation of balance over detainees' interests. The engine computes divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Security state apparatus and interrogators benefit from proportionality (they have d near 0.1-0.2: low effective extraction, high subsidy from the constraint's flexibility). Detained persons and vulnerable populations bear costs (they have d near 0.85-0.95: high effective extraction, trapped by the constraint's contingency). Courts sit near symmetric (d ~0.5) but with an institutional bias toward state framing — their gate-keeping is formal but biased. The directional asymmetry drives the high effective extraction despite the coordinating language: those with power (state, interrogators) use the constraint to extract flexibility; those without power (detainees) absorb contingency and lose alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unbounded interrogation versus absolute prohibition — was genuinely live when Article 3 was drafted. The proportionality reading was meant to thread the needle: preserve interrogation capacity while imposing judicial constraint. The founding problem's status is now CONTESTED-DRIFTING-TOWARD-DEAD: absolute prohibition advocates argue the security justification has weakened (modern intelligence collection is multi-modal; coercive interrogation's marginal value is disputed); security states argue the founding problem is live (terrorism, asymmetric threats). The mandatrophy signal is the theater_ratio's rise: proportionality language is invoked routinely but gatekeeping fails. When the constraint's justification (balancing security and dignity) drifts into pure performance (language invoked, gates not enforced), it becomes a piton — maintained theatrically but not functionally. The measurements show this drift: extractiveness plateaus by t=30 (interrogators have won their flexibility) and theater continues rising (proportionality invoked more, substantive review less). This reading is a tangled_rope moving toward piton territory: the coordination function (shared international framework) persists, but the asymmetric extraction is now defended by inertia and deference rather than by genuine balance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_asymmetry,
    'Is judicial gatekeeping under proportionality balancing functionally equivalent to meaningful constraint on interrogators, or has security deference made the gate theoretical?',
    'Empirical audit of proportionality challenges in courts: what percentage of detainee challenges to interrogation techniques are upheld vs. dismissed on security grounds? What timescale applies (pre-interrogation authorization vs. post-interrogation review)?',
    'If gatekeeping is theoretical (>80% dismissal on security), the constraint shifts from tangled_rope (balanced coordination + extraction) toward piton (performative maintenance of a dead constraint). If gatekeeping is real (>50% substantive review), the constraint holds as tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gatekeeping_asymmetry, empirical, 'Effective constraint asymmetry: judicial review''s real vs. theoretical role in limiting interrogation').

omega_variable(
    proportionality_operationalization_ambiguity,
    'What does proportionality mean in practice? How do courts and interrogators actually measure and compare dignity harm against security benefit?',
    'Comparative jurisprudence across courts; interrogation procedure analysis; expert testimony on weighing mechanisms. The answer will show whether proportionality is an operational constraint (comparable weights, predictable outcomes) or a legitimacy fiction (incommensurable quantities, outcome-dependent reasoning).',
    'If proportionality operationalizes (common scales, predictable application), the constraint is a real barrier; if it is incommensurable across courts and interrogators, it is a cover story for divergent state practice — the effective constraint is ''whatever states do is proportional'' (pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_operationalization_ambiguity, conceptual, 'Whether proportionality is operationalized or incommensurable — whether it constrains or legitimates state practice').

omega_variable(
    vulnerability_exacerbation_mechanism,
    'Does the proportionality standard''s case-by-case balancing disproportionately harm vulnerable populations (ethnic minorities, non-speakers of court language, political opponents) compared to detained persons with legal resources and sympathetic home states?',
    'Disaggregated data on proportionality outcomes by detainee profile (nationality, language access, legal representation, identity group). Qualitative study of how proportionality balancing is applied differently to vulnerable vs. privileged detainees.',
    'If vulnerability exacerbates under proportionality (vulnerable detainees get worse treatment under the same framework), the constraint is a mechanism for identity-targeted extraction within the general detainee category — a snare for vulnerable populations even if tangled_rope for privileged ones. This would suggest mandatrophy for the absolute-prohibition reading as protective of the vulnerable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_exacerbation_mechanism, empirical, 'Identity-stratification of proportionality outcomes: whether the standard produces worse outcomes for vulnerable populations').

omega_variable(
    kernel_contestation_forecast,
    'Which sibling reading will prevail as interrogation practice evolves: absolute prohibition (strengthened by international monitoring and human rights pressure), contextual necessity (amplified by security state practice and terrorism discourse), or continued proportionality balancing (maintained by institutional inertia)?',
    'Longitudinal tracking of international law interpretation (ICJ opinions, International Committee of the Red Cross guidance, state treaty reservations); empirical observation of interrogation practice drift; political-economy analysis of state incentives.',
    'Prevalence of absolute_prohibition would reclassify detained persons from targets under tangled_rope to protected under mountain; prevalence of contextual_necessity would reclassify them to victims under snare. The reading that wins shapes the structural type from the detainee seat''s perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contestation_forecast, preference, 'Which reading of the humane_treatment_standard kernel will institutionally dominate: proportionality, absolutism, or contextual necessity').

omega_variable(
    suppression_internalization,
    'Among detained persons and vulnerable populations, is the suppression measured by the constraint structural (legal barriers, isolation, security classification) or partly internalized (belief that resistance is futile, self-censorship, identity-fusion with detainee role)?',
    'Post-release interview studies with formerly detained persons on perceived legal access, belief in appeal efficacy, identity shifts during detention. Comparison of suppression levels pre-detention (when alternatives are visible) vs. post-release (after exit from the constraint).',
    'If suppression is substantially internalized, the effective suppression is higher than the structural measure suggests — detainees carry the constraint''s suppression with them after release, and proportionality balancing''s psychological effects persist. This would increase effective extraction and shift the constraint toward pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression mechanism in detention under proportionality standard').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__proportionality_balancing, theater_ratio, 0, 0.25).
narrative_ontology:measurement(huma_tr_t5, humane_treatment_standard__proportionality_balancing, theater_ratio, 5, 0.3).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__proportionality_balancing, theater_ratio, 10, 0.36).
narrative_ontology:measurement(huma_tr_t15, humane_treatment_standard__proportionality_balancing, theater_ratio, 15, 0.41).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__proportionality_balancing, theater_ratio, 20, 0.45).
narrative_ontology:measurement(huma_tr_t25, humane_treatment_standard__proportionality_balancing, theater_ratio, 25, 0.47).
narrative_ontology:measurement(huma_tr_t30, humane_treatment_standard__proportionality_balancing, theater_ratio, 30, 0.48).
narrative_ontology:measurement(huma_tr_t40, humane_treatment_standard__proportionality_balancing, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__proportionality_balancing, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(huma_be_t5, humane_treatment_standard__proportionality_balancing, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__proportionality_balancing, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(huma_be_t15, humane_treatment_standard__proportionality_balancing, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__proportionality_balancing, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(huma_be_t25, humane_treatment_standard__proportionality_balancing, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(huma_be_t30, humane_treatment_standard__proportionality_balancing, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(huma_be_t40, humane_treatment_standard__proportionality_balancing, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__proportionality_balancing, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(huma_su_t5, humane_treatment_standard__proportionality_balancing, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__proportionality_balancing, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(huma_su_t15, humane_treatment_standard__proportionality_balancing, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__proportionality_balancing, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(huma_su_t25, humane_treatment_standard__proportionality_balancing, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(huma_su_t30, humane_treatment_standard__proportionality_balancing, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(huma_su_t40, humane_treatment_standard__proportionality_balancing, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__proportionality_balancing, 0.15).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, interrogation_disclosure_standards).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, detention_legal_access).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested humane_treatment_standard kernel. The absolute_prohibition reading and contextual_necessity reading are sibling constraints with different ε values, beneficiary structures, and classifications. All three readings share the same referent (Common Article 3) but instantiate different structural arrangements: absolute_prohibition permits no interrogation techniques (mountain from detainee seat, snare from state seat); contextual_necessity permits all techniques when state deems necessary (snare from detainee seat, rope from state seat); proportionality_balancing (this reading) permits techniques balanced against dignity, with judicial review (tangled_rope from both seats, with asymmetric directionality). The three readings are linked via network.affects_constraints; each reading's classification differs because each reading's structural arrangement differs, not because the metrics are perspective-relative. The ε-invariance principle is preserved: each reading has a fixed ε and fixed stakeholder set; only the reading changes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humane_treatment_standard__proportionality_balancing, institutional, 0.15).
constraint_indexing:directionality_override(humane_treatment_standard__proportionality_balancing, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
