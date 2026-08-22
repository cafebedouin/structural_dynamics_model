% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__public_health_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Vaccine Mandate Authority — Public Health Primacy Reading
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This story instantiates the public-health-primacy reading of the vaccine
 *   mandate legitimacy kernel: the state's duty to prevent collective harm
 *   from communicable disease justifies compulsory vaccination as a condition
 *   of employment, education, and public participation, because unvaccinated
 *   status is classified as an externality imposed on others rather than a
 *   purely private medical choice. This is a distinct constraint from the
 *   bodily-autonomy-primacy reading (which holds coercion categorically
 *   impermissible regardless of collective outcome) and the
 *   risk-stratification reading (which permits targeted but not blanket
 *   mandates). Each reading has its own epsilon, its own victim set, and its
 *   own classification; they are linked only via network edges, not merged
 *   into one story.
 *
 * KEY AGENTS:
 *   - public_health_bureaucracy: Primary agenda-setter and beneficiary — gains durable mandate authority and enforcement precedent
 *   - vaccine_refusers: Primary target — loses employment/education/access access as consequence of noncompliance, reclassified as harm-causer
 *   - immunocompromised_and_vulnerable_populations: Powerless beneficiary — genuinely protected by coordination function but has no leverage over its terms
 *   - unvaccinated_essential_workers: Secondary target — bears heaviest occupational-exposure-justified enforcement
 *   - bodily_autonomy_advocates: Excluded voice — categorical objection heard in litigation but structurally unable to shape the externality framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.58).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.71).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Vaccine Mandate Authority — Public Health Primacy Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, 'd64e0353-91ba-4506-aab6-0d5a0ab34c75').
narrative_ontology:cs_kernel_codification('d64e0353-91ba-4506-aab6-0d5a0ab34c75', distributed).
narrative_ontology:cs_authority_grounding('d64e0353-91ba-4506-aab6-0d5a0ab34c75', extraction).
narrative_ontology:cs_interpretation_layer_present('d64e0353-91ba-4506-aab6-0d5a0ab34c75').
narrative_ontology:cs_reading_relation('d64e0353-91ba-4506-aab6-0d5a0ab34c75', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('d64e0353-91ba-4506-aab6-0d5a0ab34c75', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('d64e0353-91ba-4506-aab6-0d5a0ab34c75', foundational, collective_harm_duty_overrides_bodily_autonomy).
narrative_ontology:cs_axiom_status(collective_harm_duty_overrides_bodily_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('d64e0353-91ba-4506-aab6-0d5a0ab34c75', collective_harm_duty_overrides_bodily_autonomy, instrumental).
narrative_ontology:cs_axiom('d64e0353-91ba-4506-aab6-0d5a0ab34c75', foundational, unvaccinated_status_constitutes_actionable_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_constitutes_actionable_externality, holdable).
narrative_ontology:cs_axiom_grounding('d64e0353-91ba-4506-aab6-0d5a0ab34c75', unvaccinated_status_constitutes_actionable_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('d64e0353-91ba-4506-aab6-0d5a0ab34c75', police_power_communicable_disease_doctrine).
narrative_ontology:cs_drift_state('d64e0353-91ba-4506-aab6-0d5a0ab34c75', post_acute_crisis_plateau, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d64e0353-91ba-4506-aab6-0d5a0ab34c75', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_and_vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, religious_and_medical_exemption_seekers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_essential_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_administering_compliance).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, state_police_power_over_communicable_disease).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, collective_harm_externality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets mandate policy, determines exemption criteria, and enforces compliance through employment conditions, school admission, and licensing gates. Justifies the mandate by framing unvaccinated status as an externality imposed on others, which grounds an expansion of its own authority to compel medical intervention. Gains durable regulatory jurisdiction and precedent for future mandate cycles regardless of whether this specific pathogen's threat resolves.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, beneficiary).

% Already compliant, so the mandate imposes no direct cost on them while raising herd-immunity thresholds and removing unvaccinated coworkers, classmates, and customers from shared spaces. They benefit from reduced transmission risk without bearing enforcement costs themselves.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population, beneficiary,
    organized, biographical, mobile, national).

% Face elevated risk from any circulating pathogen and cannot rely on their own vaccination for full protection; the mandate's coordination function most directly serves this group by reducing the pool of contacts who could transmit to them. They have no exit from their biological vulnerability and depend on population-level compliance they cannot themselves compel.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_and_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Lose employment, access to education, or public accommodation as a direct consequence of noncompliance. Under this reading, their unvaccinated status is classified as an externality imposed on others, which strips their objection of standing as a private medical choice and recasts refusal as harm-causing. Exit means forfeiting livelihood, schooling, or public participation — not a genuine alternative for most.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers, payer,
    powerless, biographical, trapped, national).

% Attempt to use narrow exemption categories the bureaucracy administers restrictively; under the public-health-primacy framing, exemptions are treated as tolerated leaks in the externality-prevention logic rather than as protected rights, so approval rates and exemption scope tighten whenever transmission risk is invoked. Their exit route depends entirely on discretionary approval by the same authority that benefits from narrow exemptions.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, religious_and_medical_exemption_seekers, payer,
    powerless, biographical, constrained, national).

% Occupy roles (healthcare, food supply, emergency services) where mandate noncompliance triggers termination but where labor market alternatives are limited by licensure or the mandate itself following them to comparable jobs across employers who adopt the same policy. Bear the heaviest asymmetric enforcement because their occupational exposure is cited as justifying the externality claim most strongly against them.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_essential_workers, payer,
    moderate, biographical, constrained, regional).

% Required to verify and enforce mandate compliance under threat of regulatory penalty, absorbing administrative cost and workforce disruption while having little say over the underlying policy. Pass termination consequences to individual workers but do not themselves set the externality framing.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_administering_compliance, agenda_setter,
    organized, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_administering_compliance, payer).

% Argue that medical self-sovereignty is categorically prior to collective-harm calculations and that no externality framing can license bodily intrusion; this position is treated as a threat-to-be-managed rather than a legitimate competing premise within the public-health-primacy reading's own operation, so it is heard in litigation and public comment but does not shape mandate design.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, bodily_autonomy_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__public_health_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Raises population-level vaccination coverage to reduce transmission of a communicable pathogen, protecting individuals who cannot be protected by their own vaccination alone (the immunocompromised, infants, the medically ineligible) and reducing systemic burden on healthcare capacity.
% TRANSFER_FUNCTION: Moves bodily-decision authority from individuals to the state and moves employment/education/public-access continuity from noncompliant individuals to compliant ones; moves compliance-verification labor and cost onto employers and institutions administering the mandate.
% ABSENT_VOICES: Bodily autonomy advocates and individuals citing prior adverse reactions or specific risk-stratification arguments are present in comment periods and litigation but do not shape the categorical framing; the externality doctrine treats their objections as instances of the harm to be prevented rather than as competing legitimate claims.
% DISAPPEARANCE_RATIONALE: If the mandate authority vanished, terminated employees would return to work, exemption litigation would dissolve, the public health bureaucracy would lose a major enforcement lever and precedent basis for future mandates, and transmission dynamics would shift according to voluntary uptake alone — a substantial rearrangement across labor, education, and public health administration.
% FOUNDING_PROBLEM: A communicable pathogen with severe outcomes for vulnerable populations was spreading through a population with insufficient voluntary vaccination uptake to reach protective thresholds, and existing voluntary public health measures were judged insufficient to prevent avoidable death and healthcare system collapse.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists outside the enforcing bureaucracy corroborate that transmission dynamics genuinely responded to vaccination coverage during the acute phase; independent legal scholars and civil liberties organizations outside both the bureaucracy and the refuser population attest that mandate scope expanded and persisted past the period of peak severity, and that exemption administration tightened even as the founding acute-crisis justification weakened — supporting a contested rather than settled status.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rose sharply (0.42 to 0.60) during the acute enforcement period as mandates expanded from healthcare workers to broader employment and education sectors, then plateaued and drifted slightly downward as acute severity receded but enforcement infrastructure remained. Theater ratio rose steadily (0.10 to 0.28) as exemption administration and compliance verification increasingly functioned as procedural gatekeeping distinct from the original transmission-reduction rationale. Suppression requirement rose fastest early (0.55 to 0.74) as enforcement machinery was built, then settled at a high plateau (0.71) reflecting durable institutionalized coercive capacity rather than acute-crisis response — this is the enforcement-ratchet pattern: infrastructure built for a crisis outlives the crisis's acute justification.
 *
 * PERSPECTIVAL GAP:
 *   From the bureaucracy's seat, this is Tangled Rope trending toward Rope: a genuine coordination function (herd immunity, protection of the vulnerable) legitimately justifying authority. From the refuser and exemption-seeker seats, the same structure computes as substantially extractive: their private medical decision has been redefined as an externality specifically to license the coercive apparatus imposed on them, and exit (losing livelihood or schooling) is not a real alternative. This divergence is the seat-computation the engine is built to surface — the public-health-primacy reading's own framing produces this asymmetry as a matter of definition, not incidentally.
 *
 * DIRECTIONALITY LOGIC:
 *   The public health bureaucracy sits at the low-d beneficiary end: it does not merely coordinate, it gains expanded jurisdiction and enforcement precedent that persists independent of pathogen severity. Vaccinated and vulnerable populations sit near the beneficiary end as genuine recipients of the coordination function's transmission-reduction effect. Vaccine refusers and exemption seekers sit at the high-d target end: under this reading's own externality doctrine, their noncompliance is definitionally reclassified as harm-causing, which is precisely the move that licenses coercion against them and removes their objection's standing as a private-choice claim. Unvaccinated essential workers sit even higher on effective extraction despite moderate nominal power, because their occupational exposure is cited as the strongest instance of the externality claim, concentrating enforcement on them specifically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (acute transmission risk overwhelming health capacity) is contested as live versus dead: epidemiologists corroborate the coordination function was real during the acute phase, but suppression_requirement and theater_ratio both continued rising and then plateaued at a high level even as the measurements suggest declining marginal extractiveness — consistent with an enforcement apparatus that outlived its most acute justification and now persists partly on institutional momentum. This reading does not resolve mandatrophy; it explicitly holds the founding problem as ongoing (state duty to prevent collective harm is treated as a standing duty, not a crisis-bounded one), which is exactly the feature that distinguishes it from a scaffold reading with a sunset clause — this reading's own logic authorizes indefinite persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_classification_is_contested_premise,
    'Is treating unvaccinated status as a genuine externality (like pollution) structurally sound, or is it a rhetorical move that imports collective-harm logic into what remains, biologically, primarily a self-regarding risk?',
    'Comparative epidemiological analysis of actual transmission attributable to unvaccinated individuals versus vaccinated-but-still-transmitting individuals (breakthrough transmission), which would establish whether the externality is large enough to bear the doctrinal weight placed on it, and whether it is meaningfully larger than externalities tolerated in other domains without triggering mandate authority.',
    'If the externality is empirically thin relative to the coercive apparatus built on it, this reading''s own foundational premise is weaker than its enforcement posture assumes, and the tangled_rope classification tilts further toward the extraction pole; if the externality is large and well-corroborated, the coordination function is more clearly genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_classification_is_contested_premise, empirical, 'Whether the externality premise this reading depends on is empirically load-bearing.').

omega_variable(
    kernel_disagreement_location,
    'Where exactly do the three sibling readings of the vaccine_mandate_legitimacy kernel diverge — is it a factual disagreement about transmission risk, or a categorical disagreement about whether collective harm can ever override bodily autonomy regardless of magnitude?',
    'Trace each reading''s response to a stipulated hypothetical (e.g., 95% transmission-blocking vaccine vs. 10% transmission-blocking vaccine): if bodily_autonomy_primacy_reading''s conclusion does not change with risk magnitude, the disagreement with public_health_primacy_reading is categorical, not factual; if risk_stratification_reading''s conclusion tracks the magnitude precisely, it occupies a genuinely intermediate position rather than a compromise position.',
    'If the disagreement is categorical, no amount of additional epidemiological evidence resolves the kernel contest between this reading and bodily_autonomy_primacy_reading — they operate on incommensurable premises (this reading forecloses that one). If it is substantially factual, risk_stratification_reading is the more defensible synthesis and this reading''s blanket approach is harder to justify as anything but administratively convenient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Locating whether the kernel contest is factual or categorical — routes the committer structure per Rule 2.').

omega_variable(
    bureaucratic_authority_persistence_beyond_crisis,
    'Does mandate authority, once established under this reading''s externality doctrine, structurally tend to persist and be reused for subsequent pathogens/policies even after the original crisis resolves, independent of whether that reuse is justified?',
    'Track whether mandate infrastructure (verification systems, exemption review boards, enforcement personnel) is repurposed for subsequent unrelated health mandates, and whether legislative sunset provisions were ever attached or subsequently added.',
    'If authority persists and is reused without fresh proportionality review, this supports reclassifying the bureaucracy''s position closer to institutional-identity-lock (the agency''s function has become indistinguishable from its mandate power) rather than temporary crisis-response coordination, strengthening the tangled_rope-toward-snare reading over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bureaucratic_authority_persistence_beyond_crisis, empirical, 'Whether mandate authority built under this reading outlives its founding crisis and gets reused.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(vacc_tr_t18, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 36, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(vacc_be_t18, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 36, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(vacc_su_t18, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(vacc_su_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 36, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, risk_stratification_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial concept 'vaccine mandate legitimacy' per the ε-invariance principle: measuring the constraint from the public-health-duty observable yields a substantially different ε and victim set than measuring it from the bodily-autonomy observable or the risk-stratification observable. Each reading is authored as its own constraint with its own stable ε; they are linked here via affects_constraints rather than merged. This reading's externality doctrine forecloses the autonomy-primacy reading's core premise (see cs_structure.reading_relations) and exerts non-foreclosing structural pressure on the risk-stratification reading by establishing blanket-mandate precedent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
