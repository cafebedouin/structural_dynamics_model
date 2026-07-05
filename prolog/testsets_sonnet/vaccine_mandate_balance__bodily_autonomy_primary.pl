% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__bodily_autonomy_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy Primacy Reading of Vaccine Mandate Kernel
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   A state or state-authorized institution (employer, school system) imposes
 *   vaccination as a precondition for employment, enrollment, or civic
 *   participation, enforced through exclusion mechanisms rather than
 *   persuasion. This reading holds that consent violation is the extraction
 *   regardless of epidemiological outcome — the harm is constituted by the
 *   compulsion mechanism itself, not contingent on whether the vaccine works,
 *   whether the disease is severe, or whether uptake targets are met. The
 *   claim (snare) and the metrics are authored independently: extraction and
 *   suppression are both authored high because, on this reading's own
 *   premises, any successful compulsion mechanism is by definition an
 *   override of an inviolable right — there is no coordination function this
 *   reading recognizes as legitimating the override, so nothing offsets the
 *   extraction reading.
 *
 * KEY AGENTS:
 *   - state_public_health_agencies: institutional agenda-setter, designs and enforces compulsion mechanisms
 *   - unvaccinated_coerced_workers/students: powerless, trapped, bear direct extraction
 *   - religious_and_conscience_objectors: powerless, constrained exit via narrowing exemption process
 *   - immunocompromised_exposed: excluded from this reading's victim set by design — their exposure risk is recharacterized as an accepted cost of a liberty-respecting baseline, not a constraint-caused harm
 *   - constitutional_courts: analytical observer adjudicating the doctrine this reading asserts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.79).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.81).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.79).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Bodily Autonomy Primacy Reading of Vaccine Mandate Kernel").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, '65e9ae03-5238-4326-a5f9-d90be1e5620c').
narrative_ontology:cs_kernel_codification('65e9ae03-5238-4326-a5f9-d90be1e5620c', distributed).
narrative_ontology:cs_authority_grounding('65e9ae03-5238-4326-a5f9-d90be1e5620c', distributed).
narrative_ontology:cs_reading_relation('65e9ae03-5238-4326-a5f9-d90be1e5620c', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('65e9ae03-5238-4326-a5f9-d90be1e5620c', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('65e9ae03-5238-4326-a5f9-d90be1e5620c', foundational, consent_inviolable_regardless_of_magnitude).
narrative_ontology:cs_axiom_status(consent_inviolable_regardless_of_magnitude, holdable).
narrative_ontology:cs_axiom_grounding('65e9ae03-5238-4326-a5f9-d90be1e5620c', consent_inviolable_regardless_of_magnitude, deontological).
narrative_ontology:cs_axiom('65e9ae03-5238-4326-a5f9-d90be1e5620c', foundational, collective_benefit_never_licenses_bodily_compulsion).
narrative_ontology:cs_axiom_status(collective_benefit_never_licenses_bodily_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('65e9ae03-5238-4326-a5f9-d90be1e5620c', collective_benefit_never_licenses_bodily_compulsion, deontological).
narrative_ontology:cs_reference_frame('65e9ae03-5238-4326-a5f9-d90be1e5620c', consent_based_medical_autonomy_doctrine).
narrative_ontology:cs_drift_state('65e9ae03-5238-4326-a5f9-d90be1e5620c', post_pandemic_mandate_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('65e9ae03-5238-4326-a5f9-d90be1e5620c', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, state_public_health_agencies).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, compliant_majority_population).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, employers_seeking_liability_shield).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_workers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_students).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, religious_and_conscience_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_injured_uncompensated).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce mandate policy via licensing, employment conditions, school enrollment, and travel access. Frame consent override as necessary for collective welfare; on this reading, their compulsion mechanisms constitute the extraction itself rather than legitimate coordination, because they override an inviolable individual right regardless of the magnitude of collective benefit claimed.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, state_public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Face termination, suspension, or exclusion from employment for declining an intervention they have not consented to. Exit requires abandoning livelihood; for many this is not a real option. The compulsion itself, independent of any medical outcome, constitutes the harm on this reading.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_workers, payer,
    powerless, biographical, trapped, national).

% Barred from school enrollment or campus access absent vaccination, foreclosing education access as leverage to secure compliance. Minors additionally cannot exercise autonomous consent, making the compulsion doubly structural: it runs through the family's forced choice rather than the individual's own agency.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_students, payer,
    powerless, biographical, trapped, national).

% Hold sincere objection grounded in religious or conscientious belief; exemption processes are frequently narrowed, litigated against, or administratively denied, converting a claimed accommodation into a further coercion vector. Exit means forfeiting the underlying social good (employment, education, travel) their conscience already cost them.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, religious_and_conscience_objectors, payer,
    powerless, biographical, constrained, national).

% Suffered adverse effects from a mandated intervention they did not freely choose; compensation and liability pathways are narrow, slow, or unavailable, compounding the initial consent violation with an uncompensated harm they did not agree to bear.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_injured_uncompensated, payer,
    powerless, biographical, trapped, national).

% Complies voluntarily or under mild pressure and experiences the mandate as background policy rather than coercion; benefits from reduced disease circulation attributable in part to others' coerced compliance, without bearing the compulsion cost themselves.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, compliant_majority_population, beneficiary,
    organized, biographical, mobile, national).

% Impose mandates as workplace conditions to secure regulatory compliance, insurance terms, or litigation protection, transferring the consent question onto employees while capturing the institutional benefit of a vaccinated workforce and reduced liability exposure.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, employers_seeking_liability_shield, beneficiary,
    institutional, biographical, arbitrage, national).

% Bear elevated risk from community disease circulation and would prefer higher vaccination uptake among others, but on this reading their exposure risk is a consequence of a liberty-respecting baseline, not a harm imposed by this constraint — they are not counted among its victims because risk acceptance is inherent to a regime that does not compel others' bodies for their benefit. Their voice is structurally absent from a framework organized around the objector's consent, not their exposure.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_exposed, excluded,
    powerless, biographical, constrained, national).

% Adjudicate challenges to mandate authority under bodily autonomy and religious liberty doctrines; can enjoin or narrow enforcement mechanisms but do not themselves bear the compulsion or its costs.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized on this reading at the level of the individual body: whatever population-level disease suppression a mandate might achieve does not constitute a coordination problem that licenses overriding an individual's inviolable consent right. The only coordination this reading validates is voluntary — persuasion, incentive, and information provision, not compulsion.
% TRANSFER_FUNCTION: Moves bodily self-determination from the individual to the state (or state-authorized employer/institution), and moves the material costs of non-compliance — employment, education, travel, uncompensated injury — onto those who withhold consent, while the compliant majority and institutional actors capture the collective epidemiological and liability benefits without paying the compulsion cost.
% ABSENT_VOICES: Vaccine-injured claimants attempting compensation, religious objectors whose exemption claims are administratively narrowed, and minors compelled through school-access leverage are structurally underrepresented in mandate design processes, which are set by public health agencies and ratified through employer and institutional policy rather than negotiated with those who bear the coercion.
% DISAPPEARANCE_RATIONALE: If compulsory mechanisms disappeared overnight, coerced workers and students would regain employment and enrollment access without a medical precondition, exemption litigation would collapse for lack of a compulsion to contest, and public health agencies would need to substitute voluntary uptake strategies (incentive, persuasion, access removal of a different kind) — the material stakes currently carried by objectors would evaporate, though voluntary uptake levels might fall, which is precisely the tradeoff this reading holds is not the state's to make on the individual's behalf.
% FOUNDING_PROBLEM: Historical origin: population-level disease control during epidemics where voluntary uptake was judged insufficient to prevent mass mortality or system collapse, prompting states to compel intervention as a public health tool.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and the compliant majority attest the founding problem (insufficient voluntary uptake amid lethal transmission) remains live and justifies continued compulsion. Civil liberties organizations, constitutional law scholars outside any health agency, and courts reviewing exemption denials attest that, whatever the epidemiological merits, the compulsion mechanism itself is not corroborated as necessary by any showing that voluntary alternatives were exhausted — this reading treats that absence of exhaustion, attested from outside the enforcing agencies, as decisive.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε=0.79) is authored high because, on the bodily_autonomy_primary premise, any compulsion mechanism that succeeds in overriding consent is constitutive extraction — there is no discount for epidemiological effectiveness because effectiveness is not a legitimating consideration under this reading. Suppression (0.81) is authored comparably high because enforcement runs through exclusion from employment, education, and travel — high-stakes levers with few substitutes. Theater ratio is kept low (0.22) because the enforcement machinery (verification systems, exemption review boards, employer compliance audits) is largely functional rather than performative; the coercion is real, not symbolic. Accessibility collapse (0.58) is moderate-high but not extreme because exemption pathways nominally exist even though this reading treats their narrowing as itself part of the extraction. Resistance (0.74) is high, consistent with the sustained legal and civil challenges mandates provoke under this framework.
 *
 * PERSPECTIVAL GAP:
 *   From the state agency seat, the mandate looks like legitimate, if imperfect, coordination against a lethal externality. From the coerced-worker or objector seat, the same mechanism computes as pure extraction: a right the reading holds cannot be waived by aggregation of collective benefit is overridden anyway. The engine should register this divergence structurally through the differing power/exit profiles authored above, not through any adjudication this file performs — the file states the bodily-autonomy premise and its consequences, not a verdict on which reading is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   State public health agencies and employers seeking liability protection sit at the beneficiary end: they set the policy and capture institutional or epidemiological gains without personally bearing the compulsion. Unvaccinated-coerced workers/students, objectors, and vaccine-injured uncompensated sit at the target end: trapped or constrained exit, direct cost-bearing, no meaningful alternative given the stakes attached (livelihood, education). The compliant majority is a beneficiary of low salience — they experience the mandate as ambient policy, not personal coercion, while still capturing whatever collective benefit accrues. Immunocompromised-exposed are deliberately placed outside the victim set on this reading: their elevated exposure risk is real, but this reading's core axiom holds that risk acceptance is inherent to a liberty-respecting baseline — the constraint does not cause their exposure, the absence of compulsion does, and absence of compulsion is not itself extraction under this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (insufficient voluntary uptake during lethal outbreaks) may remain epidemiologically live in some jurisdictions, but this reading's corroboration trail shows the compulsion mechanism itself was never shown, from outside the enforcing agencies, to be necessary rather than merely available. Where mandates persist past acute crisis phases into routine administrative policy — becoming a standing precondition for ordinary civic participation — the founding emergency problem is dead while the compulsion apparatus persists, which is the structural signature this reading treats as confirming rather than resolving its classification as extraction: the mandate's survival past the emergency is evidence for, not against, this reading's snare verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the correct reading of the vaccine_mandate_balance kernel bodily_autonomy_primary (consent inviolable regardless of collective benefit), public_health_primary (collective protection supersedes consent under vulnerability conditions), or proportionality_reading (a threshold test mediates between them)?',
    'No empirical resolution exists — this is a genealogically contested normative kernel. Resolution would require either broad political/philosophical consensus on the priority of individual rights versus collective welfare, or a controlling constitutional doctrine that forecloses competing readings within a given jurisdiction''s legal framework.',
    'Under bodily_autonomy_primary, unvaccinated-coerced populations are victims and immunocompromised-exposed populations are not; under public_health_primary, this membership substantially reverses; under proportionality_reading, membership depends on a severity/safety threshold this file''s reading treats as a category error. Each reading is authored as its own constraint file with its own ε and classification per the ε-invariance principle — this omega documents that the three files are siblings under one kernel, not competing measurements of one constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, preference, 'Which reading of the vaccine mandate kernel governs is a values/framing question, not an empirical one, and this file commits to one reading only.').

omega_variable(
    risk_acceptance_baseline_for_immunocompromised,
    'Is the exclusion of immunocompromised-exposed persons from this reading''s victim set defensible, given that their elevated exposure risk is causally connected to the non-compulsion of others, even if not to compulsion itself?',
    'Philosophical analysis of the distinction between harms of commission (compulsion) and harms of omission (permitted non-compliance); could be informed by comparative case law on duty-to-protect doctrines and whether a liberty baseline generates affirmative obligations toward third parties.',
    'If the omission/commission distinction does not hold structurally, immunocompromised-exposed populations should be added to some victim-adjacent category even under this reading, which would push the classification toward tangled_rope (coordination function partially recognized, asymmetric costs on both objectors and the exposed) rather than a clean snare reading focused solely on the coerced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_acceptance_baseline_for_immunocompromised, conceptual, 'Whether excluding the immunocompromised-exposed from the victim set is a stable structural move or an artifact of this reading''s own framing.').

omega_variable(
    emergency_versus_routine_compulsion_drift,
    'Does the classification differ meaningfully between mandates imposed during acute, time-limited epidemic emergencies versus mandates that persist as routine, indefinite administrative preconditions for civic participation?',
    'Temporal analysis of specific mandate regimes: track whether sunset clauses were attached, invoked, and honored, versus whether emergency mandates were quietly converted into permanent policy.',
    'A mandate authored with a genuine sunset clause and lifted on schedule would look structurally closer to scaffold even under this reading''s skepticism of compulsion; a mandate that persists indefinitely past the acute emergency strengthens the snare reading authored here. This file''s high, flattening extraction/suppression trajectory (rising through midpoint, then plateauing) reflects an assumption of persistence past the acute phase — a genuinely time-limited mandate would need a different temporal profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_versus_routine_compulsion_drift, empirical, 'Whether observed mandate persistence past emergency conditions is a general pattern or specific to particular jurisdictions modeled here.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 4, 0.12).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 8, 0.15).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 12, 0.19).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 16, 0.21).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 20, 0.22).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 12, 0.74).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 16, 0.79).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 24, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 12, 0.79).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 16, 0.81).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 24, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% This file is one of three sibling constraint stories decomposing the colloquial 'vaccine mandate' claim per the ε-invariance principle: bodily_autonomy_primary (this file, snare-leaning, ε=0.79), public_health_primary (expected tangled_rope or rope-leaning, lower ε, different victim set centered on the exposed-vulnerable), and proportionality_reading (expected scaffold or tangled_rope, ε mediated by a severity/safety threshold test). They share a kernel (vaccine_mandate_balance) but are NOT the same constraint measured three ways — each reading redraws the beneficiary/victim boundary and produces a different ε from a stable set of positional atoms. Link all three via affects_constraints in each file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
