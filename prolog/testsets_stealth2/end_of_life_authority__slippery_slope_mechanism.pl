% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__slippery_slope_mechanism, []).

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
 *   constraint_id: end_of_life_authority__slippery_slope_mechanism
 *   human_readable: Assisted-Dying Eligibility Drift Regime (Slippery-Slope Reading)
 *   domain: bioethics/end-of-life-policy
 *
 * SUMMARY:
 *   Statutory assisted-dying frameworks beginning with Oregon (1997), the
 *   Benelux laws (2002), and Canada (2016) created lawful, physician-mediated
 *   channels for ending life, initially restricted to adults with
 *   decision-making capacity facing terminal prognoses. This story
 *   instantiates the slippery-slope reading of the end-of-life-authority
 *   kernel: it treats the standing arrangement — the enacted regime together
 *   with its safeguard machinery — as a structure whose eligibility boundary
 *   has moved outward at every examination, admitting non-terminal chronic
 *   sufferers (Canada's 2021 non-terminal track), psychiatric suffering
 *   (scheduled for 2027), and patients without contemporaneous consent (the
 *   2004 Groningen Protocol for infants; advance-directive provisions). The
 *   reading's ε referent is the enacted regime as it operates — not the
 *   narrower settlement it replaced, not any rival arrangement. Sibling
 *   readings author different ε over the same referent: the autonomy reading
 *   indexes ε to access gained by competent terminal patients (low), the
 *   sanctity reading to every intentional life-ending (very high); this
 *   reading indexes ε to the populations drawn in without the consent
 *   grounding that justified the framework (mid-high). KEY AGENTS (by
 *   structural relationship): - incompetent_patients: primary target
 *   (powerless/trapped) — admitted without contemporaneous consent, cannot
 *   organize or refuse - non_terminal_chronic_sufferers: target
 *   (moderate/constrained) — admitted by criterion drift past the founding
 *   rationale - vulnerable_elderly_patients: target
 *   (moderate/identity_locked) — request under burden-aversion fused with
 *   self-concept - state_health_payers: primary beneficiary
 *   (institutional/arbitrage) — receives the fiscal differential -
 *   assisted_dying_provider_physicians: agenda-setter and beneficiary
 *   (institutional/mobile) — operate the gates, collect fees -
 *   constitutional_courts: agenda-setter (institutional/analytical) — reset
 *   eligibility boundaries by ruling - surrogate_decision_makers:
 *   dual-positioned beneficiary/payer (moderate/constrained) — exercise
 *   substituted judgment, relieved of care, bearing moral residue -
 *   disability_rights_advocates: excluded voice (organized/constrained) —
 *   oppose each widening from outside eligibility drafting -
 *   palliative_care_specialists: target (organized/mobile) — bear
 *   crowding-out and marginalization costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.74).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.72).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.74).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "Assisted-Dying Eligibility Drift Regime (Slippery-Slope Reading)").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "bioethics/end-of-life-policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, 'a00f2b7f-7d54-493a-ae29-e187f5862710').
narrative_ontology:cs_kernel_codification('a00f2b7f-7d54-493a-ae29-e187f5862710', formalized).
narrative_ontology:cs_authority_grounding('a00f2b7f-7d54-493a-ae29-e187f5862710', distributed).
narrative_ontology:cs_reading_relation('a00f2b7f-7d54-493a-ae29-e187f5862710', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('a00f2b7f-7d54-493a-ae29-e187f5862710', end_of_life_authority__sanctity_reading, influences).
narrative_ontology:cs_axiom('a00f2b7f-7d54-493a-ae29-e187f5862710', foundational, safeguards_cannot_contain_eligibility_drift).
narrative_ontology:cs_axiom_status(safeguards_cannot_contain_eligibility_drift, holdable).
narrative_ontology:cs_axiom_grounding('a00f2b7f-7d54-493a-ae29-e187f5862710', safeguards_cannot_contain_eligibility_drift, empirically_contingent).
narrative_ontology:cs_axiom('a00f2b7f-7d54-493a-ae29-e187f5862710', foundational, consent_grounding_erodes_without_capacity).
narrative_ontology:cs_axiom_status(consent_grounding_erodes_without_capacity, holdable).
narrative_ontology:cs_axiom_grounding('a00f2b7f-7d54-493a-ae29-e187f5862710', consent_grounding_erodes_without_capacity, deontological).
narrative_ontology:cs_reference_frame('a00f2b7f-7d54-493a-ae29-e187f5862710', competent_terminal_voluntary_settlement).
narrative_ontology:cs_drift_state('a00f2b7f-7d54-493a-ae29-e187f5862710', contemporary_post_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a00f2b7f-7d54-493a-ae29-e187f5862710', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, state_health_payers).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, assisted_dying_provider_physicians).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, surrogate_decision_makers).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, non_terminal_chronic_sufferers).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, vulnerable_elderly_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, palliative_care_specialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, surrogate_decision_makers).
narrative_ontology:constraint_vindicates(end_of_life_authority__slippery_slope_mechanism, substituted_judgment_validity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons who have lost, or never possessed, decision-making capacity and whose deaths can be authorized through previously written directives, infant protocols, or surrogate petitions. They make no contemporaneous request; admission to the framework's widening eligibility is decided entirely by statute, diagnosis, and other people's judgments. They cannot withdraw from the category because membership is assigned to them rather than chosen, and they cannot organize, petition, or testify.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients, payer,
    powerless, biographical, trapped, national).

% Patients with chronic physical or psychiatric conditions causing sustained suffering whose prognoses are not terminal. Successive amendments and court rulings admit them under grievous-and-irremediable-style tests that no longer require a terminal trajectory. They may decline to request provision and may seek treatment in other jurisdictions, but the lawful option reshapes what clinicians propose, what families expect, and what insurers cover.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, non_terminal_chronic_sufferers, payer,
    moderate, biographical, constrained, national).

% Older patients with declining independence who report requesting provision chiefly to avoid burdening relatives or exhausting savings. Qualitative studies of request rationales find welfare-of-others motives dominant in a substantial minority of cases. Their self-concept as non-burdensome is fused with the choice: continuing to live feels like taking from the family, so the alternative to provision is experienced as a wrong against loved ones rather than as an available option.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, vulnerable_elderly_patients, payer,
    moderate, biographical, identity_locked, national).

% Public health systems and insurers that finance end-of-life care. Actuarial and parliamentary analyses in operating jurisdictions show a provisioned death costs a small fraction of prolonged intensive or institutional care. Payer bodies submit supportive briefs during expansion debates and absorb the fiscal difference as budgets balance; they can redirect funds and adjust coverage terms at will.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, state_health_payers, beneficiary,
    institutional, generational, arbitrage, national).

% Physicians certified to assess eligibility, prescribe, and administer. They operate the assessment gates, staff review structures, receive fees per provisioned case, and accumulate the caseload expertise that review panels rely on. Conscience protections let some decline participation, and some do, but the specialty's institutional weight consolidates around those who continue.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, assisted_dying_provider_physicians, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, assisted_dying_provider_physicians, beneficiary).

% Superior and constitutional courts in several jurisdictions have struck terminal-illness limits as discriminatory, ordering legislatures to widen eligibility. Their rulings reset the eligibility boundary without an electoral mandate and bind subsequent legislative sessions; later benches treat the widened baseline as settled law.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Family members and appointed representatives authorized to interpret advance directives or exercise substituted judgment for incapacitated relatives. An authorized death ends caregiving burdens they carry daily; documented cases also involve inheritance interests. They decide inside statutory tests they did not write, under grief, fatigue, and family pressure, and bear moral residue afterward.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, surrogate_decision_makers, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, surrogate_decision_makers, payer).

% Organized disability rights organizations argue the framework prices disabled lives as burdensome and oppose each widening. They hold consultative seats at best in eligibility-design processes, have litigated without reversing enacted criteria, and remain outside the rooms where eligibility text is drafted.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, disability_rights_advocates, excluded,
    organized, generational, constrained, global).

% Specialists in symptom relief and end-of-life care. They report funding and staffing diverted toward provision services, referral expectations that frame continued treatment as prolonging suffering, and professional marginalization when they counsel against provision. Several national palliative associations formally oppose successive widenings and publish outcome data the review machinery does not collect.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, palliative_care_specialists, payer,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__slippery_slope_mechanism, state_health_payers).
narrative_ontology:fixing_cost_class(end_of_life_authority__slippery_slope_mechanism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a lawful, medically supervised channel for ending unbearable suffering: standardized eligibility assessment, prescribing and administration protocols, mandatory reporting, and review committees replace clandestine assistance and unmanaged terminal distress for the population the framework was founded to serve.
% TRANSFER_FUNCTION: Moves death-decision authority and timing from patients and disease courses into a regulated institutional channel; moves the cost of prolonged end-of-life care away from payers and family caregivers; moves professional authority over death from general prohibition to licensed administration by a certified specialty.
% ABSENT_VOICES: The incompetent patients the drift pulls in cannot speak in any eligibility deliberation; disability rights advocates hold consultative seats at best; palliative care bodies are heard during debate but outside eligibility drafting; future patients subject to the next widening have no seat at all.
% DISAPPEARANCE_RATIONALE: If the framework and its machinery vanished overnight, clandestine assistance and travel-for-death would resume, court-ordered access would be extinguished pending new litigation, payers would reabsorb prolonged-care costs, the provider specialty would dissolve, and surrogates would lose the authorization instrument — the end-of-life arrangements of every named seat reorganize around its absence.
% FOUNDING_PROBLEM: Competent, terminally ill patients facing unbearable suffering had no lawful option: physician assistance was criminal, driving clandestine practice, botched suicides, and death tourism, while palliative care left refractory symptoms untreated in a minority of cases.
% FOUNDING_PROBLEM_CORROBORATION: Palliative-care literature and parliamentary inquiry testimony from patients and families attest the founding problem was real and remains live in jurisdictions without frameworks. Disability rights organizations and comparative-law reviews attest from outside the benefiting parties that inside operating jurisdictions the founding problem is substantially addressed and the machinery's persistence now tracks expansion dynamics rather than the original need. No neutral arbiter adjudicates between these attestations; the disagreement is itself the contested finding.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__slippery_slope_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises monotonically across the interval because each examined period adds a newly eligible population or a loosened test, and no examined jurisdiction has narrowed eligibility back to its founding line; the endpoint value 0.74 reflects the accumulated drift, not any single year's operation. Suppression (0.72) is authored as a raw structural property — the enforcement machinery (assessment gates, reporting duties, review boards, court entrenchment) actively holds the widened boundary and constrains objecting professionals — and is deliberately NOT scaled by power or scope; only extractiveness is scaled, by the engine, through directionality and scope. Theater ratio climbs from 0.15 to 0.46 because the safeguard apparatus increasingly functions as legitimation: waiting periods and second opinions are cited in expansion debates while review outcomes rarely deny requests, so a growing share of procedural activity defends the framework's image rather than policing its boundary. Accessibility collapse is 0.48: reversal remains visible as an option but each widening raises its political and constitutional cost, so alternatives erode without vanishing. Resistance is 0.60: organized, sustained, and losing on the current record — legislative defeats in some jurisdictions, litigation losses in others. The three series share one time grid (points 0, 6, 12, 18, 24, 30) so the engine samples every metric at every examined point; the t30 row is marked projected because the 2027 psychiatric-eligibility step is scheduled but not yet operative. The trajectory is a ratchet, not a cycle: no oscillation appears in the record, so no intermittent-reinforcement mechanism is claimed. Receipt concentrates at the state payer seat — the fiscal differential between provisioned death and prolonged care is the largest measurable flow — which is why gain_flow names that seat; provider gains are transactional fees rather than captured surplus. Fixing is prohibitive: at least one jurisdiction's widened baseline is court-entrenched, and everywhere the machinery operates, repeal faces grandfathered expectations and organized provider interests.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the provider and court seats the arrangement is a rights architecture they built and defend: each widening is discrimination remedied. From the incompetent-patient seat the same structure operates as exposure without voice — the only seat that cannot testify, litigate, or decline, which is why its directionality sits nearest the full-target end despite zero agency; coalition power, the usual corrective for powerless agents, is structurally unavailable to a seat whose members cannot communicate. From the palliative seat the structure operates as crowding-out and mission distortion. The vulnerable-elderly seat exhibits identity lock: the binding mechanism is relational self-concept — being non-burdensome is constitutive of who they take themselves to be — so exit (living while dependent) is experienced as wrongdoing rather than as an option; if that identity frame broke, their measured uptake would fall and the framework's demand profile would look materially different. Surrogates are genuinely dual-positioned: they collect relief from the same act that loads them with moral residue, and the derivation handles this through their secondary role rather than an override.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: state_health_payers (arbitrage-grade exit, institutional power) sit near the beneficiary pole; provider physicians derive low d from their beneficiary declaration despite administering the machinery; surrogates sit moderately low with the payer secondary role pulling them back toward center. Victim declarations map to high directionality: incompetent_patients combine full-target position with trapped exit, maximizing amplification; non_terminal_chronic_sufferers and vulnerable_elderly_patients are near-full targets with constrained and identity-locked exits respectively; palliative_care_specialists are diffuse-cost targets with mobile exit, damping their effective extraction somewhat. No directionality overrides are authored: the derivation chain produces accurate d values from the beneficiary/victim data plus exit atoms for every seat, and the schema's override mechanism is keyed by power atom rather than agent, so any override here would distort same-power seats with opposite positions. Scope amplification applies modestly: the regime operates at national scope, where verification of safeguard fidelity is harder than locally.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both halves of the structure visible and prevents symmetric mislabeling. Calling the whole arrangement a snare erases the genuine coordination function — a lawful, supervised channel that retired clandestine practice and unmanaged terminal distress for the founding population — and would predict abolition rather than boundary repair. Calling it a rope erases the four victim sets the drift record names and the asymmetric enforcement that holds the widened boundary. A piton reading fails on the facts: the extraction is administered, not inertial — agenda-setters actively widen the boundary, so nothing here persists by mere theatrical maintenance, though the rising theater series marks the safeguard layer's growing performative share. On the genealogy interview: the founding problem is contested — live in non-framework jurisdictions, substantially addressed inside them — so the machinery's persistence now serves expansion dynamics as much as the founding need. The mismatch consumer reads founding_problem_status x disappearance_verdict: contested x world_rearranges registers the dispute without firing the dead-mandate zombie flag, and the rising theater series is the corroborating symptom that the mandate's center of gravity has shifted from service to boundary defense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the end_of_life_authority kernel; what would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Cross-reading comparison of the three files'' victim sets, beneficiary sets, and epsilon values over the shared referent (the enacted regime): the autonomy reading removes incompetent patients from the victim set and drops epsilon toward the access-gained pole; the sanctity reading makes every intentionally ended life a cost event regardless of consent and pushes epsilon toward the maximum.',
    'Adopting the autonomy reading dissolves this reading''s victim structure into consent exercises; adopting the sanctity reading dissolves the coordination half entirely, since no lawful channel survives its premise; this reading stands or falls on the drift record being structural rather than incidental.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame omega recording that this story is the slippery_slope_mechanism reading of the end_of_life_authority kernel and naming the structural deltas its siblings would introduce.').

omega_variable(
    drift_causation_attribution,
    'Is the observed eligibility expansion caused by the internal logic of autonomy-based frameworks (making drift inevitable under any permission regime) or by external contingencies (court composition, fiscal pressure, advocacy campaigns)?',
    'Comparative institutional analysis across jurisdictions with different court structures, funding arrangements, and advocacy densities: if narrow frameworks persist where external drivers are absent, drift is contingent; if all permission regimes widen on similar trajectories, the mechanism is internal to the framework design.',
    'If external, containment is possible and the foundational axiom safeguards_cannot_contain_eligibility_drift weakens to a jurisdiction-specific regularity; if internal, drift is structural and the tangled_rope reading hardens toward snare as each new cohort enters the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_causation_attribution, empirical, 'Whether eligibility drift is an inherent property of permission frameworks or an artifact of particular institutional environments.').

omega_variable(
    burden_pressure_internalization,
    'How much of vulnerable elderly patients'' uptake reflects internalized burden-aversion versus structural incentives (coverage design, referral patterns, family economics)?',
    'Longitudinal studies of request motivations and post-decision interviews, compared against jurisdictions that strengthened palliative funding concurrently with legalization: if uptake falls when the identity frame is counter-supported, the internalized component is large.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — patients carry the pressure with them independent of the machinery — and the identity_locked exit atom understates the trap; if structural, coverage reform alone would relieve it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_pressure_internalization, empirical, 'Structural versus internalized split of the suppression operating on the vulnerable elderly seat.').

omega_variable(
    payer_cost_differential_influence,
    'What fraction of payer support for successive widenings traces to the cost differential between provisioned death and prolonged care?',
    'Disclosure of payer actuarial submissions and budget projections during expansion debates, cross-checked against payer positions on measures that raise provision costs (enhanced palliative mandates).',
    'A dominant cost motive confirms an economic extraction layer riding the consent framework and strengthens the drift toward snare; a negligible motive leaves the payer seat an incidental beneficiary and keeps the tangled_rope classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(payer_cost_differential_influence, empirical, 'Whether the fiscal differential is a driver of expansion or a side effect of it.').

omega_variable(
    safeguard_functionality_audit,
    'Are the safeguard mechanisms (waiting periods, second opinions, review committees) functioning constraints on eligibility or legitimation theater?',
    'Audit of review outcomes across operating jurisdictions: denial rates, deviation patterns between assessed and administered cases, and whether review findings ever reverse an eligibility trend.',
    'High denial rates and reversals would cut the theater series down and support the rope-half of the tangled_rope claim; rubber-stamp patterns confirm the rising theater trajectory and indicate the coordination function is migrating into performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(safeguard_functionality_audit, empirical, 'Whether the safeguard apparatus polices the boundary or performs policing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(end__tr_t0, observed).
narrative_ontology:measurement(end__tr_t6, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 6, 0.2).
narrative_ontology:measurement_basis(end__tr_t6, observed).
narrative_ontology:measurement(end__tr_t12, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(end__tr_t12, observed).
narrative_ontology:measurement(end__tr_t18, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 18, 0.34).
narrative_ontology:measurement_basis(end__tr_t18, observed).
narrative_ontology:measurement(end__tr_t24, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(end__tr_t24, observed).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 30, 0.46).
narrative_ontology:measurement_basis(end__tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(end__be_t0, observed).
narrative_ontology:measurement(end__be_t6, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(end__be_t6, observed).
narrative_ontology:measurement(end__be_t12, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(end__be_t12, observed).
narrative_ontology:measurement(end__be_t18, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 18, 0.6).
narrative_ontology:measurement_basis(end__be_t18, observed).
narrative_ontology:measurement(end__be_t24, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(end__be_t24, observed).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 30, 0.74).
narrative_ontology:measurement_basis(end__be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(end__su_t0, observed).
narrative_ontology:measurement(end__su_t6, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 6, 0.5).
narrative_ontology:measurement_basis(end__su_t6, observed).
narrative_ontology:measurement(end__su_t12, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 12, 0.56).
narrative_ontology:measurement_basis(end__su_t12, observed).
narrative_ontology:measurement(end__su_t18, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 18, 0.62).
narrative_ontology:measurement_basis(end__su_t18, observed).
narrative_ontology:measurement(end__su_t24, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(end__su_t24, observed).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(end__su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__sanctity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the euthanasia debate' conflates three structurally distinct claims, decomposed per the epsilon-invariance principle. end_of_life_authority__autonomy_reading (permission claim; low epsilon indexed to competent-terminal access gained), end_of_life_authority__sanctity_reading (prohibition claim; very high epsilon indexed to every intentional life-ending), and this file (dynamics claim; mid-high epsilon indexed to populations admitted without the founding consent grounding). All three share the referent — the standing assisted-dying arrangement — and differ only in reading-indexed epsilon, per OQ-26. The upstream autonomy claim is routinely cited as the justification whose abuse this reading documents, so this story links to both siblings; neither sibling's file can substitute for this one because their victim sets and failure modes are disjoint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
