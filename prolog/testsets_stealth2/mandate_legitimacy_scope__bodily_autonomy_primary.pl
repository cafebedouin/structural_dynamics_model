% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Compulsory Medical Intervention Regime — Bodily-Autonomy-Primary Reading
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   The standing arrangement under contest is the compulsory medical
 *   intervention regime: vaccination mandates enforced through employment,
 *   education, and institutional gatekeeping, with exemption processes of
 *   varying generosity. This file instantiates the bodily_autonomy_primary
 *   reading of the mandate_legitimacy_scope kernel, under which that
 *   arrangement is assessed as a transfer of bodily decision-authority
 *   executed without free consent. Per the kernel-reading epsilon rule, the
 *   referent of extractiveness is the standing mandate arrangement as this
 *   reading sees it — not the rights-respecting alternative this reading
 *   would put in place, which would score near zero by construction.
 *   Constraint-family decomposition per the epsilon-invariance principle:
 *   'mandate legitimacy' is one colloquial label covering three structurally
 *   distinct constraints. Over the same standing arrangement,
 *   public_health_primary authors low extractiveness (coordination cost, no
 *   coerced-victim set), proportionality_reading authors conditionally
 *   variable extractiveness (parameterized by severity, safety, efficacy, and
 *   alternatives), and this reading authors high extractiveness (0.82) with
 *   the unvaccinated-coerced in the victim set. The three files are linked
 *   via network.affects_constraints. Claim/metric independence is preserved
 *   deliberately: the claimed type is tangled_rope because the structure
 *   retains a real coordination core (coverage for the unprotectable), while
 *   the metrics record the high extraction this reading assesses — the
 *   divergence between the structural claim and the reading-indexed
 *   extraction is the measurement, not an error to reconcile.
 *
 * KEY AGENTS:
 *   - unvaccinated_coerced_individuals: Primary target (moderate/constrained) — bears the transfer of decision-authority and the compliance penalties
 *   - conscientious_exemption_seekers: Target with identity-locked exit (organized/identity_locked) — refusal is constitutive of their commitments
 *   - medically_vulnerable_populations: Primary beneficiary (powerless/trapped) — their protection depends entirely on the coverage the rules produce
 *   - vaccinated_majority: Net beneficiary (moderate/mobile) — receives protection at modest personal cost
 *   - public_health_agencies: Agenda-setter and collector (institutional/arbitrage) — writes and enforces the rules, receives the transferred authority
 *   - mandate_administering_institutions: Implementing beneficiary with cost exposure (powerful/constrained) — front-line verification and exclusion
 *   - institutionalized_persons_under_mandate: Excluded voice (powerless/trapped) — bears intervention without consent capacity or exit
 *   - constitutional_courts: Analytical observer (institutional/analytical) — adjudicates the kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.82).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.72).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.82).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Compulsory Medical Intervention Regime — Bodily-Autonomy-Primary Reading").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, 'b626a841-bdab-41ad-b87c-e22ce835a7aa').
narrative_ontology:cs_kernel_codification('b626a841-bdab-41ad-b87c-e22ce835a7aa', formalized).
narrative_ontology:cs_authority_grounding('b626a841-bdab-41ad-b87c-e22ce835a7aa', lineage).
narrative_ontology:cs_interpretation_layer_present('b626a841-bdab-41ad-b87c-e22ce835a7aa').
narrative_ontology:cs_reading_relation('b626a841-bdab-41ad-b87c-e22ce835a7aa', mandate_legitimacy_scope__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('b626a841-bdab-41ad-b87c-e22ce835a7aa', mandate_legitimacy_scope__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('b626a841-bdab-41ad-b87c-e22ce835a7aa', foundational, bodily_integrity_inviolable_without_consent).
narrative_ontology:cs_axiom_status(bodily_integrity_inviolable_without_consent, holdable).
narrative_ontology:cs_axiom_grounding('b626a841-bdab-41ad-b87c-e22ce835a7aa', bodily_integrity_inviolable_without_consent, deontological).
narrative_ontology:cs_axiom('b626a841-bdab-41ad-b87c-e22ce835a7aa', secondary, collective_welfare_insufficient_to_override_consent).
narrative_ontology:cs_axiom_status(collective_welfare_insufficient_to_override_consent, holdable).
narrative_ontology:cs_axiom_grounding('b626a841-bdab-41ad-b87c-e22ce835a7aa', collective_welfare_insufficient_to_override_consent, deontological).
narrative_ontology:cs_reference_frame('b626a841-bdab-41ad-b87c-e22ce835a7aa', inviolable_bodily_sovereignty).
narrative_ontology:cs_drift_state('b626a841-bdab-41ad-b87c-e22ce835a7aa', contemporary_mass_mandate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b626a841-bdab-41ad-b87c-e22ce835a7aa', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, vaccinated_majority).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, medically_vulnerable_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_administering_institutions).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, conscientious_exemption_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_administering_institutions).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, herd_immunity_threshold_model).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, state_police_power_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Work in hospitals, schools, care facilities, and other covered settings and face a choice between accepting a vaccination they have not freely agreed to and losing employment, enrollment, or access. Consent forms are signed under notice that refusal ends participation. Some relocate, change sectors, or obtain exemptions where offered; those paths are costly, slow, and unevenly available.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    moderate, biographical, constrained, national).

% Hold religious or philosophical commitments that forbid the intervention; accepting it would violate the commitments that organize their lives and communities. They navigate exemption applications, hearings, and documentation requirements whose outcomes vary by jurisdiction and reviewer, and many bear the same employment and enrollment consequences as other refusers.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, conscientious_exemption_seekers, payer,
    organized, biographical, identity_locked, national).

% Cannot be vaccinated or mount immune responses — infants, transplant recipients, chemotherapy patients, the frail elderly. Their protection against circulating disease depends almost entirely on the immunity of the people around them, which broad uptake produces. They have no way to purchase equivalent protection individually and no alternative shield if coverage falls.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medically_vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Accept the intervention, generally willingly, and receive personal protection along with the reduced transmission produced by others' compliance. They fund programs through taxation, encounter occasional documentation requirements, and carry a small share of adverse-event risk. Declining remains available to most of them at modest personal cost.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, vaccinated_majority, beneficiary,
    moderate, biographical, mobile, national).

% Set coverage targets, draft mandate rules and exemption criteria, run enforcement through employers and institutions, and impose or recommend penalties. They receive the decision-authority the rules transfer, administer compliance data and penalty processes, and answer politically for outbreak failures. Their position survives changes of government better than most participants' positions survive changes of policy.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_agencies, beneficiary).

% Employers, hospital systems, universities, and schools that implement the rules at the front line: verifying status, processing exemptions, and applying exclusions. They gain workforce continuity assurances, reduced outbreak disruption, and liability cover, while absorbing administrative cost, staffing losses from resignations, and employee-relations damage.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_administering_institutions, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_administering_institutions, payer).

% Adjudicate challenges to mandate legality, weighing bodily-integrity claims against collective-harm rationales. Their rulings reshape what rules agencies may write and which exemptions must be honored. They sit outside the program's administration and neither pay its costs nor receive its compliance.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Prisoners, military recruits, and residents of care facilities — including people with cognitive impairment — are subject to intervention rules inside settings where refusing is practically unavailable and consent capacity is often absent or mediated by guardians. They have no market exit, no employment alternative, and rarely any channel through which their objection would register.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, institutionalized_persons_under_mandate, excluded,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in infectious-disease control: population immunity above the herd threshold is a public good each individual would prefer to free-ride on, so voluntary uptake plateaus below the level that protects people who cannot be vaccinated. Compulsory rules align individual incentives with coverage targets.
% TRANSFER_FUNCTION: Moves decision-authority over a specific medical intervention from individuals (and their guardians) to state and public-health authorities; moves the risk of adverse events onto those who did not freely agree while distributing protection across the population; moves compliance costs — documentation, testing, penalties, lost employment — onto refusers.
% ABSENT_VOICES: Institutionalized persons (prisoners, care-facility residents with cognitive impairment) bear the intervention without capacity to consent or effective channels to refuse; undocumented residents avoid enforcement contact and cannot safely object; subjects of emergency-declared mandates had no deliberative representation while ordinary legislative process was suspended.
% DISAPPEARANCE_RATIONALE: If the compulsory regime vanished overnight, coverage would fall below herd thresholds in pockets of refusal, outbreaks would reach the medically vulnerable within seasons, and institutions would rebuild either mandates or expensive incentive and screening systems. The protection the vulnerable depend on is an arrangement, not a natural fact — it rearranges if the arrangement goes.
% FOUNDING_PROBLEM: Recurrent epidemic disease with free-rider dynamics: voluntary immunization uptake plateaus below the coverage needed to protect those who cannot vaccinate, so states built compulsory immunization to reach coverage targets.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiology and the history of medicine attest the founding problem: sub-threshold voluntary uptake and resulting outbreaks are documented across the public-health literature and the historical record (smallpox, polio, measles resurgences). Judicial opinions across the interpretive spectrum — including rulings sympathetic to autonomy claims — concede the factual basis while disputing its normative weight. Bioethics scholarship outside the administering agencies corroborates both the problem's persistence and the contest over the remedy. No corroborating source attests that the problem is dead.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.82, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored high (0.82 at interval end) because the extracted good is bodily decision-authority — the most intimate locus of control a regime can move — taken from a concentrated minority and uncompensated; the reading's own lights price non-consensual intervention at the top of the scale regardless of the benefit delivered to others. Suppression (0.72) is structural: termination, exclusion, and penalty schedules suppress the refusal alternative; it is authored as a raw unscaled property, with the internalized-duty component carried separately in omega internalized_duty_vs_structural_penalty. Theater is moderate-low (0.28): the coverage function is real and load-bearing, but the exemption-hearing and documentation apparatus grew visibly performative during the emergency spike. Accessibility_collapse (0.45) reflects alternatives that persist but narrowed: exemptions, sector changes, relocation. Resistance (0.6) reflects sustained litigation, protest, and legislative rollback attempts. Temporal series run on one shared grid (t indexes years since 2000; t=20 is the mass-mandate emergency): a slow ratchet in scope (school to occupational to travel settings), a sharp crisis spike at t=20, and a partial retreat that settles ABOVE the pre-crisis trend line — the crisis oscillation is documented rather than smoothed, and the failure of the retreat to return to baseline is the extraction-accumulation signature across crisis cycles. Enforcement-capacity change is the traced dynamic, hence suppression_requirement is authored; its trajectory mirrors the ratchet-spike-partial-retreat shape.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently by construction. From the coerced individual's position the arrangement is a demand on their body backed by livelihood threats; from the exempt-seeking seat it is worse, because exit would require abandoning constitutive commitments; from the medically vulnerable seat it is the only shield available; from the agency seat it is its own instrument, staffed and administered; from the bench it is a balancing question. The engine computes per-seat classifications from the structural data — the authored claim does not adjudicate which experience is the constraint's true face.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The vaccinated majority (beneficiary, mobile exit) sits near the full-beneficiary end. The medically vulnerable are the instructive case: they are trapped, but trapped as DEPENDENTS on the good the arrangement delivers — their directionality stays near the beneficiary end because the arrangement subsidizes them, and their lack of exit reflects reliance on the protection, not costs borne. The unvaccinated-coerced (victim, constrained exit) sit near the target end; the exemption seekers' identity lock amplifies their d toward full-target; the institutionalized, though authored only as an excluded voice, in fact bear the intervention with zero exit and would derive the highest d of any seat if seated. Public health agencies combine agenda-setting with collection of the transferred authority, placing them near the beneficiary end despite institutional power. Administering institutions mix benefit (continuity, liability cover) with cost (attrition, administration), pulling their d up from pure beneficiary. No directionality_overrides are authored: the two powerless seats diverge through their role declarations, not their power atom, and per-seat derivation reads exactly that difference.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sub-threshold voluntary coverage exposing the unprotectable — is live, corroborated from outside the benefiting parties, so no mandatrophy resolution is declared and none should be. The classification work here is preventing two opposite mislabels. A pure-snare verdict would erase the genuine coordination core: the coverage the rules produce really does protect people who cannot protect themselves, and the free-rider problem is real, not cover. A pure-rope verdict would launder the transfer: the costs fall concentrated and uncompensated on a minority that did not agree, sustained by active enforcement. The tangled_rope structure with reading-indexed high epsilon records both truths at once — and the epsilon's referent discipline matters: scoring the arrangement this reading ENDORSES would fabricate a near-zero epsilon for every advocacy reading and destroy the corpus's ability to compare readings over the same arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This story instantiates the bodily_autonomy_primary reading of the mandate_legitimacy_scope kernel; which reading governs determines the victim set and epsilon — where exactly do the readings diverge?',
    'Track which reading courts and legislatures adopt in mandate controversies; the adopted reading selects which constraint file in the family governs classification of the same standing arrangement.',
    'Under public_health_primary the unvaccinated-coerced leave the victim set and epsilon collapses toward coordination-cost levels; under proportionality_reading victimhood becomes conditional on empirical parameters (severity, safety, alternatives), making epsilon unstable across episodes. Only under this reading does the authored high-epsilon profile hold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one reading of a three-reading kernel; the disagreement is located in whether collective-benefit claims can override individual consent.').

omega_variable(
    duress_consent_classification,
    'Does consent executed under threat of employment termination, educational exclusion, or institutional discharge count as informed consent for purposes of locating the victim set?',
    'Comparative legal-philosophical analysis of consent validity under material threat, plus empirical study of the conditions under which mandate-subject consent is actually given.',
    'If duress-conditioned consent counts as consent, the coerced are unwilling-but-consenting and epsilon falls well below the authored value, converging toward proportionality territory; if it does not count, the victim set stands as authored and the categorical reading''s high epsilon is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_consent_classification, conceptual, 'Whether nominal consent under penalty is consent — the hinge on which the victim set turns.').

omega_variable(
    emergency_ratchet_permanence,
    'Do emergency-period expansions of compulsory intervention permanently reset the baseline of acceptable coercion, or do they revert once the emergency lapses?',
    'Longitudinal policy tracking across successive emergency episodes: compare pre-emergency, peak, and post-emergency mandate scope, exemption generosity, and enforcement machinery.',
    'A permanent reset sustains the high-extraction plateau (ratchet dynamics compound across crises); full reversion makes the elevated extraction episodic and lowers the arrangement''s lifetime epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_ratchet_permanence, empirical, 'Whether the crisis-cycle retreat returns to baseline or leaves a residue — the temporal series shows partial retreat above the pre-crisis trend.').

omega_variable(
    internalized_duty_vs_structural_penalty,
    'How much of observed compliance reflects internalized civic or professional duty rather than avoidance of structural penalties?',
    'Attitudinal surveys and compliance behavior in jurisdictions that removed penalties while retaining recommendations — if compliance holds without penalties, duty dominates; if it collapses, penalty-avoidance dominates.',
    'If internalized duty dominates, the structural suppression measure overstates coercive force and the arrangement is more coordination-like than the authored suppression indicates; if penalty-avoidance dominates, suppression is fully structural and the authored value stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_duty_vs_structural_penalty, empirical, 'Attribution of the suppression scalar between structural penalty machinery and internalized obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(mand_tr_t0, observed).
narrative_ontology:measurement(mand_tr_t4, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 4, 0.13).
narrative_ontology:measurement_basis(mand_tr_t4, observed).
narrative_ontology:measurement(mand_tr_t8, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 8, 0.15).
narrative_ontology:measurement_basis(mand_tr_t8, observed).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 12, 0.18).
narrative_ontology:measurement_basis(mand_tr_t12, observed).
narrative_ontology:measurement(mand_tr_t16, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(mand_tr_t16, observed).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(mand_tr_t20, observed).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(mand_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(mand_be_t0, observed).
narrative_ontology:measurement(mand_be_t4, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 4, 0.62).
narrative_ontology:measurement_basis(mand_be_t4, observed).
narrative_ontology:measurement(mand_be_t8, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 8, 0.65).
narrative_ontology:measurement_basis(mand_be_t8, observed).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 12, 0.69).
narrative_ontology:measurement_basis(mand_be_t12, observed).
narrative_ontology:measurement(mand_be_t16, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 16, 0.73).
narrative_ontology:measurement_basis(mand_be_t16, observed).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 20, 0.86).
narrative_ontology:measurement_basis(mand_be_t20, observed).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 24, 0.82).
narrative_ontology:measurement_basis(mand_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(mand_su_t0, observed).
narrative_ontology:measurement(mand_su_t4, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 4, 0.53).
narrative_ontology:measurement_basis(mand_su_t4, observed).
narrative_ontology:measurement(mand_su_t8, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 8, 0.56).
narrative_ontology:measurement_basis(mand_su_t8, observed).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(mand_su_t12, observed).
narrative_ontology:measurement(mand_su_t16, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 16, 0.64).
narrative_ontology:measurement_basis(mand_su_t16, observed).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 20, 0.76).
narrative_ontology:measurement_basis(mand_su_t20, observed).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 24, 0.72).
narrative_ontology:measurement_basis(mand_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint family per the epsilon-invariance principle: 'mandate legitimacy' is one colloquial label decomposing into three structurally distinct constraints over the same standing arrangement (the compulsory-intervention regime). This file (bodily_autonomy_primary) authors epsilon 0.82 with the unvaccinated-coerced in the victim set; mandate_legitimacy_scope__public_health_primary authors low epsilon with no coerced-victim set; mandate_legitimacy_scope__proportionality_reading authors conditionally variable epsilon keyed to severity, safety, and alternatives. Upstream/downstream structure: public_health_primary is the historically upstream claim (police-power doctrine) cited as justification for the arrangement; this reading exerts downstream pressure on both siblings through litigation and rights-based challenge, changing their operating environment without resolving the dispute. Every family member links to the others via affects_constraints; orphan stories would be a code smell.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
