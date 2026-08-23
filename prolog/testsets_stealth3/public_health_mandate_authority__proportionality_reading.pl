% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__proportionality_reading, []).

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
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Public Health Mandate Authority — Proportionality Reading (Sliding-Scale Legitimacy)
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This file authors ONE reading of the public-health-mandate-authority
 *   kernel: the proportionality reading, on which mandate legitimacy is
 *   conditional on a four-factor showing — severity of threat, availability
 *   of alternatives, magnitude of coercion, duration of imposition. Per
 *   DP-001, epsilon's referent is the standing mandate arrangement as
 *   actually exercised across the 2020-2023 emergency arc, assessed by this
 *   reading's own lights — NOT the fully calibrated regime this reading would
 *   prefer to install. On that referent the arrangement is substantially but
 *   unevenly extractive: lethal-phase exercises bought genuine protection for
 *   people who cannot vaccinate into safety, while the long tail — renewals
 *   past evidentiary warrant, narrowly denied medical exemptions, undisclosed
 *   elite carve-outs — ran as uncompensated imposition. The victim boundary
 *   is dynamic rather than fixed: over-imposition places unvaccinated and
 *   medically contraindicated individuals in the payer seat; premature
 *   lifting moves immunocompromised patients there. Sibling stories (other
 *   files, not folded into this one):
 *   public_health_mandate_authority__public_health_primary (upstream,
 *   categorical commons-protection obligation) and
 *   public_health_mandate_authority__bodily_autonomy_primary (categorical
 *   bodily sovereignty); both are linked via network.affects_constraints and
 *   via cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - - public_health_agencies: Agenda-setter and discretion collector (institutional/arbitrage) — drafts, renews, and retires each mandate; publishes the evidence reviews that justify continuation
 *   - - immunocompromised_patients: Primary protected beneficiary with a shadow payer position (moderate/trapped) — protected when calibration holds, exposed when it slips
 *   - - hospital_systems: Institutional beneficiary and self-administering enforcer (institutional/constrained)
 *   - - frontline_healthcare_workers: Primary payer seat with a genuine secondary benefit (organized/constrained) — employment-contingent compliance
 *   - - medically_contraindicated_individuals: Pure bearer of imposition costs (powerless/trapped) — cannot comply, cannot exit biology
 *   - - vaccine_hesitant_adults: Payer seat under escalating-cost choice architecture (moderate/constrained)
 *   - - elite_exemption_holders: Carve-out beneficiaries (powerful/arbitrage) — released from rules they sustain
 *   - - constitutional_courts: Analytical observer (institutional/analytical) — resets the showing each renewal must carry
 *   - - disability_rights_advocates: Excluded voice (organized/trapped) — locked out of criterion-drafting, re-entering only through litigation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.52).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.44).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority — Proportionality Reading (Sliding-Scale Legitimacy)").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '2d19113f-1490-4e8b-8c29-a3ea38a77349').
narrative_ontology:cs_kernel_codification('2d19113f-1490-4e8b-8c29-a3ea38a77349', formalized).
narrative_ontology:cs_authority_grounding('2d19113f-1490-4e8b-8c29-a3ea38a77349', lineage).
narrative_ontology:cs_interpretation_layer_present('2d19113f-1490-4e8b-8c29-a3ea38a77349').
narrative_ontology:cs_reading_relation('2d19113f-1490-4e8b-8c29-a3ea38a77349', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('2d19113f-1490-4e8b-8c29-a3ea38a77349', public_health_mandate_authority__public_health_primary, influences).
narrative_ontology:cs_axiom('2d19113f-1490-4e8b-8c29-a3ea38a77349', foundational, conditional_legitimacy_sliding_scale).
narrative_ontology:cs_axiom_status(conditional_legitimacy_sliding_scale, holdable).
narrative_ontology:cs_axiom_grounding('2d19113f-1490-4e8b-8c29-a3ea38a77349', conditional_legitimacy_sliding_scale, instrumental).
narrative_ontology:cs_axiom('2d19113f-1490-4e8b-8c29-a3ea38a77349', foundational, least_coercive_effective_means).
narrative_ontology:cs_axiom_status(least_coercive_effective_means, holdable).
narrative_ontology:cs_axiom_grounding('2d19113f-1490-4e8b-8c29-a3ea38a77349', least_coercive_effective_means, deontological).
narrative_ontology:cs_reference_frame('2d19113f-1490-4e8b-8c29-a3ea38a77349', balanced_police_powers_framework).
narrative_ontology:cs_drift_state('2d19113f-1490-4e8b-8c29-a3ea38a77349', post_emergency_stand_down, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2d19113f-1490-4e8b-8c29-a3ea38a77349', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, hospital_systems).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, elite_exemption_holders).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, medically_contraindicated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, frontline_healthcare_workers).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, vaccine_hesitant_adults).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, frontline_healthcare_workers).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, immunocompromised_patients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft the emergency orders, set the exemption criteria, and decide when each mandate begins, tightens, and lapses. Each renewal extends the agency's operating discretion; each lapse hands decisions back to legislatures and courts. Agencies also publish the evidence reviews that justify continuation and staff the committees that hear exemption appeals. Exiting the arrangement means surrendering emergency-authority statutes piece by piece, which agencies rarely initiate themselves.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__proportionality_reading, public_health_agencies, beneficiary).

% Cannot mount full vaccine response and rely on reduced community transmission for protection. When mandates hold circulation down they receive protection they cannot purchase otherwise; when mandates lift early or thresholds follow a political calendar rather than an epidemiological one, they absorb renewed exposure with no compensating control. The susceptibility travels with their bodies; there is nowhere to move away from it.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_patients, beneficiary,
    moderate, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__proportionality_reading, immunocompromised_patients, payer).

% Operate intensive-care capacity and imposed employee vaccination requirements ahead of government orders in many regions. They gain predictable staffing and fewer ward-acquired outbreaks when uptake is high, and gain a shield when an official order rather than their own policy carries the enforcement burden. They bear recruitment and replacement costs when enforcement drives resignations. Declining to enforce shifts outbreak liability back onto themselves.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, hospital_systems, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__proportionality_reading, hospital_systems, agenda_setter).

% Work where exposure risk concentrates. Condition-of-employment rules made continued employment contingent on vaccination; those who declined lost positions, and those with open questions faced deadlines rather than deliberation. The same rules lowered the odds they carry infection home or acquire it on the ward. Relocating to non-mandate states or leaving clinical work was feasible for some specialties and ruinous for others.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, frontline_healthcare_workers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__proportionality_reading, frontline_healthcare_workers, beneficiary).

% Have conditions listed as reasons not to vaccinate, yet many jurisdictions routed their exemption applications through narrow criteria and long processing delays, denying a share despite documented contraindications. They bore masking-and-testing regimens, employment exclusions, and social suspicion without any ability to remove the ground of the exclusion. No change of address exempts them from their physiology.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, medically_contraindicated_individuals, payer,
    powerless, biographical, trapped, national).

% Weighed side-effect profiles, prior experiences with medical institutions, and generalized distrust, and concluded the vaccination was not for them. Where mandates reached their employers or the venues they used, the price of that conclusion arrived as termination notices, recurring testing fees, or forfeited access. Compliance ended the penalty; the choice architecture escalated the cost of refusal instead of opening negotiation.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, vaccine_hesitant_adults, payer,
    moderate, biographical, constrained, national).

% Legislators, executives, and their staff supported mandate continuations while securing testing-only carve-outs, quiet deferrals, or office-specific exemptions for their own workplaces. The carve-outs were seldom announced; disclosure typically followed records requests. Their release from rules they voted for was administrative, immediate, and largely invisible to the public.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, elite_exemption_holders, beneficiary,
    powerful, biographical, arbitrage, national).

% Adjudicate the boundary between the police-power tradition and individual liberty. Early in the emergency they deferred broadly to health agencies; as the emergency lengthened they began demanding evidentiary records, severing overbroad orders, and reviewing workplace mandates under ordinary rather than exceptional scrutiny. Their rulings reset what an agency must show before continuing each mandate.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Organizations representing people whose disabilities intersect with exemption criteria sought seats on the committees drafting exemption standards and were consulted late or pro forma. They argue from case files of denied applicants that the filtering criteria were drawn without the people they filter. Their route back into the process runs through litigation after the criteria have already operated on real applications.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, disability_rights_advocates, excluded,
    organized, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__proportionality_reading, public_health_agencies).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Addresses the free-rider problem in communicable disease control: individual vaccination and distancing decisions generate externalities, so coordinated uptake above a protection threshold maintains healthcare capacity and shields people who cannot generate their own immunity.
% TRANSFER_FUNCTION: Moves decision authority over bodily intervention from individuals to public health authorities; moves infection-risk reduction toward the non-immune and immunocompromised; moves enforcement and documentation costs onto employers and venues; when exercised past evidentiary warrant, moves employment and access security from the mandated population into the agencies' discretionary portfolio.
% ABSENT_VOICES: Medically contraindicated individuals had no seat where exemption criteria were drafted; disability-rights advocates objected from outside after the criteria were fixed; courts entered only after injury had accrued. The immunocompromised were addressed constantly in rhetoric ('protect the vulnerable') but the concrete tradeoff — how much coercion per unit of marginal risk reduction — was never put to them as a question with a negotiable answer.
% DISAPPEARANCE_RATIONALE: If mandate authority vanished overnight, outbreak response would reorganize around voluntary measures and exhortation; hospitals would triage surge capacity differently; the immunocompromised would face a higher infection baseline they cannot insure against; employer HR policy would fragment regionally. The arrangements of every seated party depend on the authority existing in some form.
% FOUNDING_PROBLEM: Epidemic control where voluntary uptake cannot reach the protection threshold — first codified against smallpox in the Jacobson era, revived when a novel lethal pathogen threatened to overrun hospital capacity faster than consent-based uptake could respond.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts attest the historical founding problem (the Jacobson lineage exists precisely because the problem recurred), and the epidemiological record of smallpox campaigns corroborates it independently of any benefiting party. Current liveness is disputed: public health agencies attest the problem persists for novel pathogens, while legislative majorities in several states attest it is resolved for routine respiratory management. No outside consensus exists on present-day liveness — the dispute itself is the signal.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__proportionality_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scalars report the interval-END state (t=36, post-stand-down), consistent with the shared-grid rule: every tracked metric is authored at every time point {0,6,12,18,24,30,36}. Extractiveness (0.52 at end) reflects a mixed arc: tight, well-justified imposition during the lethal phase rising through broadened employer mandates and terminated employment (peak 0.65 near t=24, after Omicron had shifted severity but before enforcement stood down), then partial retreat. Theater (0.27 at end, peaking 0.34 at t=24) captures documentation rituals and badge-status displays persisting past protective utility. Suppression_requirement is tracked deliberately because enforcement CAPACITY changed dramatically across the arc — build-up through t=18 (terminations executed, venue exclusion active), ratchet-release afterward — so a flat scalar would misrepresent the story; the trajectory is authored on the same grid as the other series. The rise-and-fall is cyclical but externally driven (variant severity updates, court rulings, electoral turnover), not intermittent reinforcement; however, the lag between severity updates and enforcement response is itself an extraction channel — mandates renewed on calendar rather than evidence (the duration prong failing) accumulate extraction during the lag phases, which is the T17-advisory signature visible in the mid-interval climb. Accessibility_collapse (0.45): alternatives existed (testing routes, medical exemptions) but were unevenly honored and sometimes functionally blocked; the reading's own legitimacy criterion makes alternative availability central, so partial collapse is the honest reading. Resistance (0.60): mass litigation, protests, healthcare-worker resignations, and state-level preemption statutes are documented, sustained resistance.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute divergently. From the agency seat the arrangement reads as calibrated necessity it administers and defends (coordination-dominant); from the medically contraindicated seat it reads as unconditional imposition with no compliant path (extraction-dominant); from the healthcare-worker seat it reads as a conditional bargain — coercion traded for ward-level protection. Inter-institutional dynamics matter: courts changed the constraint's operating environment mid-arc by tightening scrutiny (an observer resetting the agenda-setter's burden), and hospital systems enforced ahead of government, splitting the agenda-setter function across public and private administrators. Same-level lateral differentiation: frontline healthcare workers and vaccine-hesitant adults hold comparable nominal standing, yet their exits differ structurally — unionized workplace rules versus dispersed venue access — so identical global power yields different computed positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. public_health_agencies derive low d from the beneficiary declaration, tempered by the arbitrage exit; their secondary beneficiary role encodes the discretion-rent accrual. hospital_systems sit near the beneficiary end (protection and liability relief accrue to them; enforcement costs are passed to employees). immunocompromised_patients derive low d primarily, with the trapped exit pulling upward — their dual position is the reading's defining ambiguity and is carried by omega rather than flattened. The three payer groups derive high d: medically contraindicated individuals nearest the full-target end (trapped, no compliance path), workers next (constrained exit, organized resistance capability), hesitant adults similar. elite_exemption_holders derive low d despite paying nothing — their exemption is precisely the arbitrage that insulates them. Suppression is authored as a RAW structural property and is intentionally left unscaled in commentary reasoning; only extractiveness is scaled by directionality and scope in the engine's computation, and the national scope applies only a modest verification-hardening amplification.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled_rope prevents two opposite mislabels. Reading every mandate exercise as pure predation (snare) erases the demonstrable coordination function — at the calibrated end the arrangement solved a real collective-action problem and measurably protected the non-vaccinatable. Laundering the long tail as coordination cost (rope) ignores the duration creep, denied exemptions, and carve-outs that extracted without proportionate return. The R5 interview shows the founding problem CONTESTED, not dead, so no zombie flag fires — but the danger this reading exists to catch is mandatrophy by stealth: mandates renewed by calendar rather than by evidence. The duration_prong omega and the theater_ratio hump at t=24 are the tripwires for that failure mode; if a future arc shows renewals decoupled from evidence while enforcement machinery persists, the correct terminal classification drifts toward piton (theatrical maintenance of a spent imposition) even while aggregate extraction falls.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (proportionality_reading) of the kernel public_health_mandate_authority; would instantiating either sibling reading change the structural facts this file asserts?',
    'Re-run decomposition under each sibling: public_health_primary fixes the victim boundary on the unvaccinated regardless of threat level; bodily_autonomy_primary fixes every mandated person as bearing uncompensated imposition regardless of benefit delivered. The disagreement''s location is which premise is upstream — commons protection, sliding scale, or bodily sovereignty. Cross-reading comparison must travel the network edges, not re-parameterization of this file.',
    'Sibling instantiations produce different epsilon values and different victim sets over the same referent arrangement; classifications computed here are valid only for the proportionality reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this file is one of three readings of one contested kernel.').

omega_variable(
    threat_severity_calibration,
    'Does the arrangement''s extractiveness track threat severity as the sliding scale requires — heavy imposition legitimate against an Ebola-grade pathogen, the same apparatus extractive against a mild endemic respiratory virus?',
    'Pathogen-specific recomputation keyed to infection fatality ratio, transmissibility, healthcare-strain occupancy, and vaccine effectiveness against the circulating strain; the 2020-2023 arc supplies natural variation across the scale.',
    'At grave-threat parameterization the computed type moves toward rope (coordination dominant, imposition proportionate); at mild-threat parameterization toward snare (coercion without proportionate return). The tangled_rope verdict holds across the mixed arc; single-regime snapshots resolve differently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threat_severity_calibration, empirical, 'The reading''s defining dynamism: extractiveness varies with threat level along the sliding scale.').

omega_variable(
    victim_boundary_fluidity,
    'Which seat occupies the victim position at a given calibration state — the mandated (over-imposition regime) or the immunocompromised (premature-lifting regime)?',
    'Per-regime recomputation conditioned on the threat_severity_calibration outcome; the static beneficiaries and victims arrays record the interval-end snapshot only and must not be read as a fixed boundary.',
    'The chi distribution flips across seats depending on which direction calibration error runs; the aggregate tangled_rope classification survives, but per-seat classifications invert between regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_boundary_fluidity, conceptual, 'Dynamic victim boundary: both unvaccinated and immunocompromised enter the payer seat under different calibration errors.').

omega_variable(
    duration_creep_evidence_linkage,
    'Did mandate continuance track live evidence, or did renewals run on calendar and institutional momentum — the duration prong of the scale failing?',
    'Audit each renewal decision against contemporaneous efficacy and severity data; compare jurisdictions with statutory sunset mechanisms against those renewing by administrative extension.',
    'If decoupled, extraction accumulated through inertia during the interval''s second half (T17-advisory accumulation hypothesis), and theater_ratio at the tail understates performative maintenance of a spent imposition — pushing the terminal classification toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duration_creep_evidence_linkage, empirical, 'Duration prong integrity: evidence-linked renewal versus calendar-driven persistence.').

omega_variable(
    alternative_genuineness,
    'Were testing routes and medical exemptions genuinely available alternatives, or formally present and functionally blocked through narrow criteria, denials, and processing delays?',
    'Exemption grant-rate and turnaround-time data by jurisdiction; compare stated criteria against adjudicated outcomes, using the denied-applicant case files disability advocates compiled.',
    'If alternatives were functionally blocked, accessibility_collapse is underestimated here and the least-coercive-means prong failed more often than headline policy suggests — raising effective extraction among the trapped seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_genuineness, empirical, 'Availability-of-alternatives prong: formal presence versus functional access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__proportionality_reading, theater_ratio, 6, 0.16).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__proportionality_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__proportionality_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__proportionality_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__proportionality_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__proportionality_reading, theater_ratio, 36, 0.27).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__proportionality_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__proportionality_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__proportionality_reading, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__proportionality_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__proportionality_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__proportionality_reading, base_extractiveness, 36, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__proportionality_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__proportionality_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__proportionality_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__proportionality_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__proportionality_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__proportionality_reading, suppression_requirement, 36, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, resource_allocation).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial concept 'public health mandate authority' decomposes into three readings of ONE kernel, each with a distinct stable epsilon and victim structure — per DP-001 these are separate stories, not observables of one story. public_health_mandate_authority__public_health_primary is upstream (Jacobson-era orthodoxy; the authority this reading balances against and the claim cited to justify exercises). This proportionality reading is the mediating discipline: its four-factor showing reshapes when the upstream obligation may legitimately be exercised (influences edge). public_health_mandate_authority__bodily_autonomy_primary is the downstream categorical challenger (foreclosed BY this reading within any single framework, though live as a position held by other parties). Same referent arrangement, reading-indexed epsilon values (OQ-26); the epsilon differences across the family are the corpus's measurement of the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
