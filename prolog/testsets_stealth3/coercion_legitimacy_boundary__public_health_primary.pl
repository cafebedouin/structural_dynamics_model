% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__public_health_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: Public-Health-Primary Compulsion Boundary: State Mandate Enforcement Regime
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   The standing arrangement under contest: state and employer mandates that
 *   compel vaccination and allied screening on pain of school exclusion,
 *   workplace termination, venue denial, and in some jurisdictions fines —
 *   administered by public health authorities, bounded by exemption
 *   carve-outs of narrowing width, and subject to constitutional review. This
 *   file instantiates the public_health_primary reading of the
 *   coercion_legitimacy_boundary kernel: the state may compel medical
 *   intervention when collective harm-prevention outweighs individual
 *   autonomy. Per the epsilon-referent rule, extractiveness is authored for
 *   the standing mandate-enforcement arrangement as THIS reading assesses it
 *   — not for the arrangement the bodily_autonomy_primary sibling would
 *   install, and not averaged across readings. The sibling readings are
 *   separate constraint files linked through the network block. Claim and
 *   metrics are independent authored facts: the structure reads as
 *   tangled_rope (genuine coordination function, asymmetric enforced burden,
 *   active enforcement required) and the metrics describe the arrangement's
 *   actual operation; neither was tuned to the other. KEY AGENTS (by
 *   structural relationship): - public_health_authorities: agenda-setter
 *   (institutional/constrained) — designs and enforces the mandate regime,
 *   receives enforcement yield - mandate_refusing_individuals: primary target
 *   (moderate/constrained) — bears exclusion and penalty costs -
 *   conscientious_objector_parents: target, identity-locked
 *   (moderate/identity_locked) - unvaccinated_employees: target, economically
 *   trapped (powerless/trapped) - immunocompromised_patients: protected
 *   beneficiary (powerless/trapped) - pre_vaccination_age_infants: protected
 *   beneficiary (powerless/trapped) - frail_elderly_residents: protected
 *   beneficiary (powerless/trapped) - hospital_systems: institutional
 *   beneficiary (institutional/mobile) - vaccine_manufacturers: commercial
 *   beneficiary (powerful/arbitrage) - constitutional_courts: analytical
 *   observer (institutional/analytical) - medical_distrust_communities:
 *   excluded voice (powerless/trapped) - vaccine_injury_claimants: excluded
 *   voice (powerless/trapped)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.65).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.62).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "Public-Health-Primary Compulsion Boundary: State Mandate Enforcement Regime").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, '27c51a65-6ef6-4e72-bc87-9d87ef9c016e').
narrative_ontology:cs_kernel_codification('27c51a65-6ef6-4e72-bc87-9d87ef9c016e', formalized).
narrative_ontology:cs_authority_grounding('27c51a65-6ef6-4e72-bc87-9d87ef9c016e', lineage).
narrative_ontology:cs_interpretation_layer_present('27c51a65-6ef6-4e72-bc87-9d87ef9c016e').
narrative_ontology:cs_reading_relation('27c51a65-6ef6-4e72-bc87-9d87ef9c016e', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('27c51a65-6ef6-4e72-bc87-9d87ef9c016e', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('27c51a65-6ef6-4e72-bc87-9d87ef9c016e', foundational, collective_harm_prevention_trumps_autonomy_when_outweighed).
narrative_ontology:cs_axiom_status(collective_harm_prevention_trumps_autonomy_when_outweighed, holdable).
narrative_ontology:cs_axiom_grounding('27c51a65-6ef6-4e72-bc87-9d87ef9c016e', collective_harm_prevention_trumps_autonomy_when_outweighed, instrumental).
narrative_ontology:cs_axiom('27c51a65-6ef6-4e72-bc87-9d87ef9c016e', secondary, individual_refusal_confers_no_collective_veto).
narrative_ontology:cs_axiom_status(individual_refusal_confers_no_collective_veto, holdable).
narrative_ontology:cs_axiom_grounding('27c51a65-6ef6-4e72-bc87-9d87ef9c016e', individual_refusal_confers_no_collective_veto, conventional).
narrative_ontology:cs_reference_frame('27c51a65-6ef6-4e72-bc87-9d87ef9c016e', collective_harm_prevention_supremacy).
narrative_ontology:cs_drift_state('27c51a65-6ef6-4e72-bc87-9d87ef9c016e', contemporary_post_pandemic_mandate_retrenchment, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('27c51a65-6ef6-4e72-bc87-9d87ef9c016e', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, pre_vaccination_age_infants).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, frail_elderly_residents).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, hospital_systems).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, vaccine_manufacturers).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, public_health_authorities).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, mandate_refusing_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, conscientious_objector_parents).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_employees).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, herd_immunity_threshold_theory).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, police_power_harm_prevention_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the scope of mandatory vaccination and screening rules, declare outbreak conditions, set compliance deadlines, and run the enforcement machinery: school-entry checks, workplace requirements, exclusion orders, and penalty schedules. Enforcement programs arrive with dedicated appropriations, reporting systems, and staffing, so the agency's reach expands with each mandate it administers. It cannot resign the function, but it answers to legislatures and electorates that can rewrite or defund it.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, public_health_authorities, beneficiary).

% Cannot be vaccinated or mount a durable immune response, and some are medically contraindicated. Their safety depends on the immunity of the people around them. They have no exit from exposure risk except self-isolation, which carries its own medical and social costs.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, national).

% Too young for the scheduled dose series, they depend entirely on surrounding coverage for protection. They have no voice and no mobility; their exposure is set entirely by adult decisions.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, pre_vaccination_age_infants, beneficiary,
    powerless, immediate, trapped, national).

% Live with waning immune response, often in congregate settings where a single introduction spreads widely. Protection reaches them through community coverage rather than anything they can purchase individually.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, frail_elderly_residents, beneficiary,
    powerless, biographical, trapped, regional).

% Absorb the surge costs of preventable outbreaks and benefit when mandates keep admissions predictable. They advocate for mandate policies, can shift capacity and staff across regions, and bear no personal compliance burden.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, hospital_systems, beneficiary,
    institutional, generational, mobile, continental).

% Sell into a market where mandates convert hesitant demand into guaranteed volume. Liability shields and advance-purchase agreements insulate margins. Production and registration can be moved across jurisdictions, giving wide arbitrage freedom.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, vaccine_manufacturers, beneficiary,
    powerful, generational, arbitrage, global).

% Decline one or more mandated interventions and bear the consequences: school and venue exclusion, employment barriers, fines in some jurisdictions. Exemptions exist but are narrowing. Exit is possible — relocating, homeschooling, changing industries — but each path carries heavy financial and social cost.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, mandate_refusing_individuals, payer,
    moderate, biographical, constrained, national).

% Refuse on settled belief grounds; complying would violate commitments that organize their family life and community standing. They accept exclusion, hearings, litigation, and sometimes relocation rather than comply. Their refusal is not bargaining posture; it is constitutive of who they understand themselves to be.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, conscientious_objector_parents, payer,
    moderate, biographical, identity_locked, regional).

% Work under employer mandates where refusal means termination. Household obligations, specialized skills, and thin labor-market alternatives make quitting unrealistic; compliance is the price of keeping income.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_employees, payer,
    powerless, immediate, trapped, national).

% Hear challenges to mandate authority, define the limits of the state's protective power, and review exemption regimes. They hold no enforcement budget and decide cases that bind every other seat.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Carry historical memory of exploitative research and uneven enforcement. They are rarely represented in the proceedings that design mandates, yet they meet enforcement contact disproportionately and would contest both the rules and their administration if seated.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, medical_distrust_communities, excluded,
    powerless, generational, trapped, regional).

% Suffer serious adverse events after compliant vaccination. Compensation channels are narrow, slow, and adversarial in many jurisdictions; their losses are real but largely unpriced in the arrangement's accounting.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, vaccine_injury_claimants, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__public_health_primary, public_health_authorities).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in infectious-disease control: each individual does best by skipping the intervention while everyone else complies, so voluntary uptake undershoots the coverage threshold at which transmission stalls. Compulsion aligns individual action with the population-level threshold.
% TRANSFER_FUNCTION: Moves compliance burdens — procedure costs, bodily risk-bearing, autonomy, and in enforcement cases fines and lost employment — from the general population onto the non-compliant minority, and moves outbreak risk away from the immuno-vulnerable and onto those who refuse.
% ABSENT_VOICES: Medical-distrust communities and vaccine-injury claimants are structurally absent from mandate design: they would contest both the rules and the compensation architecture but hold no seat in rulemaking, which is dominated by agencies, professional bodies, and industry. Refusers appear mainly as litigants after rules are fixed, not as participants before them.
% DISAPPEARANCE_RATIONALE: Coverage would fall below transmission thresholds in susceptible pockets, outbreaks would resurge on outbreak-cycle timescales, hospitals would rebuild surge protocols, immuno-vulnerable households would retreat into isolation, and jurisdictions would come under pressure to reconstruct some compulsory or quasi-compulsory coordination mechanism — the arrangement's disappearance triggers reorganization, not equilibrium.
% FOUNDING_PROBLEM: Recurring epidemic mortality — smallpox, then measles, polio, pertussis — that voluntary uptake could not suppress because free-riding undercut coverage; nineteenth- and early-twentieth-century compulsory vaccination laws, crystallized in Jacobson v. Massachusetts (1905), were built to force coverage past the threshold.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: WHO and PAHO measles-elimination reversals document resurgence wherever coverage slips; historical demography records the pre-vaccine mortality baseline; courts applying the Jacobson lineage adjudicate without a budgetary stake in agency enforcement. No seat that merely collects from the arrangement supplies the attestation.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.65: the enforcement apparatus concentrates real costs — exclusion, termination, penalties, litigation exposure — on a refusing minority while the protective yield diffuses across the population; this reading's own lights hold much of that burden justified, which caps epsilon below snare territory but does not erase it. Suppression 0.62: compulsion is constitutive, not incidental, but bounded by exemption carve-outs and judicial review. Theater_ratio 0.20: coverage maintenance is functional; ceremony (exemption hearings, compliance paperwork) is a minority share of activity. Accessibility_collapse 0.45: exits persist (relocation, homeschooling, occupational change, remaining exemption routes) but narrow with each repeal cycle. Resistance 0.55: sustained litigation, exemption politics, and electoral backlash. All three tracked series share one time grid (points 0-30, unit approximately one year, spanning the modern school-entry-mandate era through pandemic-era employment mandates); trajectories rise stepwise at outbreak-driven reform points (exemption repeals, employment-mandate waves), modeling an enforcement ratchet rather than smooth drift. Suppression_requirement is tracked because the story's dynamic IS enforcement-capacity hardening. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and spatial scope, and the engine owns that arithmetic.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as its own legitimate instrument; the payer seats experience the identical structure as imposed burden. Among payers at similar nominal standing, exit quality differentiates everything: mandate_refusing_individuals hold costly-but-real exits (constrained); conscientious_objector_parents are identity_locked — their refusal is fused with family and community identity, so exit-as-compliance is unthinkable and their effective burden peaks; unvaccinated_employees are trapped by livelihood. If the objectors' identity frame broke, their seat would migrate toward constrained exit and their computed burden would fall with no change in the rules. Institutional seats diverge as well: courts constrain enforcement without bearing it; hospitals and manufacturers collect without complying; the excluded seats bear costs with no procedural voice. Coalition potential among refusers is real (litigation networks, ballot campaigns) and is why resistance sits at 0.55 rather than lower.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Trapped, powerless protected classes (immunocompromised patients, pre-vaccination infants, frail elderly) sit nearest the beneficiary pole — the arrangement subsidizes them and they cannot arbitrage it. Hospital systems and manufacturers collect with mobile/arbitrage exit, damping their directionality further. Public health authorities administer the regime and receive enforcement yield, placing them low-d with an agenda-setting lever. The three payer seats sit near the target pole: identity_locked and trapped exit push toward full-target, constrained exit holds slightly off it. Excluded seats (medical-distrust communities, injury claimants) are structurally target-side with no compensating channel. Most seats carry national scope, which modestly amplifies effective extraction through verification difficulty; the engine owns that modifier.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so no mandatrophy resolution is declared. The classification work here is boundary-keeping: a pure-snare label would erase the genuine coordination function — herd immunity protects people who cannot consent, and voluntary uptake demonstrably undershoots transmission thresholds; a pure-rope label would erase the asymmetric, actively enforced burden concentrated on refusers and the unpriced injury externality carried by a compliant residual few. Tangled_rope keeps both faces legible and lets the engine compute where each seat actually sits. The low theater ratio confirms the function has not atrophied into performance; the rising enforcement series is ratchet, not decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Is the live disagreement inside the coercion_legitimacy_boundary kernel located in the balancing standard itself (this reading''s terrain) or in the empirical inputs fed to any balancing standard (the proportionality sibling''s terrain)?',
    'Institutional convergence tracking: observe which standard appellate courts and legislatures operationalize across successive mandate controversies, and whether severity-contingent triggers displace the general balancing test.',
    'If the proportionality reading prevails institutionally, the coerced set becomes severity-contingent (refusers burdened mainly during high-severity outbreaks) and steady-state effective extraction falls; if this reading prevails, the coerced set stays broad across disease conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Locating the kernel dispute between balancing-standard and input-variable framings.').

omega_variable(
    balancing_weight_indeterminacy,
    'This reading''s operative test — collective harm-prevention outweighs individual autonomy — specifies no weighting function; who computes the balance, with what uncertainty bounds and appeal rights, determines how large the coerced set is.',
    'Comparative analysis of threshold-setting procedures across jurisdictions: who declares outbreak conditions, whose risk models count, how exemption boards weigh testimony, and what review attaches to the balancing determination.',
    'Procedurally captured balancing (agency both computes the balance and collects from its outcome) would push the arrangement toward a snare profile; symmetric expert proceedings with judicial review would stabilize the tangled_rope reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balancing_weight_indeterminacy, conceptual, 'Indeterminacy of the balancing function that sizes the victim set.').

omega_variable(
    exemption_ratchet_direction,
    'Are exemption carve-outs (medical, religious, philosophical) a stable structural feature of the arrangement or an eroding remnant under outbreak-driven repeal ratchets?',
    'Longitudinal jurisdiction-level tracking of exemption availability, grant rates, and legislative repeal events following outbreak episodes.',
    'Continued erosion raises suppression and the burden on refusing seats, drifting the computed classification toward snare; stabilized carve-outs anchor the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_ratchet_direction, empirical, 'Direction of travel of the exemption carve-outs that bound suppression.').

omega_variable(
    uncompensated_injury_externality,
    'Does the arrangement systematically impose vaccine-injury costs on a compliant residual few without functioning compensation, and does that uncompensated residue count as part of the arrangement''s extraction?',
    'Audit of compensation-program coverage, award latency, and denial rates against independently estimated adverse-event incidence.',
    'Systematic uncompensated injury widens the victim set beyond refusers and raises effective extraction materially; adequate compensation internalizes the cost and lowers it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncompensated_injury_externality, empirical, 'Whether the unpriced adverse-event burden on compliant injured parties is extraction or tragic residual cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(coer_tr_t0, observed).
narrative_ontology:measurement(coer_tr_t5, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 5, 0.13).
narrative_ontology:measurement_basis(coer_tr_t5, observed).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(coer_tr_t10, observed).
narrative_ontology:measurement(coer_tr_t15, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 15, 0.16).
narrative_ontology:measurement_basis(coer_tr_t15, observed).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(coer_tr_t20, observed).
narrative_ontology:measurement(coer_tr_t25, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 25, 0.19).
narrative_ontology:measurement_basis(coer_tr_t25, observed).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 30, 0.2).
narrative_ontology:measurement_basis(coer_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(coer_be_t0, observed).
narrative_ontology:measurement(coer_be_t5, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(coer_be_t5, observed).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(coer_be_t10, observed).
narrative_ontology:measurement(coer_be_t15, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(coer_be_t15, observed).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(coer_be_t20, observed).
narrative_ontology:measurement(coer_be_t25, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 25, 0.63).
narrative_ontology:measurement_basis(coer_be_t25, observed).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(coer_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(coer_su_t0, observed).
narrative_ontology:measurement(coer_su_t5, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 5, 0.47).
narrative_ontology:measurement_basis(coer_su_t5, observed).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(coer_su_t10, observed).
narrative_ontology:measurement(coer_su_t15, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 15, 0.54).
narrative_ontology:measurement_basis(coer_su_t15, observed).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(coer_su_t20, observed).
narrative_ontology:measurement(coer_su_t25, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 25, 0.6).
narrative_ontology:measurement_basis(coer_su_t25, observed).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(coer_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'state coercion for public health' covers three structurally distinct claims with different victim sets and different epsilon: a categorical-permissibility claim (bodily_autonomy_primary), a severity-contingent claim (proportionality_reading), and a general-balancing claim (this file). Decomposed per the epsilon-invariance principle; family links run through affects_constraints. Structural propagation: the proportionality sibling shares this reading's balancing architecture but swaps the input variable (severity dynamics for aggregate benefit), so movement in either propagates to the other; the autonomy sibling is the categorical counter-frame that this reading's foundational axiom logically excludes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
