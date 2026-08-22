% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: Consent-Gate Legitimacy for Medical Interventions (Bodily-Autonomy-Primary Reading)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   A contested kernel — what makes a medical intervention legitimate —
 *   decomposes into three structurally distinct readings. This story
 *   instantiates the bodily-autonomy-primary reading: informed consent is a
 *   necessary condition of legitimacy, and state coercion violates bodily
 *   integrity regardless of public benefit. The epsilon referent is the
 *   standing arrangement under contest — the actual regime of state-coerced
 *   medical intervention (employment-linked mandates, school-entry
 *   requirements, emergency treatment powers) — assessed by this reading's
 *   own lights, under which every coerced submission is a rights violation
 *   and the arrangement reads as substantially extractive. Sibling readings
 *   instantiate different constraints: public_health_primary reads refusal as
 *   externality imposition and coercion as legitimate when morbidity falls
 *   (no coerced-victim set, low epsilon); proportionality_reading weights
 *   autonomy against threat severity (epsilon scaling with disease
 *   characteristics). Those are separate files linked through
 *   network.affects_constraints; their epsilon values differ because their
 *   victim sets differ, not because one observable is being measured two
 *   ways. KEY AGENTS (by structural relationship): - public_health_agencies:
 *   Agenda-setter and primary beneficiary (institutional/mobile) —
 *   administers and enforces, collects compliance - coerced_medical_subjects:
 *   Primary target (moderate/trapped) — submits under penalty -
 *   penalized_decliners: Secondary target (moderate/constrained) — refuses
 *   and absorbs penalties - protected_high_risk_groups: Beneficiary
 *   (organized/trapped) — collects indirect protection -
 *   institutional_employers: Secondary beneficiary (powerful/arbitrage) —
 *   collects workforce uniformity - medical_professional_bodies:
 *   Dual-positioned beneficiary and target (organized/identity_locked) -
 *   civil_liberties_advocates: Excluded voice (organized/mobile) -
 *   constitutional_courts: Observer (institutional/analytical)
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda-setter and primary beneficiary (institutional/mobile) — designs and enforces mandate regimes, collects compliance and outcome metrics
 *   - coerced_medical_subjects: primary target (moderate/trapped) — would decline but submits under employment or access penalty
 *   - penalized_decliners: secondary target (moderate/constrained) — refuses and absorbs job loss, exclusion, and sanction
 *   - protected_high_risk_groups: beneficiary (organized/trapped) — collects indirect protection, cannot exit risk status
 *   - institutional_employers: secondary beneficiary (powerful/arbitrage) — collects workforce uniformity and liability reduction, can relocate or seek carve-outs
 *   - medical_professional_bodies: dual-positioned beneficiary and target (organized/identity_locked) — endorses the framework while their members are its most mandated workforce
 *   - civil_liberties_advocates: excluded voice (organized/mobile) — argues against coercion-based design, heard mainly through after-the-fact litigation
 *   - constitutional_courts: observer (institutional/analytical) — adjudicates the legitimacy line case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.68).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.62).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "Consent-Gate Legitimacy for Medical Interventions (Bodily-Autonomy-Primary Reading)").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '00520be4-4805-4bd0-9213-50b6fa84f2f2').
narrative_ontology:cs_kernel_codification('00520be4-4805-4bd0-9213-50b6fa84f2f2', distributed).
narrative_ontology:cs_authority_grounding('00520be4-4805-4bd0-9213-50b6fa84f2f2', distributed).
narrative_ontology:cs_reading_relation('00520be4-4805-4bd0-9213-50b6fa84f2f2', legitimate_health_intervention__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('00520be4-4805-4bd0-9213-50b6fa84f2f2', legitimate_health_intervention__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('00520be4-4805-4bd0-9213-50b6fa84f2f2', foundational, bodily_integrity_inviolable_without_consent).
narrative_ontology:cs_axiom_status(bodily_integrity_inviolable_without_consent, holdable).
narrative_ontology:cs_axiom_grounding('00520be4-4805-4bd0-9213-50b6fa84f2f2', bodily_integrity_inviolable_without_consent, deontological).
narrative_ontology:cs_axiom('00520be4-4805-4bd0-9213-50b6fa84f2f2', foundational, public_benefit_cannot_purchase_intervention_legitimacy).
narrative_ontology:cs_axiom_status(public_benefit_cannot_purchase_intervention_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('00520be4-4805-4bd0-9213-50b6fa84f2f2', public_benefit_cannot_purchase_intervention_legitimacy, deontological).
narrative_ontology:cs_reference_frame('00520be4-4805-4bd0-9213-50b6fa84f2f2', informed_consent_absolute_gate).
narrative_ontology:cs_drift_state('00520be4-4805-4bd0-9213-50b6fa84f2f2', contemporary_post_emergency_powers_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('00520be4-4805-4bd0-9213-50b6fa84f2f2', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, protected_high_risk_groups).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, institutional_employers).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, coerced_medical_subjects).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, penalized_decliners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, medical_professional_bodies).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, medical_professional_bodies).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, police_powers_health_doctrine).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, population_outcome_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce mandate regimes through employment, education, and access rules; collect compliance data and outcome metrics that justify budget and statutory standing. They can shift instruments — mandate to incentive to education campaign — as political conditions change, and enforcement success compounds their administrative capacity.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, public_health_agencies, agenda_setter,
    institutional, generational, mobile, national).

% Individuals who would decline an intervention but submit under penalty — threatened job loss, school exclusion, or benefit denial. They bear the intervention's risks and the loss of decision authority over their own bodies; exiting means forfeiting livelihood or education, which for most is not a real option.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, coerced_medical_subjects, payer,
    moderate, biographical, trapped, national).

% Individuals who refuse and absorb the scheduled penalty — terminated jobs, restricted venues and travel, social sanction. Some reorganize life around refusal through remote work or home schooling; the penalty schedule effectively prices their dissent rather than forbidding it, and the price falls hardest on those with the least savings.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, penalized_decliners, payer,
    moderate, biographical, constrained, national).

% Immunocompromised people, transplant recipients, and elderly people with comorbidities who rely on population-level uptake for indirect protection they cannot obtain directly. Advocacy organizations speak for them, but individual members cannot exit their risk status and bear residual harm whenever uptake falls.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, protected_high_risk_groups, beneficiary,
    organized, biographical, trapped, national).

% Hospital systems, large corporations, and universities that gain uniform workforce health status, reduced outbreak liability, and predictable staffing. When compliance costs bite they can relocate operations, negotiate carve-outs, or lobby for exemption categories — exit levers unavailable to their employees.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, institutional_employers, beneficiary,
    powerful, generational, arbitrage, global).

% Licensing boards and professional associations that endorse mandate frameworks and lend them clinical legitimacy, while their members constitute the most heavily mandated workforce in the economy. Dissenting clinicians face discipline and referral networks close ranks; professional identity is fused with the endorsement, making internal criticism career-costly and exit from the profession unthinkable for most.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, medical_professional_bodies, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, medical_professional_bodies, payer).

% Constitutional litigators and rights organizations who argue against coercion-based design on bodily-integrity and due-process grounds. They are consulted late or not at all during emergency rulemaking, gaining standing chiefly through after-the-fact litigation rather than design-stage voice in the rooms where mandates are drafted.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, civil_liberties_advocates, excluded,
    organized, generational, mobile, national).

% Adjudicate where the legitimacy line falls — reviewing whether specific mandates exceed lawful police powers under reasonableness standards. They shape the constraint's boundaries case by case without administering it, and their rulings feed back into agency drafting practices.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Produce the conceptual vocabulary both sides deploy — autonomy, solidarity, proportionality, the limits of state power over the body. They map the disagreement rather than administer it, and their framing choices determine which considerations count in later judicial and legislative argument.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, bioethics_scholars, observer,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__bodily_autonomy_primary, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in population-level disease control: individual decisions about accepting medical interventions generate transmission externalities, and coordinated high uptake protects members who cannot be protected directly. Mandates produce uptake at scale and speed that voluntary campaigns may not reach during acute outbreaks.
% TRANSFER_FUNCTION: Moves decision authority over one's own medical care from individuals to administrative bodies, enforced through employment, education, and access leverage; moves the intervention's risk-bearing onto coerced individuals while distributing protection benefits across the population and compliance-capacity gains to enforcing institutions.
% ABSENT_VOICES: The coerced appear in design processes only as aggregate compliance statistics, not as seated participants; conscientious objectors and disability-rights advocates are largely absent from emergency rulemaking; civil liberties counsel enters after rules are fixed; and future generations who inherit precedent-setting coercion powers have no seat at all.
% DISAPPEARANCE_RATIONALE: If the coercive enforcement machinery vanished overnight, uptake would fall among hesitant populations, transmission risk would shift onto the unprotected and the high-risk, institutions would rebuild access rules around testing and accommodation, and the state would lose a demonstrated compliance instrument — the health-policy landscape would reorganize around whatever voluntary-and-incentive architecture replaced it. Whether that rearrangement counts as restoration or harm is precisely what the sibling readings dispute.
% FOUNDING_PROBLEM: Recurrent epidemic disease exposed a collective-action failure: voluntary uptake left protective gaps precisely where vulnerability concentrated, and institutions lacked any lawful instrument to secure the uptake levels that indirect protection requires.
% FOUNDING_PROBLEM_CORROBORATION: Independent academic epidemiology, historical pre-mandate mortality records, and insurer actuarial data attest that the founding problem was and remains real — these sources sit outside the enforcing agencies. What no outside source attests is that coercion is the only or best remaining solution to it; that claim is disputed by voluntary-uptake jurisdictions and by the civil-liberties bar, and the dispute is live in the literature.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68 at interval end) because, from this reading's seat, each coerced submission transfers decision authority over bodily integrity wholesale to administrative bodies, with the transfer enforced by livelihood and access leverage rather than compensated. Suppression (0.62) is predominantly structural: exit is priced through employment termination, school exclusion, and venue access denial, not through criminalized dissent in most jurisdictions. Theater is moderate-low (0.25): the epidemiological function is real and measurable, but symbolic enforcement activity swells during crisis tails and decays afterward. Accessibility collapse is moderate (0.45): exemptions, testing alternatives, accommodation pathways, and jurisdictional arbitrage persist, but they collapse partially wherever employment or enrollment is directly tied. Resistance is substantial (0.60): sustained litigation, protest movements, exemption campaigns, and electoral backlash meet the arrangement continuously. The temporal series run on one shared grid (points 0, 5, 10, 15, 20, 25) for all three metrics. The suppression_requirement series is authored deliberately because enforcement-capacity change is the dynamic this story tracks: a slow ratchet (0.38 to 0.48) followed by a sharp crisis-era build-up (0.74) and partial post-crisis relaxation (0.62) — note the post-relaxation floor sits well above the pre-crisis baseline, a ratchet signature. Extractiveness accumulates monotonically across the cycle: each enforcement episode leaves the compliance architecture more entrenched, so the cyclical pattern is not noise but a rectifying ratchet — oscillation with an upward-drifting mean.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently by construction. From the public_health_agencies seat the arrangement is a functioning instrument it built, staffed, and defends — coordination with costs borne elsewhere. From the coerced_medical_subjects seat the identical structure operates as uncompensated seizure of bodily decision authority with no viable exit. Penalized_decliners experience a third variant: purchasable dissent, where the constraint prices rather than forbids refusal. Medical_professional_bodies straddle the line — endorsing the framework that disciplines their own dissenting members. Constitutional_courts see a bounded reasonableness question rather than a rights-absolutes question. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: public_health_agencies (agenda-setter, collects compliance directly) and institutional_employers (arbitrage-grade exit) sit near the beneficiary end of d; protected_high_risk_groups are beneficiaries but trapped in their risk status, pulling their d above a pure beneficiary's. Coerced_medical_subjects are trapped targets — trapped exit pushes them toward the full-target end of d, so effective extraction amplifies for them beyond the base rate. Penalized_decliners are constrained rather than trapped: their dissent is expensive but possible, sitting slightly below the trapped target. Medical_professional_bodies derive beneficiary-side d from their endorsement role, but their identity_locked exit and their members' mandated status place their lived position mid-range — the derivation captures the institutional role, the commentary records the split. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness scales, and the national scope of the enforcement architecture raises effective extraction modestly through verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — recurrent epidemics exposing a collective-action failure in voluntary uptake — is live, so no mandatrophy is declared and the mismatch consumer should find status=live paired with verdict=world_rearranges, producing no zombie flag. The tangled-rope classification guards against both mislabeling errors this domain invites: calling the arrangement a snare erases the genuine coordination function (disease-control externalities) that even this reading acknowledges as descriptive fact — the reading disputes coercion's legitimacy, not the existence of the problem; calling it a rope erases the coerced victim set this reading places at the center. The theater_ratio stays below piton range because the enforcement activity retains functional content; the arrangement is maintained by active enforcement and constituency interest, not inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (bodily_autonomy_primary) of the kernel legitimate_health_intervention — what changes structurally if a sibling reading is adopted instead?',
    'Comparative read of the three sibling story files: victim sets, epsilon, and enforcement structure differ by construction. Adoption of public_health_primary removes coerced individuals from the victim set (refusal becomes externality imposition) and drops epsilon toward the low end; adoption of proportionality_reading makes epsilon disease-severity-indexed rather than fixed.',
    'Classification of this file is stable only within this reading. Under public_health_primary the state exits the extractor seat and mandate-refusers enter it; under proportionality_reading the victim set contracts to coercion exceeding the proportionality threshold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one-of-three readings of the legitimacy kernel; sibling adoption rewrites the victim set and epsilon.').

omega_variable(
    disagreement_location_side_constraint_vs_weighting,
    'Is the disagreement between readings located in empirical beliefs about mandate effectiveness, or in the deontological status of bodily integrity (absolute side-constraint versus weighted consideration)?',
    'Conceptual analysis: the readings diverge even under identical effectiveness data — public_health_primary and bodily_autonomy_primary can agree on every epidemiological number and still disagree on legitimacy, so the dispute survives full empirical resolution.',
    'If located empirically, better evidence could converge the readings; if deontological (as authored), no evidence resolves it and the foreclosure relation to both siblings stands permanently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_side_constraint_vs_weighting, conceptual, 'Locates the kernel contest in deontology rather than epidemiology.').

omega_variable(
    enforcement_severity_epsilon_variance,
    'How much of the measured epsilon is driven by enforcement severity — termination-level employment leverage versus accommodation-first requirements versus criminalized noncompliance — which varies across jurisdictions and periods?',
    'Cross-jurisdiction comparison of penalty schedules against measured refusal rates and welfare losses; within-jurisdiction before/after analysis of enforcement escalation episodes.',
    'At termination-level leverage epsilon sits near the authored 0.68; under accommodation-first regimes epsilon falls materially, potentially moving computed seat classifications toward rope territory for affected seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_severity_epsilon_variance, empirical, 'Epsilon varies with enforcement severity across jurisdictions and enforcement episodes.').

omega_variable(
    coercion_separability_from_uptake,
    'Is high intervention uptake achievable through consent-compatible means (access expansion, information campaigns, paid incentives, accommodation) such that the coercive component is separable surplus rather than load-bearing?',
    'Natural experiments: jurisdictions achieving comparable uptake voluntarily; incentive-only programs; uptake deltas when mandates convert to testing-and-accommodation regimes.',
    'If separable, the coercive layer is extractive surplus riding a coordinable function, sharpening the extraction reading at the margin; if inseparable, part of the measured extraction is the irreducible price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_separability_from_uptake, empirical, 'Whether the coercion component is separable from the uptake it produces.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t5, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 5, 0.17).
narrative_ontology:measurement_basis(legi_tr_t5, observed).
narrative_ontology:measurement(legi_tr_t10, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(legi_tr_t10, observed).
narrative_ontology:measurement(legi_tr_t15, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 15, 0.29).
narrative_ontology:measurement_basis(legi_tr_t15, observed).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(legi_tr_t20, observed).
narrative_ontology:measurement(legi_tr_t25, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 25, 0.25).
narrative_ontology:measurement_basis(legi_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t5, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(legi_be_t5, observed).
narrative_ontology:measurement(legi_be_t10, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(legi_be_t10, observed).
narrative_ontology:measurement(legi_be_t15, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(legi_be_t15, observed).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(legi_be_t20, observed).
narrative_ontology:measurement(legi_be_t25, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(legi_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t5, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(legi_su_t5, observed).
narrative_ontology:measurement(legi_su_t10, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(legi_su_t10, observed).
narrative_ontology:measurement(legi_su_t15, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(legi_su_t15, observed).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(legi_su_t20, observed).
narrative_ontology:measurement(legi_su_t25, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(legi_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legitimate health intervention' decomposes into three structurally distinct constraints sharing one kernel. This member (bodily_autonomy_primary) carries the widest victim set (all coerced submissions) and fixed high epsilon; public_health_primary carries no coerced-victim set and epsilon indexed to outcome data; proportionality_reading carries a threshold-contingent victim set and severity-indexed epsilon. The upstream/downstream structure runs from public_health_primary (highest empirical confidence in its outcome claims) toward this reading, which it cites as the foil that defines what is being rejected. Each file links the other two via affects_constraints; the epsilon differences reflect different victim sets, not different observables over one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
