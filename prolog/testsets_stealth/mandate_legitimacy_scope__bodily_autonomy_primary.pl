% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Compulsory Medical Intervention Without Case-by-Case Consent (Bodily-Autonomy-Primary Reading)
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   The standing arrangement under contest is the
 *   compulsory-medical-intervention regime: state and employer mandates that
 *   condition employment, schooling, military service, and public access on
 *   submitting to vaccination or comparable intervention without case-by-case
 *   informed consent. This file instantiates ONE reading of the contested
 *   kernel mandate_legitimacy_scope — the bodily-autonomy-primary reading,
 *   which holds that non-consensual intervention violates fundamental bodily
 *   integrity regardless of collective benefit. The sibling readings
 *   (public_health_primary, proportionality_reading) are separate constraints
 *   in separate files with different victim sets and different epsilon; they
 *   are linked through the network, not averaged here. The claim/metric split
 *   is deliberate: the reading CLAIMS tangled_rope because it concedes the
 *   arrangement solves a real collective-action problem while denying that
 *   this legitimizes what it takes from the coerced, while the authored
 *   metrics describe the arrangement's actual operation from this reading's
 *   seat. Time axis maps approximately t = 1998 + t: t0 is the
 *   exemption-broad era, t18 the first exemption-tightening wave, t24 the
 *   pandemic mandate peak, t27 the partial retrenchment.
 *
 * KEY AGENTS:
 *   - - state_public_health_authorities: Agenda-setter and institutional beneficiary (institutional/constrained) — drafts and enforces the mandate instruments, collects compliance, budget, and doctrinal authority
 *   - - unvaccinated_coerced_individuals: Primary target (powerless/trapped) — bears the non-consensual intervention and the exclusion penalties attached to refusal
 *   - - immunocompromised_and_elderly: Protected beneficiary (moderate/constrained) — receives the coverage externality they cannot produce by their own action
 *   - - healthcare_workers_under_mandates: Dual-positioned payer-beneficiary (organized/constrained) — bound by the same instruments they administer; protected in the wards they staff
 *   - - religious_conscientious_objectors: Identity-locked payer (moderate/generational) — refusal is constitutive of their self-concept; compliance is not a live option
 *   - - civil_liberties_bioethics_observers: Analytical observer (organized/analytical) — litigates, publishes, and shapes precedent without bearing compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.78).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.72).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Compulsory Medical Intervention Without Case-by-Case Consent (Bodily-Autonomy-Primary Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, '967a0f14-ead0-486d-a90f-c1e9436f8199').
narrative_ontology:cs_kernel_codification('967a0f14-ead0-486d-a90f-c1e9436f8199', formalized).
narrative_ontology:cs_authority_grounding('967a0f14-ead0-486d-a90f-c1e9436f8199', lineage).
narrative_ontology:cs_interpretation_layer_present('967a0f14-ead0-486d-a90f-c1e9436f8199').
narrative_ontology:cs_reading_relation('967a0f14-ead0-486d-a90f-c1e9436f8199', mandate_legitimacy_scope__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('967a0f14-ead0-486d-a90f-c1e9436f8199', mandate_legitimacy_scope__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('967a0f14-ead0-486d-a90f-c1e9436f8199', foundational, bodily_integrity_categorical_against_collective_override).
narrative_ontology:cs_axiom_status(bodily_integrity_categorical_against_collective_override, holdable).
narrative_ontology:cs_axiom_grounding('967a0f14-ead0-486d-a90f-c1e9436f8199', bodily_integrity_categorical_against_collective_override, deontological).
narrative_ontology:cs_axiom('967a0f14-ead0-486d-a90f-c1e9436f8199', secondary, informed_consent_precondition_of_legitimate_intervention).
narrative_ontology:cs_axiom_status(informed_consent_precondition_of_legitimate_intervention, holdable).
narrative_ontology:cs_axiom_grounding('967a0f14-ead0-486d-a90f-c1e9436f8199', informed_consent_precondition_of_legitimate_intervention, deontological).
narrative_ontology:cs_reference_frame('967a0f14-ead0-486d-a90f-c1e9436f8199', inviolable_bodily_sovereignty).
narrative_ontology:cs_drift_state('967a0f14-ead0-486d-a90f-c1e9436f8199', contemporary_post_jacobson_expansion, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('967a0f14-ead0-486d-a90f-c1e9436f8199', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, immunocompromised_and_elderly).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_authorities).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, healthcare_workers_under_mandates).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, healthcare_workers_under_mandates).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, religious_conscientious_objectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts the order instruments, sets school-entry and employment conditions, and runs the enforcement and exemption-administration machinery. Collects compliance statistics, budget appropriations, and the doctrinal authority that comes with operating the instruments. Its own movement is bounded by statute and judicial review; it cannot simply abandon the authority it has accumulated, and legislatures can strip or expand it.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_authorities, beneficiary).

% Face termination, school exclusion, fines, or denial of venues and services unless they accept an injection they have not consented to case-by-case. Exemption channels are narrow and discretionarily administered. Relocation to a different jurisdiction is the main escape and carries heavy personal cost; within their own life circumstances the choice set is submit or lose.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, biographical, trapped, national).

% Depend on the surrounding population's coverage for protection many of them cannot generate through their own immune response. They receive the protective externality the instruments produce and cannot opt out of exposure to uncovered pockets. Their advocacy supports the instruments' continuation; their own bodies are rarely the site of compulsion.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, immunocompromised_and_elderly, beneficiary,
    moderate, biographical, constrained, national).

% Bound by employer and state conditions of practice while simultaneously staffing the facilities where coverage protects them. Unions bargain over exemption processes and disciplinary timelines. Leaving the profession forfeits career-specific training and licensure investment, so the practical options are comply, fight through grievance and litigation, or absorb exclusion.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, healthcare_workers_under_mandates, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__bodily_autonomy_primary, healthcare_workers_under_mandates, beneficiary).

% Refusal is constitutive of a religious identity passed across generations; compliance would violate the community's self-concept rather than merely its preferences. They bear exclusion, litigation costs, and social sanction, and some maintain closed communities that reduce exposure at the price of economic integration. Exit in the ordinary sense would mean abandoning the identity itself.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, religious_conscientious_objectors, payer,
    moderate, generational, identity_locked, regional).

% Scholars, public-interest litigators, and ethics boards who document consent doctrine, file challenges, and publish analyses of the instruments' justification structure. They bear no compliance burden themselves and shape the precedent environment over decades. Their leverage is argument and adjudication, not administration.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, civil_liberties_bioethics_observers, observer,
    organized, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_authorities).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in infectious-disease control: individual vaccination decisions generate externalities, and voluntary uptake stalls below the coverage threshold at which transmission chains break. Compulsory instruments push coverage past that threshold centrally instead of relying on the aggregate of private decisions.
% TRANSFER_FUNCTION: Moves bodily decision authority over a medical intervention from individuals to state public-health authorities, along with compliance costs and the risk of employment, education, and venue exclusion; moves the resulting protective externality to vulnerable populations and gives institutions predictable staffing and attendance.
% ABSENT_VOICES: The coerced themselves are thinly represented where the instruments are designed: emergency-rule processes and employer directives proceed without the negotiated agreement of those bound by them, exemption hearings place objectors in an adversarial posture rather than a deliberative one, and future persons whose bodily-integrity precedent is being set have no seat at all.
% DISAPPEARANCE_RATIONALE: School attendance rules, hospital staffing rosters, military readiness policy, and disease-transmission dynamics all reorganize immediately; coverage would drift toward the voluntary equilibrium, outbreak risk profiles shift, and the exemption-and-litigation apparatus built around the instruments loses its object within a season.
% FOUNDING_PROBLEM: Recurring epidemic disease killing at scales voluntary uptake could not prevent — the smallpox-era compulsory-vaccination laws and the Jacobson v. Massachusetts line (1905) were built for dense cities where transmission outran private decision-making.
% FOUNDING_PROBLEM_CORROBORATION: Historical mortality records and the independent epidemiological literature corroborate that the founding problem was live at codification. Contemporary status is disputed across seats: public-health authorities attest recurring pandemic-scale threats; civil-liberties scholarship and dissenting opinions in modern mandate cases attest that per-disease justification has weakened while the instruments persist unchanged. Courts occupy the middle, upholding the authority in principle while narrowing its application — corroboration exists for the founding moment, and the present status is genuinely fought over by parties outside the beneficiary set.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.78, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is high (0.78 at interval end) because, from this reading's seat, the arrangement takes bodily decision authority categorically: every operated mandate instance transfers a consent decision from the individual to the state, and the size of the collective benefit does not enter the ledger. Suppression (0.72) is structural, not internalized — it consists of legal penalties, employment and education exclusion, and venue denial enforced by institutions; nothing here depends on the target's beliefs about deserving the treatment. Accessibility collapse is moderate (0.45): exemption channels exist though they narrowed over the interval, and jurisdictional relocation remains a costly but real exit, so alternatives are degraded rather than eliminated. Resistance is high (0.70): litigation waves, legislative repeal campaigns, exemption surges, and organized parent and worker coalitions — the victim class has repeatedly found coalition leverage despite individually weak positions. Theater ratio (0.38) is elevated and rising across the series: mandates continued past the epidemiological windows that justified them, and a growing share of enforcement activity defends the instrument itself rather than tracked transmission reduction. The temporal series runs on one shared grid (ten points, all three metrics at every point) and shows a crisis-ratchet cycle rather than monotonic drift: long plateaus, an emergency spike (t21-t24), partial retreat (t27) — but each trough sits above the previous plateau, so the oscillation functions as intermittent reinforcement: every emergency re-normalizes a higher baseline of compelled intervention. The base_properties scalars report the end-of-interval state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the arrangement is a functioning coordination achievement it built and defends; from the trapped payer seat the identical instrument operates as a categorical override of bodily self-determination. The beneficiary seat experiences protection with little direct imposition. Two same-level differentiations matter: healthcare workers and religious objectors hold similar nominal civic standing but diverge sharply on exit — the worker faces career-specific capital loss (constrained), the objector faces self-dissolution (identity_locked), so the same penalty schedule lands with very different effective weight. Inter-institutionally, public-health agencies, professional medical bodies, and labor unions all touch the same instruments with opposite valences: the agencies issue them, the professional bodies endorse them, the unions negotiate their exemptions. If the religious objectors' identity frame broke — doctrinal reinterpretation admitting compliance — their effective position would converge toward the ordinary constrained payer, and the measured resistance profile would drop accordingly.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. Unvaccinated coerced individuals sit near the full-target end: they bear the intervention directly and their exit is trapped, which amplifies their effective position further. Religious conscientious objectors share the target position with identity lock amplifying it beyond the mobile-objector baseline. State public health authorities sit near the beneficiary end — they collect compliance and authority and run the machinery. Immunocompromised and elderly beneficiaries receive a genuine protective externality at low personal imposition, placing them well toward the subsidized end, though their benefit is diffuse and contingent on everyone else's compliance. Healthcare workers net toward the target side: the mandate binds them personally while the workplace-protection benefit accrues partly independently of their own compliance. No directionality overrides were needed — the beneficiary/victim structure plus exit options reproduce these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — epidemic disease killing at scales voluntary uptake could not prevent — was live at codification and is now contested per-disease: the instruments built for smallpox-era lethality now apply across a severity spectrum the founders never faced. The mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): arrangements still load-bearing, justification no longer uniform — no zombie flag yet, but the theater series is the early-warning channel, and the post_crisis_persistence_theater omega names the trigger. The classification discipline cuts both ways: calling the whole arrangement pure extraction erases the real coordination function (coverage externalities the voluntary equilibrium cannot reach), while calling it pure coordination erases the categorical imposition this reading identifies. Tangled rope holds both facts in one structure without letting either launder the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the mandate_legitimacy_scope kernel governs the standing arrangement — this bodily-autonomy-primary reading, the public-health-primary reading, or the proportionality reading?',
    'Constitutional adjudication and democratic deliberation that force the readings into explicit conflict; comparative observation of jurisdictions that have adopted each reading.',
    'Under public_health_primary the victim set collapses to mandates lacking serious-harm necessity and epsilon falls sharply; under proportionality_reading the victim set becomes conditionally defined and epsilon tracks disease severity; under this reading the victim set includes every coerced individual whenever mandates operate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is one reading of the mandate_legitimacy_scope kernel; sibling readings instantiate different victim sets and different epsilon over the same standing arrangement.').

omega_variable(
    epsilon_referent_is_standing_arrangement,
    'Is epsilon authored over the standing mandate arrangement as this reading sees it, rather than over the consent-based alternative this reading endorses?',
    'Authoring audit against the fixed-referent rule: the referent is the arrangement under contest; the reading''s endorsed alternative appears nowhere in the metric.',
    'If the referent drifted to the endorsed alternative, epsilon would collapse toward zero for every advocacy reading and cross-reading comparability across the kernel family would be destroyed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_is_standing_arrangement, conceptual, 'Fixes the epsilon referent to the existing compulsory-intervention regime, not the reading''s preferred consent regime.').

omega_variable(
    categorical_axiom_severity_limit,
    'Does the categorical premise — no collective benefit overrides consent — hold across the full pathogen-severity spectrum, or do this reading''s own adherents concede threshold cases such as near-certain-lethality exposures?',
    'Survey of adherent positions across severity gradients; analysis of whether the reading''s own tradition contains internal carve-outs such as accepted emergency-powers doctrines.',
    'If a severity threshold exists even within the reading, the axiom is gradient rather than categorical and the reading converges structurally toward the proportionality reading, shrinking the victim set at the margins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_axiom_severity_limit, preference, 'Whether the foundational axiom is truly categorical or conceals a severity-dependent concession.').

omega_variable(
    post_crisis_persistence_theater,
    'Do mandate instruments persist after the epidemiological justification that triggered them has faded, and is the residual enforcement activity functional or performative?',
    'Compare mandate continuation against transmission and severity data after each crisis window; track enforcement actions that cite justifications already expired.',
    'Sustained persistence with faded justification drives theater_ratio upward and pushes the arrangement toward inertial drift; rapid sunset after crises would support the coordination-function reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_crisis_persistence_theater, empirical, 'Whether the visible rise in performative maintenance after crisis peaks marks the start of atrophy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(mand_tr_t3, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 3, 0.18).
narrative_ontology:measurement(mand_tr_t6, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 6, 0.2).
narrative_ontology:measurement(mand_tr_t9, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 9, 0.2).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 12, 0.22).
narrative_ontology:measurement(mand_tr_t15, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 15, 0.24).
narrative_ontology:measurement(mand_tr_t18, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 18, 0.28).
narrative_ontology:measurement(mand_tr_t21, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 21, 0.35).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 24, 0.42).
narrative_ontology:measurement(mand_tr_t27, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 27, 0.38).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(mand_be_t3, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 3, 0.61).
narrative_ontology:measurement(mand_be_t6, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 6, 0.59).
narrative_ontology:measurement(mand_be_t9, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 9, 0.57).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(mand_be_t15, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(mand_be_t18, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 18, 0.7).
narrative_ontology:measurement(mand_be_t21, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 21, 0.84).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 24, 0.88).
narrative_ontology:measurement(mand_be_t27, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 27, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mand_su_t3, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 3, 0.56).
narrative_ontology:measurement(mand_su_t6, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 6, 0.54).
narrative_ontology:measurement(mand_su_t9, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 9, 0.52).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(mand_su_t15, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(mand_su_t18, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 18, 0.66).
narrative_ontology:measurement(mand_su_t21, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 21, 0.8).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 24, 0.84).
narrative_ontology:measurement(mand_su_t27, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 27, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'are medical mandates legitimate?' decomposes into three structurally distinct constraints sharing the mandate_legitimacy_scope kernel. This bodily-autonomy-primary reading carries a victim set of every coerced individual whenever mandates operate and high epsilon from mandate presence alone. The public_health_primary sibling carries a victim set limited to mandates lacking serious-harm necessity. The proportionality sibling defines its victim set conditionally through severity, safety, and less-restrictive-alternatives tests. Epsilon differs across the family because the readings differ, not because any observable was switched mid-constraint — each member is separately epsilon-invariant. Historically the public_health_primary reading is upstream: it supplied the doctrinal resources (the police-power lineage) that the other two readings accept, balance, or repudiate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
