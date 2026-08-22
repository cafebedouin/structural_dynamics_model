% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__proportionality_reading, []).

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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Proportionality Reading of Health Intervention Legitimacy
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the legitimate_health_intervention
 *   kernel: the proportionality reading, under which state health coercion is
 *   legitimate exactly when intervention severity tracks threat magnitude,
 *   with both population harm and individual autonomy weighted by disease
 *   characteristics. The standing arrangement under contest — and therefore
 *   the epsilon referent — is the proportionality-governed intervention
 *   regime as it actually operates: severity declarations, emergency orders,
 *   mandates, quarantines, closures, judicial review, and the administrative
 *   apparatus that activates and deactivates around epidemic waves. The
 *   reading's endorsed alternative (a perfectly calibrated regime) is NOT the
 *   referent. The claim/metric gap is deliberate: the reading is CLAIMED as
 *   tangled_rope because the same structure that performs genuine calibration
 *   coordination also episodically extracts from concentrated seats, while
 *   the authored metrics describe the observed oscillating operation
 *   including the post-wave ratchet. The structural signature of this reading
 *   is CONDITIONALITY: the victim set is empty at low severity (flu-season
 *   baseline), activates during high-severity episodes (measles outbreaks,
 *   pandemic waves), and scales with transmissibility and case-fatality rate
 *   — this enters the data through the temporal series, not through hedged
 *   epsilon, which remains a single stable value for the standing arrangement
 *   across its full operating range.
 *
 * KEY AGENTS:
 *   - public_health_authorities: Primary agenda setter (institutional/constrained) — declares severity, issues orders, accrues enforcement revenue and discretionary power; dual-positioned as beneficiary
 *   - judiciary: Co-administrator (institutional/constrained) — adjudicates proportionality challenges, trims overreach, shapes the doctrine through precedent
 *   - emergency_order_subjects: Episodic target (moderate/constrained) — bears mandated vaccination, quarantine, and exclusion burdens during activations
 *   - ordered_business_operators: Episodic economic target (moderate/constrained) — bears closure and capacity-order losses
 *   - immunocompromised_and_elderly: Protection beneficiary (moderate/trapped) — depends on calibrated interventions and others' compliance; cannot exit disease risk
 *   - autonomy_protected_individuals: Restraint beneficiary (moderate/mobile) — spared disproportionate coercion in low-threat periods; interest is portable
 *   - hourly_workers_under_quarantine: Excluded cost-bearer (powerless/trapped) — bears compliance costs without paid leave; absent from severity deliberations
 *   - scientific_advisory_bodies: Analytical observer (institutional/analytical) — supplies the severity assessments the entire weighting rides on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.46).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.49).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.49).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Proportionality Reading of Health Intervention Legitimacy").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, 'd189953b-b620-4f69-b43e-a6320f25a2b7').
narrative_ontology:cs_kernel_codification('d189953b-b620-4f69-b43e-a6320f25a2b7', formalized).
narrative_ontology:cs_authority_grounding('d189953b-b620-4f69-b43e-a6320f25a2b7', lineage).
narrative_ontology:cs_interpretation_layer_present('d189953b-b620-4f69-b43e-a6320f25a2b7').
narrative_ontology:cs_reading_relation('d189953b-b620-4f69-b43e-a6320f25a2b7', legitimate_health_intervention__public_health_primary, influences).
narrative_ontology:cs_reading_relation('d189953b-b620-4f69-b43e-a6320f25a2b7', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('d189953b-b620-4f69-b43e-a6320f25a2b7', foundational, intervention_severity_must_track_threat_magnitude).
narrative_ontology:cs_axiom_status(intervention_severity_must_track_threat_magnitude, holdable).
narrative_ontology:cs_axiom_grounding('d189953b-b620-4f69-b43e-a6320f25a2b7', intervention_severity_must_track_threat_magnitude, instrumental).
narrative_ontology:cs_axiom('d189953b-b620-4f69-b43e-a6320f25a2b7', foundational, disease_characteristics_set_legitimacy_weights).
narrative_ontology:cs_axiom_status(disease_characteristics_set_legitimacy_weights, holdable).
narrative_ontology:cs_axiom_grounding('d189953b-b620-4f69-b43e-a6320f25a2b7', disease_characteristics_set_legitimacy_weights, empirically_contingent).
narrative_ontology:cs_reference_frame('d189953b-b620-4f69-b43e-a6320f25a2b7', threat_calibrated_police_power).
narrative_ontology:cs_drift_state('d189953b-b620-4f69-b43e-a6320f25a2b7', contemporary_post_pandemic_review, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d189953b-b620-4f69-b43e-a6320f25a2b7', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, immunocompromised_and_elderly).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, autonomy_protected_individuals).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, emergency_order_subjects).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, ordered_business_operators).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, hourly_workers_under_quarantine).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, proportionality_doctrine).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, threat_calibrated_police_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declare disease severity, issue emergency orders, operate enforcement (fines, exclusion, compulsory measures), and administer the preparedness apparatus between waves. Collect enforcement revenue and expanding discretionary authority with each activation. Are bound to justify each measure as proportional to assessed threat before courts; abandoning the framework entirely would cost them the legitimacy their actions depend on.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, public_health_authorities, beneficiary).

% Review emergency orders against the proportionality standard, sustaining measures that track assessed threat and striking those that exceed it. Shape the doctrine's content through precedent across successive activation cycles. Cannot decline the adjudicative role; are bound by statute and their own prior rulings.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Individuals subject to mandatory vaccination conditions, quarantine, isolation, or exclusion from work and public spaces during declared high-severity periods. Bear bodily, liberty, and income costs concentrated in activation windows. Options are compliance, litigation after the fact, or evasion with penalty exposure; the burden arrives whether or not they participated in the severity assessment that triggered it.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, emergency_order_subjects, payer,
    moderate, biographical, constrained, national).

% Owners of venues, restaurants, and gathering places closed or capacity-limited by health orders during activations. Bear revenue loss, fixed-cost accumulation, and in some jurisdictions partial compensation. Cannot reopen in defiance of an order; their recourse is challenge proceedings that resolve slower than the orders expire.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, ordered_business_operators, payer,
    moderate, biographical, constrained, regional).

% People for whom vaccine-preventable and respiratory disease carries elevated fatality risk. Depend on community-level intervention calibrated to actual threat: strong enough to interrupt severe-disease transmission, restrained enough to remain sustainable and politically durable. Cannot exit their exposure risk; their protection depends on others' compliance and on interventions arriving when severity is real.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, immunocompromised_and_elderly, beneficiary,
    moderate, biographical, trapped, national).

% People whose bodily decision-making is left undisturbed during low-severity periods because the framework withholds coercive measures that a purely outcome-driven standard might impose. Their interest travels with them: the restraint they benefit from in one jurisdiction is the argument they can invoke in another.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, autonomy_protected_individuals, beneficiary,
    moderate, biographical, mobile, national).

% Workers without paid sick leave who comply with isolation and quarantine orders at direct wage loss. Bear among the sharpest per-person costs of any seat during activations yet have no seat in the severity deliberations, the order drafting, or the compensation design; their situation surfaces only incidentally, through advocacy intermediaries, after orders issue.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, hourly_workers_under_quarantine, excluded,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, hourly_workers_under_quarantine, payer).

% Produce the transmissibility, case-fatality, and treatability estimates the entire weighting structure consumes. Gain influence and funding from being load-bearing in legitimacy determinations. Advise but do not decide; their estimates enter a political process that can amplify, dampen, or selectively cite them.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, scientific_advisory_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, threat-calibrated standard for when health coercion is legitimate: intervention severity must track disease characteristics (transmissibility, case-fatality rate, treatability), so that responses scale with actual threat rather than with political preference or assessor habit, and so that both population protection and bodily self-determination are priced into every determination.
% TRANSFER_FUNCTION: During declared high-severity periods, moves bodily liberty and economic activity from order-subjects and business operators into state-controlled channels; moves reduced exposure risk to vulnerable populations; and moves discretionary authority and enforcement revenue to the assessing and administering agencies. Between waves, transfers are largely suspended but the retained apparatus keeps collecting budget and jurisdiction.
% ABSENT_VOICES: Hourly workers bearing uncompensated quarantine costs are absent from severity deliberations and order design; residents of under-resourced regions absorb closures without income support and without representation in the tradeoff calculus; affected communities historically entered the process only after orders issued, through litigation or press coverage.
% DISAPPEARANCE_RATIONALE: If the proportionality standard vanished overnight, legitimacy contests over health coercion would resolve by raw political force or by default to one of the rival standards: a pure outcome standard would license coercion at every nonzero threat, a pure consent standard would disable response to severe outbreaks. Emergency-order litigation pipelines, preparedness bureaucracies, and mandate regimes would all lose their governing test and reorganize around whichever successor standard captured the institutions.
% FOUNDING_PROBLEM: Reconcile the state's disease-control power with individual liberty: prevent both unchecked epidemic spread under a hands-off regime and arbitrary medical intrusion under an unconstrained police power — the problem crystallized in the early constitutional case law sustaining compelled health measures while insisting they stop short of arbitrariness.
% FOUNDING_PROBLEM_CORROBORATION: Courts outside the benefiting parties continuously attest liveness: every activation cycle generates proportionality challenges that judges decide on the merits, treating the calibration question as unresolved rather than settled. Bioethics scholarship from both the autonomy-side and the public-health-side — neither a beneficiary of the existing arrangement — documents the recurring calibration problem with each new pathogen. Legislative hearings after recent waves, convened to rebalance emergency powers, corroborate from outside the administering agencies. Health authorities also attest liveness, but the corroboration does not rest on them.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).
:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.46 at interval end) reflects an oscillating regime: quiescent-period baselines near 0.31-0.38, wave peaks at 0.55-0.62, and a visibly ratcheting floor — each cycle ends higher than the last began (0.38 at T0 vs 0.46 at T30) because emergency powers, surveillance infrastructure, and preparedness bureaucracies are only partially retired. Suppression (0.49) is the raw structural property of the enforcement machinery: fines, exclusion, and compulsory measures that activate with declarations; it is unscaled by power or scope in the engine's computation, unlike extractiveness. Theater ratio (0.47) drifts upward between waves — standing committees, plan documents, and review boards maintained performatively — and dips during genuine waves when real operations displace performance; its slow secular rise is Goodhart drift in the preparedness apparatus. Accessibility collapse (0.45): alternative legitimacy frameworks (the two sibling readings) remain live in courts, legislatures, and scholarship, and voluntary-compliance approaches persist, so alternatives are narrowed but not eliminated. Resistance (0.50): anti-mandate litigation and protest reliably intensify during activations and subside between them. CYCLICAL DYNAMICS: the series show roughly three full activation cycles on one shared 11-point time grid (every tracked metric authored at every point — no per-metric grids). The oscillation is partly the extraction mechanism itself: intermittent activation normalizes emergency authority incrementally (each wave's temporary powers leave residue), an intermittent-reinforcement pattern rather than mere external noise. Base properties were measured at T30, the post-wave elevated-baseline phase.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same events. From the public_health_authorities seat, the arrangement is a legitimate calibrated instrument they administer and from which they accrue enforcement capacity and discretionary power — a coordination structure they built and defend. From the emergency_order_subjects and ordered_business_operators seats, the same activation is experienced as sudden, concentrated imposition with truncated recourse — the episodic face of the structure. The judiciary seat experiences a manageable doctrinal test that both authorizes and limits. The hourly_workers_under_quarantine seat experiences the burden without even the procedural standing the other payer seats possess. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Immunocompromised_and_elderly and autonomy_protected_individuals sit near the beneficiary pole (d low): the first receives protection it cannot procure by exit, the second receives restraint from disproportionate coercion. Public_health_authorities are declared beneficiaries as well — they collect discretionary authority, enforcement revenue, and a legitimating framework — though their agenda_setter role means they also bear the justification burden. Emergency_order_subjects, ordered_business_operators, and hourly_workers_under_quarantine sit near the target pole (d high): they bear the concentrated episodic costs, with exit ranging from constrained (litigate, comply, relocate) to trapped (quarantined workers facing wage loss with no paid leave). Judiciary and scientific_advisory_bodies carry no beneficiary/victim declaration and fall to canonical fallbacks appropriate to their adjudicative and analytical positions. Suppression is authored as a raw structural property; only extractiveness is directionality- and scope-scaled by the engine.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling police-power disease control with individual liberty after the era of unchecked sanitary coercion — remains LIVE: epidemics recur, and the calibration question re-arises with each pathogen. This blocks a dead-mandate reading. The tangled_rope claim prevents two symmetrical mislabels: calling the arrangement a snare would erase its genuine coordination achievement (threat-calibrated legitimacy allocation that both authorizes necessary intervention and blocks disproportionate intrusion — the doctrine routinely strikes overreach as often as it sustains action); calling it a rope would erase the documented asymmetry (concentrated episodic burdens on identifiable seats, discretionary severity assessment that accrues power to the assessor, and the inter-wave ratchet that leaves each cycle's baseline elevated). The R5 mismatch consumer reads founding_problem_status=live x disappearance_verdict=world_rearranges — a matched pair producing no zombie flag; the arrangement's persistence tracks a problem that still exists, even as the ratchet omega flags where drift is accumulating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    severity_assessment_discretion,
    'Does the proportionality weighting in practice operate on disease characteristics (transmissibility, case-fatality rate, treatability), or does severity assessment collapse into assessor incentive and political convenience?',
    'Compare declared severity levels against retrospective epidemiological consensus across multiple activation episodes; measure the lag and divergence between assessed and reconstructed threat.',
    'If assessor discretion dominates, this reading behaves in operation like the public-health-primary sibling — the effective victim set expands toward refusal-as-externality and the episodic burden stops tracking disease severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_assessment_discretion, conceptual, 'Whether the weighting tracks disease parameters or assessor discretion.').

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading (proportionality_reading) of the legitimate_health_intervention kernel; how would instantiating bodily_autonomy_primary or public_health_primary instead restructure the victim set and epsilon?',
    'Generate the two sibling stories as separate constraints and compare computed per-seat classifications; the disagreement is located in the weighting premise — whether autonomy, population outcome, or calibrated tradeoff grounds legitimacy.',
    'bodily_autonomy_primary eliminates the episodic coercion burden on order-subjects but creates a new harmed set (the unprotected exposed to preventable severe disease); public_health_primary maximizes protection but converts refusers into externality-imposers with near-zero exit. The proportionality reading''s distinctive structure is the conditional, severity-indexed victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: sibling readings would move the victim set and epsilon to opposite extremes.').

omega_variable(
    emergency_power_ratchet,
    'Do emergency powers and preparedness infrastructure fully sunset between epidemic waves, or does each activation cycle leave a permanently elevated baseline of retained authority?',
    'Statutory sunset-compliance audit across successive activation cycles: inventory powers activated at each wave peak and verify which were formally retired versus administratively continued.',
    'Full sunset supports a periodically-activated coordination reading with bounded burden; persistent elevation indicates the episodic structure is accreting into a standing apparatus, shifting the long-run classification toward heavier extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_power_ratchet, empirical, 'Whether the activation cycle ratchets or fully resets.').

omega_variable(
    episodic_burden_extraction_status,
    'Is the concentrated, episodic burden borne by order-subjects during high-severity activations asymmetric extraction riding on the coordination structure, or the correctly-priced cost of the coordination itself?',
    'Test whether the burden tracks marginal contribution to harm reduction: if mandated individuals bear costs disproportionate to their causal role in transmission (e.g., blanket orders where targeted measures suffice), the excess is extraction; if burden and contribution track, it is coordination cost.',
    'Pure pricing pushes this reading toward the rope boundary; identified excess concentrated on specific seats confirms the tangled_rope classification and identifies where the asymmetry sits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(episodic_burden_extraction_status, conceptual, 'Whether the conditional burden is extraction or coordination price.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__proportionality_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t3, legitimate_health_intervention__proportionality_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement_basis(legi_tr_t3, observed).
narrative_ontology:measurement(legi_tr_t6, legitimate_health_intervention__proportionality_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement_basis(legi_tr_t6, observed).
narrative_ontology:measurement(legi_tr_t9, legitimate_health_intervention__proportionality_reading, theater_ratio, 9, 0.33).
narrative_ontology:measurement_basis(legi_tr_t9, observed).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__proportionality_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement_basis(legi_tr_t12, observed).
narrative_ontology:measurement(legi_tr_t15, legitimate_health_intervention__proportionality_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(legi_tr_t15, observed).
narrative_ontology:measurement(legi_tr_t18, legitimate_health_intervention__proportionality_reading, theater_ratio, 18, 0.39).
narrative_ontology:measurement_basis(legi_tr_t18, observed).
narrative_ontology:measurement(legi_tr_t21, legitimate_health_intervention__proportionality_reading, theater_ratio, 21, 0.27).
narrative_ontology:measurement_basis(legi_tr_t21, observed).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__proportionality_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(legi_tr_t24, observed).
narrative_ontology:measurement(legi_tr_t27, legitimate_health_intervention__proportionality_reading, theater_ratio, 27, 0.44).
narrative_ontology:measurement_basis(legi_tr_t27, observed).
narrative_ontology:measurement(legi_tr_t30, legitimate_health_intervention__proportionality_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement_basis(legi_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__proportionality_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t3, legitimate_health_intervention__proportionality_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement_basis(legi_be_t3, observed).
narrative_ontology:measurement(legi_be_t6, legitimate_health_intervention__proportionality_reading, base_extractiveness, 6, 0.34).
narrative_ontology:measurement_basis(legi_be_t6, observed).
narrative_ontology:measurement(legi_be_t9, legitimate_health_intervention__proportionality_reading, base_extractiveness, 9, 0.31).
narrative_ontology:measurement_basis(legi_be_t9, observed).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__proportionality_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(legi_be_t12, observed).
narrative_ontology:measurement(legi_be_t15, legitimate_health_intervention__proportionality_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement_basis(legi_be_t15, observed).
narrative_ontology:measurement(legi_be_t18, legitimate_health_intervention__proportionality_reading, base_extractiveness, 18, 0.36).
narrative_ontology:measurement_basis(legi_be_t18, observed).
narrative_ontology:measurement(legi_be_t21, legitimate_health_intervention__proportionality_reading, base_extractiveness, 21, 0.62).
narrative_ontology:measurement_basis(legi_be_t21, observed).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__proportionality_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement_basis(legi_be_t24, observed).
narrative_ontology:measurement(legi_be_t27, legitimate_health_intervention__proportionality_reading, base_extractiveness, 27, 0.44).
narrative_ontology:measurement_basis(legi_be_t27, observed).
narrative_ontology:measurement(legi_be_t30, legitimate_health_intervention__proportionality_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement_basis(legi_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__proportionality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t3, legitimate_health_intervention__proportionality_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement_basis(legi_su_t3, observed).
narrative_ontology:measurement(legi_su_t6, legitimate_health_intervention__proportionality_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement_basis(legi_su_t6, observed).
narrative_ontology:measurement(legi_su_t9, legitimate_health_intervention__proportionality_reading, suppression_requirement, 9, 0.33).
narrative_ontology:measurement_basis(legi_su_t9, observed).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__proportionality_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement_basis(legi_su_t12, observed).
narrative_ontology:measurement(legi_su_t15, legitimate_health_intervention__proportionality_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement_basis(legi_su_t15, observed).
narrative_ontology:measurement(legi_su_t18, legitimate_health_intervention__proportionality_reading, suppression_requirement, 18, 0.37).
narrative_ontology:measurement_basis(legi_su_t18, observed).
narrative_ontology:measurement(legi_su_t21, legitimate_health_intervention__proportionality_reading, suppression_requirement, 21, 0.72).
narrative_ontology:measurement_basis(legi_su_t21, observed).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__proportionality_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement_basis(legi_su_t24, observed).
narrative_ontology:measurement(legi_su_t27, legitimate_health_intervention__proportionality_reading, suppression_requirement, 27, 0.47).
narrative_ontology:measurement_basis(legi_su_t27, observed).
narrative_ontology:measurement(legi_su_t30, legitimate_health_intervention__proportionality_reading, suppression_requirement, 30, 0.49).
narrative_ontology:measurement_basis(legi_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legitimate health intervention' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. Each reading has a different victim set and different epsilon: bodily_autonomy_primary (order-subjects vanish as victims; the unprotected become the harmed set), this proportionality_reading (conditional, severity-indexed victim set; epsilon scales with transmissibility and case-fatality rate through the activation cycle), public_health_primary (refusers become externality-imposers; maximal protection, maximal coercion). This story links to both siblings; the upstream public_health_primary reading typically supplies the empirical severity claims this reading's weighting consumes, while the bodily_autonomy_primary reading supplies the autonomy term this reading weights. Cross-file comparison of computed per-seat classifications is the intended consumption path — do not merge the three into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
