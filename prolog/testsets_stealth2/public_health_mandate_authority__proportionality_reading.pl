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
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Public Health Mandate Authority — Proportionality Reading
 *   domain: public health law/constitutional rights/bioethics
 *
 * SUMMARY:
 *   A public health mandate regime — compulsory vaccination, testing,
 *   masking, and quarantine rules enforced through employment termination,
 *   venue exclusion, and fines — stands as the arrangement under contest.
 *   Under the proportionality reading instantiated here, the regime's
 *   legitimacy is real but always conditional: severity of threat,
 *   availability of less coercive alternatives, magnitude of coercion, and
 *   duration of imposition must balance for any given mandate to stand.
 *   Assessed by that scale's own lights, the standing regime is a hybrid:
 *   during acute threat it delivers genuine collective protection, while
 *   across the arc of a pandemic it persistently fails the scale — mandates
 *   outlive the threat levels that justified them, coercion escalates rather
 *   than seeking the least restrictive means, and alternatives are excluded
 *   rather than weighed. The victim boundary is dynamic: the unvaccinated
 *   carry the regime's burdens throughout, while the immunocompromised are
 *   protected when the regime is proportionate and exposed when it
 *   substitutes blunt universalism for targeted protection. This story is one
 *   reading of the public_health_mandate_authority kernel; the sibling
 *   readings are separate constraint stories (see kernel_context). KEY AGENTS
 *   (by structural relationship): - public_health_agencies: Primary
 *   agenda-setter (institutional/arbitrage) — administers mandates, controls
 *   the threat assessment that sets the severity factor, collects authority
 *   and crisis budget - unvaccinated_workers: Primary target
 *   (moderate/constrained) — bears termination, exclusion, and stigma -
 *   immunocompromised_individuals: Dual-positioned beneficiary/target
 *   (organized/trapped) — protected when proportionate, exposed when not -
 *   healthcare_system_operators: Secondary beneficiary
 *   (institutional/constrained) — gains capacity stability, carries staffing
 *   losses - vaccine_manufacturers: Secondary beneficiary
 *   (institutional/arbitrage) — compelled demand under liability shields -
 *   state_legislatures: Excluded would-be duration-limiter
 *   (institutional/constrained) - reviewing_courts: Adjudicating observer
 *   (institutional/analytical) - bioethicists: Analytical observer
 *   (analytical/analytical) — authors of the scale itself
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda-setter (institutional/arbitrage) — administers the regime and controls threat assessment
 *   - unvaccinated_workers: primary target (moderate/constrained) — bears the regime's coercion
 *   - immunocompromised_individuals: dual-positioned beneficiary/target (organized/trapped) — the dynamic victim boundary made flesh
 *   - healthcare_system_operators: secondary beneficiary (institutional/constrained) — capacity gains and staffing losses
 *   - vaccine_manufacturers: secondary beneficiary (institutional/arbitrage) — compelled demand, liability shields
 *   - state_legislatures: excluded (institutional/constrained) — bypassed police of the duration factor
 *   - reviewing_courts: observer (institutional/analytical) — case-by-case proportionality adjudication
 *   - bioethicists: observer (analytical/analytical) — authors of the sliding scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.58).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.55).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority — Proportionality Reading").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public health law/constitutional rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '14823ccb-043a-4f1c-b33d-382eac64753b').
narrative_ontology:cs_kernel_codification('14823ccb-043a-4f1c-b33d-382eac64753b', formalized).
narrative_ontology:cs_authority_grounding('14823ccb-043a-4f1c-b33d-382eac64753b', expertise).
narrative_ontology:cs_interpretation_layer_present('14823ccb-043a-4f1c-b33d-382eac64753b').
narrative_ontology:cs_reading_relation('14823ccb-043a-4f1c-b33d-382eac64753b', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('14823ccb-043a-4f1c-b33d-382eac64753b', public_health_mandate_authority__public_health_primary, influences).
narrative_ontology:cs_axiom('14823ccb-043a-4f1c-b33d-382eac64753b', foundational, mandate_legitimacy_requires_proportionality).
narrative_ontology:cs_axiom_status(mandate_legitimacy_requires_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('14823ccb-043a-4f1c-b33d-382eac64753b', mandate_legitimacy_requires_proportionality, deontological).
narrative_ontology:cs_axiom('14823ccb-043a-4f1c-b33d-382eac64753b', secondary, least_restrictive_means_required).
narrative_ontology:cs_axiom_status(least_restrictive_means_required, holdable).
narrative_ontology:cs_axiom_grounding('14823ccb-043a-4f1c-b33d-382eac64753b', least_restrictive_means_required, instrumental).
narrative_ontology:cs_reference_frame('14823ccb-043a-4f1c-b33d-382eac64753b', proportionality_conditioned_police_power).
narrative_ontology:cs_drift_state('14823ccb-043a-4f1c-b33d-382eac64753b', post_acute_pandemic_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('14823ccb-043a-4f1c-b33d-382eac64753b', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, healthcare_system_operators).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, vaccine_manufacturers).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, unvaccinated_workers).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, immunocompromised_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, healthcare_system_operators).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__proportionality_reading, proportionality_doctrine).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__proportionality_reading, least_restrictive_means_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue, extend, and rescind mandates; run the threat assessments that set the severity factor; enforce through workplace rules, venue access, and fines. Crisis operation brings emergency authority, budget, and staffing. They can shift mandates across domains (workplace, travel, schools) when one is struck down, and they bear political blowback and litigation costs when impositions overshoot.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Cannot be fully vaccinated and depend on population-level protection and functioning hospital capacity. When mandates hold transmission down they gain real protection; when mandates persist past usefulness they instead carry the residual risk of measures that no longer work and that crowd out the targeted protections (fit-tested respirators, ventilation, visitor screening) their care depends on. They cannot exit their vulnerability.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_individuals, beneficiary,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__proportionality_reading, immunocompromised_individuals, payer).

% Face termination, recurring testing regimes, and exclusion from venues and employment for declining vaccination. Changing employers rarely escapes the rules, which span industries and jurisdictions; compliance against conscience or medical judgment is the main exit. Some organize through litigation networks and advocacy groups, but each individual bears the burden alone.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, unvaccinated_workers, payer,
    moderate, biographical, constrained, national).

% Run hospitals and clinics. Mandates protect staffing levels and patient flow during surges, a direct operational gain. The same mandates drive resignations and recruitment difficulty in tight labor markets, and operators must administer and defend the rules. They cannot opt out of the regime; it is their delivery vehicle.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, healthcare_system_operators, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__proportionality_reading, healthcare_system_operators, payer).

% Sell vaccines whose demand is guaranteed and expanded by mandates, under liability shields that cap downside. Revenue scales with mandate breadth; they do not administer the rules and carry little of the controversy.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, vaccine_manufacturers, beneficiary,
    institutional, biographical, arbitrage, global).

% Hold the ordinary power to set duration limits, sunset clauses, and emergency-power boundaries. During the acute phase they were bypassed by executive emergency authority; attempts to bar mandates were sometimes preempted or vetoed. They would police the duration factor of the legitimacy scale if seated in the assessment.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, state_legislatures, excluded,
    institutional, biographical, constrained, national).

% Hear challenges to specific mandates and apply deferential review during acute phases, stricter scrutiny as threat recedes. They have not adopted a systematic sliding-scale doctrine; rulings arrive case-by-case, after the fact, and do not bind the initial assessment.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, reviewing_courts, observer,
    institutional, generational, analytical, national).

% Develop and publish the proportionality framework — severity, alternatives, coercion magnitude, duration — and critique specific mandates against it. They hold no enforcement power; their influence runs through courts, agencies, and public argument.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, bioethicists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__proportionality_reading, public_health_agencies).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the infectious-disease collective-action problem: individual vaccination and compliance decisions underproduce population-level protection, exposing people who cannot be vaccinated and straining healthcare capacity. Mandates internalize that externality by compelling contribution to the protection commons.
% TRANSFER_FUNCTION: Moves bodily autonomy and employment security from noncompliant individuals to the collective protection commons and to administering institutions; concentrates compliance costs (termination, exclusion, stigma) on a minority; moves revenue to vaccine manufacturers through compelled demand; moves authority and crisis budget to public health agencies.
% ABSENT_VOICES: Workers facing termination had no hearing on whether their specific imposition was proportionate — the scale was applied by the same agencies that imposed, without adversarial process. State legislatures, the natural police of the duration factor, were bypassed by executive emergency authority. Immunocompromised patients had no seat when blunt universal mandates crowded out the targeted protection their survival depends on.
% DISAPPEARANCE_RATIONALE: Employment law, healthcare staffing policy, emergency-power doctrine, and vaccine procurement all organize around the mandate regime. Overnight removal would reorganize workplace rules, reopen exemption litigation, force healthcare systems onto voluntary and targeted protection strategies, and strip agencies of standing emergency authority — the world rearranges.
% FOUNDING_PROBLEM: Epidemic free-riding: in outbreaks, voluntary compliance underproduces herd protection, exposing those who cannot be vaccinated and overwhelming healthcare capacity. The arrangement was built to compel contribution when persuasion fails.
% FOUNDING_PROBLEM_CORROBORATION: Hospital administrators and epidemiologists outside the benefiting parties attest the free-riding problem is live (outbreak-driven capacity strain, transmission from under-protected pockets). Courts in the Jacobson lineage attest the problem's reality while disputing the permissible scope of response. The bioethics literature, independent of the agencies, corroborates both the problem and the proportionality limits on any response.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__proportionality_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness (0.58) is authored for the standing regime across the full arc as the proportionality scale assesses it: near the coordination floor at peak threat (T6, 0.30 — severe threat, no alternatives, coercion justified), rising steeply as threat receded while mandates persisted and coercion escalated (T24 peak, 0.72), settling at a residual 0.58 with disproportionate mandates still operating in pockets. Suppression (0.55) is authored as a raw structural property — termination, exclusion, fines — and is NOT scaled by power or scope in this authoring; only extractiveness is scaled, by the engine. Theater (0.40) reflects ritualized enforcement past function in the late arc (passes in low-transmission settings) against real crisis function early. Accessibility collapse (0.50): alternatives — testing, masking, remote work, targeted protection of the vulnerable — remained visible but were excluded by flat rules rather than weighed, so alternatives collapse only partly. Resistance (0.70): litigation, protests, legislative bans, and noncompliance met the regime continuously. All three tracked metric series run on ONE shared grid (T0–T42, eight points, every metric at every point) so the engine samples without scalar substitution. The arc is crisis-shaped rather than monotonic: escalation, peak enforcement, decay, residual. The oscillation is partly a burden-accumulation mechanism — each emergency cycle leaves residual mandates, so the next cycle's trough starts higher; this is documented in the duration_ratchet omega.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda-setter seat should compute different types from the same structural data. From unvaccinated_workers (moderate power, constrained exit), the regime operates as coerced bodily intervention backed by job loss — high effective burden. From public_health_agencies (institutional power, arbitrage exit), the same regime is crisis governance it controls and is resourced by — low effective burden. The immunocompromised seat is genuinely dual: protected commons-beneficiary when the scale is satisfied, abandoned payer when blunt mandates substitute for targeted protection. Healthcare operators and manufacturers collect without running the scale. The engine computes these per-seat classifications from role, power, and exit data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (immunocompromised_individuals, healthcare_system_operators, public_health_agencies, vaccine_manufacturers) drive d toward the beneficiary end for those seats; victim declarations (unvaccinated_workers, and immunocompromised_individuals secondarily) drive d toward the target end. The dual declaration for immunocompromised_individuals — listed in BOTH arrays — is what places that seat mid-scale rather than at an endpoint, encoding the reading's dynamic victim boundary. Exit modulates within that: trapped (immunocompromised individuals cannot exit their vulnerability) pushes their d up from pure beneficiary; constrained (unvaccinated workers can change jobs but not exit the regime's span) keeps them near full target; arbitrage (agencies re-impose across domains; manufacturers reprice) holds those beneficiaries near the subsidy end. No directionality overrides are used — the beneficiary/victim declarations plus exit options produce the intended d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — epidemic free-riding — is live, so this is not a resolved mandatrophy; the regime's core crisis function remains real. But the regime exhibits partial, application-relative mandatrophy: broad mandates persisting past threat are components operating past their proportionate function, maintained by institutional inertia and asymmetric agency incentives (overprediction is costless to the agency, underprediction catastrophic) rather than by the scale the reading demands. Classifying the regime as tangled_rope prevents both mislabelings: a pure-extraction verdict would erase the genuine coordination delivered at peak threat and the immunocompromised's real protection; a pure-coordination verdict would erase the burden that accumulates whenever duration, coercion, or alternatives fail the scale. Because the victim boundary is dynamic, mandatrophy is indexed to the threat regime: the same instrument that functions as coordination at T6 accumulates unjustified burden by T24. The mismatch consumer should read founding_problem_status=live against the partial ratchet, not against full obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_assessment_capture,
    'Does the severity factor of the sliding scale track independent epidemiological assessment, or the assessing agency''s institutional incentives?',
    'Compare agency threat claims at imposition time with retrospective seroprevalence and severity data; audit forecast calibration across jurisdictions.',
    'Systematic overstatement of severity collapses proportionality across most applications and pushes the regime toward pure extraction; accurate assessment leaves much of the regime standing as genuine conditional coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_assessment_capture, empirical, 'Whether the scale''s first factor is honestly measured or captured by the assessor''s incentives.').

omega_variable(
    victim_boundary_dynamism,
    'Are the immunocompromised and the unvaccinated victims of this constraint, or only of particular disproportionate applications of it?',
    'Per-application proportionality review with published factor assessments would localize burden status to specific impositions rather than to the regime as a whole.',
    'If burden status is application-indexed, the constraint has no fixed victim boundary and classification must be recomputed per threat regime; a fixed-boundary verdict would push toward pure extraction (for the unvaccinated) or near-pure coordination (for the immunocompromised).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_boundary_dynamism, conceptual, 'Whether the victim set is a property of the constraint or of each application under the sliding scale.').

omega_variable(
    duration_ratchet,
    'Does the regime''s duration track threat duration, or does each emergency declaration leave permanent residual mandates?',
    'Inventory the mandate set at emergency declaration and at 24 months post-declaration across jurisdictions; measure the decay rate of residual rules.',
    'Ratchet dynamics would push classification toward pure extraction across repeated cycles and confirm the duration factor as the binding failure of the standing arrangement; full decay would support the conditional-coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duration_ratchet, empirical, 'Whether each crisis cycle permanently enlarges the residual mandate stock.').

omega_variable(
    compliance_internalization,
    'Is the measured suppression structural (termination, exclusion, fines) or internalized (moralized stigma that makes refusal socially untenable even where rules lapse)?',
    'Post-lift behavior: if vaccination-rate gains and compliance norms persist after enforcement removal, part of the suppression is internalized rather than structural.',
    'Internalized suppression raises effective suppression above the structural measure and persists after exit from the mandate''s scope; purely structural suppression decays with enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_internalization, empirical, 'Structural vs. internalized mechanism behind the measured suppression.').

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the proportionality_reading of the public_health_mandate_authority kernel — what would each sibling reading change structurally, and where exactly is the disagreement located?',
    'The sibling readings are separate constraint stories (public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__public_health_primary); compare their authored epsilon, victim sets, and axioms against this story''s.',
    'bodily_autonomy_primary would set extraction near maximal with a categorical victim set (any non-consensual intervention, regardless of threat level); public_health_primary would set extraction near the coordination floor with no victim set (mandates as obligation). The disagreement is located in whether collective benefit may enter the legitimacy calculus at all, and whether individual refusal may be overridden without factor-by-factor weighing. This story''s epsilon and dynamic victim boundary are valid only under the proportionality reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints with different epsilon and victim sets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__proportionality_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__proportionality_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__proportionality_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__proportionality_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__proportionality_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__proportionality_reading, theater_ratio, 36, 0.44).
narrative_ontology:measurement(publ_tr_t42, public_health_mandate_authority__proportionality_reading, theater_ratio, 42, 0.4).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__proportionality_reading, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__proportionality_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__proportionality_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__proportionality_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__proportionality_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__proportionality_reading, base_extractiveness, 36, 0.61).
narrative_ontology:measurement(publ_be_t42, public_health_mandate_authority__proportionality_reading, base_extractiveness, 42, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__proportionality_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__proportionality_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__proportionality_reading, suppression_requirement, 18, 0.74).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__proportionality_reading, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__proportionality_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__proportionality_reading, suppression_requirement, 36, 0.58).
narrative_ontology:measurement(publ_su_t42, public_health_mandate_authority__proportionality_reading, suppression_requirement, 42, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__public_health_primary).

% DUAL FORMULATION NOTE:
% The public_health_mandate_authority kernel decomposes into three readings with different victim boundaries and epsilon assessments: this proportionality_reading (conditional legitimacy, dynamic victim set, epsilon indexed to threat level), bodily_autonomy_primary (categorical victim set, maximal extraction claim), and public_health_primary (no victim set, extraction at the coordination floor). The siblings are separate constraint stories linked here; this story's epsilon is authored only for the standing regime under the sliding scale's lights, and the family link carries the decomposition relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
