% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__simulation_as_proxy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__simulation_as_proxy, []).

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
 *   constraint_id: competence_exercise_validity__simulation_as_proxy
 *   human_readable: Simulation-as-Proxy Competence Validity Criterion
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Across process-chemical, nuclear, aviation, and offshore domains,
 *   regulators and industry standards accept completed simulation exercises
 *   (tabletop drills, simulator scenarios, announced emergency exercises) as
 *   valid exercise of emergency-response competence. The drill functions as a
 *   proxy-catastrophe: it stands in for the rare, high-consequence event that
 *   cannot ethically or economically be staged. Under this reading of the
 *   competence-exercise-validity kernel, a passed drill validates retained
 *   competence, a clean safety record under a compliant drill regime
 *   evidences adequacy, and the compliance artifact discharges the duty to
 *   maintain readiness. Over the interval the arrangement drifted: scenarios
 *   grew scripted and announced, pass rates approached ceiling, and the
 *   program's output shifted from exercised judgment toward auditable
 *   evidence of exercise. The claim/metric gap is deliberate: the constraint
 *   is CLAIMED as tangled_rope from the authoring seat (genuine
 *   proxy-rehearsal coordination entangled with an assurance-extraction
 *   asymmetry) while the metrics are authored independently as descriptive
 *   estimates; the engine measures any divergence.
 *
 * KEY AGENTS:
 *   - safety_regulators: agenda-setter (institutional/constrained) - define what counts as valid exercise, audit compliance, collect the evidence the arrangement produces
 *   - site_senior_management: primary beneficiary (powerful/mobile) - receives documented readiness, liability shielding, and board-grade assurance without bearing drill-floor costs
 *   - training_departments: beneficiary and day-to-day administrator (organized/identity_locked) - budget and headcount depend on the program; the department has become its drill calendar
 *   - simulation_vendors: beneficiary (organized/arbitrage) - sell scenarios, simulators, and accreditation services into demand the validity criterion itself manufactures
 *   - industrial_insurers: secondary beneficiary and tail-risk payer (institutional/arbitrage) - accept compliance artifacts as underwriting evidence, absorb catastrophe losses when the proxy fails
 *   - shift_operators: primary target (moderate/constrained) - surrender shift time to drills of declining marginal fidelity; carry the gap between drilled and real conditions
 *   - host_communities: silent target (powerless/constrained) - bear residual catastrophe risk priced by nobody at the drill-design table
 *   - dissenting_process_safety_engineers: excluded voice (moderate/mobile) - dispute simulation fidelity from inside the profession, outside the validity-setting conversation
 *   - human_factors_researchers: analytical observer (analytical/analytical) - measure transfer decay and scenario inflation; see the full structure no participant sees
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, 0.64).
domain_priors:suppression_score(competence_exercise_validity__simulation_as_proxy, 0.58).
domain_priors:theater_ratio(competence_exercise_validity__simulation_as_proxy, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, extractiveness, 0.64).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__simulation_as_proxy, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__simulation_as_proxy, "Simulation-as-Proxy Competence Validity Criterion").
narrative_ontology:topic_domain(competence_exercise_validity__simulation_as_proxy, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__simulation_as_proxy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__simulation_as_proxy, '59c8309c-365e-4a41-a5c6-a3bff3d8c562').
narrative_ontology:cs_kernel_codification('59c8309c-365e-4a41-a5c6-a3bff3d8c562', formalized).
narrative_ontology:cs_authority_grounding('59c8309c-365e-4a41-a5c6-a3bff3d8c562', expertise).
narrative_ontology:cs_interpretation_layer_present('59c8309c-365e-4a41-a5c6-a3bff3d8c562').
narrative_ontology:cs_reading_relation('59c8309c-365e-4a41-a5c6-a3bff3d8c562', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_reading_relation('59c8309c-365e-4a41-a5c6-a3bff3d8c562', competence_exercise_validity__continuous_refresh_hybrid, coexists_with).
narrative_ontology:cs_axiom('59c8309c-365e-4a41-a5c6-a3bff3d8c562', foundational, simulation_transfer_validates_competence).
narrative_ontology:cs_axiom_status(simulation_transfer_validates_competence, holdable).
narrative_ontology:cs_axiom_grounding('59c8309c-365e-4a41-a5c6-a3bff3d8c562', simulation_transfer_validates_competence, empirically_contingent).
narrative_ontology:cs_axiom('59c8309c-365e-4a41-a5c6-a3bff3d8c562', foundational, clean_record_evidences_readiness).
narrative_ontology:cs_axiom_status(clean_record_evidences_readiness, holdable).
narrative_ontology:cs_axiom_grounding('59c8309c-365e-4a41-a5c6-a3bff3d8c562', clean_record_evidences_readiness, empirically_contingent).
narrative_ontology:cs_axiom('59c8309c-365e-4a41-a5c6-a3bff3d8c562', secondary, compliance_artifact_discharges_competence_duty).
narrative_ontology:cs_axiom_status(compliance_artifact_discharges_competence_duty, holdable).
narrative_ontology:cs_axiom_grounding('59c8309c-365e-4a41-a5c6-a3bff3d8c562', compliance_artifact_discharges_competence_duty, conventional).
narrative_ontology:cs_reference_frame('59c8309c-365e-4a41-a5c6-a3bff3d8c562', validated_simulation_baseline).
narrative_ontology:cs_drift_state('59c8309c-365e-4a41-a5c6-a3bff3d8c562', contemporary_post_incident_review_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('59c8309c-365e-4a41-a5c6-a3bff3d8c562', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, training_departments).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, simulation_vendors).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, site_senior_management).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, safety_regulators).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, shift_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, host_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, industrial_insurers).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, site_senior_management).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, industrial_insurers).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, simulation_transfer_hypothesis).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, compliance_record_adequacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and revise the standards that define what counts as a valid competence exercise, accredit training providers, and audit sites against the exercise records those standards generate. Collect the resulting compliance evidence as the basis for license decisions. When a site with a clean drill record suffers a real event, the agency owns the public failure alongside the operator. Leaving the role means leaving the mandate; the agency is bound to its statute and budget cycle.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Fund the drill program and sign off on its outputs. Receive documented readiness: closed audits, board assurances, and liability positions strengthened by a paper trail of exercised crews. The costs they bear are budget lines; the costs of a real event exceeding the drilled scenarios land on the crews and the neighbors, not on the sign-off. Career paths run through multiple facilities and firms.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, site_senior_management, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__simulation_as_proxy, site_senior_management, payer).

% Own the drill calendar, design scenarios, schedule crews, and produce the completion records. Department headcount and budget scale with the volume of mandated exercises. The department's purpose has become the program; its staff identify with drill delivery itself. Exiting would mean dismantling the unit's reason to exist.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, training_departments, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__simulation_as_proxy, training_departments, agenda_setter).

% Sell scenario libraries, simulator hardware, instructor certification, and audit-preparation services to sites required to run exercises. Demand for the product is created by the validity standards themselves. Client bases span industries, so a downturn in one sector redirects rather than ends revenue.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, simulation_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Price premiums partly on documented exercise compliance, treating drill records as underwriting evidence. Collect premiums shaped by that evidence; pay claims when a real event exceeds what the drilled scenarios anticipated. Can reprice or withdraw from a line of business going forward; cannot withdraw from the tail once written.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, industrial_insurers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__simulation_as_proxy, industrial_insurers, payer).

% Surrender scheduled hours to drills, repeat scenarios whose outlines they can predict, and sign the completion records. Carry the difference between drilled conditions and real ones: ambiguous instrumentation, cascading failures, stress load. Declining participation is a disciplinary matter; changing trades means forfeiting licensed seniority.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, shift_operators, payer,
    moderate, biographical, constrained, regional).

% Live downwind or downstream of the facilities the drills nominally prepare. Have no role in scenario selection or exercise frequency. Bear the consequences when real events exceed drilled ones. Moving away from a facility means abandoning homes and employment anchored to it.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, host_communities, payer,
    powerless, generational, constrained, local).

% Work inside the same plants and firms, hold that current scenario fidelity misses the competencies real events punish, and say so in conference papers and internal memos. Are not seated in the committees that set validity criteria. Can change employers or publish, at professional cost.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, dissenting_process_safety_engineers, excluded,
    moderate, biographical, mobile, national).

% Study skill decay, transfer of trained skill to operational conditions, and scenario difficulty inflation. Publish findings available to all parties; hold no enforcement or budget position. Observe the whole loop of design, delivery, record, and event that no single participant sees end to end.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, human_factors_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__simulation_as_proxy, simulation_vendors).
narrative_ontology:fixing_cost_class(competence_exercise_validity__simulation_as_proxy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a repeatable, schedulable, auditable substitute for rehearsing rare high-consequence events; synchronizes exercise expectations across sites, shifts, contractors, and regulators; produces comparable records of exercise activity that license renewal and insurance underwriting can consume.
% TRANSFER_FUNCTION: Moves operator shift-hours and attention into scheduled drill performance; converts those hours into documented readiness artifacts transferred upward to management, regulators, and insurers; moves training budgets outward to internal training units and simulation vendors; leaves the residual gap between drilled and real conditions, and the catastrophe risk it implies, concentrated on operators and host communities.
% ABSENT_VOICES: Dissenting process-safety engineers who dispute simulation fidelity are inside the profession but outside the validity-setting conversation; host communities have no seat in exercise design or scenario selection; operators' structured post-drill assessments rarely feed back into validity criteria; human-factors transfer research is cited selectively rather than seated.
% DISAPPEARANCE_RATIONALE: If the 'simulation counts' criterion vanished overnight, every licensed site would need replacement competence evidence within its next audit cycle; training budgets would reflow toward whatever the successor criterion recognizes; simulation vendors would lose the demand the criterion manufactures; regulators would need to reissue validity standards; insurers would reprice underwriting evidence. The proxy-rehearsal need itself would persist, so the world would rearrange around a new answer to it rather than revert to undrilled operations.
% FOUNDING_PROBLEM: Rare, high-consequence emergencies cannot be practiced against real conditions without staging real catastrophes; after successive industrial disasters, regulators required a demonstrable, auditable means for operators to rehearse emergency response at scale.
% FOUNDING_PROBLEM_CORROBORATION: Independent accident-investigation boards and the peer-reviewed human-factors literature corroborate from outside the benefiting parties that the proxy-rehearsal need is live, and the same sources dispute this reading's sufficiency claim, documenting passed-drill/failed-event pairs. No source outside the beneficiary set attests that simulation-only validation is sufficient; that attestation comes only from the arrangement's own collectors.
narrative_ontology:disappearance_verdict(competence_exercise_validity__simulation_as_proxy, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__simulation_as_proxy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__simulation_as_proxy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_validity__simulation_as_proxy, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__simulation_as_proxy, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__simulation_as_proxy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__simulation_as_proxy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.64) is substantial but bounded: drills retain real training value, so the arrangement is not pure extraction; what accumulates is the widening spread between what drills cost operators and what the resulting assurance is worth to its collectors. Suppression (0.58) is structural first (participation is compulsory under license conditions and audit schedules, and alternative validity forms such as unannounced full-scale exercises, cross-site rotations, and apprenticeship on degraded-but-real operations are simply not recognized as counting) with an internalized residue in professional identity ('we drill, therefore we are ready'). Theater_ratio (0.52) crossed the substitution threshold late in the interval: a majority of drill activity now demonstrates compliance rather than tests capacity. Accessibility_collapse (0.45) is moderate: rival readings and higher-fidelity formats remain imaginable and occasionally piloted, but accepting 'simulation counts' largely halts the search for alternatives. Resistance (0.35) is low-to-moderate: grievance is diffuse, per-operator cost is tolerable, and union channels exist but have not made drill design a priority. The temporal series show a crisis-ratchet cycle, not monotonic drift: after each salient accident, enforcement and scenario severity step up; between crises both relax while pass rates inflate; each cycle resets at a higher floor. The oscillation is partly the extraction mechanism itself (intermittent reinforcement lets the regime harvest reform credibility after each disaster while re-accumulating ritual between them). End-state base_properties values reflect the post-ratchet phase of the latest cycle. Receipt discipline: the recurring monetary extraction (exercise budgets and accreditation spend) demonstrably accrues to the simulation_vendors seat; training units, management, and regulators accrue derivative assurance value rather than the extracted flow itself, so gain_flow names the vendor seat rather than asserting diffuse. Fixing cost: migrating validity criteria to the continuous_refresh_hybrid format would require industry-wide simulator buildout, schedule disruption, and vendor-model upheaval against benefits that are probabilistic and discounted, hence prohibitive for whoever could fix it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats the arrangement is functioning assurance infrastructure: drills happen, records exist, audits close. From the payer seats the same structure operates as a compulsory time-tax that purchases confidence for others using hours taken from them, while masking the specific competencies (stress response under ambiguous instrumentation, multi-system cascade recognition) that real events punish. Same nominal power diverges sharply by site: an operator on a plant running unannounced, high-fidelity, failure-likely scenarios gets genuine exercise from the identical regulation that delivers an operator elsewhere a scripted annual tabletop with a published pass rate. Inter-institutionally, the regulator (constrained exit, blame exposure), the insurer (arbitrage exit, tail exposure), and the vendor (arbitrage exit, fee income) occupy adjacent institutional seats with opposite structural relationships to the same artifact.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for training_departments, simulation_vendors, site_senior_management, and safety_regulators; victim declarations drive high d for shift_operators and host_communities. Suppression is authored as a raw structural property and is deliberately NOT scaled; only extractiveness is scaled, by directionality and spatial scope. Scope amplification matters here: the validity criterion operates at national-to-global standard scope, where verification of scenario quality is hardest, so effective extraction on the trapped targets is amplified beyond the base epsilon. Two nuances the derivation handles imperfectly are recorded here rather than overridden: safety_regulators collect compliance evidence (beneficiary-side) but also own the reputational catastrophe when a fully compliant site fails, pulling their realized position toward symmetric; industrial_insurers collect premium-relevant assurance but bear the tail loss when the proxy fails, likewise pulling toward symmetric. Neither warrants a per-atom override given the shared power atoms involved; both are recorded as qualitative corrections.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rare catastrophes cannot be rehearsed live, so a proxy is necessary) remains live, so this is not a resolved-mandatrophy case and mandatrophy_resolved is left undeclared. The classification work is keeping the two failure directions distinct. Reading the arrangement as a rope would launder the assurance-extraction asymmetry (scripted drills harvesting operator time to mint management liability shields); reading it as a snare would erase the genuine coordination function (some proxy rehearsal is strictly necessary, and simulation is the only ethical candidate at scale). Tangled_rope holds both truths. The forward risks differ by direction: if scenario fidelity keeps eroding while the validity criterion stands, the arrangement drifts toward piton (ritual drills maintained because the calendar exists; theater_ratio already past 0.5); if enforcement hardens to protect the artifact economy against fidelity criticism, it drifts toward snare. The continuous_refresh_hybrid sibling reading is the natural repair path; its adoption cost is what fixing_cost records as prohibitive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_adequacy,
    'Does current simulation fidelity actually exercise the competencies that fail in real events (stress response under ambiguity, multi-system cascade recognition), or only the competencies simulations can conveniently present?',
    'Correlation studies linking drill performance to subsequent real-event performance; controlled comparisons of outcome rates between sites running high-fidelity unannounced scenarios and sites running scripted announced ones.',
    'If fidelity is inadequate, the validity claim collapses toward the real_catastrophe_only sibling, base extractiveness rises (hours purchased without competence returned), and the arrangement''s coordination leg thins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_adequacy, empirical, 'Whether simulation exercises transfer to the competencies real catastrophes punish.').

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (simulation_as_proxy) of the competence_exercise_validity kernel; what would the sibling readings (real_catastrophe_only, continuous_refresh_hybrid) change structurally, and where exactly is the disagreement located?',
    'Adoption of a sibling reading by the standard-setting bodies: real_catastrophe_only would make the satisfaction condition nearly unstagingable and drive suppression toward resignation or tolerance of small real events; continuous_refresh_hybrid would convert one-time validation into ongoing cycles, importing scaffold-like transition dynamics and a sunset on one-time compliance artifacts.',
    'Classification is stable only within this reading; under continuous_refresh_hybrid the persistence structure changes (transitional support rather than steady-state entitlement), and under real_catastrophe_only the victim set expands to include everyone exposed during the unstaged real events the reading implicitly accepts as tuition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which kernel, which reading, what siblings would alter.').

omega_variable(
    safety_record_causality,
    'Does the clean safety record under compliant drill regimes evidence the simulations'' adequacy, or do confounders (engineering redundancy, automation, the sheer rarity of trigger events) explain the record?',
    'Decompose accident precursor frequencies from major-event frequencies; near-miss databases analyzed against drill-regime intensity; counterfactual comparison across jurisdictions with differing validity standards.',
    'If the record reflects engineering layers rather than drills, the ''record proves adequacy'' leg of the reading fails, the arrangement''s justification thins, and drift toward piton (or snare if defense of the artifact hardens) accelerates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_record_causality, empirical, 'Whether the safety record causally credits the simulation regime.').

omega_variable(
    suppression_mechanism_split,
    'Is operator acquiescence to the drill regime structural (compulsory scheduling, license conditions, audit consequences) or internalized (professional identity fused with drill participation)?',
    'Post-exit surveys of retired operators on whether the drill obligation would have felt optional absent penalty; union bargaining histories over drill design; comparison of acquiescence across jurisdictions with different enforcement intensity.',
    'If a large share is internalized, effective suppression persists even if the mandate is relaxed, and remedies aimed only at the structural layer will underperform; the structural share is estimated at roughly 0.7 of the authored suppression value, the internalized share roughly 0.3.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized components of the measured suppression.').

omega_variable(
    pass_rate_inflation_direction,
    'Are drill pass rates rising because crew competence is improving or because scenarios are softening to protect completion statistics?',
    'Blind re-scoring of archived scenarios by external assessors against original design intent; longitudinal item-difficulty analysis of scenario banks.',
    'If scenarios are softening, the rising theater_ratio reflects deliberate metric substitution (Goodhart drift) rather than improvement, strengthening the case that the validity criterion has detached from its referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pass_rate_inflation_direction, empirical, 'Direction of the mechanism behind rising pass rates.').

omega_variable(
    framing_underdetermination,
    'Is the defensible framing of this constraint the validity criterion itself (declared here), or the assurance-artifact economy layered above it (the market in compliance records that management, insurers, and vendors trade on)?',
    'Compare classifications under both framings: signals favoring the declared framing are the kernel context naming the validity criterion as the axis; signals favoring the alternative are that the criterion''s persistence is best explained by the value of the artifacts it mints rather than by any exercise function.',
    'Under the artifact-economy framing the arrangement reads more snare-flavored (the coordination story as cover for an evidence market, with management and insurers as captors); under the declared framing it reads tangled_rope. The choice changes the computed type, not the underlying facts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_underdetermination, conceptual, 'Two coherent framings of the same arrangement yield different classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__simulation_as_proxy, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__simulation_as_proxy, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comp_tr_t4, competence_exercise_validity__simulation_as_proxy, theater_ratio, 4, 0.27).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_validity__simulation_as_proxy, theater_ratio, 8, 0.24).
narrative_ontology:measurement(comp_tr_t12, competence_exercise_validity__simulation_as_proxy, theater_ratio, 12, 0.34).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_validity__simulation_as_proxy, theater_ratio, 16, 0.3).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__simulation_as_proxy, theater_ratio, 20, 0.39).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_validity__simulation_as_proxy, theater_ratio, 24, 0.36).
narrative_ontology:measurement(comp_tr_t28, competence_exercise_validity__simulation_as_proxy, theater_ratio, 28, 0.44).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_validity__simulation_as_proxy, theater_ratio, 32, 0.41).
narrative_ontology:measurement(comp_tr_t36, competence_exercise_validity__simulation_as_proxy, theater_ratio, 36, 0.52).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(comp_be_t4, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(comp_be_t8, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(comp_be_t12, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(comp_be_t16, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(comp_be_t24, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(comp_be_t28, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 28, 0.59).
narrative_ontology:measurement(comp_be_t32, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(comp_be_t36, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 36, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(comp_su_t4, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 4, 0.47).
narrative_ontology:measurement(comp_su_t8, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 8, 0.43).
narrative_ontology:measurement(comp_su_t12, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(comp_su_t16, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(comp_su_t24, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(comp_su_t28, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 28, 0.56).
narrative_ontology:measurement(comp_su_t32, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(comp_su_t36, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 36, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__simulation_as_proxy, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__real_catastrophe_only).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% The colloquial label 'competence exercise validity' decomposes into three structurally distinct constraints corresponding to three readings of one kernel. This file authors the simulation_as_proxy reading: epsilon is assessed for the standing arrangement in which completed simulations discharge the exercise duty, by that reading's own lights. The real_catastrophe_only sibling authors a constraint whose satisfaction condition is nearly unstagingable (radically different suppression profile); the continuous_refresh_hybrid sibling authors a constraint with an ongoing-cycle persistence structure. This reading sits upstream of both in resource terms: its dominance channels training budgets and performance data toward simulation formats, weakening the rivals' operating environment without logically eliminating them, hence coexists_with edges and this note across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
