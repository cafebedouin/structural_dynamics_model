% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__near_miss_as_bridge, []).

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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Near-Miss Investigation as the Competence Bridge
 *   domain: safety engineering/organizational learning/high-reliability organizations
 *
 * SUMMARY:
 *   This story instantiates the near_miss_as_bridge reading of the
 *   competence_retention_exercise kernel: the standing arrangement under
 *   assessment is the hybrid regime in which high-fidelity simulators
 *   preserve routine skill while mandatory near-miss investigation supplies
 *   the real-world feedback that recalibrates training — catastrophes treated
 *   as neither necessary nor sufficient. The arrangement is a constructed
 *   institutional practice, not a natural feature: it was built deliberately
 *   after early aviation, nuclear, and chemical disasters showed that
 *   catastrophe-driven learning was intolerably priced. Its coordination
 *   function is genuine — it converts distributed, individually costly
 *   observations into shared knowledge of latent failure modes — but the same
 *   structure channels costs asymmetrically: crews supply the reporting labor
 *   and carry the blame exposure, self-disclosing operators carry enforcement
 *   risk their silent competitors avoid, while management, regulators, and
 *   insurers collect legitimacy, oversight capability, and actuarial
 *   resolution from the accumulated data. Family note: the sibling readings
 *   (simulation_as_sufficient, catastrophe_as_necessary) are separate
 *   constraints with separate epsilon values over different referents; this
 *   file authors epsilon only for the hybrid arrangement as this reading sees
 *   it.
 *
 * KEY AGENTS:
 *   - safety_regulators: Agenda setter (institutional/constrained) — mandates reporting, audits investigations, collects oversight capability cheaply
 *   - senior_operations_management: Primary beneficiary with payer overlay (powerful/mobile) — funds the apparatus and captures the legitimacy dividend of the data
 *   - frontline_operating_crews: Primary target (organized/constrained) — bears reporting labor, debrief burden, and blame exposure when just culture fails
 *   - self_disclosing_operators: Secondary target (powerful/constrained) — accepts enforcement exposure for forbearance that non-disclosers evade
 *   - insurers_and_reinsurers: Beneficiary (institutional/arbitrage) — converts reports into actuarial resolution, repricing or withdrawing at will
 *   - traveling_public: Diffuse beneficiary (powerless/trapped) — holds the residual-risk stake without any seat in the loop
 *   - contractor_and_contingent_workforce: Excluded voice (moderate/constrained) — performs adjacent work outside the reporting perimeter
 *   - accident_investigation_boards: Analytical observer (institutional/analytical) — sees both the loop and its blind spots across operators
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.52).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.42).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.52).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Near-Miss Investigation as the Competence Bridge").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety engineering/organizational learning/high-reliability organizations").

domain_priors:requires_active_enforcement(competence_retention_exercise__near_miss_as_bridge).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, 'a5ec8a91-0dc3-4cde-9b76-7b058940359a').
narrative_ontology:cs_kernel_codification('a5ec8a91-0dc3-4cde-9b76-7b058940359a', distributed).
narrative_ontology:cs_authority_grounding('a5ec8a91-0dc3-4cde-9b76-7b058940359a', expertise).
narrative_ontology:cs_interpretation_layer_present('a5ec8a91-0dc3-4cde-9b76-7b058940359a').
narrative_ontology:cs_reading_relation('a5ec8a91-0dc3-4cde-9b76-7b058940359a', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_reading_relation('a5ec8a91-0dc3-4cde-9b76-7b058940359a', competence_retention_exercise__simulation_as_sufficient, forecloses).
narrative_ontology:cs_axiom('a5ec8a91-0dc3-4cde-9b76-7b058940359a', foundational, near_miss_signal_sufficiency).
narrative_ontology:cs_axiom_status(near_miss_signal_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('a5ec8a91-0dc3-4cde-9b76-7b058940359a', near_miss_signal_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('a5ec8a91-0dc3-4cde-9b76-7b058940359a', foundational, simulation_requires_empirical_recalibration).
narrative_ontology:cs_axiom_status(simulation_requires_empirical_recalibration, holdable).
narrative_ontology:cs_axiom_grounding('a5ec8a91-0dc3-4cde-9b76-7b058940359a', simulation_requires_empirical_recalibration, instrumental).
narrative_ontology:cs_reference_frame('a5ec8a91-0dc3-4cde-9b76-7b058940359a', simulator_nearmiss_hybrid_regime).
narrative_ontology:cs_drift_state('a5ec8a91-0dc3-4cde-9b76-7b058940359a', contemporary_metricification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a5ec8a91-0dc3-4cde-9b76-7b058940359a', '2026-08-10T14:22:05Z').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, senior_operations_management).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, insurers_and_reinsurers).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, traveling_public).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, frontline_operating_crews).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, self_disclosing_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, frontline_operating_crews).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, senior_operations_management).
narrative_ontology:constraint_vindicates(competence_retention_exercise__near_miss_as_bridge, heinrich_incident_pyramid).
narrative_ontology:constraint_vindicates(competence_retention_exercise__near_miss_as_bridge, just_culture_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set mandatory occurrence-reporting requirements, define just-culture protections, and audit the quality of operator investigations. Receive a continuous stream of system-health data at low collection cost and publish aggregate trend statistics. Cannot abandon the reporting framework without political cost; their performance is judged publicly by the absence of catastrophes, and favorable trend curves are cited as evidence of effective oversight.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Fund investigation departments, decide which findings trigger procedure changes, and present near-miss statistics to boards, regulators, and insurers as evidence of diligent operation. Pay for the investigative apparatus and occasionally absorb enforcement consequences of their own disclosures. Individuals transfer between firms and sectors; the statistics belong to the institution, not to the person.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, senior_operations_management, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, senior_operations_management, payer).

% File occurrence reports, sit for debriefings and post-incident testing where programs require it, and implement the procedural changes that come back down. Protected by just-culture policies when those hold; exposed to discipline, litigation, and reputational harm when they fail. Professional licenses tie them to the sector; changing employer is feasible, leaving the profession is costly.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, frontline_operating_crews, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, frontline_operating_crews, beneficiary).

% Voluntarily report their own deviations and deficiencies to authorities under no-penalty or reduced-penalty programs. Accept legal and commercial exposure that quieter competitors avoid, betting that disclosure purchases forbearance and regulatory trust. Exiting means joining the non-disclosing population, which degrades the program's data and the operator's standing.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, self_disclosing_operators, payer,
    powerful, biographical, constrained, national).

% Price premiums and set insurability conditions using reported incident data. Obtain actuarial resolution unavailable through any other channel and can reprice or withdraw capacity from operators whose reporting is thin. Their exposure ends at the policy boundary.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, insurers_and_reinsurers, beneficiary,
    institutional, generational, arbitrage, global).

% Depend on transport, energy, and medical systems whose crews train against rare failures. Receive the resulting safety margins without participating in or observing the reporting loop, and cannot opt out of the residual risk beyond choosing among providers.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, traveling_public, beneficiary,
    powerless, generational, trapped, global).

% Perform maintenance, ground handling, and support tasks adjacent to reported operations but sit outside the reporting perimeter; their occurrences enter the record only when a licensed crew files. They would contest the completeness of the system's picture of operational reality but hold no seat in program governance.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, contractor_and_contingent_workforce, excluded,
    moderate, biographical, constrained, global).

% Investigate major accidents and reconstruct near-miss precursors after the fact, seeing both the reporting loop and its blind spots across many operators. Publish findings that periodically expose the gap between what was reported and what was actually happening.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, accident_investigation_boards, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__near_miss_as_bridge, senior_operations_management).
narrative_ontology:fixing_cost_class(competence_retention_exercise__near_miss_as_bridge, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of maintaining competence against events too rare and too dangerous to experience directly: converts thousands of individually small, locally observed deviations into a shared map of latent failure modes, validates simulator scenarios against what actually happens in operation, and directs corrective attention by signal content rather than by outcome severity.
% TRANSFER_FUNCTION: Moves error exposure, reporting labor, and disclosure risk upward from crews and self-disclosing operators into organizational and regulatory databases; moves legitimacy, actuarial clarity, and oversight capability outward to management, regulators, and insurers; moves corrective-action resources back toward the hazards the data identifies when the loop actually closes.
% ABSENT_VOICES: Contractor and contingent workers outside the reporting perimeter; crews operating under weak just-culture conditions whose silence is compelled rather than chosen; passengers and host communities bearing residual risk with no seat; small operators unable to fund investigation infrastructure, whose absence biases the dataset toward large, well-resourced reporters.
% DISAPPEARANCE_RATIONALE: If the near-miss bridge vanished overnight, simulator curricula would drift from operational reality within a few years; latent failure modes currently caught as precursors would surface first as accidents; insurers would lose actuarial resolution and reprice blindly; the industry would revert to the pre-institutional cycle of catastrophe, inquiry, reform, and forgetting.
% FOUNDING_PROBLEM: How can an organization train and validate competence for events so rare and so destructive that direct experience is unavailable — the problem aviation, nuclear power, and chemical processing confronted once early disasters showed that waiting for catastrophes to teach was intolerable?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: accident investigation board reports repeatedly reconstruct catastrophes from precursor events the reporting loop failed to convert into correction; peer-reviewed human-factors and resilience-engineering literature attests the problem is ongoing; post-accident legislative testimony documents the same. The corroboration is partial: the same literature disputes the simple incident ratios the regime inherited, attesting the founding problem is live while contesting parts of the inherited answer.
narrative_ontology:disappearance_verdict(competence_retention_exercise__near_miss_as_bridge, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__near_miss_as_bridge, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__near_miss_as_bridge, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__near_miss_as_bridge, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__near_miss_as_bridge, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__near_miss_as_bridge_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__near_miss_as_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type and metrics are authored independently. The claim is tangled_rope on structure: a real collective-action solution (rare-event learning without catastrophe pricing) that simultaneously moves costs onto identifiable seats and depends on active enforcement (mandates, just-culture administration, audit) to hold — all three canonical gates are satisfied by the declared data. The metrics describe observed operation. Extractiveness 0.52 is moderate: the loop delivers real corrective value, but reporting labor, blame leakage, disclosure risk, and the legitimacy capture documented by investigation boards are persistent through the same structure. Suppression 0.42 is a raw structural property, unscaled by power or scope — it reflects mandated reporting plus the residual chill of discipline and litigation, not the heavier coercion seen in predatory arrangements; the engine scales only extractiveness. Theater 0.35 reflects the growing share of near-miss activity that is dashboard maintenance rather than correction. Accessibility collapse is low (0.25): the alternatives — simulation-only regimes, catastrophe-driven learning, informal reporting — remain conceptually and practically available, which is precisely why this is a contested kernel rather than settled law. Resistance 0.50 is the classic signature of chronic underreporting, gaming, and managerial bad-news filtering. The measurement series share one grid (t=0..40, mapping roughly 1985 institutionalization to 2025): extractiveness dips mid-interval as just-culture protections mature, then climbs as metricification and legitimacy capture grow; theater rises monotonically with dashboard proliferation; suppression_requirement traces the enforcement-machinery arc — build-up through the just-culture codification era, plateau, then partial decay as trust erosion and litigation chill weakened the reporting compact. All endpoint values match the scalar base_properties by construction of the shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats should compute divergent types from identical structural data. From senior management's position the arrangement is a system it built, funds, and legitimately showcases; from the crew seat the same structure is a machine that consumes their errors, exposes them to discipline when protections lapse, and returns procedure changes of variable quality. Self-disclosing operators experience the disclosure bargain as enforced generosity — they pay in legal exposure what non-disclosers never pay — while regulators experience the same flow as costless vigilance. The engine computes this per-seat divergence; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for regulators, management, insurers, and the public; victim declarations drive high directionality for crews and self-disclosing operators. Exit modulation sharpens the spread: insurers hold arbitrage-grade exit (repricing, withdrawal) and sit nearest the beneficiary pole; crews are constrained by licensure and professional identity, holding them near the full-target end despite their organized power and their secondary benefit (maintained competence, blame protection when just culture holds); management's mobility softens its payer overlay; the traveling public is a trapped beneficiary — subsidized by the arrangement, unable to exit the residual risk. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — competence against unexperienceable events — is live, so this is not a resolved mandate; the zombie-flag combination (dead status x world_rearranges verdict) does not arise. The mandatrophy-relevant risk sits at the sub-function level: the corrective function can decay while the statistical-legitimacy function persists, which is exactly what the rising theater_ratio series models. Reading the arrangement as pure coordination (its self-description) would miss the asymmetric costs the investigation boards keep documenting; reading it as pure extraction (as blame-case critics do) would miss the demonstrated corrective value and the world-rearranging counterfactual. Tangled_rope holds both facts in one classification. If the warning_vs_reassurance omega resolves toward reassurance dominance, expect theater to continue climbing and the arrangement to drift toward inertially maintained performance — the temporal series is the early-warning instrument for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the near_miss_as_bridge reading of the competence_retention_exercise kernel; would the simulation_as_sufficient or catastrophe_as_necessary readings of the same kernel produce a structurally different constraint?',
    'Comparative learning-outcome analysis across operators and domains running different competence-maintenance mixes, holding technology and traffic density constant; each sibling reading is authored as its own constraint file with its own epsilon and victim structure.',
    'Switching readings changes the referent arrangement, the beneficiary/victim sets, and epsilon substantially: the simulation-only referent concentrates doubt on transfer fidelity, the catastrophe referent prices learning in disaster frequency. Cross-reading comparison is valid only through the linked family, not within this file.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame membership: one of three mutually exclusive readings of the competence-retention kernel.').

omega_variable(
    signal_sufficiency_domain_variance,
    'Does near-miss volume and signal density suffice equally across domains, or does sufficiency hold only where events are frequent enough to populate the base of the incident ratio?',
    'Cross-domain comparison of recalibration rates: aviation and healthcare generate dense near-miss streams; nuclear and chemical operations generate sparse ones. Test whether simulator curricula in sparse-event domains measurably lag operational reality relative to dense-event domains.',
    'If sufficiency is domain-contingent, the reading holds for high-frequency domains and degrades toward the catastrophe_as_necessary position in sparse-event ones — changing the constraint''s classification and its victims in those sectors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signal_sufficiency_domain_variance, empirical, 'Whether the near-miss bridge carries enough signal in sparse-event industries.').

omega_variable(
    warning_vs_reassurance_function,
    'Does accumulated near-miss data function operationally as warning (triggering correction) or as reassurance (normalizing deviation and licensing margin-taking)?',
    'Trace closed loops: sample investigated near-misses and follow whether findings changed procedures, staffing, or design within a fixed horizon; compare against organizations'' public use of the same statistics in legitimacy claims.',
    'If reassurance dominates, the arrangement''s coordination function is decaying behind its legitimating function — theater continues rising and the structure drifts toward inertially maintained performance; if warning dominates, the measured extraction is the price of a functioning loop.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warning_vs_reassurance_function, empirical, 'Normalization-of-deviance direction: whether the bridge teaches or anesthetizes.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured reporting chill structural (formal discipline, litigation exposure, license jeopardy) or internalized (crews who have absorbed blame culture self-censor even where protections hold)?',
    'Compare reporting rates for identical occurrence classes under anonymous versus attributed channels, and track rate trajectories after just-culture protections are introduced: if attributed reporting stays depressed where anonymous reporting is healthy, the residual suppression is carried internally.',
    'If internalized, effective suppression exceeds the structural measure and persists after formal protections improve — enforcement-side remedies will underperform and the payer seat''s directionality is more fully target-weighted than the structural data alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized component of reporting suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(near_miss_bridge_tr_t0, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t0, observed).
narrative_ontology:measurement(near_miss_bridge_tr_t6, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 6, 0.17).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t6, observed).
narrative_ontology:measurement(near_miss_bridge_tr_t12, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t12, observed).
narrative_ontology:measurement(near_miss_bridge_tr_t18, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t18, observed).
narrative_ontology:measurement(near_miss_bridge_tr_t24, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t24, observed).
narrative_ontology:measurement(near_miss_bridge_tr_t30, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t30, observed).
narrative_ontology:measurement(near_miss_bridge_tr_t36, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 36, 0.34).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t36, observed).
narrative_ontology:measurement(near_miss_bridge_tr_t40, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(near_miss_bridge_be_t0, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(near_miss_bridge_be_t0, observed).
narrative_ontology:measurement(near_miss_bridge_be_t6, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 6, 0.4).
narrative_ontology:measurement_basis(near_miss_bridge_be_t6, observed).
narrative_ontology:measurement(near_miss_bridge_be_t12, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 12, 0.37).
narrative_ontology:measurement_basis(near_miss_bridge_be_t12, observed).
narrative_ontology:measurement(near_miss_bridge_be_t18, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 18, 0.39).
narrative_ontology:measurement_basis(near_miss_bridge_be_t18, observed).
narrative_ontology:measurement(near_miss_bridge_be_t24, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 24, 0.43).
narrative_ontology:measurement_basis(near_miss_bridge_be_t24, observed).
narrative_ontology:measurement(near_miss_bridge_be_t30, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 30, 0.47).
narrative_ontology:measurement_basis(near_miss_bridge_be_t30, observed).
narrative_ontology:measurement(near_miss_bridge_be_t36, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 36, 0.5).
narrative_ontology:measurement_basis(near_miss_bridge_be_t36, observed).
narrative_ontology:measurement(near_miss_bridge_be_t40, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(near_miss_bridge_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(near_miss_bridge_su_t0, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(near_miss_bridge_su_t0, observed).
narrative_ontology:measurement(near_miss_bridge_su_t6, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 6, 0.38).
narrative_ontology:measurement_basis(near_miss_bridge_su_t6, observed).
narrative_ontology:measurement(near_miss_bridge_su_t12, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 12, 0.46).
narrative_ontology:measurement_basis(near_miss_bridge_su_t12, observed).
narrative_ontology:measurement(near_miss_bridge_su_t18, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 18, 0.5).
narrative_ontology:measurement_basis(near_miss_bridge_su_t18, observed).
narrative_ontology:measurement(near_miss_bridge_su_t24, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 24, 0.48).
narrative_ontology:measurement_basis(near_miss_bridge_su_t24, observed).
narrative_ontology:measurement(near_miss_bridge_su_t30, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(near_miss_bridge_su_t30, observed).
narrative_ontology:measurement(near_miss_bridge_su_t36, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 36, 0.43).
narrative_ontology:measurement_basis(near_miss_bridge_su_t36, observed).
narrative_ontology:measurement(near_miss_bridge_su_t40, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(near_miss_bridge_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__near_miss_as_bridge, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__catastrophe_as_necessary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how is catastrophe-avoidance competence exercised?' decomposes into three structurally distinct constraints — one per reading of the competence_retention_exercise kernel. Each member authors its own epsilon over its own referent: this file authors moderate epsilon (0.52) over the hybrid simulator-plus-near-miss arrangement; the simulation_as_sufficient sibling authors epsilon over a simulation-only referent (where the contested quantity is transfer fidelity); the catastrophe_as_necessary sibling authors epsilon over a catastrophe-driven referent (where learning is priced in disaster frequency). The upstream/downstream structure runs from this reading outward: wherever the hybrid regime operates, it shapes the resource environment of both siblings — simulator budgets and curriculum authority depend on near-miss recalibration claims, and catastrophe-driven reform episodes draw their recommendations from the same investigation infrastructure. All three files link one another via affects_constraints; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
