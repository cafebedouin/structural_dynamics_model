% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
 *   human_readable: Simulation-as-Sufficient-Readiness Claim (Lived-Catastrophe-Necessity Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This story instantiates the lived-catastrophe-necessity reading of the
 *   exercise-as-competence-maintenance kernel: the claim that only an actual
 *   catastrophe — with genuine stakes, genuine consequence, and genuine
 *   uncertainty about outcome — exercises the competence kernel that
 *   crisis-response depends on. Simulation, on this reading, is rehearsal of
 *   procedure and communication pathways, but the specific
 *   judgment-under-irreversible-stakes component of competence does not
 *   transfer from simulated to real exposure, and in fact atrophies covertly
 *   behind a certification record that keeps getting renewed. The constraint
 *   under contest is the standing institutional arrangement in which exercise
 *   completion is treated (by administrators, certifiers, and insurers) as
 *   equivalent to verified readiness. This reading holds that arrangement is
 *   substantially extractive: it manufactures a legible, tradeable proxy for
 *   competence (the certification) while leaving the actual competence gap
 *   unmeasured and, under this reading's premise, silently widening. Sibling
 *   readings of the same kernel (simulation_sufficiency_reading,
 *   hybrid_decay_reading) are NOT part of this story — they are separate
 *   constraints with their own ε and stakeholder structure, linked here only
 *   via network edges and the omega variables that route the committer
 *   disagreement.
 *
 * KEY AGENTS:
 *   - exercise_program_administrators: institutional agenda-setter, arbitrage exit — designs and certifies exercises, bears no real-catastrophe exposure
 *   - certification_bodies: institutional beneficiary — business model depends on exercise-as-sufficient-proxy
 *   - frontline_operators: moderate power, constrained exit — hold unearned confidence under this reading
 *   - downstream_public_exposed_to_failure: powerless, trapped — inherits the untested gap with no visibility into it
 *   - safety_researchers: analytical observer — post-incident evidence source, arrives too late to affect the current certification cycle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.62).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.58).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Simulation-as-Sufficient-Readiness Claim (Lived-Catastrophe-Necessity Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, '82428ca3-3603-42d4-8081-e89d2cb49ae3').
narrative_ontology:cs_kernel_codification('82428ca3-3603-42d4-8081-e89d2cb49ae3', distributed).
narrative_ontology:cs_authority_grounding('82428ca3-3603-42d4-8081-e89d2cb49ae3', practice).
narrative_ontology:cs_interpretation_layer_present('82428ca3-3603-42d4-8081-e89d2cb49ae3').
narrative_ontology:cs_reading_relation('82428ca3-3603-42d4-8081-e89d2cb49ae3', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('82428ca3-3603-42d4-8081-e89d2cb49ae3', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('82428ca3-3603-42d4-8081-e89d2cb49ae3', foundational, stakes_irreducibility_of_judgment_competence).
narrative_ontology:cs_axiom_status(stakes_irreducibility_of_judgment_competence, holdable).
narrative_ontology:cs_axiom_grounding('82428ca3-3603-42d4-8081-e89d2cb49ae3', stakes_irreducibility_of_judgment_competence, empirically_contingent).
narrative_ontology:cs_axiom('82428ca3-3603-42d4-8081-e89d2cb49ae3', secondary, covert_competence_atrophy_under_simulation_only_regime).
narrative_ontology:cs_axiom_status(covert_competence_atrophy_under_simulation_only_regime, holdable).
narrative_ontology:cs_axiom_grounding('82428ca3-3603-42d4-8081-e89d2cb49ae3', covert_competence_atrophy_under_simulation_only_regime, empirically_contingent).
narrative_ontology:cs_reference_frame('82428ca3-3603-42d4-8081-e89d2cb49ae3', post_incident_manufactured_proxy_necessity).
narrative_ontology:cs_drift_state('82428ca3-3603-42d4-8081-e89d2cb49ae3', contemporary_certification_economy, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('82428ca3-3603-42d4-8081-e89d2cb49ae3', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_program_administrators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, certification_bodies).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, insurance_underwriters).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, senior_management).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, frontline_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, downstream_public_exposed_to_failure).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, emergency_responders_relying_on_untested_teams).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and run tabletop and simulated-crisis exercises, then certify participants and facilities as 'ready.' They set the exercise calendar, choose scenario fidelity, and issue the readiness attestations that satisfy regulators. They bear no personal exposure if the real catastrophe reveals gaps the simulation never surfaced.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Issue readiness certifications keyed to exercise completion and pass rates. Their business model depends on exercises being treated as sufficient proof of competence; if lived-catastrophe activation were required for certification, their entire audit product would be obsolete.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, certification_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Price risk and set premium discounts based on documented exercise completion. They benefit from a legible, auditable proxy for competence (exercise records) rather than an unmeasurable, unauditable one (actual disaster performance, which by definition has not yet occurred for most insured entities).
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, insurance_underwriters, beneficiary,
    institutional, generational, arbitrage, continental).

% Approve exercise budgets and sign off on readiness reports to boards and regulators. They collect the reputational and liability-shielding benefit of 'we exercised for this' without bearing the operational consequence if the exercised skill was never truly load-tested.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, senior_management, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, senior_management, agenda_setter).

% Perform the simulated drills and are told they are 'certified ready.' Under this reading, their judgment-under-genuine-stakes has never actually been exercised — the confidence the certification grants them is unearned, and they will discover the gap, if it exists, only at the moment of real catastrophe when the cost of discovery is highest.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, frontline_operators, payer,
    moderate, biographical, constrained, local).

% Live near the plant, fly on the aircraft, or depend on the grid whose operators are certified via simulation alone. They have no visibility into whether the underlying competence is real or a rehearsal artifact, and no exit from the exposure — they inherit whatever gap exists between simulated and lived competence.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, downstream_public_exposed_to_failure, payer,
    powerless, generational, trapped, regional).

% Arrive at the actual incident and must coordinate with operators whose competence was certified through simulation. If the operators' judgment collapses under real stakes in ways the exercise never revealed, responders inherit an escalated, less-controlled scene than the readiness paperwork promised.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, emergency_responders_relying_on_untested_teams, payer,
    moderate, immediate, trapped, regional).

% Study post-incident reports comparing pre-incident exercise records against actual performance under genuine catastrophe. They are the primary source of evidence for or against the lived-catastrophe-necessity claim, but their findings arrive only after the exercise-certification cycle has already collected its institutional value.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, safety_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, diffuse).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Exercises do coordinate genuinely useful things: procedural muscle memory, communication pathways, equipment familiarity, and cross-team contact lists get real practice. The coordination problem — many actors needing a shared rehearsed script — is genuinely solved by simulation, up to a point.
% TRANSFER_FUNCTION: Moves liability-shielding certainty and premium/regulatory credit from the domain of 'unverifiable claim' to the domain of 'audited artifact,' at the cost of transferring unexamined risk onto operators and the exposed public, who bear the consequence if the certified competence turns out to be simulation-only and does not transfer to real-stakes performance.
% ABSENT_VOICES: Survivors and responders from prior real catastrophes involving previously-'certified-ready' teams are rarely brought into the exercise-design process; their testimony that the simulation did not prepare them is institutionally inconvenient and is typically filed as an isolated incident rather than routed back into the certification standard.
% DISAPPEARANCE_RATIONALE: If exercise-based certification vanished overnight, administrators and certifiers would lose their institutional product, insurers would need new (and possibly more expensive or less legible) risk proxies, and operators would lose the confidence conferred by certification — but under this reading, the underlying real-stakes competence gap would be unchanged, since it was never closed by the exercises in the first place. Whether the world 'rearranges' or 'stays the same' depends on whether one asks the certifying institutions (rearranges, catastrophically) or the operators actually facing catastrophe (stays the same, because the gap this reading identifies was never closed to begin with).
% FOUNDING_PROBLEM: Organizations cannot ethically or practically manufacture real catastrophes to test their people, so exercises were built as the best available proxy for maintaining and verifying crisis competence between actual events.
% FOUNDING_PROBLEM_CORROBORATION: Certification bodies and administrators attest the founding problem is solved — exercises adequately maintain competence. Independent post-incident investigators (e.g., accident review boards examining cases where certified-ready teams performed poorly under real stakes) and academic safety researchers outside the certification economy attest the founding problem persists in disguised form: the proxy has been substituted for the target it was meant to measure.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, contested).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 (rising to that level over the interval) because the certification-and-insurance economy built on exercise completion diverts institutional attention and resources away from acknowledging an uncloseable gap, and that diversion is itself a transfer of unexamined risk onto operators and the exposed public. Theater ratio is authored high and rising (0.42 to 0.71) because, under this reading, an increasing share of exercise activity is optimized for audit legibility (documentable completion, scored checklists) rather than for the judgment-under-genuine-stakes component the reading holds cannot be simulated at all — this is Goodhart drift on the certification proxy. Suppression is moderate (0.58): the constraint does not physically bar alternatives, but it structurally discourages honest acknowledgment of the readiness gap because doing so would undermine certification value, insurance pricing, and management's liability shield. Accessibility collapse is moderate (0.45) — operators and researchers who dissent from the sufficiency claim are not silenced outright, but the institutional incentive structure makes dissent costly and rare. Resistance is moderate (0.55), reflecting ongoing but marginalized researcher and post-incident-investigator pushback against the sufficiency framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries — administrators, certifiers, insurers, senior management — sit near the low-d end: they collect legibility, liability-shielding, and premium credit from the exercise-certification apparatus, and their exit options (arbitrage, mobile) let them absorb little of the downside if the underlying competence claim is false. Payers — frontline operators, the downstream public, and responding emergency teams — sit near the high-d end: they are trapped or constrained, cannot exit the exposure, and bear the entire cost if the lived-catastrophe-necessity premise is correct and the certified competence does not, in fact, transfer. Frontline operators occupy an ambiguous middle position (moderate power, constrained exit) because they are simultaneously the credentialed beneficiaries of the certification (professional status, employment continuity) and its principal victims (unearned confidence, exposure at the moment of real testing) — this dual position is a structural feature of the reading, not an authoring error.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that real catastrophes cannot ethically be manufactured for testing purposes, so a proxy is needed — remains genuinely live; this is not a case of a dead mandate persisting by inertia. What this reading identifies as mandatrophy-adjacent is narrower: the proxy (exercise completion) has been substituted for the target (verified real-stakes competence) inside the certification and insurance apparatus, and that substitution is now defended institutionally regardless of whether it tracks the target. Classifying this as tangled_rope rather than snare preserves the genuine coordination value (procedural rehearsal, communication-pathway practice) that exercises do deliver, while still naming the asymmetric extraction (legibility and liability-shielding captured by administrators/certifiers/insurers, unexamined risk absorbed by operators and the public) that rides on the same structure. A pure snare framing would erase the real coordination function; a pure rope framing would erase the victim set this reading insists on naming.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Does simulated catastrophe exercise the SAME competence kernel as lived catastrophe (simulation_sufficiency_reading), a DIFFERENT but overlapping kernel split into procedural and judgment components (hybrid_decay_reading), or does only lived catastrophe exercise the kernel at all (this reading)?',
    'Longitudinal comparison of operator performance in real catastrophes, stratified by exercise history and fidelity, controlling for prior real-incident exposure — if operators with extensive high-fidelity simulation but zero real-incident exposure perform indistinguishably from operators with real-incident exposure, the sufficiency reading is supported; if a persistent, non-closing gap remains regardless of simulation fidelity, this reading is supported; if the gap appears only in judgment-under-irreversible-stakes decisions specifically, the hybrid reading is supported.',
    'Determines which of the three sibling constraints in this kernel family is the empirically correct description of the exercise-certification apparatus''s actual function — directly affects whether the certification regime described here should be read as tangled_rope (genuine partial coordination plus extraction, this reading), rope (genuine sufficient coordination, sibling reading), or a more granular hybrid classification (sibling reading).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Which reading of the exercise-as-competence-maintenance kernel is structurally correct.').

omega_variable(
    covert_atrophy_detectability,
    'If competence genuinely atrophies without real-stakes activation as this reading claims, is that atrophy detectable by any means short of an actual catastrophe — and if not, does the certification apparatus have any way to distinguish real readiness from certified-but-atrophied readiness before the fact?',
    'Development and validation of proxy measures (e.g., high-consequence near-miss response analysis, unannounced no-notice drills with genuine material consequence, physiological stress-response instrumentation during exercises) that could detect judgment-under-stakes decay without waiting for an actual disaster.',
    'If no such proxy exists, the reading''s core claim is empirically unfalsifiable in the ordinary case and the extraction identified here persists by structural necessity rather than institutional bad faith — softening the tangled_rope classification toward something closer to an intractable measurement problem. If proxies can be developed and validated, failure to adopt them would sharpen the extraction claim considerably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_atrophy_detectability, empirical, 'Whether covert competence atrophy under this reading is detectable before a real catastrophe forces the test.').

omega_variable(
    beneficiary_capture_vs_genuine_belief,
    'Do exercise-program administrators and certification bodies genuinely believe simulation is sufficient (in which case the sufficiency framing is a sincere but possibly mistaken professional judgment), or do they maintain the sufficiency framing because it is the only framing compatible with their institutional survival (in which case it is closer to capture)?',
    'Internal document discovery, whistleblower testimony, or comparison of stated positions before and after career/institutional stakes were introduced (e.g., does an administrator''s stated confidence in simulation change after moving to a non-certifying research role?).',
    'Sincere belief supports a lower suppression score and a more sympathetic tangled_rope reading; demonstrated capture would support treating the beneficiary seats as closer to bad-faith extraction and would push the classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_genuine_belief, empirical, 'Whether the beneficiary seats'' sufficiency framing is sincere professional judgment or self-serving capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(exer_tr_t4, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 4, 0.5).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 8, 0.57).
narrative_ontology:measurement(exer_tr_t12, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 12, 0.62).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 16, 0.66).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 20, 0.69).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 24, 0.71).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(exer_be_t4, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(exer_be_t12, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(exer_su_t4, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(exer_su_t12, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language concept 'exercises maintain competence,' per the ε-invariance principle: the label conflates structurally distinct claims about what simulation can and cannot achieve. lived_catastrophe_necessity_reading (this file) authors high extraction and high theater ratio because it holds the certification apparatus is built on an uncloseable gap. simulation_sufficiency_reading is expected to author substantially lower extraction (the coordination function is treated as fully adequate, with no covert-atrophy victim set). hybrid_decay_reading is expected to author intermediate extraction, isolating the judgment-under-stakes subcomponent as the only genuinely uncovered risk while treating the procedural subcomponent as adequately exercised by simulation. All three share the same kernel identity (exercise_as_competence_maintenance) but are NOT the same constraint — each has its own ε, its own beneficiary/victim structure, and its own stakeholder seats, linked here for contamination-propagation and family-comparison purposes only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
