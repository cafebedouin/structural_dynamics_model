% ============================================================================
% CONSTRAINT STORY: tempo_margin_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tempo_margin_regime, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tempo_margin_regime
 *   human_readable: Tempo Margin Regime: Rule-Revision-Speed vs. Pressure-Duration Mismatch
 *   domain: moral_psychology/political_theory/institutional_design
 *
 * SUMMARY:
 *   This story isolates the instrument-engineering axis identified in the
 *   tempo-margin analysis of the authenticity essay: independent of which
 *   reading of 'authentic preference' one holds, there is a separate failure
 *   mode in how fast a binding rule can be revised relative to how long the
 *   pressure motivating it actually lasts. Two canonical failure directions
 *   recur across cases: fast-revision collapses the rule to a transient
 *   intention (doorstep sales, impulse-lock platform defaults), while
 *   slow-revision calcifies the rule into a coffin that outlives its
 *   justifying circumstance (indissoluble marriage, lifetime diet contracts).
 *   This constraint is deliberately agnostic among the four kernel readings
 *   of authentic_preference_boundary — it would hold under behaviorist,
 *   phenomenological, genealogical, or capability framings alike, because the
 *   mismatch is a property of the instrument's calibration, not of what
 *   authenticity consists in.
 *
 * KEY AGENTS:
 *   - whoever_sets_the_default_tempo_of_an_institution_e_g_marriage_law_platform_defaults: primary beneficiary and agenda-setter — chooses revision speed, bears no cost of mismatch
 *   - those_bound_by_a_rule_whose_tempo_was_set_by_someone_else_and_outlives_its_pressure: primary victim — inherits whichever mismatch direction their case falls into
 *   - short_pressure_high_speed_test_cases: victims of the fast-collapse failure mode
 *   - long_pressure_low_speed_test_cases: victims of the slow-calcification failure mode
 *   - instrument_designers: analytical observer measuring the ratio directly
 *   - future_bound_parties_not_yet_subject_to_the_rule: excluded voice — no seat when the default is set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tempo_margin_regime, 0.61).
domain_priors:suppression_score(tempo_margin_regime, 0.68).
domain_priors:theater_ratio(tempo_margin_regime, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tempo_margin_regime, extractiveness, 0.61).
narrative_ontology:constraint_metric(tempo_margin_regime, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(tempo_margin_regime, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tempo_margin_regime, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(tempo_margin_regime, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tempo_margin_regime, tangled_rope).
narrative_ontology:human_readable(tempo_margin_regime, "Tempo Margin Regime: Rule-Revision-Speed vs. Pressure-Duration Mismatch").
narrative_ontology:topic_domain(tempo_margin_regime, "moral_psychology/political_theory/institutional_design").

domain_priors:requires_active_enforcement(tempo_margin_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tempo_margin_regime, whoever_sets_the_default_tempo_of_an_institution_e_g_marriage_law_platform_defaults).
narrative_ontology:constraint_victim(tempo_margin_regime, those_bound_by_a_rule_whose_tempo_was_set_by_someone_else_and_outlives_its_pressure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tempo_margin_regime, short_pressure_high_speed_test_cases).
narrative_ontology:constraint_victim(tempo_margin_regime, long_pressure_low_speed_test_cases).
narrative_ontology:constraint_vindicates(tempo_margin_regime, instrument_engineering_is_distinct_from_authenticity_adjudication).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets how fast a rule can be revised relative to how long the pressure it responds to is expected to last — marriage-dissolution procedure, cooling-off periods, platform default settings, diet-program lock-in terms. Collects legitimacy, administrative simplicity, and often direct revenue (renewal fees, switching costs, procedural fees) from whatever tempo is chosen, and bears none of the cost when the chosen tempo turns out to be wrong for a given bound party's actual pressure duration.
narrative_ontology:constraint_stakeholder(tempo_margin_regime, whoever_sets_the_default_tempo_of_an_institution_e_g_marriage_law_platform_defaults, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tempo_margin_regime, whoever_sets_the_default_tempo_of_an_institution_e_g_marriage_law_platform_defaults, beneficiary).

% Are bound by a revision-speed they did not choose. When the rule revises faster than their actual pressure lasts (e.g., an impulsive doorstep decision made instantly bindable), the rule collapses to whatever they felt in the moment, capturing a transient state as if it were durable. When the rule revises slower than their pressure lasts (e.g., an indissoluble arrangement outliving the circumstance that justified it), the rule calcifies into a coffin — a structure that persists in form long after its underlying function died. Either way, the mismatch itself is a distinct cost from any question of whether the underlying preference was ever 'authentic.'
narrative_ontology:constraint_stakeholder(tempo_margin_regime, those_bound_by_a_rule_whose_tempo_was_set_by_someone_else_and_outlives_its_pressure, payer,
    powerless, biographical, trapped, local).

% Doorstep sales, impulse subscriptions, one-click platform defaults: pressure lasts seconds to minutes, but the rule locks in on first contact. These parties experience the fast-collapse failure mode — the instrument mistakes an instant for a settled preference and bindingness snaps shut before reflection is even possible.
narrative_ontology:constraint_stakeholder(tempo_margin_regime, short_pressure_high_speed_test_cases, payer,
    moderate, immediate, constrained, local).

% Indissoluble marriage regimes, lifetime diet-program contracts, permanent institutional memberships: pressure (the circumstance that motivated the original commitment) can last decades or evaporate within a year, but the revision mechanism is calibrated to near-zero speed regardless. These parties experience the slow-calcification failure mode — the coffin problem — where the rule outlives whatever it was built to hold.
narrative_ontology:constraint_stakeholder(tempo_margin_regime, long_pressure_low_speed_test_cases, payer,
    moderate, civilizational, trapped, local).

% Study the ratio of rule-revision-difficulty to pressure-duration as an engineering variable, independent of any position on which reading of authenticity is correct. Their diagnostic tool is comparative: measure revision-difficulty in seconds/months/life-scale terms and measure typical pressure-duration for the population bound by the rule, then check for mismatch in either direction. They can identify the tempo-margin failure without adjudicating whether any given bound party's preference is genealogically, phenomenologically, or dispositionally authentic.
narrative_ontology:constraint_stakeholder(tempo_margin_regime, instrument_designers, observer,
    analytical, generational, analytical, global).

% Will be bound by whatever tempo is set now, but have no voice in setting it — the default tempo is fixed by current agenda-setters (legislatures, platform designers) years or decades before the people it will eventually bind are even in a position to object. Their pressure-durations are unknown at design time, so the mismatch is baked in before their case exists.
narrative_ontology:constraint_stakeholder(tempo_margin_regime, future_bound_parties_not_yet_subject_to_the_rule, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tempo_margin_regime, whoever_sets_the_default_tempo_of_an_institution_e_g_marriage_law_platform_defaults).
narrative_ontology:fixing_cost_class(tempo_margin_regime, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides institutions with a stable, low-negotiation-cost default for how quickly a binding rule can be revised, so that every individual case does not require bespoke negotiation of revision speed against expected pressure duration — genuinely useful when the tempo is roughly right for the population it governs.
% TRANSFER_FUNCTION: Moves the cost of tempo-mismatch from the party who sets the default (who bears no consequence for guessing wrong) to the party who is bound by it (who inherits either a too-fast lock-in on a transient state or a too-slow lock-in that outlives its justifying pressure).
% ABSENT_VOICES: Future bound parties, whose actual pressure-durations are unknown at the moment the tempo default is fixed, have no seat in setting it; by the time their mismatch becomes visible, the tempo is already institutionalized and difficult to revisit case-by-case.
% DISAPPEARANCE_RATIONALE: If tempo defaults vanished and every rule required bespoke revision-speed negotiation matched to each case's actual pressure duration, the coordination savings (no negotiation overhead) would disappear, transaction costs would rise sharply, but the two failure modes (intention-collapse and coffin-calcification) would also disappear — institutions would have to justify their tempo case by case, and both the beneficiary's free option and the victim's forced mismatch would end.
% FOUNDING_PROBLEM: Institutions need a workable default answer to 'how hard should it be to change this rule' without re-litigating the question for every individual case — the tempo default exists to avoid the administrative cost of bespoke revision-speed calibration.
% FOUNDING_PROBLEM_CORROBORATION: Instrument designers and comparative-institutional researchers (family-law reform commissions, behavioral-economics critiques of platform dark patterns) attest that many current tempo defaults were set for administrative convenience or extraction and have never been re-benchmarked against actual pressure-duration data; agenda-setters themselves generally attest the tempo is fine as set, which is exactly the corroboration this genealogy discounts as self-interested.
narrative_ontology:disappearance_verdict(tempo_margin_regime, world_rearranges).
narrative_ontology:founding_problem_status(tempo_margin_regime, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tempo_margin_regime, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-16',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(tempo_margin_regime, 'none', 1).
narrative_ontology:epsilon_provenance(tempo_margin_regime, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tempo_margin_regime_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tempo_margin_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tempo_margin_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects that the mismatch cost is real and systematically transferred, but is lower than a pure snare because the underlying coordination function (a stable default so every case need not be renegotiated) is genuine and does sometimes land close to correct. Suppression (0.68) is high because both failure directions require active enforcement to persist — a too-fast lock-in requires refusing later reconsideration; a too-slow lock-in requires actively blocking dissolution or revision even once the justifying pressure has visibly ended. Theater ratio is moderate and rising (0.14→0.30): institutions increasingly perform 'flexibility' (cooling-off periods, opt-out clauses) that do not actually recalibrate the tempo to the case's real pressure duration, producing a growing gap between claimed responsiveness and actual mismatch correction.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter's seat, the tempo default looks like a rope: a sensible, low-overhead coordination mechanism serving the average case well. From the bound party's seat — especially one whose actual pressure duration falls far from the population average the default was calibrated to — the same structure operates as an enforced mismatch with no individualized remedy. The engine's per-seat computation should register this divergence: agenda-setter directionality sits near the beneficiary end (institutional power, arbitrage exit — they can redesign the rule at will), while the bound-party seat sits near the full-target end (powerless, trapped).
 *
 * DIRECTIONALITY LOGIC:
 *   Whoever sets the default tempo benefits from administrative simplicity and often direct fees tied to the chosen tempo (renewal costs, switching penalties), while bearing none of the downstream cost of miscalibration — this derives a low d. Those bound by a mismatched tempo bear the cost of either a captured transient state or an outlived commitment, with trapped or constrained exit — this derives a high d. The two test-case groups (fast/slow mismatch) are differentiated instances of the same victim structure, included separately to show the mismatch is bidirectional, not simply 'too much enforcement' in one direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two collapse errors: treating the whole regime as pure extraction (which would miss that a correctly-calibrated tempo default is a genuine coordination good — most doorstep cooling-off periods and most divorce waiting periods are roughly proportionate to typical pressure duration) and treating it as pure coordination (which would miss that misaligned defaults persist via active enforcement long after their calibration has become visibly wrong, extracting from a stable, identifiable population). The founding-problem genealogy shows the coordination function is live in general but frequently contested in specific instantiations, which is exactly the tangled_rope signature — coordination and extraction riding the same structure, separable in principle but not currently separated in practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tempo_margin_measurement_standard,
    'Is there a principled, domain-general way to measure ''pressure duration'' independent of the very rule being evaluated, or does the rule itself partly shape how long the pressure appears to last?',
    'Comparative natural experiments: jurisdictions or platforms that vary revision-speed while holding the underlying circumstance constant (e.g., comparing divorce-waiting-period lengths across otherwise similar legal systems) could isolate whether pressure-duration is measurable independent of rule design.',
    'If pressure duration is not independently measurable, the tempo-margin diagnosis risks circularity — the ''correct'' tempo could be read off from whatever tempo currently prevails, undermining the claim that mismatch is empirically detectable rather than definitionally assumed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tempo_margin_measurement_standard, empirical, 'Whether pressure duration can be measured independently of the rule being evaluated.').

omega_variable(
    instrument_axis_independence_from_kernel,
    'Does the tempo-margin failure mode genuinely survive agnosticism among all four kernel readings of authentic_preference_boundary, or does identifying ''mismatch'' implicitly presuppose one reading (e.g., the genealogical reading''s assumption that a determinate fact about origin-timing exists to be mismatched against)?',
    'Check whether the tempo-margin diagnosis can be restated purely in terms of observable case outcomes (does the bound party later report regret, seek revision, or exit if given the option) without any claim about the metaphysical status of their preference — if so, independence holds.',
    'If the tempo axis secretly depends on one kernel reading, this story is not structurally independent as claimed and should be re-linked more tightly to that reading''s constraint file rather than presented as reading-agnostic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrument_axis_independence_from_kernel, conceptual, 'Whether the instrument-engineering axis is truly independent of the contested authenticity kernel.').

omega_variable(
    who_bears_redesign_cost,
    'When a tempo mismatch is identified, who bears the cost of redesigning the instrument — the original agenda-setter, or a new institutional actor — and does that redesign cost itself create a second-order tempo-margin problem?',
    'Track historical cases of tempo-default reform (e.g., no-fault divorce reform, platform cooling-off-period mandates) for how long the reform process itself took relative to the harm accumulating under the old tempo.',
    'If redesign is itself slow relative to accumulating harm, the tangled_rope classification may understate persistence — the regime could better resemble a piton in jurisdictions where reform has stalled entirely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(who_bears_redesign_cost, empirical, 'Whether the cost of fixing a mismatched tempo generates a second-order mismatch of its own.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tempo_margin_regime, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, tempo_margin_regime, theater_ratio, 0, 0.14).
narrative_ontology:measurement(temp_tr_t8, tempo_margin_regime, theater_ratio, 8, 0.18).
narrative_ontology:measurement(temp_tr_t16, tempo_margin_regime, theater_ratio, 16, 0.21).
narrative_ontology:measurement(temp_tr_t24, tempo_margin_regime, theater_ratio, 24, 0.25).
narrative_ontology:measurement(temp_tr_t32, tempo_margin_regime, theater_ratio, 32, 0.28).
narrative_ontology:measurement(temp_tr_t40, tempo_margin_regime, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, tempo_margin_regime, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(temp_be_t8, tempo_margin_regime, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(temp_be_t16, tempo_margin_regime, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(temp_be_t24, tempo_margin_regime, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(temp_be_t32, tempo_margin_regime, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(temp_be_t40, tempo_margin_regime, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, tempo_margin_regime, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(temp_su_t8, tempo_margin_regime, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(temp_su_t16, tempo_margin_regime, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(temp_su_t24, tempo_margin_regime, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(temp_su_t32, tempo_margin_regime, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(temp_su_t40, tempo_margin_regime, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tempo_margin_regime, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tempo_margin_regime, 0.12).
narrative_ontology:affects_constraint(tempo_margin_regime, authentic_preference_boundary_behaviorist_counterfactual_reading).
narrative_ontology:affects_constraint(tempo_margin_regime, authentic_preference_boundary_genealogical_origin_reading).

% DUAL FORMULATION NOTE:
% This constraint is deliberately NOT a reading of the authentic_preference_boundary kernel — it is a structurally independent claim about instrument engineering (rule-revision-speed vs. pressure-duration matching) that holds regardless of which of the four kernel readings is correct. It is linked via affects_constraints because the tempo-margin failure interacts with kernel readings downstream: the behaviorist_counterfactual_reading's re-exposure test presupposes some revision opportunity exists, which is exactly what a too-slow tempo forecloses, and the genealogical_origin_reading's tolerance for permanently unknowable facts interacts with a too-fast tempo's tendency to lock in origin-timing before any genealogy is even legible. No reading_relations or axioms are authored here since this is not itself a kernel reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
