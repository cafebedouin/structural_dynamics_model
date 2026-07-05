% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Instrumentalist Reading of Orthographic Legitimacy (Literacy/Administrative Efficiency)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   A state undertakes a national script reform — replacing an older writing
 *   system with a phonetically regular alternative — and grounds the reform's
 *   legitimacy exclusively in literacy statistics and administrative cost
 *   savings. This is the instrumentalist reading of a contested
 *   orthographic-legitimacy kernel: the same reform can be, and elsewhere is,
 *   justified by rupture-from-tradition narratives (modernist reading) or
 *   resisted in the name of continuity with religious and literary heritage
 *   (continuity reading). This story treats ONLY the instrumentalist
 *   justification as its own constraint, with its own ε, its own
 *   beneficiary/victim structure, and its own metrics — it does not average
 *   across the sibling readings or describe the contest between them within
 *   this file.
 *
 * KEY AGENTS:
 *   - reforming_state_authority: agenda_setter (institutional/arbitrage) — designs and enforces the transition
 *   - newly_literate_population: beneficiary (moderate/constrained) — gains low-cost literacy access
 *   - state_administrative_apparatus: beneficiary/agenda_setter (institutional/arbitrage) — collects the efficiency dividend
 *   - arabic_script_literate_elite: payer (powerful/trapped) — sunk-cost skill devaluation
 *   - religious_scholarly_class: payer/excluded (organized/constrained) — interpretive authority weakened, voice excluded from the justificatory frame
 *   - literacy_statisticians: observer (analytical/analytical) — produce the evidentiary basis the reading depends on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.42).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.55).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Instrumentalist Reading of Orthographic Legitimacy (Literacy/Administrative Efficiency)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, 'e6b82b84-735f-40b3-ab44-c0aa458cce67').
narrative_ontology:cs_kernel_codification('e6b82b84-735f-40b3-ab44-c0aa458cce67', distributed).
narrative_ontology:cs_authority_grounding('e6b82b84-735f-40b3-ab44-c0aa458cce67', extraction).
narrative_ontology:cs_interpretation_layer_present('e6b82b84-735f-40b3-ab44-c0aa458cce67').
narrative_ontology:cs_reading_relation('e6b82b84-735f-40b3-ab44-c0aa458cce67', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6b82b84-735f-40b3-ab44-c0aa458cce67', orthographic_legitimacy_kernel__continuity_reading, influences).
narrative_ontology:cs_axiom('e6b82b84-735f-40b3-ab44-c0aa458cce67', foundational, script_is_a_pragmatic_tool_not_an_identity_marker).
narrative_ontology:cs_axiom_status(script_is_a_pragmatic_tool_not_an_identity_marker, holdable).
narrative_ontology:cs_axiom_grounding('e6b82b84-735f-40b3-ab44-c0aa458cce67', script_is_a_pragmatic_tool_not_an_identity_marker, instrumental).
narrative_ontology:cs_axiom('e6b82b84-735f-40b3-ab44-c0aa458cce67', foundational, legitimacy_is_measured_by_literacy_and_efficiency_statistics).
narrative_ontology:cs_axiom_status(legitimacy_is_measured_by_literacy_and_efficiency_statistics, holdable).
narrative_ontology:cs_axiom_grounding('e6b82b84-735f-40b3-ab44-c0aa458cce67', legitimacy_is_measured_by_literacy_and_efficiency_statistics, empirically_contingent).
narrative_ontology:cs_reference_frame('e6b82b84-735f-40b3-ab44-c0aa458cce67', pre_reform_administrative_literacy_baseline).
narrative_ontology:cs_drift_state('e6b82b84-735f-40b3-ab44-c0aa458cce67', post_transition_normalization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e6b82b84-735f-40b3-ab44-c0aa458cce67', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_script_literate_elite).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, religious_scholarly_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates the new Latin-based script through law, school curricula, and official document standards, justifying the change with literacy rate targets and administrative simplification metrics. Enforces adoption by making the old script legally void for state business within a set transition window.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, reforming_state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Rural and working-class populations who had little or no access to Arabic-script literacy under the old system gain a script with regularized phonetic mapping, dramatically lowering the barrier to reading and writing. Their exit option from the reform is minimal because they had little invested in the prior system to lose.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population, beneficiary,
    moderate, biographical, constrained, national).

% Census-taking, taxation, conscription, and legal record-keeping become measurably faster and cheaper with a phonetic script that maps more directly to bureaucratic training pipelines. This efficiency gain is the primary evidentiary basis the reform cites for its own legitimacy.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus, agenda_setter).

% Scribes, bureaucrats, and educated professionals whose decades of acquired Arabic-script literacy are abruptly devalued as a professional asset. They cannot simply re-skill fast enough to preserve their institutional standing, and the reform offers no compensation for the sunk cost of their training.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_script_literate_elite, payer,
    powerful, biographical, trapped, national).

% Scholars whose authority rested on transmitting religious and legal texts in the old script find their interpretive monopoly weakened as the new generation is schooled in a script that does not directly connect to the received textual tradition. Their objections are framed by the state as sentimental rather than administrative, and are structurally excluded from the literacy-rate justification the reform runs on.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, religious_scholarly_class, payer,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__instrumentalist_reading, religious_scholarly_class, excluded).

% Compile the census and school-enrollment literacy figures the reform cites as its justification. Their measurements are the evidentiary spine of the instrumentalist claim, and their methodology choices (what counts as literate, over what time horizon) materially shape whether the reform appears successful.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, literacy_statisticians, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__instrumentalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, phonetically regular writing system taught uniformly in state schools, lowering the cost of achieving basic literacy and standardizing the training pipeline for administrators, teachers, and clerks across the national territory.
% TRANSFER_FUNCTION: Moves literacy-acquisition cost from the population (via a simpler script) to the previously credentialed literate class (via devaluation of a specialized, hard-won skill); moves administrative training and processing cost from the state apparatus to a one-time transition cost borne largely by displaced scribes and religious scholars.
% ABSENT_VOICES: The religious scholarly class objects that literacy-rate statistics say nothing about continuity of access to centuries of legal and devotional texts, but the instrumentalist framing has no metric for that loss and does not include it in the reform's justificatory apparatus.
% DISAPPEARANCE_RATIONALE: If the instrumentalist justification were withdrawn tomorrow, the reform itself might still stand on political fiat, but its legitimacy claim would collapse — the state would need a different justificatory story (continuity, modernity, or naked assertion of authority), literacy-rate reporting would lose its evidentiary function, and the devalued elite would have new grounds to contest the reform as unjustified rather than merely painful.
% FOUNDING_PROBLEM: Low national literacy rates and slow, costly administrative processes were attributed in part to the old script's irregular phonetic mapping and steep learning curve, which functionally limited literacy to a narrow trained class.
% FOUNDING_PROBLEM_CORROBORATION: State-commissioned literacy statisticians attest the problem was real and substantially resolved by the reform. Independent linguists and later social historians outside the reforming administration corroborate that literacy rates did rise measurably post-reform, but also document that administrative efficiency gains were partly a byproduct of broader centralization efforts occurring simultaneously, making the script change's independent causal contribution harder to isolate than the state's own reporting claimed.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).
:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) because the reform genuinely lowers the cost of literacy acquisition for the previously excluded majority — this is real coordination, not a pure rent-seeking gloss. But it is not zero: the elite's skill devaluation is a real, uncompensated transfer running through the same structure that produces the literacy gain, and the state administrative apparatus captures an efficiency dividend it did not have to share with those it displaced. Suppression starts high (0.75) during the enforced transition window (old-script documents voided, curricula mandated) and falls as the reform normalizes and active coercion is no longer needed to sustain adoption — a genuine enforcement-decay trajectory, not a static picture, which is why suppression_requirement is tracked temporally here rather than left as a flat scalar.
 *
 * PERSPECTIVAL GAP:
 *   From the state administrative apparatus's seat, the reform looks like clean coordination success measured in falling training costs and rising census-literacy figures. From the Arabic-script literate elite's seat, the identical structure is an uncompensated professional expropriation justified by statistics that never mention them. The engine computes these as different seat-level classifications from the same structural facts; this story does not adjudicate which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   The newly literate population and the state administrative apparatus sit near the beneficiary end: the former gains an asset (basic literacy) it did not have, the latter gains measurable efficiency without bearing the retraining cost. The Arabic-script literate elite and the religious scholarly class sit near the target end: their capital (specialized script literacy, interpretive authority) is devalued by the same act that produces the literacy gain, and their exit options are poor — professional reinvention is slow for the elite (trapped), and the scholarly class's exit is merely constrained because some institutional standing persists but is structurally weakened.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (low literacy, slow administration under the old script) is genuinely contested as to whether it is dead or live: literacy rates did rise, corroborated by sources outside the reforming administration, but administrative efficiency gains are partially confounded with simultaneous centralization efforts, meaning some of the credited efficiency was never purely a script effect. This keeps the reading from being either a clean Rope (coordination fully vindicated) or a clean Snare (pure extraction dressed as coordination) — it is read here as rope-leaning with a real, uncompensated victim class, which is the honest middle the metrics (moderate ε, real beneficiary, real victim, declining but nonzero suppression) are meant to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrumentalist_reading_is_one_of_three,
    'This story treats the instrumentalist justification (literacy/efficiency) as the operative legitimacy claim for the script reform. The same reform historically carried at least two other justificatory readings — modernist (civilizational rupture) and continuity (preservation of tradition) — held simultaneously by different factions of the same reforming coalition and its opponents. Which reading was doing the actual legitimating work at any given moment is underdetermined by the historical record.',
    'Archival analysis of contemporaneous state rhetoric (parliamentary debates, ministerial decrees, school curricula framing documents) coded for which justificatory register dominates at each phase of the reform''s rollout; compare against the literacy-statistics reading''s predicted timeline (front-loaded during the enforcement window, receding as the reform normalizes).',
    'If the instrumentalist framing was a retrospective rationalization laid over a reform actually driven by modernist rupture logic, this story''s beneficiary/victim structure remains valid on its own terms as one reading, but its explanatory priority over the sibling readings should be downweighted in any composite account of why the reform happened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalist_reading_is_one_of_three, conceptual, 'Whether the instrumentalist justification was the reform''s true operative logic or a partial post-hoc rationalization among competing kernel readings.').

omega_variable(
    literacy_statistics_measurement_validity,
    'Are the literacy-rate statistics that ground this reading''s legitimacy claim measuring what the state apparatus claims they measure (population-wide functional literacy), or are they measuring something narrower (school-enrollment proxy, urban-biased sampling) that overstates the reform''s coordination success?',
    'Independent re-analysis of the underlying census methodology by historians or linguists outside the reforming administration''s statistical apparatus, comparing enrollment figures against functional literacy assessments where available.',
    'If the statistics overstate literacy gains, the beneficiary population''s true benefit is smaller than claimed, pushing the story''s ε upward and weakening the rope-leaning classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_statistics_measurement_validity, empirical, 'Whether the reform''s own evidentiary basis (literacy statistics) is methodologically sound or self-serving.').

omega_variable(
    elite_devaluation_compensable,
    'Was the Arabic-script literate elite''s skill devaluation an unavoidable byproduct of a genuinely beneficial coordination reform, or could the state have structured a transition (grandfathering, retraining subsidies, dual-script transition period) that preserved the literacy gains while compensating the displaced class?',
    'Comparative case analysis against other script-reform states that did implement transition compensation mechanisms, measuring whether literacy gains were comparably achieved without the uncompensated elite displacement.',
    'If compensation was feasible and simply not offered, the victim harm becomes a policy choice rather than a structural necessity of the coordination function, which would push the classification from rope-leaning toward tangled_rope even at the same ε value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_devaluation_compensable, preference, 'Whether the uncompensated elite harm was structurally necessary to the reform or a discretionary policy choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(orth_tr_t8, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(orth_tr_t16, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(orth_tr_t24, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(orth_tr_t32, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(orth_tr_t40, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(orth_be_t8, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(orth_be_t16, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(orth_be_t24, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(orth_be_t32, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 32, 0.38).
narrative_ontology:measurement(orth_be_t40, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(orth_su_t8, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(orth_su_t16, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(orth_su_t24, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(orth_su_t32, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(orth_su_t40, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__instrumentalist_reading, 0.1).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of a single contested kernel (orthographic_legitimacy_kernel): instrumentalist (this file, moderate rope-leaning ε grounded in literacy/efficiency statistics), modernist (legitimacy via civilizational rupture and alignment with Western modernity — expected higher suppression and a more identity-coded victim/beneficiary structure), and continuity (legitimacy via preservation of religious/literary access — expected to invert the beneficiary/victim structure relative to this reading, with the Arabic-script literate and religious scholarly classes as beneficiaries of continuity rather than victims of instrumentalist reform). Each reading is authored as its own constraint with its own ε; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
