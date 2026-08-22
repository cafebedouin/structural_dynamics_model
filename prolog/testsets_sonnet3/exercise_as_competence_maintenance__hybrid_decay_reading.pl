% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__hybrid_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__hybrid_decay_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Simulation-Based Competence Maintenance (Hybrid Decay Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This story instantiates the hybrid_decay_reading of the
 *   exercise_as_competence_maintenance kernel: the competence being
 *   maintained is not one thing but two — a procedural/muscle-memory
 *   component that scripted simulation genuinely exercises and retains, and a
 *   judgment-under-stakes/improvisation component that simulation, by its
 *   nature as a lower-stakes rehearsal of known scenarios, does not exercise
 *   at all. The arrangement is coordinative for the first component and
 *   quietly extractive for the second: institutions certify total
 *   preparedness on the strength of the procedural half while the judgment
 *   half decays or was never built, and the gap surfaces only when a real
 *   incident departs from the script. Victims are frontline responders forced
 *   to improvise beyond what their exercise regime prepared them for, and the
 *   populations who bear the consequences of that improvisation gap.
 *
 * KEY AGENTS:
 *   - exercise_program_administrators: agenda_setter (institutional/arbitrage) — designs and certifies the exercise regime, captures compliance and budget benefit from completion metrics
 *   - regulatory_compliance_officers: beneficiary (institutional/constrained) — discharges its own mandate by accepting exercise logs as proof of preparedness
 *   - simulation_vendors: beneficiary (organized/mobile) — revenue tied to exercise frequency, not verified judgment retention
 *   - frontline_responders_facing_novel_crises: payer (moderate/trapped) — retains procedural competence but bears the judgment gap under real stakes
 *   - populations_affected_by_improvisation_failures: payer (powerless/trapped) — bears consequences when improvisation fails
 *   - veteran_incident_commanders: excluded (moderate/constrained) — dissenting testimony about the procedure/judgment gap is not incorporated
 *   - safety_science_researchers: observer (analytical/analytical) — documents the gap empirically without design authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.58).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.52).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Simulation-Based Competence Maintenance (Hybrid Decay Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, 'bb574a1f-5d8d-4429-b8e9-ca7678675330').
narrative_ontology:cs_kernel_codification('bb574a1f-5d8d-4429-b8e9-ca7678675330', distributed).
narrative_ontology:cs_authority_grounding('bb574a1f-5d8d-4429-b8e9-ca7678675330', practice).
narrative_ontology:cs_interpretation_layer_present('bb574a1f-5d8d-4429-b8e9-ca7678675330').
narrative_ontology:cs_reading_relation('bb574a1f-5d8d-4429-b8e9-ca7678675330', exercise_as_competence_maintenance__simulation_sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb574a1f-5d8d-4429-b8e9-ca7678675330', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, influences).
narrative_ontology:cs_axiom('bb574a1f-5d8d-4429-b8e9-ca7678675330', foundational, competence_kernel_is_componentially_divisible).
narrative_ontology:cs_axiom_status(competence_kernel_is_componentially_divisible, holdable).
narrative_ontology:cs_axiom_grounding('bb574a1f-5d8d-4429-b8e9-ca7678675330', competence_kernel_is_componentially_divisible, empirically_contingent).
narrative_ontology:cs_axiom('bb574a1f-5d8d-4429-b8e9-ca7678675330', foundational, judgment_under_stakes_requires_stakes_not_merely_fidelity).
narrative_ontology:cs_axiom_status(judgment_under_stakes_requires_stakes_not_merely_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('bb574a1f-5d8d-4429-b8e9-ca7678675330', judgment_under_stakes_requires_stakes_not_merely_fidelity, empirically_contingent).
narrative_ontology:cs_reference_frame('bb574a1f-5d8d-4429-b8e9-ca7678675330', procedural_drill_as_full_readiness_proxy).
narrative_ontology:cs_drift_state('bb574a1f-5d8d-4429-b8e9-ca7678675330', post_major_incident_reviews, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bb574a1f-5d8d-4429-b8e9-ca7678675330', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_program_administrators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, regulatory_compliance_officers).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_vendors).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders_facing_novel_crises).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, populations_affected_by_improvisation_failures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and mandate the drill/simulation calendar, certify staff as 'exercised,' and report completion rates upward as the institution's evidence of preparedness. Their career and budget position depend on exercise throughput, not on how well judgment holds up in a genuinely novel emergency. They control which scenarios get simulated and which are declared 'covered' by procedure.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Accept completed simulation logs as satisfying preparedness mandates. Auditing actual judgment-under-stakes performance is expensive and legally fraught, so they certify against the cheaper, legible proxy — exercise completion — which discharges their own compliance obligation regardless of whether it tracks the underlying capacity.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, regulatory_compliance_officers, beneficiary,
    institutional, biographical, constrained, national).

% Sell scenario software, tabletop kits, and certification packages. Revenue scales with exercise frequency and scripted-scenario coverage, not with any independently verified improvement in improvisational capacity, so they have no structural incentive to highlight the procedure/judgment gap their product cannot close.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_vendors, beneficiary,
    organized, biographical, mobile, national).

% Drill repeatedly on scripted scenarios and genuinely retain procedural fluency and muscle memory from this. But when a real event departs from the rehearsed script — the novel failure mode, the compounding cascading fault, the scenario nobody scripted — they must improvise under stakes with a capacity the simulation never actually exercised. They cannot opt out of the exercise regime and cannot, from inside it, generate the missing judgment component themselves.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders_facing_novel_crises, payer,
    moderate, immediate, trapped, local).

% Are on the receiving end when a real crisis diverges from the drilled script and the responders' judgment-under-stakes capacity proves thinner than their procedural competence suggested. They have no visibility into which parts of preparedness are genuinely exercised versus certified-but-untested, and no channel to demand the distinction be made before the failure occurs.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, populations_affected_by_improvisation_failures, payer,
    powerless, immediate, trapped, regional).

% Have lived through genuine novel-crisis judgment calls and often argue internally that the exercise regime over-credits procedural drilling and under-credits the improvisational capacity that only real stakes seem to build. Their dissent is treated as anecdotal and is rarely incorporated into how compliance officers or administrators define 'exercised.'
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, veteran_incident_commanders, excluded,
    moderate, biographical, constrained, regional).

% Study post-incident reports and simulation validity, documenting where drilled procedures held and where judgment failed under real-world deviation. Their findings inform the hybrid-decay account but are not binding on program design, since administrators and compliance officers are not obligated to revise the exercise regime in response to the research.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, safety_science_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__hybrid_decay_reading, diffuse).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__hybrid_decay_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simulation genuinely coordinates and maintains procedural competence: scripted drills build real, transferable muscle memory for the sequences that recur across incidents, and this component of the exercise regime solves a real organizational-learning problem.
% TRANSFER_FUNCTION: Moves institutional and public confidence (certified 'preparedness') from actual judgment-under-stakes capacity to a legible proxy (exercise completion), and moves the residual risk of the un-exercised judgment component onto frontline responders and the populations they serve when real crises depart from script.
% ABSENT_VOICES: Veteran incident commanders who have lived through genuine improvisation-under-stakes routinely flag the procedure/judgment gap, but their testimony is treated as anecdotal against the legible metrics compliance officers rely on; safety science researchers document the gap empirically but hold no authority to revise program design.
% DISAPPEARANCE_RATIONALE: If the exercise regime vanished, the procedural-competence component would genuinely atrophy — that part of the world does depend on it. But the judgment-under-stakes component the regime claims to maintain was never actually being exercised by simulation in the first place, so its disappearance would not change real-crisis outcomes for that component at all. Administrators and compliance officers would say the world rearranges catastrophically; safety researchers and veteran commanders would say only the procedural half rearranges, because the judgment half was already unaddressed.
% FOUNDING_PROBLEM: Organizations needed a repeatable, auditable way to maintain operational readiness for crisis response without waiting for real disasters to occur, and to demonstrate that readiness to regulators, boards, and the public.
% FOUNDING_PROBLEM_CORROBORATION: Safety science researchers, publishing outside the certifying bodies, corroborate that the procedural half of the founding problem remains genuinely addressed by simulation, while post-incident analyses (also produced outside the administering institutions) corroborate that the judgment-under-stakes half of the founding problem was never actually solved by the exercise regime and remains live. Administrators and compliance officers, who benefit from the completion-rate proxy, are the primary source asserting the founding problem is fully resolved by current practice; that assertion is not corroborated by any party outside the certifying chain.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, contested).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and theater_ratio (0.61) sit at moderate-to-elevated levels because a real coordination function (procedural drilling) is being used to certify a claim (total preparedness) that only partially holds — this is not a story of pure fabrication, but of one genuinely-exercised component subsidizing the reputation of a second, un-exercised component. Suppression (0.52) reflects that dissenting evidence (veteran commanders, safety researchers) exists and is voiced but structurally discounted, not silenced outright. Accessibility_collapse (0.45) is moderate: alternatives to the current exercise regime (e.g., high-fidelity crisis simulation, deliberate judgment-stress training) exist and are known, but institutional incentives keep the cheaper procedural-completion proxy dominant. Resistance (0.55) reflects active internal pushback from practitioners who have experienced the gap directly.
 *
 * DIRECTIONALITY LOGIC:
 *   Administrators, compliance officers, and vendors sit near the beneficiary end: each captures value (budget legitimacy, regulatory discharge, revenue) from exercise completion as a metric, independent of whether the judgment-under-stakes component is actually being maintained. Frontline responders and affected populations sit near the target end: they bear the consequence when the un-exercised judgment component fails under real stakes, and their exit options are trapped — a responder cannot decline deployment to a novel incident, and an affected population cannot select for better-trained responders in the moment. Veteran commanders occupy an intermediate position: they see the gap but their moderate power and constrained exit limit their ability to force revision.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents two mislabeling errors symmetric to the sibling readings: it does not, as the simulation_sufficiency_reading would, certify the whole competence kernel as maintained by simulation (which would erase the judgment-decay victims entirely and read as pure rope); and it does not, as the lived_catastrophe_necessity_reading would, declare simulation worthless for the procedural component (which would erase the genuine coordination function and misclassify the whole arrangement as pure snare). By splitting the kernel into two components with different exercise requirements, the tangled_rope classification here holds only the genuinely tangled claim — real coordination on one axis, real extraction (via mis-certification) on the other — without averaging the two into a single blurred ε.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_componentiality,
    'Is the competence kernel genuinely decomposable into a procedural component and a judgment-under-stakes component with different exercise requirements, or is this decomposition itself an artifact of how simulation happens to be designed (i.e., simulation designers script for what they can script, and ''judgment'' is just the residual)?',
    'Compare post-incident performance data across responders with high procedural-drill exposure but varying real-incident (non-simulated) exposure, controlling for tenure; if judgment-under-stakes performance tracks real-incident exposure independent of drill frequency, componentiality is supported.',
    'If the kernel does not decompose cleanly, this reading collapses toward either simulation_sufficiency_reading (if judgment turns out to be trainable via better simulation) or lived_catastrophe_necessity_reading (if judgment turns out to require real stakes categorically, not as a matter of degree).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_componentiality, conceptual, 'Whether the two-component kernel model is a real structural distinction or a byproduct of simulation design limits.').

omega_variable(
    reading_selection_evidence,
    'Which reading of the exercise_as_competence_maintenance kernel does the available post-incident and near-miss evidence actually support — hybrid decay, simulation sufficiency, or lived-catastrophe necessity?',
    'Systematic meta-analysis of after-action reports across high-reliability organizations, coded for whether failures occurred in procedurally-scripted sequences (would support simulation_sufficiency failing) versus in judgment/improvisation moments outside scripted sequences (would support hybrid_decay or lived_catastrophe).',
    'Directly bears on which of the three sibling constraints most accurately describes the operative reality, and thus which victim set (if any) is real versus an artifact of this reading''s framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_evidence, empirical, 'Committer-structure omega: names the kernel contest and what would move it toward one reading over the siblings.').

omega_variable(
    administrator_awareness_of_gap,
    'Do exercise program administrators privately understand the procedural/judgment distinction and choose completion metrics anyway (extraction with knowledge), or do they genuinely believe exercise completion tracks total preparedness (extraction without malice, via honest but mistaken metric substitution)?',
    'Internal program design documents, budget justifications, and interviews with administrators about how they weigh drill frequency against novel-scenario stress-testing investment.',
    'If administrators knowingly substitute the cheaper metric, the constraint reads closer to a deliberately maintained tangled_rope bordering on snare; if the substitution is unwitting Goodhart drift, it is better read as an emergent piton-adjacent failure of measurement rather than intentional extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(administrator_awareness_of_gap, empirical, 'Whether the metric-substitution driving extraction is knowing or an artifact of honest measurement error.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(exer_tr_t4, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 8, 0.44).
narrative_ontology:measurement(exer_tr_t12, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 16, 0.54).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 24, 0.61).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(exer_be_t4, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(exer_be_t12, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(exer_su_t4, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(exer_su_t12, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__hybrid_decay_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, lived_catastrophe_necessity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'exercising the competence kernel' (the exercise_as_competence_maintenance kernel). simulation_sufficiency_reading claims simulation fully exercises the whole kernel (near-rope, low ε from that reading's own lights). lived_catastrophe_necessity_reading claims simulation exercises nothing structurally real and only genuine catastrophe maintains competence (higher ε, different victim framing — near-snare from that reading's lights, since the entire simulation apparatus is read as theater). This hybrid_decay_reading occupies the middle: partial genuine coordination (procedural) plus partial extraction via mis-certification (judgment), yielding tangled_rope. All three share the same underlying kernel and are linked via affects_constraints; each authors its own stable ε rather than averaging across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
