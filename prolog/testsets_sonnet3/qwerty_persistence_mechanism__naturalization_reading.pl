% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__naturalization_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Keyboard Layout — Adequacy/Fair-Competition Reading
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This story instantiates the naturalization reading of the
 *   QWERTY-persistence kernel: the claim that QWERTY's dominance today
 *   reflects genuine, if unglamorous, adequacy rather than either
 *   coordination failure (the lock-in reading) or active suppression of
 *   alternatives by a beneficiary class (the beneficiary-extraction reading).
 *   On this reading, the original mechanical rationale for the layout is
 *   dead, but a new and independent justification took its place: a century
 *   of accumulated typing skill, universal hardware/software support for
 *   switching that goes largely unused, and an empirical record on
 *   alternative layouts (chiefly Dvorak) that never robustly demonstrated the
 *   efficiency gains its advocates claimed. The extraction and suppression
 *   scores are authored low and largely flat because this reading holds that
 *   no systematic beneficiary class is capturing rents from lock-in —
 *   switching is cheap at the hardware/software layer and the real cost of
 *   switching is the typist's own sunk skill investment, which is not
 *   extraction, it is the coordination benefit doing its job.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.18).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Keyboard Layout — Adequacy/Fair-Competition Reading").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic/technological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, 'dcc9a0a2-e734-4a05-bc21-f1a181394f7f').
narrative_ontology:cs_kernel_codification('dcc9a0a2-e734-4a05-bc21-f1a181394f7f', distributed).
narrative_ontology:cs_authority_grounding('dcc9a0a2-e734-4a05-bc21-f1a181394f7f', practice).
narrative_ontology:cs_reading_relation('dcc9a0a2-e734-4a05-bc21-f1a181394f7f', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('dcc9a0a2-e734-4a05-bc21-f1a181394f7f', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('dcc9a0a2-e734-4a05-bc21-f1a181394f7f', foundational, standard_adequacy_survives_original_rationale_death).
narrative_ontology:cs_axiom_status(standard_adequacy_survives_original_rationale_death, holdable).
narrative_ontology:cs_axiom_grounding('dcc9a0a2-e734-4a05-bc21-f1a181394f7f', standard_adequacy_survives_original_rationale_death, instrumental).
narrative_ontology:cs_axiom('dcc9a0a2-e734-4a05-bc21-f1a181394f7f', foundational, alternative_layout_advantage_empirically_unproven).
narrative_ontology:cs_axiom_status(alternative_layout_advantage_empirically_unproven, holdable).
narrative_ontology:cs_axiom_grounding('dcc9a0a2-e734-4a05-bc21-f1a181394f7f', alternative_layout_advantage_empirically_unproven, empirically_contingent).
narrative_ontology:cs_reference_frame('dcc9a0a2-e734-4a05-bc21-f1a181394f7f', mechanical_typebar_collision_avoidance).
narrative_ontology:cs_drift_state('dcc9a0a2-e734-4a05-bc21-f1a181394f7f', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dcc9a0a2-e734-4a05-bc21-f1a181394f7f', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, typists_trained_on_qwerty).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, software_input_ecosystem).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__naturalization_reading, layout_choice_is_a_solved_coordination_problem).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__naturalization_reading, dvorak_advantage_is_empirically_unsettled).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have invested years of practice into a specific motor skill on the QWERTY layout. That investment pays off precisely because everyone else made the same investment: a common layout means typing skill transfers across any job, any machine, any country. Switching to an alternative layout would mean re-earning fluency they already possess, for a benefit that remains genuinely unproven in rigorous testing.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, typists_trained_on_qwerty, beneficiary,
    organized, biographical, constrained, global).

% Manufacture keyboards to the layout the market demands. They are not defending QWERTY out of self-interest tied to switching costs — alternative-layout keyboards are cheap and simple to produce, and several manufacturers sell Dvorak and other layouts alongside QWERTY at no meaningful premium. They ship whatever sells.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers, beneficiary,
    organized, generational, mobile, global).

% Operating systems and firmware universally support layout remapping at negligible engineering cost. The software layer imposes no lock-in of its own; any user who wants Dvorak or Colemak can select it in a settings menu. The persistence of QWERTY as the default is a reflection of installed skill and demand, not an engineered barrier.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, software_input_ecosystem, beneficiary,
    institutional, generational, mobile, global).

% Argue Dvorak and similar layouts offer meaningful typing-speed and ergonomic advantages and that QWERTY's dominance reflects historical accident rather than merit. Under this reading, their empirical claims have not survived controlled testing (Liebowitz and Margolis's re-analysis of the original efficiency studies found the claimed Dvorak advantage was not well-supported), so their exclusion from the mainstream is a product of the evidence rather than of suppression.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, alternative_layout_advocates, excluded,
    moderate, biographical, mobile, global).

% Conduct comparative studies on layout efficiency across decades. Findings are genuinely mixed: some early studies (funded by Dvorak's own patent holder) claimed large advantages; later re-analyses found the original studies methodologically compromised. No study design has produced a robust, replicated, layout-driven productivity gap large enough to justify mass retraining costs.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, typing_speed_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__naturalization_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__naturalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, near-universal keyboard layout lets typing skill, muscle memory, keyboard manufacturing, software input handling, and touch-typing instruction all coordinate on one standard, so skill and hardware are portable across employers, countries, and decades without retraining.
% TRANSFER_FUNCTION: Under this reading, nothing systematic transfers from a victim class to a beneficiary class — the arrangement mostly locks in a shared standard that lets accumulated typing skill retain its value across contexts. What modest costs exist (foregone hypothetical efficiency gains from switching) are diffuse and speculative rather than a rent flowing to an identifiable extractor.
% ABSENT_VOICES: Alternative-layout advocates and some ergonomics researchers would object that the standard is arbitrary and imposes an efficiency cost, but their empirical case has not, on this reading, cleared the bar required to justify displacing an entrenched, functioning standard — their exclusion from mainstream adoption reflects unresolved evidence, not suppression.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight with no replacement standard, the coordination value it provides — universal portability of typing skill across devices, employers, and borders — would have to be rebuilt from scratch, likely converging back on some single dominant layout through the same competitive process, quite possibly QWERTY again given the scale of the installed skill base.
% FOUNDING_PROBLEM: Early typewriter mechanisms jammed when common letter pairs were struck in rapid succession from adjacent keys; QWERTY's arrangement was built to slow and stagger frequent key-pairings so mechanical typebars would not collide, and it stabilized as the shared standard once mechanical constraints were later resolved.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology (attesting from outside both manufacturer and typist interest) confirm the original mechanical jamming problem no longer exists in any modern keyboard; under this reading that does not indict the standard, because the layout's persistence has since been carried by genuine accumulated skill investment and unresolved efficiency evidence, not by the original mechanical rationale.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__naturalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__naturalization_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__naturalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__naturalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__naturalization_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__naturalization_reading_tests).
:- end_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18) is authored low because this reading finds no identifiable party siphoning value from the standard's persistence: manufacturers profit equally from any layout they sell, software switching costs are near zero, and the main 'cost' of switching (retraining time) falls on the same party who would receive any benefit, which is a self-borne investment decision rather than a transfer. Suppression (0.12) is low because no active mechanism blocks alternative-layout adoption — remapping is a settings-menu operation on every modern OS. Theater ratio is low and drifts up only slightly (0.10 to 0.15) reflecting a modest increase over time in institutional rhetoric defending the standard (ergonomics marketing, keyboard-shortcut lock-in narratives) without a corresponding functional change. Accessibility collapse is authored at a moderate 0.55, not high: alternatives are technically available and legally unconstrained, but the accumulated base of trained typists and the absence of a decisively proven substitute make switching unattractive in practice — this is friction from genuine specificity of investment, not suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, beneficiaries (typists, manufacturers, the software ecosystem) sit near the low end of directionality because the standard subsidizes the portability of their existing investment; there is no victim group declared because this reading does not find a class of agents who bear a net cost that a beneficiary class captures. Alternative-layout advocates are marked excluded rather than a victim group: they are outside the mainstream because their empirical case has not prevailed, which this reading treats as an evidentiary outcome, not an extraction outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical typebar jamming) is dead, which could look like a classic mandatrophy signature — a mandate that outlived its function, maintained by inertia. This reading resists that classification: it holds that the arrangement's justification shifted from the original mechanical rationale to a new, independently sufficient coordination rationale (skill portability, low switching cost, unproven substitute), so persistence is not zombie institutional inertia but a live coordination function riding on new grounds. The disappearance_verdict of world_rearranges reflects that stakeholders' arrangements do genuinely depend on the standard even though its original justification is gone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dvorak_advantage_empirical_status,
    'Do rigorous, unconflicted studies show a robust efficiency advantage for Dvorak or other alternative layouts over QWERTY sufficient to justify mass retraining costs?',
    'A meta-analysis restricted to studies with no funding or patent interest in the outcome, controlling for typist selection effects and training-hours confounds; compare against the Liebowitz-Margolis re-analysis of the original Navy/GSA studies.',
    'If a robust advantage is confirmed, this naturalization reading weakens substantially — persistence would look more like coordination failure (lock-in reading) than genuine adequacy. If the null result holds, this reading''s core claim (no proven substitute exists) is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_advantage_empirical_status, empirical, 'Whether the alternative-layout efficiency claim central to the lock-in reading is empirically supported.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is QWERTY''s persistence better explained as adequacy-plus-fair-competition (this reading), path-dependent lock-in despite inferiority (sibling reading), or active beneficiary maintenance (sibling reading) — and could more than one mechanism be simultaneously true at different points in the standard''s 150-year history?',
    'Historical process-tracing distinguishing the mechanical-era rationale (pre-1900s, mountain-like necessity), the mid-20th-century Dvorak contest era (where beneficiary incentives around patent and training-manual markets were live), and the post-digital era (where switching costs are purely software-trivial and the persistence question becomes almost entirely about accumulated skill).',
    'If the three eras call for three different readings, no single kernel reading is ''the'' true account of QWERTY — the kernel itself may be under-specified across time, and each reading is a claim about a specific era rather than the whole 150-year history. This story''s referent is the contemporary, post-digital arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the naturalization/lock-in/extraction split maps cleanly onto one era or must itself be time-indexed.').

omega_variable(
    switching_cost_ontology,
    'Is the typist''s sunk skill investment properly modeled as a self-borne coordination cost (this reading) or as a structural lock-in mechanism that a systemic beneficiary class free-rides on without bearing (lock-in reading)?',
    'Compare typist welfare outcomes in counterfactual scenarios (e.g., regions or cohorts that adopted alternative layouts early) against QWERTY-trained cohorts, controlling for selection.',
    'If switching-cost-bearing typists are systematically worse off than they would be under an alternative standard with equivalent adoption, this reading''s ''no systematic beneficiary'' claim is undermined and the classification would move toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_ontology, conceptual, 'Whether sunk skill investment is a self-borne cost or an extraction mechanism in disguise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 40, 0.19).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 60, 0.19).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 100, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_mechanism__naturalization_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__naturalization_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__naturalization_reading, 0.02).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the qwerty_persistence_mechanism kernel, each authored as a separate ε-invariant story per the ε-invariance principle: naturalization_reading (this file, ε=0.18, Rope), lock_in_reading (path-dependent coordination failure, expected higher ε with no clean beneficiary, likely Tangled Rope or Piton), and beneficiary_extraction_reading (active incumbent maintenance, expected named beneficiary class capturing training/market rents, likely Tangled Rope or Snare). All three share the same underlying historical kernel — the persistence of the QWERTY layout — but instantiate structurally distinct claims about the mechanism of persistence, each with its own beneficiary/victim structure and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
