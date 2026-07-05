% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__lapsed_alternatives_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Keyboard Layout Persistence — Coordination/Lapsed-Alternatives Reading
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   This story instantiates the lapsed-alternatives reading of the QWERTY
 *   persistence kernel: the layout persists because it solves a genuine
 *   multi-party coordination problem, and the alternatives that arose
 *   (Dvorak, 1936; various later ergonomic layouts) failed not because
 *   incumbents suppressed them but because they never reached the adoption
 *   threshold where switching became individually rational for a critical
 *   mass of typists simultaneously. Under this reading there is no
 *   concentrated beneficiary extracting rents and no identifiable victim
 *   class bearing disproportionate cost — the switching cost is symmetric and
 *   borne by anyone who would defect from the standard alone. This is a
 *   distinct constraint from the sibling incumbent_preservation_reading,
 *   which frames the same persistence as active defense of capital investment
 *   by manufacturers with something to lose from switching; that reading
 *   would carry a beneficiary set (typewriter/keyboard manufacturers with
 *   sunk tooling investment) and a materially different epsilon driven by
 *   defensive lobbying and marketing rather than pure switching-cost
 *   friction. The two readings are not two measurements of one constraint —
 *   they are two structurally different constraints sharing a kernel, linked
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - typing_population_at_large: diffuse beneficiary of shared-standard coordination value, symmetric switching cost if any individual defects
 *   - keyboard_manufacturers: agenda-setting party but constrained by market demand, not extracting rents under this reading
 *   - dvorak_and_other_alternative_layout_advocates: excluded not by suppression but by failed critical-mass adoption
 *   - standards_bodies: analytical observer, documents practice after the fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.12).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.08).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Keyboard Layout Persistence — Coordination/Lapsed-Alternatives Reading").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology_history/industrial_standards/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, '0fee7b3a-c7d7-496a-bd31-5f3e59109c40').
narrative_ontology:cs_kernel_codification('0fee7b3a-c7d7-496a-bd31-5f3e59109c40', implicit).
narrative_ontology:cs_authority_grounding('0fee7b3a-c7d7-496a-bd31-5f3e59109c40', practice).
narrative_ontology:cs_interpretation_layer_present('0fee7b3a-c7d7-496a-bd31-5f3e59109c40').
narrative_ontology:cs_reading_relation('0fee7b3a-c7d7-496a-bd31-5f3e59109c40', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('0fee7b3a-c7d7-496a-bd31-5f3e59109c40', foundational, persistence_explained_by_coordination_equilibrium_alone).
narrative_ontology:cs_axiom_status(persistence_explained_by_coordination_equilibrium_alone, holdable).
narrative_ontology:cs_axiom_grounding('0fee7b3a-c7d7-496a-bd31-5f3e59109c40', persistence_explained_by_coordination_equilibrium_alone, empirically_contingent).
narrative_ontology:cs_axiom('0fee7b3a-c7d7-496a-bd31-5f3e59109c40', secondary, no_concentrated_beneficiary_required_for_standard_persistence).
narrative_ontology:cs_axiom_status(no_concentrated_beneficiary_required_for_standard_persistence, holdable).
narrative_ontology:cs_axiom_grounding('0fee7b3a-c7d7-496a-bd31-5f3e59109c40', no_concentrated_beneficiary_required_for_standard_persistence, empirically_contingent).
narrative_ontology:cs_reference_frame('0fee7b3a-c7d7-496a-bd31-5f3e59109c40', network_effect_coordination_equilibrium).
narrative_ontology:cs_drift_state('0fee7b3a-c7d7-496a-bd31-5f3e59109c40', contemporary_digital_keyboard_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0fee7b3a-c7d7-496a-bd31-5f3e59109c40', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, typing_population_at_large).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Learns and uses whatever layout is already taught in schools, sold on devices, and embedded in muscle memory across a lifetime of typing. Benefits from every other typist using the same layout because it means shared keyboards, shared training materials, and transferable skill. Switching layouts individually costs personal retraining time with no guarantee anyone else will switch too, so the rational move for almost everyone is to keep using what everyone else uses.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, typing_population_at_large, beneficiary,
    moderate, generational, constrained, global).

% Manufactures whatever layout the market demands. Continues stamping out QWERTY keyboards because retooling to a different layout would only pay off if enough buyers demanded it simultaneously, and no critical mass of buyer demand for alternatives has materialized. Would switch tooling readily if demand shifted; nothing prevents a competitor from selling alternative-layout keyboards today, and several do at small scale.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, beneficiary).

% Designed and promoted layouts with measurably shorter finger travel and fewer awkward digraphs. Free to type on, sell, and teach their layout at any time — no law or license bars them. Failed to reach the adoption threshold where switching became individually rational for enough people at once, so the alternative lapsed from lack of coordinated uptake, not from suppression. Their situation is a coordination failure, not an extraction claim: nobody stopped the layout from spreading, not enough people moved together.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, dvorak_and_other_alternative_layout_advocates, excluded,
    powerless, biographical, mobile, national).

% Documents and formalizes existing practice (ANSI, ISO layout specifications) after the fact rather than dictating layout choice. Could in principle promote an alternative standard but has no mechanism to compel mass simultaneous switching, since the value of any layout standard is a function of how many people already use it.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, standards_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__lapsed_alternatives_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence__lapsed_alternatives_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single widely-known keyboard layout lets any typist sit at any keyboard and type immediately, lets any employer hire without layout-specific retraining, and lets any manufacturer produce one dominant SKU at scale. This is a genuine multi-party coordination problem: the value of a shared layout scales with the number of others who share it, and there is no central authority needed to sustain it once critical mass is reached.
% TRANSFER_FUNCTION: Nothing is extracted from one party to benefit another; the arrangement moves coordination costs, not rents. Whoever would switch layouts individually bears the retraining cost alone without capturing the network benefit, which is why no one switches — the cost is borne symmetrically by anyone who considers exit, not concentrated onto an identifiable victim class.
% ABSENT_VOICES: Alternative-layout advocates (Dvorak, Colemak, and earlier 19th-century contenders) argued their designs were objectively superior on efficiency and ergonomics grounds. They were not silenced or blocked; their voice simply never reached the volume needed to shift the coordination equilibrium. Their absence from today's dominant practice reflects failed collective action, not exclusion.
% DISAPPEARANCE_RATIONALE: If the QWERTY standard vanished overnight with no replacement, chaos would follow immediately: manufacturers would have no reference layout to build to, schools would have nothing consistent to teach, and every typist would need to relearn a layout chosen ad hoc. The coordination value the standard provides is real and load-bearing — its disappearance would force costly re-coordination, which is exactly what a genuine Rope constraint predicts.
% FOUNDING_PROBLEM: Early typewriter mechanisms needed a key arrangement that reduced jamming of mechanical typebars for common English letter sequences; once that arrangement was taught widely and typists trained on it, it became the reference layout for subsequent devices regardless of whether the original jamming problem persisted in later hardware.
% FOUNDING_PROBLEM_CORROBORATION: Mechanical engineers and typewriter historians outside any manufacturer or typing-education interest confirm mechanical jam-avoidance is irrelevant to modern electronic keyboards; the persistence is now attested by independent economists and coordination-game theorists (Liebowitz & Margolis and others) as a network-effect equilibrium rather than a defended mechanical necessity — this corroboration comes from academics with no stake in keyboard sales or typing curricula.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__lapsed_alternatives_reading_tests).
:- end_tests(qwerty_persistence__lapsed_alternatives_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12 at interval end) because under this reading the entire cost of the constraint is the switching cost inherent to any network-effect equilibrium — a cost that is symmetric across parties who might consider defecting, not a rent flowing to an identifiable beneficiary. Suppression is authored low (0.08) because nothing legally or structurally bars anyone from typing, selling, teaching, or manufacturing an alternative layout at any point in the interval; the accessibility_collapse value (0.62) reflects that alternatives have become practically hard to adopt once trained muscle memory and manufacturing scale are established, but this is a friction-of-scale effect, not an enforcement effect. Theater ratio stays near zero throughout — there is no performative maintenance apparatus defending QWERTY, because under this reading nothing needs defending.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the lapsed-alternatives reading, the beneficiary set is deliberately broad and diffuse (typing_population_at_large) rather than a narrow capturing class, because the coordination value accrues to everyone who participates in the shared standard, symmetrically. There is no victim set: any individual typist or manufacturer bears the same switching-cost logic as any other, and no party is positioned to extract asymmetric rent from another party's inability to switch. This is the structural signature that should compute as Rope, not Snare or Tangled Rope — the coordination function is real and the persistence mechanism does not require active suppression of exits, only the ordinary friction of an uncoordinated collective-action problem.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mechanical-jamming problem is dead (electronic keyboards have no typebars to jam), yet the arrangement persists — this looks superficially like classic mandatrophy (a mandate outliving its function while the apparatus continues). But the R5 corroboration distinguishes this from mandatrophy: the arrangement's continued value under this reading is not the original founding function but a NEW, independently real coordination function (universal typing compatibility) that arose after the founding problem died. Mandatrophy requires the apparatus persisting on the strength of the DEAD justification; here the justification has genuinely shifted to a live one, so this is properly a case of function succession, not mandatrophy — the disappearance_verdict of world_rearranges under a still-live coordination function is the diagnostic that keeps this from being wrongly flagged as extractive inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_defense_framing,
    'Is QWERTY''s persistence better explained by a pure coordination equilibrium (no party needs to actively defend it) or by active defense of sunk capital investment by manufacturers and typing-instruction industries (the incumbent_preservation_reading)?',
    'Historical record of whether manufacturers or typing-instruction bodies ever lobbied, marketed against, or financially disadvantaged alternative-layout adoption efforts (e.g. the Dvorak Simplified Keyboard trials) versus simply not investing in alternatives absent demand signal.',
    'If active defensive behavior is found (lobbying, marketing suppression, contract terms disadvantaging alternatives), the correct reading shifts toward incumbent_preservation_reading with a beneficiary/victim structure and higher effective extraction; if only passive non-adoption is found, this reading''s zero-victim, symmetric-cost account holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_defense_framing, conceptual, 'Framing ambiguity between coordination-equilibrium and beneficiary-defense accounts of the same adoption history — this is the kernel contest itself, not resolvable by data internal to this story alone.').

omega_variable(
    critical_mass_threshold_naturalness,
    'Is the adoption threshold below which alternative layouts lapse a natural feature of network-effect goods generally, or was it artificially raised by any structural factor (e.g. bundling of layout with dominant hardware/OS platforms)?',
    'Comparative analysis of adoption thresholds for other network-effect standards where bundling was absent versus present, to isolate whether QWERTY''s threshold is typical or elevated.',
    'A typical threshold supports the pure lapsed-alternatives account (natural coordination friction); an artificially elevated threshold (e.g. due to OS/hardware bundling decisions by manufacturers) would import a beneficiary-style extraction element into this reading, blurring the boundary with the sibling reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critical_mass_threshold_naturalness, empirical, 'Whether the adoption threshold itself is a natural coordination-game parameter or was shaped by an identifiable party''s bundling decisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 1873, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1873, 0.02).
narrative_ontology:measurement(qwer_tr_t1900, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1900, 0.02).
narrative_ontology:measurement(qwer_tr_t1936, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1936, 0.03).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1980, 0.04).
narrative_ontology:measurement(qwer_tr_t2005, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(qwer_tr_t2025, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1873, 0.05).
narrative_ontology:measurement(qwer_be_t1900, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1900, 0.06).
narrative_ontology:measurement(qwer_be_t1936, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1936, 0.08).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement(qwer_be_t2005, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2005, 0.11).
narrative_ontology:measurement(qwer_be_t2025, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2025, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence__lapsed_alternatives_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__lapsed_alternatives_reading, 0.05).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence__incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% This story and qwerty_persistence__incumbent_preservation_reading are two readings of the same kernel (qwerty_persistence), not two measurements of one constraint. This reading (lapsed_alternatives_reading) carries epsilon determined purely by symmetric switching costs, no victim set, and claimed_type rope. The sibling carries a beneficiary set (incumbent manufacturers/instructors with sunk capital) and a victim set (alternative-layout adopters bearing disproportionate switching costs while incumbents actively defend), with a correspondingly higher epsilon and likely tangled_rope or snare classification. Both are internally ε-invariant; they diverge because they locate different causal mechanisms in the same historical adoption record.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
