% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__unitarian_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Unitarian Reading of Biblical Divine Nature (Numerical Singularity)
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   The unitarian reading asserts that God is numerically singular—one
 *   person, the Father—and that the Son and Spirit are subordinate beings or
 *   creatures, not co-eternal or consubstantial. This reading emerges as an
 *   intellectually coherent response to the logical paradox of trinitarian
 *   dogma and is defended by reform movements and rationalist theologians as
 *   a recovery of apostolic Christianity. It is institutionally excluded and
 *   actively suppressed by the established churches (Catholic, Orthodox,
 *   Reformed), which treat it as heresy. The unitarian reading itself is not
 *   a natural law or coordination mechanism—it is a doctrinal claim whose
 *   institutional persistence depends on enforcement against institutional
 *   authority. The suppression is doctrinal (excommunication, censure,
 *   intellectual delegitimation) and episodic (flare-ups during Reformation,
 *   Enlightenment, 19th-century modernism) rather than continuous. This story
 *   models the constraint from the perspective of those who hold the reading
 *   and suffer institutional extraction for doing so.
 *
 * KEY AGENTS:
 *   - reform_movements — Primary beneficiary (organized, generational) — gain doctrinal authority and legitimacy by framing themselves as scriptural purists.
 *   - theological_rationalizers — Secondary beneficiary (moderate, identity-locked) — gain intellectual coherence and professional vindication through exegetical clarity.
 *   - institutional_hierarchy — Primary victim (institutional, generational, mobile) — bears the extractive cost of defending paradoxical dogma against coherent challenge.
 *   - trinitarian_orthodoxy_defenders — Secondary victim (powerful, constrained) — trapped in permanent defensive posture; intellectual weight of defending three-in-one against logical critique.
 *   - scriptural_exegetes — Observer (moderate, analytical) — witness to the suppression of subordinationist texts in the institutional record.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.68).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.72).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, snare).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Unitarian Reading of Biblical Divine Nature (Numerical Singularity)").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, '139f5551-1fb3-42c0-a3e1-458d43b2c742').
narrative_ontology:cs_kernel_codification('139f5551-1fb3-42c0-a3e1-458d43b2c742', fixed_text).
narrative_ontology:cs_authority_grounding('139f5551-1fb3-42c0-a3e1-458d43b2c742', lineage).
narrative_ontology:cs_interpretation_layer_present('139f5551-1fb3-42c0-a3e1-458d43b2c742').
narrative_ontology:cs_reading_relation('139f5551-1fb3-42c0-a3e1-458d43b2c742', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('139f5551-1fb3-42c0-a3e1-458d43b2c742', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_axiom('139f5551-1fb3-42c0-a3e1-458d43b2c742', foundational, numerical_singularity_of_god).
narrative_ontology:cs_axiom_status(numerical_singularity_of_god, holdable).
narrative_ontology:cs_axiom_grounding('139f5551-1fb3-42c0-a3e1-458d43b2c742', numerical_singularity_of_god, empirically_contingent).
narrative_ontology:cs_axiom('139f5551-1fb3-42c0-a3e1-458d43b2c742', foundational, apostolic_deposit_recovery).
narrative_ontology:cs_axiom_status(apostolic_deposit_recovery, holdable).
narrative_ontology:cs_axiom_grounding('139f5551-1fb3-42c0-a3e1-458d43b2c742', apostolic_deposit_recovery, empirically_contingent).
narrative_ontology:cs_reference_frame('139f5551-1fb3-42c0-a3e1-458d43b2c742', apostolic_monotheism_unmediated).
narrative_ontology:cs_drift_state('139f5551-1fb3-42c0-a3e1-458d43b2c742', post_nicene_institutional_capture, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('139f5551-1fb3-42c0-a3e1-458d43b2c742', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, reform_movements).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, theological_rationalizers).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, institutional_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, trinitarian_orthodoxy_defenders).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, subordinationist_craftspeople).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steeply from 0.48 (time 0, pre-Reformation) to 0.68 (time 12, post-Enlightenment rationalism plateau). The rise models the constraint's intensification as rationalism and scientific materialism made paradoxical theology increasingly untenable for educated audiences, forcing institutional defenders into stronger suppression. The plateau at 0.68 after time 12 reflects the equilibrium state: unitarianism is intellectually live but institutionally contained (Unitarian denominations, dissenting academies, liberal Protestantism exist but are marginal relative to the trinitarian mainstream). Theater ratio rises from 0.28 to 0.41 and plateaus, modeling the crescendo of ceremonial defense: institutional arguments for trinitarian mystery become increasingly performative as rationalist critiques sharpen, and institutional power must increasingly rest on ritual authority rather than logical persuasiveness. Suppression requirement tracks extractiveness closely (0.54 → 0.72), indicating that the institutional cost of maintaining orthodoxy grows as the unitarian reading becomes more intellectually accessible. Resistance (authored at 0.76 in base_properties) is high because the rationalist movement, Protestant reformism, and scientific skepticism continuously mount real intellectual pressure against trinitarian dogma—it is not passively accepted but actively defended against.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_dogma,
    'Is the unitarian reading a recovery of apostolic truth suppressed by institutional power, or a constructed Enlightenment rationalization projected backward onto the early church?',
    'Textual-critical reconstruction of the earliest christologies using manuscript evidence, dating of creeds and conciliar decisions, and genealogical tracking of subordinationist theology from 2nd-century Logos theology through Arius to Reformation rationalism. The weight of corroborating evidence from non-institutional scholars (secular textual critics, historians outside the beneficiary set).',
    'If the unitarian reading is a genuine apostolic recovery, the institutional suppression is a false-summit case: the churches suppressed truth for institutional power. This would reclassify the constraint from snare to a tangled_rope with false natural-law cover, with mandatrophy consequences for institutional legitimacy. If it is a constructed rationalization, the reading remains a snare: an intellectual challenge to dogma that benefits from institutional rejection as proof of its iconoclasm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_dogma, empirical, 'Whether the unitarian reading is apostolic recovery or rationalist projection.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of unitarianism purely structural (institutional gatekeeping, credential denial, doctrinal censure) or partially internalized in the theological rationalist tradition itself (does rationalist theology police itself by accepting the trinitarian paradox as the price of intellectual legitimacy)?',
    'Historical tracking of how rationalist theologians (18th–19th century liberalism) treated unitarianism: did they reject it due to institutional pressure, or did they internalize the view that even rational theology must preserve the trinitarian consensus? Biographical analysis of theologians (Schleiermacher, Ritschl, Barth) who explicitly chose paradox over unitarianism despite having the intellectual freedom to choose otherwise.',
    'If suppression is purely structural, the constraint''s effective suppression (0.72) may understate the true institutional cost. If it is partially internalized, rationalist theologians themselves become co-suppressors, and the measured suppression may be accurate but masks an internalized layer that persists even after institutional pressure is removed. This affects the durability of the constraint: an internalized suppression would persist even if institutional enforcement weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of unitarianism is structural-only or internalized in the rationalist tradition.').

omega_variable(
    kernel_contest_foreclosure,
    'Is this constraint (unitarian reading) logically incompatible with the trinitarian and modalist readings within a single theological framework, or can they coexist as different readings of the same scriptural deposit?',
    'Logical analysis of the core premises: if the unitarian reading asserts ''God is numerically singular (one person)'' and the trinitarian reading asserts ''God is numerically three (three persons, one essence)'', can both be true in the same logical system? The answer determines the reading_relations in cs_structure: if incompatible, the relation is ''forecloses''; if compatible within pluralistic frameworks, the relation is ''coexists_with''.',
    'This is the core ambiguity of the kernel itself. If the readings foreclose each other, the constraint story is one chapter in a forced choice between readings, and one reading must eventually dominate or disappear. If they coexist, the constraint is one voice in a permanent polyphonic dispute, and its persistence depends on institutional pluralism (or institutional failure to enforce uniformity). This affects how the story models mandatrophy: a dead founding problem + constrained disappearance verdict suggests zombie-like institutional persistence under a coexistence model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure, conceptual, 'Whether the unitarian reading logically forecloses or coexists with trinitarian and modalist readings.').

omega_variable(
    beneficiary_capture_vs_genuine_reform,
    'Do reform movements benefit from the unitarian reading because they genuinely believe it is scriptural truth, or because the reading''s challenge to institutional authority serves their agenda regardless of its truth-value?',
    'Comparative analysis of reform movements that embraced unitarianism (Radical Reformation, Polish Brethren, later Unitarians) versus those that rejected it (Calvin, Luther, mainstream Reformed tradition). Examine whether acceptance or rejection tracked scriptural evidence or institutional position and political advantage.',
    'If reform movements are genuine believers in unitarian truth, they are beneficiaries of a true constraint—the unitarian reading as apostolic recovery. If they are capturing the reading to advance anti-institutional agendas, the beneficiary set is misnamed: the true beneficiary is ''anti-institutional authority'' (not a real actor), and the named beneficiaries are co-users of the reading rather than its genuine advocates. This affects the reading''s status: is it a snare (beneficiaries using a suppressed doctrinal challenge to extract institutional legitimacy) or a rope-like coordination (unified belief in apostolic truth)? The measured suppression (0.72) and extractiveness (0.68) suggest snare; genuine mass belief would show lower suppression and resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_genuine_reform, preference, 'Whether reform movements genuinely believe the unitarian reading or instrumentally use it for anti-institutional gain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__unitarian_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(bibl_tr_t3, biblical_divine_nature__unitarian_reading, theater_ratio, 3, 0.31).
narrative_ontology:measurement(bibl_tr_t6, biblical_divine_nature__unitarian_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(bibl_tr_t9, biblical_divine_nature__unitarian_reading, theater_ratio, 9, 0.39).
narrative_ontology:measurement(bibl_tr_t12, biblical_divine_nature__unitarian_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(bibl_tr_t15, biblical_divine_nature__unitarian_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement(bibl_tr_t18, biblical_divine_nature__unitarian_reading, theater_ratio, 18, 0.41).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__unitarian_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(bibl_be_t3, biblical_divine_nature__unitarian_reading, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(bibl_be_t6, biblical_divine_nature__unitarian_reading, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(bibl_be_t9, biblical_divine_nature__unitarian_reading, base_extractiveness, 9, 0.66).
narrative_ontology:measurement(bibl_be_t12, biblical_divine_nature__unitarian_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(bibl_be_t15, biblical_divine_nature__unitarian_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(bibl_be_t18, biblical_divine_nature__unitarian_reading, base_extractiveness, 18, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__unitarian_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(bibl_su_t3, biblical_divine_nature__unitarian_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(bibl_su_t6, biblical_divine_nature__unitarian_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement(bibl_su_t9, biblical_divine_nature__unitarian_reading, suppression_requirement, 9, 0.7).
narrative_ontology:measurement(bibl_su_t12, biblical_divine_nature__unitarian_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(bibl_su_t15, biblical_divine_nature__unitarian_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(bibl_su_t18, biblical_divine_nature__unitarian_reading, suppression_requirement, 18, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__unitarian_reading, 0.12).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested kernel 'biblical_divine_nature'. The unitarian reading asserts numerical singularity (Father alone is God; Son/Spirit created/subordinate). The trinitarian reading asserts three hypostases in one essence. The modalist reading asserts sequential modes of one person. Each reading has its own ε, its own beneficiary/victim structure, and its own classification. They are linked by network.affects_constraints because each reading's institutional success or failure directly shapes the others' viability. The unitarian reading's diffusion (as in 19th-century liberal Protestantism and modern Unitarianism) constrains the trinitarian reading's institutional monopoly; the trinitarian reading's institutional dominance (Catholic, Orthodox, Reformed mainstream) suppresses the unitarian reading. These are not different measurements of one constraint—they are different constraints with different structural properties, linked by institutional competition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_divine_nature__unitarian_reading, institutional, 0.85).
constraint_indexing:directionality_override(biblical_divine_nature__unitarian_reading, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
