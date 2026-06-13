% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation Requires Physical Performance (Not Study)
 *   domain: religious_law/ritual_studies
 *
 * SUMMARY:
 *   After the Second Temple's destruction in 70 CE, Jewish law faced a
 *   foundational crisis: the commandment to bring sacrifices remained
 *   textually binding, but physical performance became impossible. This
 *   constraint instantiates ONE reading of how that crisis was resolved — the
 *   performance-only reading, which holds that sacrifice obligation requires
 *   actual physical restoration and that study, while obligatory, remains
 *   preparatory rather than fulfilling. This reading structures the current
 *   generation (and all intervening generations) as permanently holding an
 *   unfulfillable obligation. Current entry into the victim set is structural
 *   to this reading: study is classified as preparation-for-future, not as
 *   satisfaction-of-present. The constraint's extractiveness is high
 *   precisely because the obligation cannot be discharged; the current
 *   generation bears perpetual obligation-debt. The theater ratio rises
 *   dramatically over the 2000-year interval as study becomes increasingly
 *   ritualized and the performative maintenance of the reading's framing
 *   grows more elaborate, while the material possibility of fulfillment
 *   recedes.
 *
 * KEY AGENTS:
 *   - Rabbinic authority structure: agenda-setter, maintains the performance-only reading as institutional doctrine and gatekeeps alternative interpretations
 *   - Current generation Jews: payers, inherit obligation classified as unfulfillable by this reading; study is reclassified as preparation
 *   - Rabbinic interpretive community: beneficiaries (alongside agenda-setting role), benefit from sustained demand for authoritative interpretation of the obligation
 *   - Future generations: powerless payers, inherit obligation-debt with no agency to resolve it
 *   - Sibling-reading advocates: excluded voices advocating study_as_performance, messianic_suspension, or archival_preservation
 *   - Temple restoration advocates: beneficiaries, political movements motivated by the reading's framing of study as insufficient
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.82).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.71).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.82).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation Requires Physical Performance (Not Study)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '4bed495a-d58f-4c1c-b18e-076bcc693e46').
narrative_ontology:cs_kernel_codification('4bed495a-d58f-4c1c-b18e-076bcc693e46', fixed_text).
narrative_ontology:cs_authority_grounding('4bed495a-d58f-4c1c-b18e-076bcc693e46', lineage).
narrative_ontology:cs_interpretation_layer_present('4bed495a-d58f-4c1c-b18e-076bcc693e46').
narrative_ontology:cs_reading_relation('4bed495a-d58f-4c1c-b18e-076bcc693e46', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('4bed495a-d58f-4c1c-b18e-076bcc693e46', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('4bed495a-d58f-4c1c-b18e-076bcc693e46', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('4bed495a-d58f-4c1c-b18e-076bcc693e46', foundational, physical_performance_required_for_obligation_fulfillment).
narrative_ontology:cs_axiom_status(physical_performance_required_for_obligation_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('4bed495a-d58f-4c1c-b18e-076bcc693e46', physical_performance_required_for_obligation_fulfillment, deontological).
narrative_ontology:cs_axiom('4bed495a-d58f-4c1c-b18e-076bcc693e46', foundational, study_is_preparation_not_performance).
narrative_ontology:cs_axiom_status(study_is_preparation_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('4bed495a-d58f-4c1c-b18e-076bcc693e46', study_is_preparation_not_performance, conventional).
narrative_ontology:cs_axiom('4bed495a-d58f-4c1c-b18e-076bcc693e46', secondary, messianic_restoration_eschatology).
narrative_ontology:cs_axiom_status(messianic_restoration_eschatology, holdable).
narrative_ontology:cs_axiom_grounding('4bed495a-d58f-4c1c-b18e-076bcc693e46', messianic_restoration_eschatology, theological).
narrative_ontology:cs_reference_frame('4bed495a-d58f-4c1c-b18e-076bcc693e46', post_70_ce_obligation_preservation).
narrative_ontology:cs_drift_state('4bed495a-d58f-4c1c-b18e-076bcc693e46', contemporary_secular_era_2000, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4bed495a-d58f-4c1c-b18e-076bcc693e46', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, rabbinic_authority_structure).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, current_generation_jews).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, future_generations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.45 at interval t=0, early rabbinic period) when the obligation is newly unfulfillable and alternative readings still circulate. It rises steeply through the medieval period as rabbinic authority consolidates institutional control and marginalizes alternatives, reaching 0.82 by the modern era. Theater ratio tracks the same trajectory but even more steeply, rising from 0.25 to 0.68, because over centuries the actual interpretive work (determining what restoration would entail, what study signifies) becomes increasingly performative — the reading is maintained rhetorically even as its material conditions remain constant. Suppression requirement rises from 0.42 to 0.71 as institutional effort is required to keep alternative readings out of mainstream teaching and interpretation. The constraint is tangled_rope because it coordinates genuine obligations around collective memory while extracting from the current generation by classifying study as preparatory rather than fulfilling. Accessibility collapse is high (0.78) because once the performance-only reading is institutionalized, alternatives feel heterodox even when present in the same sources. Resistance is moderate-to-high (0.64) because scholarly and communal movements continually advocate alternatives, though institutional authority suppresses them.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic institutional seat, this is genuine coordination: maintaining the tradition, preserving obligation, keeping readiness alive through study. From the current-generation seat, it is extraction: the obligation is classified as unfulfillable by deliberate institutional choice, not by textual necessity. A payer seat (current generation) experiences permanent obligation-debt; the agenda-setter seat (rabbinic authority) experiences interpretive authority and legitimacy. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary (rabbinic authority) derives low directionality via arbitrage exit and institutional power. Current generation derives high directionality via identity_locked exit: Jewish identity is fused with the obligation; leaving the identity-frame dissolves the obligation but terminates the agent's self-concept. Future generations derive maximum directionality (trapped exit, powerless, no voice). The sibling-reading advocates are excluded: their exit is constrained (institutional suppression of their interpretations) but their power is moderate (they maintain scholarly and some communal presence). Temple restoration advocates are beneficiaries with mobile exit (they can choose to advocate or not) and moderate power.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint would be misclassified as Rope if the tangled structure were not visible. The coordination function is real (collective memory, shared interpretive framework). But the asymmetric extraction is also real: the obligation is structured as unfulfillable by institutional choice, creating permanent obligation-debt for the current generation while the institutional structure (agenda-setter) preserves authority and interpretive legitimacy. Tangled_rope captures this duality precisely: genuine coordination wrapped around asymmetric extraction. The constraint requires active enforcement (suppression of alternative readings) to hold; without it, the study_as_performance reading would circulate as a live alternative, dissolving the obligation-debt structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_determination_vs_institutional_choice,
    'Is the performance-only reading the inevitable textual conclusion from classical sources, or one defensible reading among equally grounded alternatives?',
    'Systematic analysis of all classical sources (Talmud, medieval codes) on whether study fulfills or merely prepares. Comparison with parallel textual corpora (early Christian sacrifice theology, Islamic qurbani traditions) that faced the same foundational crisis and the variety of resolutions they adopted.',
    'If textually determined, the constraint reflects unavoidable tradition constraints (lower extractiveness ceiling). If one choice among alternatives, the institutional enforcement of this particular reading becomes visible as institutional power, not textual inevitability, raising effective extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_determination_vs_institutional_choice, empirical, 'Whether the performance-only reading is textually necessary or institutionally privileged').

omega_variable(
    identity_fusion_vs_structural_coercion,
    'Is the current generation''s obligation-debt sustained by identity-fusion (the obligation feels constitutive of Jewish selfhood) or by structural coercion (institutional gatekeeping of alternatives), or both?',
    'Post-identification trajectory analysis: if individuals exit Jewish institutional community but retain Jewish identity and report obligation-guilt, the suppression is partially internalized. If obligation-guilt dissolves after institutional exit, suppression is primarily structural. Comparison across subpopulations with different institutional embeddedness.',
    'If primarily internalized, the constraint''s suppression persists even after institutional gatekeeping is removed — the obligation is carried internally by the agent. If primarily structural, removing institutional suppression would dissolve the obligation''s force. Mixed suppression requires different remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_vs_structural_coercion, empirical, 'Suppression mechanism: internalized identity-fusion vs. structural institutional gatekeeping').

omega_variable(
    messianic_restoration_credibility,
    'How does the credibility of messianic restoration (the promised endpoint that would allow physical performance) affect the obligation''s current extractiveness?',
    'Historical tracking of mainstream eschatological belief (literal messianic resurrection vs. metaphorical restoration vs. secularization). Correlation between eschatological belief and reported obligation-burden in different periods and communities.',
    'If messianic restoration is widely believed as imminent, the obligation-debt feels temporary and extractiveness is dampened. If belief erodes or restoration is indefinitely postponed, obligation becomes perpetual and extractiveness rises. The rising theater ratio over time may correlate with declining restoration credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_restoration_credibility, empirical, 'Whether declining messianic belief correlates with rising theater and extractiveness').

omega_variable(
    kernel_reading_choice_mechanism,
    'What institutional mechanism allowed the performance-only reading to become dominant over the study_as_performance alternative, even when both were present in classical sources?',
    'Institutional history: who gained authority to interpret authoritative codes (Shulchan Aruch, Mishneh Torah)? What texts were canonized and which interpretations marginalized? What role did political power (patronage of rabbinical institutions, control of textual reproduction) play in settling the reading?',
    'If the reading was chosen for institutional authority reasons (not textual reasons), it is a committer-framing question: is the reading defensible as textually grounded, or is it institutional power dressed as textual necessity? Feeds the broader omega on textual determination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_choice_mechanism, empirical, 'Institutional mechanisms that made performance-only reading dominant').

omega_variable(
    sibling_reading_foreclosure,
    'Does the performance-only reading logically foreclose the study_as_performance reading within a single coherent framework, or do they coexist as different valid readings?',
    'Logical analysis: examine whether the two readings require contradictory interpretations of the same source text, or whether they operate on different levels (one addressing the ontological status of the obligation, the other addressing its epistemic accessibility). Can a framework hold both simultaneously?',
    'If foreclosed, the relation is ''forecloses'' in cs_structure.reading_relations. If coexistent, the relation is ''coexists_with''. This determines the scope of institutional exclusion: necessary logical defense vs. pragmatic institutional gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether performance-only logically forecloses study-as-performance or permits coexistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_obligation_continuity__performance_only, theater_ratio, 300, 0.35).
narrative_ontology:measurement(sacr_tr_t800, sacrifice_obligation_continuity__performance_only, theater_ratio, 800, 0.48).
narrative_ontology:measurement(sacr_tr_t1400, sacrifice_obligation_continuity__performance_only, theater_ratio, 1400, 0.62).
narrative_ontology:measurement(sacr_tr_t1800, sacrifice_obligation_continuity__performance_only, theater_ratio, 1800, 0.66).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__performance_only, theater_ratio, 2000, 0.68).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sacr_be_t300, sacrifice_obligation_continuity__performance_only, base_extractiveness, 300, 0.58).
narrative_ontology:measurement(sacr_be_t800, sacrifice_obligation_continuity__performance_only, base_extractiveness, 800, 0.72).
narrative_ontology:measurement(sacr_be_t1400, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1400, 0.79).
narrative_ontology:measurement(sacr_be_t1800, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1800, 0.81).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 2000, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(sacr_su_t300, sacrifice_obligation_continuity__performance_only, suppression_requirement, 300, 0.51).
narrative_ontology:measurement(sacr_su_t800, sacrifice_obligation_continuity__performance_only, suppression_requirement, 800, 0.62).
narrative_ontology:measurement(sacr_su_t1400, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1400, 0.69).
narrative_ontology:measurement(sacr_su_t1800, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 2000, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__performance_only, 0.12).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_continuity kernel decomposes into four constraint stories, each instantiating a different reading of how the post-70 CE obligation remains binding (or ceases to bind). Each reading has distinct ε, beneficiary/victim structure, and institutional enforcement profile. (1) performance_only (THIS constraint): study is preparation; current generation is victim. (2) study_as_performance (sibling): study fulfills; obligation is discharged. (3) messianic_suspension (sibling): obligation is suspended, not violated. (4) archival_preservation (sibling): obligation is no longer binding. Each reading's extractiveness depends on who benefits from that reading's dominance. Performance_only produces highest extractiveness for current generation (obligation unfulfillable) and benefits rabbinic authority (interpretive gatekeeping). Study_as_performance produces lowest extractiveness (obligation is dischargeable through study). All four stories must be authored separately; this story addresses the performance_only reading as a single coherent constraint with stable ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_continuity__performance_only, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
