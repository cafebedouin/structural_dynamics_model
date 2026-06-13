% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__rupture_reading, []).

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
 *   constraint_id: orthographic_kernel__rupture_reading
 *   human_readable: Script Reform as Deliberate Severing of Ottoman/Islamic Continuity
 *   domain: political/cultural/linguistic
 *
 * SUMMARY:
 *   A secular nationalist state undertakes an orthographic reform, replacing
 *   Arabic script with a Latin-derived alphabet. The state frames this as a
 *   modernization measure enabling technological progress and educational
 *   standardization. This story instantiates the rupture_reading: the reform
 *   is analyzed as a deliberate mechanism for severing cultural and
 *   institutional continuity with the Ottoman past and Islamic heritage,
 *   extracting authority from the old literate class and concentrating it in
 *   the post-reform state apparatus. The reading treats the script change as
 *   a snare—a coercive mechanism whose persistence depends on suppressing
 *   alternatives and whose primary function is ideological consolidation, not
 *   genuine coordination. The claim/metric independence rule applies: this
 *   reading claims the constraint is a snare; the authored metrics (very high
 *   extractiveness and suppression, moderate theater as the state's
 *   modernization narrative wears thin over the interval) support that claim
 *   but are authored independently. The sibling readings would claim rope
 *   (continuity_reading: script preserves cultural transmission and Ottoman
 *   institutional continuity without coercion) or tangled_rope
 *   (modernization_reading: script enables genuine technological progress
 *   alongside coercive identity severance). Those are separate constraints in
 *   separate files; this file is the rupture reading only.
 *
 * KEY AGENTS:
 *   - ottoman_literate_class: The professions and scholars who spent decades mastering Arabic script; the reform renders their expertise unmarketable overnight. Power: formerly powerful, structurally trapped by the script mandate.
 *   - islamic_scholarly_tradition: The institutional chain of Quranic exegesis and hadith transmission; script reform breaks the transmission chain by making foundational texts inaccessible without translation. Institutional victim with civilizational time horizon.
 *   - arabic_script_practitioners: Calligraphers, scribes, printers whose livelihoods depend on script production; the state prohibits Arabic-script publishing. Power: moderate, constrained exit.
 *   - post_reform_state_apparatus: The secular nationalist state that administers the script reform, controls the transition, monopolizes literacy certification, and benefits by consolidating authority and severing institutional continuity with Ottoman/Islamic governance. Institutional beneficiary and agenda-setter.
 *   - nationalist_ideological_project: The doctrine that new national identity requires radical rupture with the Ottoman/Islamic past; script reform is the material mechanism. Non-agent (a doctrine/ideology, not a real actor), but listed because it frames and legitimizes the extraction.
 *   - analytical_observer: Historians and linguists who examine whether the constraint is best understood as coordination (modernization) or extraction (cultural consolidation).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, 0.89).
domain_priors:suppression_score(orthographic_kernel__rupture_reading, 0.87).
domain_priors:theater_ratio(orthographic_kernel__rupture_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__rupture_reading, snare).
narrative_ontology:human_readable(orthographic_kernel__rupture_reading, "Script Reform as Deliberate Severing of Ottoman/Islamic Continuity").
narrative_ontology:topic_domain(orthographic_kernel__rupture_reading, "political/cultural/linguistic").

domain_priors:requires_active_enforcement(orthographic_kernel__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__rupture_reading, '1bd8dafc-737c-493e-b264-f05612771b2b').
narrative_ontology:cs_kernel_codification('1bd8dafc-737c-493e-b264-f05612771b2b', fixed_text).
narrative_ontology:cs_authority_grounding('1bd8dafc-737c-493e-b264-f05612771b2b', extraction).
narrative_ontology:cs_interpretation_layer_present('1bd8dafc-737c-493e-b264-f05612771b2b').
narrative_ontology:cs_reading_relation('1bd8dafc-737c-493e-b264-f05612771b2b', orthographic_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('1bd8dafc-737c-493e-b264-f05612771b2b', orthographic_kernel__modernization_reading, coexists_with).
narrative_ontology:cs_axiom('1bd8dafc-737c-493e-b264-f05612771b2b', foundational, script_embodies_cultural_identity).
narrative_ontology:cs_axiom_status(script_embodies_cultural_identity, holdable).
narrative_ontology:cs_axiom_grounding('1bd8dafc-737c-493e-b264-f05612771b2b', script_embodies_cultural_identity, deontological).
narrative_ontology:cs_axiom('1bd8dafc-737c-493e-b264-f05612771b2b', foundational, national_modernity_requires_cultural_severance).
narrative_ontology:cs_axiom_status(national_modernity_requires_cultural_severance, holdable).
narrative_ontology:cs_axiom_grounding('1bd8dafc-737c-493e-b264-f05612771b2b', national_modernity_requires_cultural_severance, instrumental).
narrative_ontology:cs_reference_frame('1bd8dafc-737c-493e-b264-f05612771b2b', ottoman_scribal_continuity).
narrative_ontology:cs_drift_state('1bd8dafc-737c-493e-b264-f05612771b2b', nationalist_rupture_moment, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('1bd8dafc-737c-493e-b264-f05612771b2b', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, nationalist_ideological_project).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, islamic_scholarly_tradition).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, arabic_script_practitioners).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, pre_reform_textual_inheritance).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__rupture_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(orthographic_kernel__rupture_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.89) because the constraint transfers cultural authority from the Ottoman literate class to the post-reform state, destroys access to pre-reform textual inheritance unless deliberately preserved (which the state initially does not do), and monopolizes literacy-based legitimacy. Suppression is correspondingly high (0.87) because the constraint's persistence depends on state prohibition of Arabic-script education and publishing, closure of traditional scholarly institutions, and enforcement of script standardization in all official contexts. The suppression is not merely a side effect of coordination—it is the mechanism that holds the constraint in place. Accessibility collapse is very high (0.91): once the script mandate is enforced, alternatives (continuing Arabic-script literacy in private networks, competing scripts, multilingual education) become structurally foreclosed by state action. Resistance is substantial (0.73) because the ottoman_literate_class and religious authorities actively resist the reform, and continuity-tradition advocates argue against it in public discourse—but the resistance is overridden by state authority and repressed through institutional dismissal or suppression. Theater is moderate-low (0.22 at interval end): the state's modernization narrative is the performative layer, but the primary function is ideological consolidation (script as cultural rupture). Over the interval, theater rises slightly as the state invests more in the narrative that script is purely technical, not cultural—but the underlying extraction persists unchanged. The measurement series track the enforcement ramp-up: extractiveness and suppression rise sharply in the first 15 years as the state consolidates control, then plateau as the old literate class ages out and the post-reform generation has no alternative. Suppression requirement rises earlier and stays higher than base_extractiveness because enforcement (closing schools, banning publications, prosecuting Arabic-script vendors) must be maintained against active resistance.
 *
 * PERSPECTIVAL GAP:
 *   The post_reform_state_apparatus and the post_reform_educated_youth will compute as experiencing rope or tangled_rope (genuine coordination benefit from literacy standardization, minor cost from disrupted access to heritage texts). The ottoman_literate_class and islamic_scholarly_tradition will compute as experiencing snare (extraction via cultural authority loss, massive suppression via textual inaccessibility). The engine computes this divergence from the structural data: the state holds institutional power and administers the mandate; the literate class holds former-powerful status but is identity-locked to pre-reform knowledge systems. The directionality derivation produces a high-d (target) value for the victims and a low-d (beneficiary) value for the state, yielding opposing type classifications from the same constraint. This is the measurement the system is designed to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   The post_reform_state_apparatus holds institutional power, sets the constraint, and collects the benefit (consolidated authority). Directionality near full-beneficiary (d ≈ 0.1). The ottoman_literate_class holds former-powerful status (now obsolete), is identity_locked (cannot retraining out of 600 years of cultural identity), and bears the primary cost (loss of professional and cultural authority). Directionality near full-target (d ≈ 0.9). The islamic_scholarly_tradition is institutional but is structurally trapped (cannot exit the nation where the mandate applies); directionality near full-target (d ≈ 0.85). The post_reform_educated_youth hold moderate power, have mobile exit (could study pre-reform texts if barriers were removed), and benefit from literacy standardization; directionality moderate-beneficiary (d ≈ 0.35). The continuity_tradition_advocates hold organized power, are constrained (can argue but are repressed), and bear costs (intellectual marginalization); directionality near-target (d ≈ 0.75). No directionality overrides are necessary; the structural derivation from beneficiary/victim + exit + power should produce the true values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy classification because the founding problem (create new national identity by severing Ottoman/Islamic continuity) remains live and the beneficiary (state apparatus consolidating authority) actively maintains enforcement. This is not a case where a functional problem has died but the constraint persists by inertia. However, the founding_problem_status is contested: the state claims the problem is live (we must sever the past to build a new nation); historians and continuity advocates claim the founding problem is obsolete or misconceived (modernization does not require cultural rupture; Ottoman identity is not an obstacle to modernity). This contention is the signal for the T17 abductive trigger: a constraint whose founding problem status is contested by non-benefiting parties deserves investigation as a potential zombie function. The constraint is not a piton (it is actively enforced and benefits a clear stakeholder), but it hovers on the boundary between functional necessity and ideological capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    script_as_literal_vs_symbolic,
    'Is the script reform''s extractive function primarily the literal destruction of pre-reform literacy (making texts unreadable) or the symbolic rupture from Ottoman/Islamic identity (making the past culturally illegitimate)?',
    'Historical analysis of state intent documents, contemporaneous elite discourse, and evidence of translation/preservation efforts post-reform. If the state invested in transliterating and preserving the pre-reform archive, the function is more symbolic than literal; if the state actively suppressed preservation efforts, the function is literal destruction.',
    'If primarily symbolic, the constraint''s extractiveness might be classified as identity-based coercion rather than textual erasure; the victim set becomes narrower (the ideological commitment to rupture, not the entire literate population). If primarily literal, the classification as snare (with massive suppression through inaccessibility) holds and the victim set expands to include all who inherit the severed textual tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(script_as_literal_vs_symbolic, empirical, 'Whether script reform''s extraction is the literal destruction of access to pre-reform texts or the symbolic severing of cultural continuity.').

omega_variable(
    suppression_internalization_ambiguity,
    'Does the suppression enforcing script reform remain structural (external barriers: state prohibition of Arabic-script publishing, closure of traditional schools) or does it become internalized (post-reform population internalizes the narrative that the old script and texts are ''backward'', making suppression persist even without external enforcement)?',
    'Post-reform generational studies: if the second and third post-reform generations voluntarily abandon interest in pre-reform texts without state prohibition, or if they experience visceral rejection of the old script (cultural shame), suppression has internalized. If restoration requires active state removal of barriers (lifting publishing bans, reopening scholarly access), suppression remains structural.',
    'If suppression becomes internalized, the constraint''s hold strengthens even as explicit state enforcement relaxes—the victims carry the suppression with them. The effective extraction persists through cultural shame rather than external coercion. If suppression remains structural, post-reform generations could re-access pre-reform texts if barriers are removed; the constraint''s power depends on active state maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Whether suppression of access to pre-reform texts is structural or internalized by post-reform generations.').

omega_variable(
    modernization_coupling_necessity,
    'Is technological and scientific modernization genuinely contingent on script change, or is the coupling between script and modernity a constructed narrative?',
    'Comparative historical analysis: did other modernizing societies achieve comparable technological/scientific advancement without orthographic rupture? Did the post-reform society''s modernization rate accelerate measurably after script change relative to pre-reform trajectory, or relative to technologically equivalent societies that retained traditional scripts?',
    'If modernization is not contingent on script change, the constraint''s justification shifts from functional necessity (coordination problem) to ideological project (cultural severance). The classification remains snare, but the beneficiary set clarifies: the benefit accrues to the state''s nationalist ideology and political consolidation, not to genuine modernization needs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modernization_coupling_necessity, empirical, 'Whether script change is necessary for technological modernization or a constructed coupling that legitimizes cultural rupture.').

omega_variable(
    kernel_reading_alternative_framing,
    'Is this reading''s distinction from the sibling readings (continuity_reading, modernization_reading) stable and structural, or do the three readings collapse under scrutiny into one constraint with three competing interpretations?',
    'ε-invariance test: if the three readings produce substantially different victim sets, beneficiary structures, and enforcement mechanisms, they are three distinct constraints (ε is intrinsic, not observer-relative). If they all describe the same empirical event with different moral framings but identical structural properties, they are one constraint with three interpretations, not three constraints.',
    'If the three readings are truly distinct constraints, each should compile to a separate .pl file with its own ε, stakeholders, and type. If they collapse to one constraint with interpretive variance, the committer frame is misapplied and the three readings should be consolidated. The current story assumes the rupture_reading is distinct (very high ε for cultural continuity, victim set = entire literate population); this depends on the empirical claim that the state''s intent and mechanism are designed to maximize cultural severance (not merely to modernize).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'Whether the rupture, continuity, and modernization readings instantiate three distinct constraints or three interpretations of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__rupture_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_kernel__rupture_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(orth_tr_t5, orthographic_kernel__rupture_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(orth_tr_t10, orthographic_kernel__rupture_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(orth_tr_t15, orthographic_kernel__rupture_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(orth_tr_t20, orthographic_kernel__rupture_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(orth_tr_t30, orthographic_kernel__rupture_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(orth_tr_t40, orthographic_kernel__rupture_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_kernel__rupture_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(orth_be_t5, orthographic_kernel__rupture_reading, base_extractiveness, 5, 0.81).
narrative_ontology:measurement(orth_be_t10, orthographic_kernel__rupture_reading, base_extractiveness, 10, 0.86).
narrative_ontology:measurement(orth_be_t15, orthographic_kernel__rupture_reading, base_extractiveness, 15, 0.88).
narrative_ontology:measurement(orth_be_t20, orthographic_kernel__rupture_reading, base_extractiveness, 20, 0.89).
narrative_ontology:measurement(orth_be_t30, orthographic_kernel__rupture_reading, base_extractiveness, 30, 0.89).
narrative_ontology:measurement(orth_be_t40, orthographic_kernel__rupture_reading, base_extractiveness, 40, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_kernel__rupture_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(orth_su_t5, orthographic_kernel__rupture_reading, suppression_requirement, 5, 0.78).
narrative_ontology:measurement(orth_su_t10, orthographic_kernel__rupture_reading, suppression_requirement, 10, 0.83).
narrative_ontology:measurement(orth_su_t15, orthographic_kernel__rupture_reading, suppression_requirement, 15, 0.85).
narrative_ontology:measurement(orth_su_t20, orthographic_kernel__rupture_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(orth_su_t30, orthographic_kernel__rupture_reading, suppression_requirement, 30, 0.87).
narrative_ontology:measurement(orth_su_t40, orthographic_kernel__rupture_reading, suppression_requirement, 40, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__rupture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__rupture_reading, 0.12).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__modernization_reading).

% DUAL FORMULATION NOTE:
% The orthographic_kernel constraint family decomposes a single historical decision (script reform) into three structurally distinct constraints, each with different epsilon values, beneficiary/victim structures, and types. The rupture_reading instantiates the reading where script change is analyzed as a deliberate mechanism for severing cultural continuity and consolidating state authority (very high ε, snare type, victims = entire pre-reform literate class). The continuity_reading would analyze the same historical event as preservation of Ottoman/Islamic institutional continuity (lower ε, rope or mountain type, no victims). The modernization_reading would analyze it as a functional coordination tool for technological progress with cultural disruption as a side effect (moderate ε, tangled_rope type, costs distributed). These readings cannot coexist in a single institutional framework—they make incompatible claims about the constraint's function and purpose. Each reading is authored as a separate constraint file and linked via affects_constraints. The family structure enables the corpus to record the actual historical contest: different parties read the same kernel differently, and those different readings instantiate different constraints with different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
