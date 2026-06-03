% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: P5 Veto Power as Westphalian Sovereignty Instantiation
 *   domain: international_law/institutional_design/constitutional
 *
 * SUMMARY:
 *   The P5 veto power enshrined in Article 27 of the UN Charter is one
 *   instantiation of a fundamental principle in international law: no state
 *   can be bound by institutional decisions that it opposes when that state
 *   possesses the military capacity to resist enforcement. This reading
 *   approaches the veto through the lens of Westphalian sovereignty and
 *   structural inevitability: the constraint is not an institutional choice
 *   but a logical consequence of the absence of a global authority superior
 *   to the nuclear-armed great powers. The veto is natural law applied to
 *   international relations. This reading directly contests the
 *   oligopoly_reading (which frames the veto as entrenchment of extractive
 *   power arrangements) and differs from the coordination_reading (which
 *   frames the veto as a mechanism preventing great-power war). The
 *   sovereignty reading asserts that the veto is not a contingent feature of
 *   the UN system — it is an inevitable feature of ANY system of sovereign
 *   states with unequal enforcement capacity.
 *
 * KEY AGENTS:
 *   - P5 Nuclear Powers (United States, Russia, China, United Kingdom, France): Structural beneficiaries of the constraint but not in the sense of extraction — beneficiaries of the principle that binding law requires consent. Their power is the reason the veto is inevitable, not the reason it extracts.
 *   - Non-permanent Security Council members: Accept structural subordination in voting power but gain voice through the Council structure. Not targets of extraction — participants in a legitimacy mechanism.
 *   - General Assembly: Coordinate body without enforcement authority. Represent the broader international community but cannot compel great-power action.
 *   - International legal community: Frames the veto as both a logical necessity (sovereignty principle) and a contingent design choice (oligarchy reading).
 *   - Analytical Observer: Sees the structural constraint — any institution operating among unequal powers must accommodate the superior power's refusal to be bound.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.08).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.02).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "P5 Veto Power as Westphalian Sovereignty Instantiation").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_law/institutional_design/constitutional").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, '34201af7-1a3d-488d-92e0-2db1709ff5df').
narrative_ontology:cs_kernel_codification('34201af7-1a3d-488d-92e0-2db1709ff5df', fixed_text).
narrative_ontology:cs_authority_grounding('34201af7-1a3d-488d-92e0-2db1709ff5df', lineage).
narrative_ontology:cs_interpretation_layer_present('34201af7-1a3d-488d-92e0-2db1709ff5df').
narrative_ontology:cs_reading_relation('34201af7-1a3d-488d-92e0-2db1709ff5df', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('34201af7-1a3d-488d-92e0-2db1709ff5df', article_27_veto_power__oligopoly_reading, forecloses).
narrative_ontology:cs_axiom('34201af7-1a3d-488d-92e0-2db1709ff5df', foundational, no_law_without_consent_structural_necessity).
narrative_ontology:cs_axiom_status(no_law_without_consent_structural_necessity, holdable).
narrative_ontology:cs_axiom_grounding('34201af7-1a3d-488d-92e0-2db1709ff5df', no_law_without_consent_structural_necessity, deontological).
narrative_ontology:cs_axiom('34201af7-1a3d-488d-92e0-2db1709ff5df', foundational, nuclear_weapons_alter_sovereignty_mechanics).
narrative_ontology:cs_axiom_status(nuclear_weapons_alter_sovereignty_mechanics, holdable).
narrative_ontology:cs_axiom_grounding('34201af7-1a3d-488d-92e0-2db1709ff5df', nuclear_weapons_alter_sovereignty_mechanics, empirically_contingent).
narrative_ontology:cs_reference_frame('34201af7-1a3d-488d-92e0-2db1709ff5df', sovereignty_principle_as_structural_necessity).
narrative_ontology:cs_drift_state('34201af7-1a3d-488d-92e0-2db1709ff5df', contemporary_multipolar_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('34201af7-1a3d-488d-92e0-2db1709ff5df', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__sovereignty_reading, nuclear_armed_great_powers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NUCLEAR WEAPONS STRUCTURAL REALITY (MOUNTAIN) — No global institution can compel a nuclear-armed state into military confrontation it rejects without risking civilization-ending war. This is not a political choice or institutional design artifact — it is a structural necessity given the distribution of atomic weapons. Any Charter variant that omitted this veto would face the same coordination failure at the moment of enforcement.
constraint_indexing:constraint_classification(article_27_veto_power__sovereignty_reading, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 2: GREAT POWER ENFORCEMENT CAPACITY (MOUNTAIN) — A state with global military reach that is compelled by vote to enforce an action it opposes faces an impossible choice: obey an institution that has no power over it, or withdraw from the institution and dismantle its legitimacy. The veto is not extraction — it is a structural reflection of the fact that institutions depend on the consent of the powerful for execution. The mountain derives from this asymmetry, which is permanent given the nuclear weapon's existence.
constraint_indexing:constraint_classification(article_27_veto_power__sovereignty_reading, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / SOVEREIGNTY PRINCIPLE (MOUNTAIN) — The Westphalian principle that no state can be bound by international law without consent is not a contingent institutional choice — it is a logical necessity given the absence of a superior authority. In a system of sovereign states, the only law that can bind is law to which states have consented. The veto codifies this structural truth. The principle emerges naturally from the anarchic structure of international relations itself.
constraint_indexing:constraint_classification(article_27_veto_power__sovereignty_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(article_27_veto_power__sovereignty_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_27_veto_power__sovereignty_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Near-zero because the mountain reading does not treat the veto as extraction from anyone — it treats it as a structural necessity. The 0.08 value captures minimal performative content (states ritually debate resolutions knowing the outcome) but no systematic extraction by one party from another. The veto is not a mechanism by which the P5 *extract* benefits from non-P5 states; it is a mechanism by which the system *respects* the structural reality that the P5 cannot be compelled. Suppression (0.02): Nearly absent. States explicitly consent to the UN system and its Charter. The veto is written, public, and known. Suppression of *alternatives to the veto* does exist (the charter cannot be amended without P5 consent, which the veto enables), but suppression of *the veto itself* is minimal — it is openly codified. Theater ratio (0.05): Extremely low. The veto is functional, not performative. When a P5 state uses the veto, the action is stopped — there is no ritual or theater, just outcome. The slight non-zero value accounts for the diplomatic language and international discourse around vetoes, but the constraint itself has almost no performative content.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces uniform classification (mountain across all perspectives) because the constraint is argued to be structurally invariant — every observer, regardless of power level or position, perceives the same structural inevitability. The absence of perspectival gap is the key diagnostic signature of this reading: if the constraint is truly natural law, it should appear as mountain from all positions. If the perspectives diverge significantly (e.g., powerful states see rope while weak states see snare), that would be evidence against the sovereignty reading and toward the oligopoly reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the sovereignty reading, directionality (d) is not computed from beneficiary/victim distinctions because the framing rejects extraction language altogether. All parties (P5 and non-P5) are positioned as accepting a structural constraint that derives from asymmetric power distribution, not from institutional design choices. The P5 are declared as beneficiaries not because they extract rents but because they benefit from the principle that their consent is required — but this benefit is coincidental to power, not extractive. The near-zero directionality logic reflects that the constraint operates at the civilizational/universal scope where power asymmetries are structural facts, not negotiable distributions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_framing,
    'Is the veto a structural inevitability of nuclear deterrence and state sovereignty, or a constructed institutional choice that naturalizes geopolitical power asymmetry?',
    'Comparative institutional analysis: examine whether alternative enforcement mechanisms (weighted voting, enforcement delegated to non-permanent members, graduated sanctions) would face the same structural failure. Test whether other nuclear-armed states show identical veto behavior or whether strategic doctrine variance exists.',
    'If structural: mountain classification holds and reading forecloses oligopoly_reading. If constructed: false summit — reclassify to tangled_rope with beneficiary extraction and reinstates oligopoly_reading''s core premise that the veto is institutional design choice, not natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_framing, conceptual, 'Whether veto is structural inevitability or constructed institutional choice').

omega_variable(
    consent_withdrawal_distinction,
    'What distinguishes ''binding without consent'' (which the sovereignty reading forbids as structurally impossible) from ''accepting institutional membership voluntarily but then voting against a resolution''?',
    'Clarify whether consent is a one-time act (joining the UN = blanket consent to majority rule) or continuous (each resolution requires affirmative consent). Compare with other international bodies (EU, regional security organizations) that lack vetoes and their enforcement mechanisms. Trace the philosophical lineage: is this Hobbesian (sovereign cannot alienate its power), Lockean (consent must be continuous and withdrawable), or Kantian (cosmopolitan law requires supermajority, not unanimity)?',
    'If consent is continuous: reading becomes rope (coordination mechanism). If consent is one-time: reading holds as mountain. The oligopoly_reading''s entire argument depends on this distinction — they argue the veto exploits the one-time interpretation to extract permanent authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_withdrawal_distinction, conceptual, 'Philosophical definition of consent as one-time or continuous act').

omega_variable(
    enforcement_capacity_asymmetry_stability,
    'As military technology, surveillance capacity, and economic interdependence change, does the structural asymmetry that justifies the veto remain stable, or does it converge toward a multipolar system where enforcement becomes genuinely distributed?',
    'Model enforcement capacity over time: track military spending, naval reach, weapons technology, cyber capacity, economic leverage. Determine whether the five-power concentration (P5) remains exceptional or whether secondary powers (India, Brazil, Japan, Germany) achieve enforcement capacity sufficient to execute global institutional decisions without P5 consent. Test convergence scenarios.',
    'If asymmetry persists: mountain remains stable. If asymmetry erodes: mountain becomes transitory — the structural justification dissolves, and the veto becomes an entrenched institution (tangled_rope or piton) defending a fading power distribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_capacity_asymmetry_stability, empirical, 'Long-term stability of enforcement capacity asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(veto_sov_theater_1945, article_27_veto_power__sovereignty_reading, theater_ratio, 1945, 0.02).
narrative_ontology:measurement(veto_sov_theater_2026, article_27_veto_power__sovereignty_reading, theater_ratio, 2026, 0.05).

% Extraction over time
narrative_ontology:measurement(veto_sov_extractiveness_1945, article_27_veto_power__sovereignty_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(veto_sov_extractiveness_1965, article_27_veto_power__sovereignty_reading, base_extractiveness, 1965, 0.07).
narrative_ontology:measurement(veto_sov_extractiveness_1990, article_27_veto_power__sovereignty_reading, base_extractiveness, 1990, 0.08).
narrative_ontology:measurement(veto_sov_extractiveness_2026, article_27_veto_power__sovereignty_reading, base_extractiveness, 2026, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, un_security_council_deadlock).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, great_power_concert_coordination).

% DUAL FORMULATION NOTE:
% The Article 27 veto constraint has been decomposed into three structurally distinct readings: the sovereignty_reading (natural law), the coordination_reading (mechanism preventing war), and the oligopoly_reading (extractive entrenchment). Each reading has a different ε value and produces a different classification from certain perspectives. All three share the same kernel text but differ on whether the veto derives from structural necessity, functional coordination, or institutional exploitation. The sovereignty_reading claims ε≈0.08 (near-zero extraction, natural law). The oligopoly_reading claims ε≈0.68 (high extraction, snare). The coordination_reading claims ε≈0.35 (moderate, rope/tangled_rope boundary). The network links show how these readings compete to interpret the same institutional feature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
