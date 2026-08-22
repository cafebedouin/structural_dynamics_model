% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: P5 Veto as Westphalian Sovereignty Principle
 *   domain: institutional/constitutional/international
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the P5 veto power
 *   granted by UN Charter Article 27: the sovereignty reading. Under this
 *   reading, the veto is not a constructed institutional choice but an
 *   expression of the Westphalian principle that no state can be bound by
 *   international law without its consent, applied to the case of great
 *   powers possessing the enforcement capacity to make that principle
 *   structurally binding. The veto emerges as inevitable given the
 *   distribution of military power, nuclear deterrence, and the absence of
 *   any supranational enforcement mechanism. This reading treats the veto as
 *   a natural constraint (a Mountain), not as extraction or coordination. The
 *   same text, Article 27, is read differently by two sibling readings: the
 *   coordination_reading frames the veto as a mechanism preventing
 *   catastrophic great-power war, and the oligopoly_reading frames it as
 *   structural entrenchment of geopolitical rent-collection. This story
 *   presents only the sovereignty reading—clean, ε-invariant, and complete on
 *   its own terms.
 *
 * KEY AGENTS:
 *   - The P5 (permanent members of the UN Security Council): the set of states with veto power, understood here not as beneficiaries but as structural instances of Westphalian sovereignty—their veto reflects the irreducible power asymmetry, not an engineered benefit.
 *   - Smaller states and the broader UN membership: structurally excluded from the veto gate by the distribution of enforcement capacity, not by institutional design choice.
 *   - The UN Security Council as an institution: the vessel in which the sovereignty principle manifests; its rules reflect the power distribution it must govern.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.08).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.02).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "P5 Veto as Westphalian Sovereignty Principle").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "institutional/constitutional/international").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, '7b807ff9-7de5-40e2-896c-4922378d526d').
narrative_ontology:cs_kernel_codification('7b807ff9-7de5-40e2-896c-4922378d526d', formalized).
narrative_ontology:cs_authority_grounding('7b807ff9-7de5-40e2-896c-4922378d526d', extraction).
narrative_ontology:cs_interpretation_layer_present('7b807ff9-7de5-40e2-896c-4922378d526d').
narrative_ontology:cs_reading_relation('7b807ff9-7de5-40e2-896c-4922378d526d', article_27_veto_power__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('7b807ff9-7de5-40e2-896c-4922378d526d', article_27_veto_power__oligopoly_reading, forecloses).
narrative_ontology:cs_axiom('7b807ff9-7de5-40e2-896c-4922378d526d', foundational, westphalian_sovereignty_principle).
narrative_ontology:cs_axiom_status(westphalian_sovereignty_principle, holdable).
narrative_ontology:cs_axiom_grounding('7b807ff9-7de5-40e2-896c-4922378d526d', westphalian_sovereignty_principle, deontological).
narrative_ontology:cs_axiom('7b807ff9-7de5-40e2-896c-4922378d526d', foundational, enforcement_capacity_determines_bindingness).
narrative_ontology:cs_axiom_status(enforcement_capacity_determines_bindingness, holdable).
narrative_ontology:cs_axiom_grounding('7b807ff9-7de5-40e2-896c-4922378d526d', enforcement_capacity_determines_bindingness, empirically_contingent).
narrative_ontology:cs_reference_frame('7b807ff9-7de5-40e2-896c-4922378d526d', westphalian_anarchy).
narrative_ontology:cs_drift_state('7b807ff9-7de5-40e2-896c-4922378d526d', contemporary_institutional_hardening, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('7b807ff9-7de5-40e2-896c-4922378d526d', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stakeholders authored EMPTY (Pattern-5: an explicit assertion that no
% entity's arrangements depend on this constraint — paired with the
% world_unchanged verdict below, enforced by the schema).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None; this reading treats the veto as a structural inevitability, not a coordination mechanism.
% TRANSFER_FUNCTION: No extraction occurs under this reading; the veto is classified as a boundary condition imposed by the distribution of enforcement capacity, not as a transfer mechanism.
% ABSENT_VOICES: Institutional reformers and smaller states whose interests are structurally excluded from the veto gate are not parties to this reading—they are not in the conversation that defines Westphalian sovereignty as the referent.
% DISAPPEARANCE_RATIONALE: Under this reading, the veto does not disappear—it is a structural feature of how power distributes across enforcement capacity. Formal removal of Article 27 would not change the underlying fact: a global institution cannot compel a nuclear-armed state with global-reach military capacity to act against its perceived vital interests. Removing the veto text would only hide the constraint, not eliminate it.
% FOUNDING_PROBLEM: How can a global institution exist at all when its members include nuclear-armed states with the capacity to enforce their will globally? The answer under Westphalian sovereignty is: only by acknowledging that no member can be bound without consent.
% FOUNDING_PROBLEM_CORROBORATION: This is corroborated by structural realism in international relations scholarship (Waltz, Mearsheimer, Grieco on the impossibility of supranational authority in anarchic systems) and by the historical record: every major-power exit threat or veto has succeeded because no enforcement mechanism exists that could compel a P5 member against its will. The founding problem is attested by scholars outside the institutional beneficiary set and by the absence of counterexample enforcement actions.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, world_unchanged).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   Extractiveness is very low (0.08, rising slightly over time) because under the sovereignty reading, the veto extracts nothing—it simply acknowledges the structural fact that a global institution cannot compel a nuclear-armed great power. Suppression is near-zero (0.02) because no coercion is required; the constraint is self-enforcing through the irreducible distribution of enforcement capacity. Accessibility collapse is very high (0.92) because once a state grasps that it possesses veto power backed by military deterrence, the illusion that a global institution could compel it collapses completely. Resistance is minimal (0.05) because no party rationally resists a constraint they cannot overcome; P5 members accept the veto as their due, and smaller states accept it as inevitable. Theater ratio is zero because there is no performative component—the veto is pure structure. The small upward drift in base_extractiveness over the interval (1945–2026) reflects the gradual institutionalization and rhetorical formalization of what was initially an implicit fact; as institutional vocabulary around the veto hardened, a minimal amount of performative justification entered (Charter interpretation, procedural precedent), but the core constraint remains structural, not theatrical. Measurements are authored on a single shared time grid (1945, 1965, 1990, 2010, 2026) for both extractiveness and theater_ratio, with sparse data because the constraint's metric profile is remarkably stable across the interval—the reading asserts structural inevitability, not change. The authored claim (Mountain) matches the metric profile: emerges_naturally = true, accessibility_collapse high, resistance low, extractiveness minimal.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in this reading because there are no multiple seats perceiving the same constraint differently. The sovereignty reading asserts a single, universally accessible fact: great powers cannot be compelled by institutions they can refuse to join or defect from. Both P5 members and smaller states must acknowledge this fact. The gap appears only when comparing readings: a P5 member might endorse both the sovereignty reading (our veto reflects power distribution) and elements of the oligopoly reading (we benefit from exclusive decision-making authority). That cross-reading perception is handled via omega variables and the sibling reading structures, not via seat-divergence in this story.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading has no stakeholders and no beneficiary/victim framing because the veto is treated as a natural law, not as a constructed transfer. The constraint derives from the distribution of enforcement capacity, not from the choice of any agent. Smaller states do not 'pay' a cost extracted by P5 members; rather, they face the structural boundary condition that they cannot compel action from more powerful states. The sovereignty reading explicitly rejects the beneficiary framing that the oligopoly_reading would impose ('P5 members benefit by extracting institutional authority rents'). Under sovereignty framing, P5 members do not benefit—they simply are the states to which Westphalian principle applies, given their power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to construct a global institution when its members include nuclear-armed states with enforcement capacity—remains live under this reading. The veto is not a degraded institution (Piton) or a transitional mechanism (Scaffold); it is a permanent, irreversible consequence of the structural conditions it expresses. Mandatrophy (where the mandate outlives its function) does not apply because the mandate is not a policy goal but a boundary condition. There is no version of this reading in which the veto becomes obsolete while persisting—if the conditions that produce it (nuclear weapons, enforcement capacity asymmetry, Westphalian principle) persist, so does the veto. Conversely, if those conditions disappeared (all nuclear weapons destroyed, a genuine supranational enforcement capacity emerged), the veto would become impossible regardless of its Charter text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_sovereignty_vs_coordination,
    'Is the P5 veto a natural law of power distribution (sovereignty reading) or a coordination mechanism that prevents catastrophic great-power war (coordination reading)?',
    'The two readings have incompatible ε referents: sovereignty_reading assesses the veto as an inevitability given physical power distribution (ε ≈ 0); coordination_reading assesses the same veto as solving a real coordination problem (ε low but nonzero, classification as Rope). The distinction is not empirical—both readings agree on the facts—but depends on whether you ask ''Is this constraint avoidable?'' (coordination framing) or ''Is this constraint an expression of irreducible power asymmetry?'' (sovereignty framing).',
    'If the sovereignty framing is correct, institutional reform of the Security Council (removing the veto, redistributing seats) would be structurally impossible without the explicit consent of every P5 member, because no enforcement mechanism could compel consent. If the coordination framing is correct, the veto solves a real problem that alternatives (consensus voting, weighted voting, supermajority) would still face. The two readings support opposite conclusions about institutional evolutionary paths.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_sovereignty_vs_coordination, conceptual, 'Whether the veto is a structural inevitability or an engineered coordination solution.').

omega_variable(
    natural_law_vs_constructed_charter,
    'Does Article 27 instantiate a law of nature (Westphalian sovereignty + nuclear deterrence), or is it a constructed institutional choice that could have been different?',
    'Historical counterfactual: if the UN Charter had been drafted by a coalition excluding the Soviet Union, or if the Charter had banned unilateral veto authority, would a global security institution have been viable? The sovereignty reading asserts it would have failed or devolved to veto de facto (informal refusal to cooperate). The constructed-choice reading asserts an alternative design was possible and would have worked differently.',
    'If the veto is a natural law, then beneficiary lists are empty and false-summit detection cannot apply. If the veto is a constructed choice, the same structure becomes a Snare or Tangled Rope: beneficiaries (P5 members who extract authority rents) and victims (smaller states whose interests are overridden). This omega directly gates FSM evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_charter, conceptual, 'Whether Article 27 reflects an irreducible structural constraint or a reversible institutional design.').

omega_variable(
    enforcement_capacity_as_boundary,
    'Is global-reach enforcement capacity the determining factor in the veto''s inevitability, or do alternative institutional designs exist that could bind great powers without their explicit consent?',
    'Theoretical: can any global institution compel a state with nuclear weapons and blue-water navy to comply with a resolution it rejects, if that state refuses? If the answer is ''no,'' the sovereignty reading holds. If theoretical designs exist (e.g., supranational military, pooled veto power), the constraint is contingent, not inevitable.',
    'If enforcement capacity is the binding constraint, the veto is a Mountain under Westphalian sovereignty. If institutional designs can overcome the enforcement gap, the veto is a negotiated arrangement (Rope or Tangled Rope), and alternative readings become viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_as_boundary, empirical, 'Whether the veto''s persistence depends on enforcement capacity asymmetry or on institutional design choices.').

omega_variable(
    reading_incompatibility_sibling_contrast,
    'Can the same Article 27 text instantiate both a natural law (sovereignty reading) and an extractive oligopoly (oligopoly reading) simultaneously, or does adopting one reading logically foreclose the other?',
    'If the veto is a natural law of power distribution, it cannot be an extractive instrument—natural laws do not extract. If the veto is extractive, it cannot be inevitable—extraction requires choice to maintain, not structural inevitability. The two readings occupy different commitment frameworks.',
    'This omega directly determines the reading_relations entry for the oligopoly_reading: if they foreclose each other, the relation is ''forecloses''; if different parties can hold each reading coherently without internal contradiction, the relation is ''coexists_with''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_incompatibility_sibling_contrast, conceptual, 'Logical compatibility of the sovereignty and oligopoly readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__sovereignty_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement(arti_tr_t1965, article_27_veto_power__sovereignty_reading, theater_ratio, 1965, 0.0).
narrative_ontology:measurement(arti_tr_t1990, article_27_veto_power__sovereignty_reading, theater_ratio, 1990, 0.0).
narrative_ontology:measurement(arti_tr_t2010, article_27_veto_power__sovereignty_reading, theater_ratio, 2010, 0.0).
narrative_ontology:measurement(arti_tr_t2026, article_27_veto_power__sovereignty_reading, theater_ratio, 2026, 0.0).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__sovereignty_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(arti_be_t1965, article_27_veto_power__sovereignty_reading, base_extractiveness, 1965, 0.06).
narrative_ontology:measurement(arti_be_t1990, article_27_veto_power__sovereignty_reading, base_extractiveness, 1990, 0.07).
narrative_ontology:measurement(arti_be_t2010, article_27_veto_power__sovereignty_reading, base_extractiveness, 2010, 0.08).
narrative_ontology:measurement(arti_be_t2026, article_27_veto_power__sovereignty_reading, base_extractiveness, 2026, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_27_veto_power__sovereignty_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% Article 27 veto power is a contested kernel. The sovereignty_reading (this file) interprets the veto as a natural law expression of Westphalian sovereignty given power distribution asymmetry (ε near-zero, Mountain). The coordination_reading interprets the same text as a mechanism preventing great-power war (ε low, Rope). The oligopoly_reading interprets it as structural entrenchment of P5 authority rents (ε high, Snare). Each reading assesses the same Charter text under a different ε referent: sovereignty asks 'Is this constraint avoidable given physical power?'; coordination asks 'Does this constraint solve a real problem?'; oligopoly asks 'Who benefits from this constraint persisting?' The readings form a constraint family linked by network.affects_constraints and distinguished by their axioms and reference frames in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
