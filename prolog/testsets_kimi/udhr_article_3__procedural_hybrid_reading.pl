% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: UDHR Article 3 Procedural Hybrid Reading â Due Process Without Substantive Resolution
 *   domain: constitutional_law/human_rights
 *
 * SUMMARY:
 *   This constraint instantiates the procedural_hybrid_reading of the
 *   udhr_article_3 kernel. It reads Article 3 as guaranteeing habeas corpus,
 *   torture prohibition, and minimum due process procedurally, while
 *   deliberately not resolving whether the provision requires only negative
 *   liberty (freedom from state violence) or positive entitlements (material
 *   conditions for life and security). It coexists with both sibling readings
 *   in practice. The constraint is moderately extractive because emergency
 *   detention regimes, state secrecy doctrines, and uneven judicial review
 *   availability create a structural gap between procedural promise and
 *   substantive protection.
 *
 * KEY AGENTS:
 *   - state_executive_power (agenda_setter / institutional / constrained) â administers both protections and emergency exceptions
 *   - domestic_judiciaries (beneficiary / institutional / constrained) â gain review authority from the procedural mandate
 *   - ordinary_detainees (beneficiary / powerless / trapped) â receive habeas and torture protection under normal process
 *   - emergency_detainees (payer / powerless / trapped) â bear extraction through suspended procedural guarantees
 *   - torture_survivors (payer / powerless / trapped) â bear extraction where prohibition is theatrically maintained but practically breached
 *   - defense_counsel (beneficiary / moderate / constrained) â gain professional tools from the procedural framework
 *   - international_treaty_bodies (observer / institutional / analytical) â sustain the interpretive framework without enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.48).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.52).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "UDHR Article 3 Procedural Hybrid Reading â Due Process Without Substantive Resolution").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional_law/human_rights").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, 'c1b40542-8e42-4818-abca-2e134374e02c').
narrative_ontology:cs_kernel_codification('c1b40542-8e42-4818-abca-2e134374e02c', formalized).
narrative_ontology:cs_authority_grounding('c1b40542-8e42-4818-abca-2e134374e02c', lineage).
narrative_ontology:cs_interpretation_layer_present('c1b40542-8e42-4818-abca-2e134374e02c').
narrative_ontology:cs_reading_relation('c1b40542-8e42-4818-abca-2e134374e02c', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1b40542-8e42-4818-abca-2e134374e02c', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('c1b40542-8e42-4818-abca-2e134374e02c', foundational, procedural_minimum_without_substantive_resolution).
narrative_ontology:cs_axiom_status(procedural_minimum_without_substantive_resolution, holdable).
narrative_ontology:cs_axiom_grounding('c1b40542-8e42-4818-abca-2e134374e02c', procedural_minimum_without_substantive_resolution, conventional).
narrative_ontology:cs_axiom('c1b40542-8e42-4818-abca-2e134374e02c', foundational, habeas_corpus_as_universal_procedural_floor).
narrative_ontology:cs_axiom_status(habeas_corpus_as_universal_procedural_floor, holdable).
narrative_ontology:cs_axiom_grounding('c1b40542-8e42-4818-abca-2e134374e02c', habeas_corpus_as_universal_procedural_floor, conventional).
narrative_ontology:cs_reference_frame('c1b40542-8e42-4818-abca-2e134374e02c', procedural_minimum_security).
narrative_ontology:cs_drift_state('c1b40542-8e42-4818-abca-2e134374e02c', contemporary_counterterror_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c1b40542-8e42-4818-abca-2e134374e02c', '').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, domestic_judiciaries).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, ordinary_detainees).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, defense_counsel).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, emergency_detainees).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, torture_survivors).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, torture_prohibition_jus_cogens).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, habeas_corpus_customary_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise jurisdiction over detention challenges and torture claims under Article 3 procedural frameworks. Gain institutional authority and a defined role in reviewing executive detention, though their independence and effectiveness vary by regime type and emergency status.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, domestic_judiciaries, beneficiary,
    institutional, generational, constrained, national).

% Individuals in ordinary criminal or immigration detention who benefit from habeas corpus review, procedural safeguards, and the formal torture prohibition under standard rule-of-law conditions.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, ordinary_detainees, beneficiary,
    powerless, immediate, trapped, national).

% Invoke habeas corpus and torture prohibition on behalf of detainees. The procedural hybrid reading provides the legal tools and standing for their practice; they benefit from a formalized adversarial process even where enforcement is partial.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, defense_counsel, beneficiary,
    moderate, biographical, constrained, national).

% Sets domestic detention policy, declares states of emergency, and administers both the procedural safeguards and their exceptions. Holds the power to designate threats and to determine when normal procedural rules apply.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, state_executive_power, agenda_setter,
    institutional, generational, constrained, national).

% Individuals held in counter-terrorism or emergency detention where procedural guarantees are formally present but practically suspended, delayed, or narrowed by state secrecy and security classifications.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, emergency_detainees, payer,
    powerless, immediate, trapped, national).

% Individuals subjected to prohibited practices in contexts where evidentiary and jurisdictional barriers prevent effective remedy. The absolute prohibition exists procedurally but fails to protect them from extraction by state interrogators.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, torture_survivors, payer,
    powerless, immediate, trapped, national).

% Monitor state compliance, adjudicate individual complaints, and issue interpretive guidance on Article 3. Provide the authoritative interpretation that sustains the hybrid reading without direct enforcement power.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, international_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__procedural_hybrid_reading, state_executive_power).
narrative_ontology:fixing_cost_class(udhr_article_3__procedural_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the international community and domestic legal orders around a minimum procedural floor for detention review and an absolute torture prohibition, without requiring agreement on substantive welfare obligations or negative-liberty absolutism.
% TRANSFER_FUNCTION: Moves obligations to provide judicial review and procedural safeguards from the international plane to states; moves liberty from emergency detainees to state executive power through exception clauses; moves institutional authority to domestic judiciaries and international monitoring bodies.
% ABSENT_VOICES: Positive-entitlement advocates who would insist on state-provided legal aid and welfare prerequisites; negative-liberty absolutists who would reject any judicial deference to emergency claims; affected communities in non-state conflict zones where state-based habeas is structurally unavailable.
% DISAPPEARANCE_RATIONALE: If the procedural hybrid reading vanished, domestic courts would lose a key international anchor for habeas review, states would face renewed pressure to clarify either a narrower negative-liberty or broader positive-entitlement reading, and the current equilibrium permitting emergency exceptions without substantive resolution would collapse.
% FOUNDING_PROBLEM: Post-WWII arbitrary state violence, disappearance, and torture without judicial remedy; the need for an internationally coordinated minimum procedural guarantee against state deprivation of life and liberty.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from the San Francisco and Geneva drafting processes (outside the current beneficiary set of domestic judiciaries) attest the founding concern with Nazi and Stalinist arbitrary detention. Contemporary emergency detainees and torture survivors attest the problem persists; state executives and some security scholars attest the problem has transformed into asymmetric threats requiring procedural flexibility. International historians corroborate the founding intent; no unanimous corroboration exists for the current status.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the constraint coordinates genuine procedural protections (habeas, torture prohibition) while states exploit emergency exceptions to extract liberty. Suppression (0.52) reflects that customary and diplomatic alternatives are partly displaced by the treaty framework but not fully collapsed. Theater ratio (0.35) captures the growing gap between ratification/reporting performance and emergency practice. Accessibility collapse (0.40) acknowledges remaining alternatives (regional courts, domestic constitutional law). Resistance (0.55) is substantial because states resist full judicial oversight while courts and civil society push back against emergency overreach.
 *
 * PERSPECTIVAL GAP:
 *   Domestic judiciaries and defense counsel experience the constraint as a genuine coordination mechanism that authorizes review and professional practice. Emergency detainees and torture survivors experience the same structure as procedural theater that legitimates extraction. The engine computes this divergence from the structural role and exit data; the authored claim does not adjudicate between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Domestic judiciaries, ordinary detainees, and defense counsel are structural beneficiaries of the procedural framework (low d, subsidized by the constraint's coordination function). State executive power sits near the middle as agenda setter: it enforces the norm but also captures the extraction through emergency prerogatives. Emergency detainees and torture survivors are full targets (high d): they are trapped, identity-locked in their legal vulnerability, and the constraint amplifies extraction toward them through scope and power asymmetry. International treaty bodies are analytical (neutral d).
 *
 * MANDATROPHY ANALYSIS:
 *   The procedural hybrid reading avoids pure mandatrophy because its founding problem â arbitrary state violence without remedy â remains live. It is not a scaffold because it carries no sunset clause and is not framed as transitional. The classification as tangled_rope is warranted because the constraint simultaneously solves a real coordination problem (proceduralizing human rights review) and extracts asymmetrically (via emergency exceptions that concentrate harm on the powerless). If judicial review became purely formal and emergency exceptions swallowed the rule, the constraint would drift toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_exception_inherence,
    'Is the emergency detention loophole an inherent structural feature of the procedural hybrid reading, or an external violation that the reading itself condemns?',
    'Comparative analysis of jurisdictions adopting the procedural hybrid reading: if emergency exceptions are systematically embedded in the doctrinal architecture, they are inherent; if they are uniformly treated as breaches, they are external.',
    'If inherent, the constraint''s extractiveness is structurally baked in and the reading is more deeply tangled_rope; if external, the extraction is defection and the constraint trends toward rope with enforcement gaps.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_exception_inherence, conceptual, 'Whether emergency extraction is internal to the reading or external defection').

omega_variable(
    procedural_positive_slippage,
    'Does meaningful habeas corpus review require state-funded legal aid and court infrastructure, causing the procedural reading to collapse into the positive entitlement reading in practice?',
    'Empirical study of detention review outcomes across jurisdictions with and without state-funded counsel and robust court budgets.',
    'If positive resources are necessary for the procedural guarantee to function, the hybrid reading''s neutrality is unstable and it effectively vindicates the positive entitlement reading despite its declaratory neutrality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_positive_slippage, empirical, 'Whether procedural rights presuppose positive state resourcing').

omega_variable(
    kernel_reading_stability,
    'Can the procedural hybrid reading remain stable when pressed by negative-liberty absolutists or positive-entitlement advocates, or does it function only as a temporary modus vivendi?',
    'Track doctrinal evolution in international and regional jurisprudence: whether the hybrid reading is consolidating as a standalone position or migrating toward one sibling under pressure.',
    'If the reading is unstable, its classification as a persistent tangled_rope is time-limited and it may be a scaffold toward a resolved reading; if stable, it represents a durable coordination/extraction equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Stability of the hybrid reading against its sibling competitors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_a3_proc_hybrid_tr_t0, udhr_article_3__procedural_hybrid_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(udhr_a3_proc_hybrid_tr_t15, udhr_article_3__procedural_hybrid_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(udhr_a3_proc_hybrid_tr_t30, udhr_article_3__procedural_hybrid_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(udhr_a3_proc_hybrid_tr_t45, udhr_article_3__procedural_hybrid_reading, theater_ratio, 45, 0.25).
narrative_ontology:measurement(udhr_a3_proc_hybrid_tr_t60, udhr_article_3__procedural_hybrid_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(udhr_a3_proc_hybrid_tr_t75, udhr_article_3__procedural_hybrid_reading, theater_ratio, 75, 0.35).

% Extraction over time
narrative_ontology:measurement(udhr_a3_proc_hybrid_be_t0, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(udhr_a3_proc_hybrid_be_t15, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(udhr_a3_proc_hybrid_be_t30, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement(udhr_a3_proc_hybrid_be_t45, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 45, 0.35).
narrative_ontology:measurement(udhr_a3_proc_hybrid_be_t60, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(udhr_a3_proc_hybrid_be_t75, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 75, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(udhr_a3_proc_hybrid_su_t0, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(udhr_a3_proc_hybrid_su_t15, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(udhr_a3_proc_hybrid_su_t30, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(udhr_a3_proc_hybrid_su_t45, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 45, 0.45).
narrative_ontology:measurement(udhr_a3_proc_hybrid_su_t60, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(udhr_a3_proc_hybrid_su_t75, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 75, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
