% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Temple Sacrifice Commitment (Performance-Only Reading)
 *   domain: religious/legal/commitment_system
 *
 * SUMMARY:
 *   Under the performance-only reading of the temple sacrifice commitment,
 *   material instantiation is a logical requirement of the law itself: to
 *   study sacrifice without performing it is to preserve an archive, not to
 *   occupy the commitment. This reading emerged from the Talmudic period
 *   onward as a way to maintain the law's binding force after the Second
 *   Temple's destruction made performance impossible. The commitment is
 *   framed as a natural-law feature of the textual tradition—a
 *   mountain—because the performance-requirement appears to inhere in the
 *   law's own logic. However, this reading concentrates interpretive
 *   authority in the hands of those who define what 'dormancy' means and when
 *   (if ever) restoration becomes obligatory. The constraint's function has
 *   substantially transformed from performance-coordination to
 *   institutional-legitimation: study now preserves not the commitment's
 *   active occupation but the authority structure that manages its
 *   impossibility.
 *
 * KEY AGENTS:
 *   - Halakhic interpretive authority: institutional agenda-setter, defines the boundary between occupation and archiving
 *   - Temple sacrifice study practitioners: organized scholars, identity-locked into tradition, bear cost of non-performance
 *   - Messianic restoration advocates: powerless, future victims if commitment reinstantiation occurs without ethical evolution
 *   - Ethical monotheist critics: excluded from halakhic conversation, voice suppressed by framing their objections as external
 *   - Comparative religious scholarship: analytical observer, measures how other traditions handle dormant commitments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.18).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.12).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.18).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, mountain).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Temple Sacrifice Commitment (Performance-Only Reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious/legal/commitment_system").

domain_priors:emerges_naturally(temple_sacrifice_commitment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, '7e766232-0570-4254-8c98-de3cf55a68af').
narrative_ontology:cs_kernel_codification('7e766232-0570-4254-8c98-de3cf55a68af', fixed_text).
narrative_ontology:cs_authority_grounding('7e766232-0570-4254-8c98-de3cf55a68af', lineage).
narrative_ontology:cs_interpretation_layer_present('7e766232-0570-4254-8c98-de3cf55a68af').
narrative_ontology:cs_reading_relation('7e766232-0570-4254-8c98-de3cf55a68af', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('7e766232-0570-4254-8c98-de3cf55a68af', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('7e766232-0570-4254-8c98-de3cf55a68af', temple_sacrifice_commitment__symbolic_transformation, forecloses).
narrative_ontology:cs_axiom('7e766232-0570-4254-8c98-de3cf55a68af', foundational, material_instantiation_requirement).
narrative_ontology:cs_axiom_status(material_instantiation_requirement, holdable).
narrative_ontology:cs_axiom_grounding('7e766232-0570-4254-8c98-de3cf55a68af', material_instantiation_requirement, deontological).
narrative_ontology:cs_axiom('7e766232-0570-4254-8c98-de3cf55a68af', foundational, dormancy_preserves_obligation).
narrative_ontology:cs_axiom_status(dormancy_preserves_obligation, holdable).
narrative_ontology:cs_axiom_grounding('7e766232-0570-4254-8c98-de3cf55a68af', dormancy_preserves_obligation, conventional).
narrative_ontology:cs_reference_frame('7e766232-0570-4254-8c98-de3cf55a68af', textual_performance_covenant).
narrative_ontology:cs_drift_state('7e766232-0570-4254-8c98-de3cf55a68af', contemporary_diaspora_ethics, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7e766232-0570-4254-8c98-de3cf55a68af', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, halakhic_interpretive_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, talmudic_tradition_custodians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, temple_sacrifice_study_practitioners).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__performance_only, temple_sacrifice_study_practitioners).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__performance_only, messianic_restoration_advocates).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, material_instantiation_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, law_dormancy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the authoritative reading of Talmudic law within diaspora Jewish tradition. Under the performance-only reading, they adjudicate whether study constitutes valid occupation of the sacrifice commitment or merely preserves an archive of defunct practice. They define the boundary between historical scholarship and living halakhic obligation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, halakhic_interpretive_authority, agenda_setter,
    institutional, civilizational, analytical, global).

% Engage in intensive study of sacrifice law and its scriptural basis. Under performance-only reading, their study is explicitly NOT performance of the commitment—it is preservation work, like archival scholarship. They continue the practice because of identity-fusion with Talmudic learning tradition, not because it occupies the standing obligation. They bear the cost of maintaining scholarly continuity without claiming to fulfill the commandment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, temple_sacrifice_study_practitioners, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__performance_only, temple_sacrifice_study_practitioners, payer).

% Believe the sacrifice commitment remains binding and will become material again upon temple restoration. Under performance-only reading, they are structurally positioned as future victims: if restoration occurs, they face potential obligation to resume material sacrifice without the intervening ethical evolution that would reframe or transform the practice. Current legal dormancy creates liability exposure for them.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, messianic_restoration_advocates, payer,
    powerless, civilizational, identity_locked, global).

% Argue animal sacrifice is ethically indefensible regardless of dormancy status, and that treating it as merely suspended law rather than historically superseded practice enables future harm. They are kept outside the halakhic interpretive community and their objections are framed as external to the commitment system itself, not as internal reformation voices.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, ethical_monotheist_critics, excluded,
    moderate, biographical, constrained, global).

% Analyzes how different religious traditions handle dormant or suspended commitments and whether material instantiation is philosophically necessary or culturally contingent. Takes an external analytical stance to the interpretive contest.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, comparative_religious_scholarship, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__performance_only, halakhic_interpretive_authority).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains interpretive discipline and preserves continuity of Talmudic law across diaspora centuries when material performance is impossible. Study coordinates the tradition's scholarly community around a single authoritative text and interpretive method, preventing fragmentation into incompatible readings.
% TRANSFER_FUNCTION: Transfers authority over the sacrifice commitment's meaning from material performance to the halakhic interpretive establishment: they alone can declare whether a dormant law is archived or suspended, whether study satisfies or merely preserves, and what conditions would trigger restoration. The reading concentrates definitional power in institutional hands.
% ABSENT_VOICES: Animal welfare advocates and those harmed by future restoration are kept out of the interpretive conversation. The ethical reformation voices within Jewish tradition (those arguing for symbolic or spiritual transformation of the practice) are sidelined in favor of the legalistic dormancy framing. Communities that would refuse restoration obligations lack a seat at the commitment-definitional table.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished, the study_as_exercise and symbolic_transformation readings would come forward, redefining the commitment as either intellectually occupied or ethically reformed. The stakes are whether the commitment is a buried obligation waiting for restoration or a superseded historical practice. The constraint's disappearance would answer that metaphysical question by default in favor of the alternatives.
% FOUNDING_PROBLEM: After the Second Temple destruction (70 CE), Jewish law faced a structural impossibility: the sacrifice commandment remained in force by its own logic, but material instantiation was forbidden under gentile rule. How can the law be alive if it cannot be performed? The commitment required a framework to remain legally binding without active performance.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities from the Talmudic period onward affirm the founding problem. Contemporary halakhic sources treat the sacrifice law as suspended rather than abrogated. However, ethical monotheist critics counter that the problem is a pseudo-problem created by literalist legal reading—they attest that the commitment was always meant to be transcended by prophetic reform, and treating it as dormant rather than superseded is a cover story for institutional authority maintenance. Academic historians of religion (external to both communities) confirm the historical fact of the interpretive crisis but do not arbitrate which reading is correct.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_commitment__performance_only),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.18) is low because the constraint's primary mechanism is definitional rather than material: it does not extract resources, wealth, or labor from living subjects; it manages the symbolic status of a dormant law. However, it DOES extract authority—the halakhic establishment's power to declare what counts as occupation of the commitment is the constraint's real rent. Theater_ratio is very high (0.71) because the study practice, while real and demanding, increasingly functions as performance-maintenance of the commitment's symbolic status rather than as a coordinating mechanism that would cease if the commitment were redefined. The measurement series shows theater rising modestly through the first 1200 years (as the study practice stabilized and became institutionalized) and then stabilizing, suggesting the practice reached equilibrium as pure maintenance theater rather than continuing to evolve toward either real performance or genuine symbolic transformation. Suppression is low (0.12) because the constraint operates through authority and interpretation, not through coercive force; the 'excluded' status of ethical critics is enforced through interpretive gatekeeping, not through legal penalties. Accessibility_collapse is very high (0.92) because the constraint presents itself as logically inevitable—if you accept the texts as binding, the performance-requirement seems to follow necessarily. Alternatives (study-as-exercise, symbolic transformation) collapse from view once the performance-only reading is institutionalized, because they require contesting the authority that defines performance. Resistance is moderate (0.34) because the commitment's dormancy status is broadly accepted across Jewish tradition; resistance comes primarily from outside critics (not counted in real resistance) and from the minority who hold alternative readings (study_as_exercise, symbolic_transformation, hybrid_preparatory).
 *
 * PERSPECTIVAL GAP:
 *   From the halakhic authority's seat, the performance-only reading appears as a faithful interpretation of the law's own requirements and a necessary doctrine to preserve the commitment's binding force during the diaspora. The reading is presented as a natural consequence of the law's text. From the study practitioners' seat, the same reading generates a costly, identity-locked practice (study without occupation) that persists because exit would fracture their identity and community, not because it fulfills the commitment. From the restoration advocates' seat, the reading creates liability exposure—potential future obligation without the ethical evolution that would make fulfillment non-harmful. From the excluded ethical critics' seat, the reading is a false summit that uses the appearance of natural law to avoid renegotiating the commitment's ethical foundations. The engine's per-seat computation will capture this divergence: the authority's seat should compute as managing a natural constraint (mountain or rope from the administrator's view), while the study practitioners' seat should compute as bearing extraction costs without real benefit (snare or tangled_rope from the target's view), and the restoration advocates' seat should compute as trapped (high d, future victim status).
 *
 * DIRECTIONALITY LOGIC:
 *   The halakhic interpretive authority holds beneficiary status (and low directionality toward extraction, near 0.15) because it gains authority over the commitment's definition and the boundary between valid and invalid interpretations. The study practitioners hold a dual role: they are beneficiaries (they belong to a tradition, maintain identity, occupy a coherent community of practice) and payers (they bear the cost of maintaining a commitment that they acknowledge they do not occupy). This dual position yields a middling directionality (~0.5). The messianic restoration advocates are future victims: under this reading, they remain in suspended obligation status, lacking the ethical evolution that would make restoration compatible with contemporary values. Their directionality is high (~0.85) because restoration without ethical transformation would impose obligations they have no power to refuse. The ethical critics' exclusion is not modeled as extraction in this reading because the performance-only reading itself suppresses them from the analysis—their objections are treated as external to the commitment system, not as internal targets. This is itself the constraint: the framing that excludes their voices.
 *
 * MANDATROPHY ANALYSIS:
 *   The performance-only reading faces a mandatrophy diagnosis: the founding problem (how to maintain the law's binding force when performance is impossible) has been solved, but the solution (institutional authority managing dormancy through study and interpretation) has become the primary function, replacing the original commitment. The constraint no longer exists to enable sacrifice—it exists to maintain authority's interpretive power. The measurement series show theater_ratio rising over time, indicating the real function has atrophied and been replaced by performance-maintenance. However, mandatrophy is not yet total: the founding problem persists as 'contested' (restoration advocates still hold the possibility alive), and the constraint's classification should reflect that. The reading is not a pure piton because real coordination work (maintaining scholarly community) is embedded in the study practice, even if that coordination's function has shifted from performance-enablement to tradition-preservation. The classification should be tangled_rope with high theater: coordination function (study community cohesion) plus extraction function (authority control) plus performance theater (study as symbolic occupation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_reading,
    'Is the requirement that sacrifice law demands material instantiation a feature of the law itself (natural-law reading) or a constructed interpretive doctrine created by institutional authority to manage the commitment''s impossibility?',
    'Textual analysis of foundational Talmudic sources: do they explicitly state that study cannot substitute for performance, or does the performance-requirement doctrine emerge from later interpretive consensus and institutional practice?',
    'If natural-law (inherent to the textual commitment), the constraint is genuinely a mountain—study is archival by logical necessity. If constructed doctrine, the constraint is a tangled_rope where institutional authority extracts interpretive control by defining what ''occupation of the commitment'' means.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_reading, conceptual, 'Whether the performance-requirement is a property of the law or an interpretive construction by authority.').

omega_variable(
    ethical_evolution_path_dependency,
    'Can the sacrifice commitment be ethically rehabilitated and reinstantiated, or does material restoration require the ethical transformation that the performance-only reading forecloses?',
    'Rabbinic debate and philosophical analysis: can animal sacrifice be reframed as symbolic, humane, or contextual in a way that honors both the law and ethical monotheism? Or does treating it as a dormant natural law prevent the ethical renegotiation that would make restoration non-harmful?',
    'If rehabilitation is possible, the performance-only reading creates unnecessary victims in any restoration scenario. If rehabilitation is impossible, the reading''s denial of ethical transformation becomes protective. This determines whether future victims are created by the reading or prevented by it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethical_evolution_path_dependency, preference, 'Whether material restoration is ethically compatible with contemporary values under any reinterpretation.').

omega_variable(
    dormancy_as_false_summit,
    'Does treating the sacrifice commitment as ''dormant'' rather than ''superseded'' benefit identifiable institutional parties (the halakhic authority that adjudicates dormancy) and thus function as a false summit—a natural law that happens to preserve authority''s power?',
    'Comparative institutional analysis: examine which interpretive reading (performance-only vs. symbolic_transformation vs. study_as_exercise) concentrates authority and which disperses it. If performance-only reading uniquely concentrates authority, it is a false-summit candidate.',
    'If confirmed false summit, the constraint reclassifies to tangled_rope: dormancy doctrine is extraction by the interpretive authority, hidden behind the claim of inherent law. Study practitioners would be identified as targets bearing the cost of maintaining the institutional authority that defines dormancy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dormancy_as_false_summit, empirical, 'Whether institutional authority benefits from the performance-only reading in ways that suggest false-summit dynamics.').

omega_variable(
    study_coordination_function,
    'Does the study practice that the performance-only reading generates function as genuine coordination (maintaining scholarly continuity, preventing fragmentation) or as pure performance theater that mimics coordination to justify maintaining the dormant commitment?',
    'Historical analysis of study practice: does the commitment to studying sacrifice law produce scholarly cohesion that would be lost if the commitment were redefined? Or does the study practice persist regardless of how the commitment is defined, suggesting the coordination benefit is incidental, not essential?',
    'If genuine coordination, study is a low-extractiveness rope even under the performance-only reading. If theater, the high theater_ratio signals that study''s function has atrophied and the practice persists by inertia (piton candidate), not by real coordination need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_coordination_function, empirical, 'Whether the study practice under performance-only reading provides genuine coordination or has become performative theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.65).
narrative_ontology:measurement_basis(temp_tr_t0, observed).
narrative_ontology:measurement(temp_tr_t400, temple_sacrifice_commitment__performance_only, theater_ratio, 400, 0.68).
narrative_ontology:measurement_basis(temp_tr_t400, observed).
narrative_ontology:measurement(temp_tr_t800, temple_sacrifice_commitment__performance_only, theater_ratio, 800, 0.7).
narrative_ontology:measurement_basis(temp_tr_t800, observed).
narrative_ontology:measurement(temp_tr_t1200, temple_sacrifice_commitment__performance_only, theater_ratio, 1200, 0.71).
narrative_ontology:measurement_basis(temp_tr_t1200, observed).
narrative_ontology:measurement(temp_tr_t1600, temple_sacrifice_commitment__performance_only, theater_ratio, 1600, 0.72).
narrative_ontology:measurement_basis(temp_tr_t1600, observed).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_commitment__performance_only, theater_ratio, 2000, 0.71).
narrative_ontology:measurement_basis(temp_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(temp_be_t0, observed).
narrative_ontology:measurement(temp_be_t400, temple_sacrifice_commitment__performance_only, base_extractiveness, 400, 0.17).
narrative_ontology:measurement_basis(temp_be_t400, observed).
narrative_ontology:measurement(temp_be_t800, temple_sacrifice_commitment__performance_only, base_extractiveness, 800, 0.18).
narrative_ontology:measurement_basis(temp_be_t800, observed).
narrative_ontology:measurement(temp_be_t1200, temple_sacrifice_commitment__performance_only, base_extractiveness, 1200, 0.19).
narrative_ontology:measurement_basis(temp_be_t1200, observed).
narrative_ontology:measurement(temp_be_t1600, temple_sacrifice_commitment__performance_only, base_extractiveness, 1600, 0.19).
narrative_ontology:measurement_basis(temp_be_t1600, observed).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_commitment__performance_only, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement_basis(temp_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__performance_only, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(temp_su_t0, observed).
narrative_ontology:measurement(temp_su_t400, temple_sacrifice_commitment__performance_only, suppression_requirement, 400, 0.09).
narrative_ontology:measurement_basis(temp_su_t400, observed).
narrative_ontology:measurement(temp_su_t800, temple_sacrifice_commitment__performance_only, suppression_requirement, 800, 0.11).
narrative_ontology:measurement_basis(temp_su_t800, observed).
narrative_ontology:measurement(temp_su_t1200, temple_sacrifice_commitment__performance_only, suppression_requirement, 1200, 0.13).
narrative_ontology:measurement_basis(temp_su_t1200, observed).
narrative_ontology:measurement(temp_su_t1600, temple_sacrifice_commitment__performance_only, suppression_requirement, 1600, 0.13).
narrative_ontology:measurement_basis(temp_su_t1600, observed).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_commitment__performance_only, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement_basis(temp_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__performance_only, 0.1).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the temple_sacrifice_commitment kernel. The performance-only reading treats the commitment as a natural-law requirement for material instantiation. The study_as_exercise, hybrid_preparatory, and symbolic_transformation readings offer structurally distinct interpretations of the same foundational law. Each reading has a different epsilon and beneficiary structure; they are NOT perspectives on a single constraint but separate constraints instantiated by different halakhic communities. The performance-only reading concentrates authority in those who define dormancy; the transformation reading disperses authority by redefining the commitment itself; the study-as-exercise reading decentralizes authority by treating intellectual engagement as performance. The network edges model how authority struggles propagate across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_commitment__performance_only, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
