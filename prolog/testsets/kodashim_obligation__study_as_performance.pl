% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_performance, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_obligation__study_as_performance
 *   human_readable: Kodashim Study as Cosmic Performance (Binding Obligation Reading)
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   After the Second Temple's destruction (~70 CE), Jewish law faced a
 *   fundamental problem: the entire Kodashim (Sacrificial Law) corpus became
 *   legally impossible to perform. The constraint here is the binding
 *   obligation to STUDY Kodashim. This particular
 *   reading—study_as_performance—holds that studying sacrificial law itself
 *   enacts the cosmic function that sacrifice performed when the Temple
 *   stood. The law's spiritual efficacy is independent of Temple or restored
 *   practice; study alone fulfills the obligation. This reading competes with
 *   two sibling readings: study_as_archive (Kodashim is preserved for
 *   historical/identity reasons, not cosmic function) and
 *   study_as_preparation (Kodashim is studied to preserve technical knowledge
 *   for messianic restoration when performance becomes possible again). The
 *   three readings share a kernel—the text of Kodashim and the fact of Temple
 *   absence—but assign radically different functions to the studying act
 *   itself.
 *
 * KEY AGENTS:
 *   - studying_community: the scholars and practitioners who engage in Kodashim study as a binding obligation
 *   - competing_reading_communities: holders of archive and preparation readings who dispute the performance thesis
 *   - cosmic_order: (non-agent) the metaphysical entity this reading holds is sustained by study performance
 *   - analytical_observer: the frame comparing the three readings' structural differences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.0).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.0).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Kodashim Study as Cosmic Performance (Binding Obligation Reading)").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious_studies/jewish_law/textual_preservation").

domain_priors:emerges_naturally(kodashim_obligation__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, 'bfe3cb33-72d0-4850-987c-8876de999442').
narrative_ontology:cs_kernel_codification('bfe3cb33-72d0-4850-987c-8876de999442', fixed_text).
narrative_ontology:cs_authority_grounding('bfe3cb33-72d0-4850-987c-8876de999442', lineage).
narrative_ontology:cs_interpretation_layer_present('bfe3cb33-72d0-4850-987c-8876de999442').
narrative_ontology:cs_reading_relation('bfe3cb33-72d0-4850-987c-8876de999442', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_reading_relation('bfe3cb33-72d0-4850-987c-8876de999442', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('bfe3cb33-72d0-4850-987c-8876de999442', foundational, study_is_cosmic_performance).
narrative_ontology:cs_axiom_status(study_is_cosmic_performance, holdable).
narrative_ontology:cs_axiom_grounding('bfe3cb33-72d0-4850-987c-8876de999442', study_is_cosmic_performance, deontological).
narrative_ontology:cs_axiom('bfe3cb33-72d0-4850-987c-8876de999442', foundational, temple_absence_irrelevant_to_efficacy).
narrative_ontology:cs_axiom_status(temple_absence_irrelevant_to_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('bfe3cb33-72d0-4850-987c-8876de999442', temple_absence_irrelevant_to_efficacy, deontological).
narrative_ontology:cs_reference_frame('bfe3cb33-72d0-4850-987c-8876de999442', study_enacts_cosmic_performance).
narrative_ontology:cs_drift_state('bfe3cb33-72d0-4850-987c-8876de999442', post_temple_destruction_stabilized, gap(stable, minor, false)).
narrative_ontology:cs_created_at('bfe3cb33-72d0-4850-987c-8876de999442', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, cosmic_order).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, studying_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish scholars and communities that engage in the mandated study of Kodashim (sacrificial law). They understand their study as fulfilling a cosmic obligation that persists regardless of whether the Temple stands or sacrificial practice is possible. The act of study itself is the performance; no external Temple or physical sacrifice is required for efficacy.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, studying_community, beneficiary,
    moderate, generational, mobile, global).

% The cosmic structure that this reading holds is sustained by the performance of Kodashim study. The constraint does not extract from any actor; it is a claim about spiritual/metaphysical efficacy independent of Temple reconstruction or material circumstance.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, cosmos_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_performance, cosmos_order).

% Scholars and communities who read Kodashim as archive-only (study_as_archive) or as preparation for restoration (study_as_preparation) are excluded from this reading's framework. They would argue that study has a different function: either historical preservation or technical maintenance for future practice, not cosmic performance.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, competing_reading_communities, excluded,
    moderate, generational, constrained, global).

% The comparative frame that evaluates whether Kodashim study is performance, archive, or preparation. This reading asserts the performance thesis; sibling readings contest it.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains cosmic order through the performance of study; no material coordination problem is solved (the Temple's absence does not break the function because the function IS study itself, not preparation for or archival of practice).
% TRANSFER_FUNCTION: No transfer; no extraction. The reading declares study efficacious independent of any external outcome or resource flow. Spiritual/metaphysical efficacy is internal to the act of engagement.
% ABSENT_VOICES: Communities holding study_as_archive and study_as_preparation readings are structurally excluded from this framework. They would object to the claim that study alone (without preparation for restoration or archive function) is the mandated obligation. Their objections are substantive reading differences, not procedural exclusions—they remain within the same textual tradition but interpret the foundation differently.
% DISAPPEARANCE_RATIONALE: If this reading's binding obligation disappeared (i.e., if Kodashim study ceased to be mandated as cosmic performance), the contest would turn on whether cosmic order is damaged (performance reading) or whether the community loses archive/preparation benefit (archive/preparation readings). The reading's disappearance would be contested precisely because the three readings assign radically different functions to the same textual obligation.
% FOUNDING_PROBLEM: After the Temple's destruction, sacrificial practice became impossible; the question is whether Kodashim (the legal corpus governing sacrifices) remains a binding obligation and, if so, why. This reading holds that study itself IS the obligatory performance—the law remains binding because studying it enacts the cosmic function sacrifice performed, independent of Temple or restored practice.
% FOUNDING_PROBLEM_CORROBORATION: Maimonides and the rabbinic tradition attest that Kodashim study is a standalone obligation (Mishneh Torah, Hilkhot Korbanot 3:1). The Talmudic and medieval responsa literature attests debate over whether study IS performance or only preparation/archive. Contemporary Jewish law scholars outside the benefiting parties (Reform and Reconstructionist analyses, academic comparative-religion work) attest the performance reading remains live, though contested by equally credible alternative readings within the same tradition.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_performance, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_performance_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_obligation__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_obligation__study_as_performance),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_obligation__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading claims ZERO extractiveness and ZERO suppression because it asserts study is the performance itself—there is no victim set, no coercive enforcement, no asymmetric benefit. Accessibility collapse is extremely high (0.95) because once the performance reading is understood, alternative readings (archive, preparation) seem logically foreclosed to someone who accepts the core premise that study enacts cosmic function. Resistance is near-zero (0.05) because the reading generates no coercive threat; it is a voluntary commitment to a spiritual discipline. The measurements hold flat across the entire interval because the reading's core claim is that efficacy is independent of historical change—whether the Temple is absent, whether restoration prospects rise or fall, study's performance value does not change. This flat trajectory is characteristic of a genuine natural law assertion.
 *
 * PERSPECTIVAL GAP:
 *   All three reading communities sit on the same traditional texts; they diverge on the FUNCTION of study. From the studying community's seat in the performance reading, their scholarly work is the cosmic act; no other performance is needed. From the archive reading seat, the same work is historical preservation and identity-maintenance. From the preparation reading seat, it is technical knowledge-keeping for a future that may never come. The engine will compute these differently because the three readings have different beneficiary sets and different structural relationships to Temple restoration. The performance reading has no victim set and zero extractiveness by its own logic; the preparation reading implicitly assumes restoration is desirable, which could generate extractiveness if preparation is involuntary. The archive reading treats study as cultural heritage, which may have extraction if the community feels obligated to maintain identity against assimilation pressures.
 *
 * DIRECTIONALITY LOGIC:
 *   The studying community is declared as beneficiary, but the reading's logic is that they receive NO extraction—they GIVE the cosmic performance. The beneficiary is cosmic order itself (non-agent), which the reading holds is sustained by the act. This is orthogonal to standard directionality: the community is not gaining advantage over others; they are fulfilling an obligation that transcends their individual benefit. The reading's zero extractiveness rests on the claim that study has no coercive overlay, no victim set, and no resource transfer—it is pure obligation. This is internally consistent if and only if the studying community genuinely experiences study as volitional participation in cosmic function, not as imposed duty.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: after Temple destruction, Kodashim study remains obligatory because, per this reading, the obligation is to study itself, not to prepare for restoration. The performance reading sidesteps mandatrophy by asserting the obligation's function has NOT changed—it was always about cosmic performance, whether or not the Temple stood. This is the most elegant response to mandatrophy in the constraint family: the archive reading admits the function changed (from practice to preservation); the preparation reading admits practice is suspended (hence study is preparation); but the performance reading claims no functional change. Whether this elegance reflects truth or clever reframing is the subject of the three omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_metaphysical_vs_practical,
    'Is the ''cosmic performance'' enacted by study a metaphysical/spiritual claim (study changes the cosmic order through its performance) or a practical claim about obligation (study is obligatory because the obligation persists, independent of Temple restoration)?',
    'Comparative textual analysis across Maimonides, Kabbalistic sources, and modern halakhic commentary distinguishing metaphysical efficacy claims from obligation-persistence claims. Phenomenological study of how practitioners understand the claim.',
    'If the claim is primarily metaphysical (study changes reality), the reading is a genuine natural law assertion about cosmic structure. If primarily practical (obligation persists for its own sake), the reading is a normative commitment that could be contested on grounds of utility or purpose. This affects whether the reading competes with study_as_preparation (both claiming obligation) or sits orthogonal to it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_metaphysical_vs_practical, conceptual, 'Ambiguity between metaphysical efficacy and normative obligation in the performance reading.').

omega_variable(
    beneficiary_identity_cosmic_order,
    'What does it mean for ''cosmic order'' to be a beneficiary? Is cosmic order a reified entity that collects benefit, a regulative ideal, or a metaphorical framing of the studying community''s own spiritual states?',
    'Theological and philosophical analysis of what ''cosmic order'' refers to in the rabbinic and medieval sources; phenomenological inquiry into whether practitioners experience themselves as benefitting the cosmos or themselves through cosmos-aligning study.',
    'If cosmic order is reified (a real entity benefitting from study), the constraint is a genuine obligation to sustain something external. If it is regulative or metaphorical, the beneficiary may be the community itself and the reading may implicitly extract obligation from participants. This determines whether the reading''s zero extractiveness claim holds against all skeptical readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identity_cosmic_order, conceptual, 'Ambiguity in what ''cosmic order'' as beneficiary means ontologically.').

omega_variable(
    natural_law_vs_constructed_obligation,
    'Is the claim that Kodashim study enacts cosmic function a discovery about how the world is (natural law—study has intrinsic cosmic efficacy) or a human construction (the rabbinical community decided study would count as performance)? The constraint declares emerges_naturally: true, but beneficiaries are present—false-summit candidate.',
    'Genealogical analysis: when and how did the performance reading emerge in Jewish legal history? Was it an interpretation of pre-existing law or a deliberate reconstruction after Temple loss? Historical comparison with other reading traditions (Christianity, Islam, secular philosophy) on how destroyed institutions are theorized.',
    'If natural law (study has intrinsic cosmic efficacy), the constraint is a genuine mountain. If constructed (the rabbinical community performed a reading-creation), the beneficiary (community maintaining a coherent self-understanding post-destruction) makes the constraint a false summit—likely a Tangled Rope (community benefits from coherence, study is performatively enforced as binding) reclassified as Mountain by FSM. This reading itself claims one answer; the omega documents the irreducible uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_obligation, empirical, 'Whether the performance reading is a discovery or a post-destruction reconstruction (FSM candidate).').

omega_variable(
    sibling_reading_joint_performability,
    'Can a single scholar or community simultaneously hold the performance reading (study IS the cosmic act) and the preparation reading (study IS preparation for restoration) without logical contradiction?',
    'Logical analysis: if restoration happens and sacrifices resume, does the community''s prior study-as-performance claim remain valid, or does it retroactively become preparation? Textual analysis of scholars who hold both readings in different contexts.',
    'If they cannot coexist (restora­tion would falsify the performance reading''s claim that Temple absence is irrelevant), the relation is forecloses. If they can coexist (study is both performance now AND preparation for later), the relation is coexists_with. If the performance reading creates structural pressure on preparation (e.g., by insisting study is complete in itself, lowering motivation for restoration), the relation is influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_joint_performability, conceptual, 'Logical and hermeneutical compatibility of the performance and preparation readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_performance, theater_ratio, 0, 0.0).
narrative_ontology:measurement(koda_tr_t25, kodashim_obligation__study_as_performance, theater_ratio, 25, 0.0).
narrative_ontology:measurement(koda_tr_t50, kodashim_obligation__study_as_performance, theater_ratio, 50, 0.0).
narrative_ontology:measurement(koda_tr_t75, kodashim_obligation__study_as_performance, theater_ratio, 75, 0.0).
narrative_ontology:measurement(koda_tr_t100, kodashim_obligation__study_as_performance, theater_ratio, 100, 0.0).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_performance, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(koda_be_t25, kodashim_obligation__study_as_performance, base_extractiveness, 25, 0.0).
narrative_ontology:measurement(koda_be_t50, kodashim_obligation__study_as_performance, base_extractiveness, 50, 0.0).
narrative_ontology:measurement(koda_be_t75, kodashim_obligation__study_as_performance, base_extractiveness, 75, 0.0).
narrative_ontology:measurement(koda_be_t100, kodashim_obligation__study_as_performance, base_extractiveness, 100, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_performance, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(koda_su_t25, kodashim_obligation__study_as_performance, suppression_requirement, 25, 0.0).
narrative_ontology:measurement(koda_su_t50, kodashim_obligation__study_as_performance, suppression_requirement, 50, 0.0).
narrative_ontology:measurement(koda_su_t75, kodashim_obligation__study_as_performance, suppression_requirement, 75, 0.0).
narrative_ontology:measurement(koda_su_t100, kodashim_obligation__study_as_performance, suppression_requirement, 100, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_archive).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% The kodashim_obligation kernel decomposes into three constraint stories corresponding to three readings of what Kodashim study means after Temple destruction. Each reading has a different epsilon value, beneficiary structure, and classification. The archive reading treats study as cultural preservation (Rope); the preparation reading treats it as technical maintenance for future restoration (Scaffold); the performance reading (this constraint) treats study as enacting cosmic function independent of Temple (Mountain). The three stories are linked via network.affects_constraints. Each story authors the kernel_id and reading_id in cs_structure to signal membership in the family and structural relationship to siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
