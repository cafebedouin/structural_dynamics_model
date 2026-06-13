% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Magisterial Authority: Composite Overdetermination Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint models Vatican II's magisterial authority through the
 *   'composite overdetermination' reading, which posits that the Council's
 *   texts intentionally encoded incompatible theological visions through
 *   ambiguous compromise formulations to achieve supermajority votes. This
 *   reading views the resulting hermeneutical struggle and implementation
 *   divergence not as failures, but as structural features of the conciliar
 *   process. The constraint is claimed as a Tangled Rope because it provided
 *   a coordination function (avoiding schism) but simultaneously created an
 *   extractive mechanism (transferring interpretive burden and control to the
 *   magisterium).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.65).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.7).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Magisterial Authority: Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, '923d9a5d-ae83-401a-a7e0-e0dac2d6a92f').
narrative_ontology:cs_kernel_codification('923d9a5d-ae83-401a-a7e0-e0dac2d6a92f', fixed_text).
narrative_ontology:cs_authority_grounding('923d9a5d-ae83-401a-a7e0-e0dac2d6a92f', lineage).
narrative_ontology:cs_interpretation_layer_present('923d9a5d-ae83-401a-a7e0-e0dac2d6a92f').
narrative_ontology:cs_reading_relation('923d9a5d-ae83-401a-a7e0-e0dac2d6a92f', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('923d9a5d-ae83-401a-a7e0-e0dac2d6a92f', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('923d9a5d-ae83-401a-a7e0-e0dac2d6a92f', foundational, conciliar_texts_encode_incompatible_visions).
narrative_ontology:cs_axiom_status(conciliar_texts_encode_incompatible_visions, holdable).
narrative_ontology:cs_axiom_grounding('923d9a5d-ae83-401a-a7e0-e0dac2d6a92f', conciliar_texts_encode_incompatible_visions, empirically_contingent).
narrative_ontology:cs_axiom('923d9a5d-ae83-401a-a7e0-e0dac2d6a92f', foundational, hermeneutical_control_is_locus_of_authority).
narrative_ontology:cs_axiom_status(hermeneutical_control_is_locus_of_authority, holdable).
narrative_ontology:cs_axiom_grounding('923d9a5d-ae83-401a-a7e0-e0dac2d6a92f', hermeneutical_control_is_locus_of_authority, conventional).
narrative_ontology:cs_reference_frame('923d9a5d-ae83-401a-a7e0-e0dac2d6a92f', conciliar_compromise_as_structural_feature).
narrative_ontology:cs_drift_state('923d9a5d-ae83-401a-a7e0-e0dac2d6a92f', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('923d9a5d-ae83-401a-a7e0-e0dac2d6a92f', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, papal_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, institutional_church_hierarchy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, theologians_seeking_clarity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, faithful_seeking_doctrinal_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of doctrinal interpretation, which benefits from the ambiguity of the conciliar texts as it allows for flexible application and the suppression of dissenting interpretations. Its authority is reinforced by the need to provide a 'final' reading.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, papal_magisterium, agenda_setter,
    institutional, generational, identity_locked, universal).

% Benefits from the compromise formulations as they allowed for supermajority votes, preserving institutional unity at the time. The ambiguity continues to serve by allowing diverse pastoral approaches while maintaining a facade of doctrinal consistency.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, institutional_church_hierarchy, beneficiary,
    institutional, generational, constrained, global).

% Struggle to reconcile the seemingly contradictory statements within the conciliar texts, leading to careers spent navigating hermeneutical impasses. Their intellectual labor is extracted to maintain the interpretive flexibility of the magisterium.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, theologians_seeking_clarity, payer,
    moderate, biographical, constrained, global).

% Experience confusion and frustration due to the lack of clear, consistent teaching, leading to internal conflict or disengagement. Their desire for clear guidance is exploited by the system's inherent ambiguity.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, faithful_seeking_doctrinal_coherence, payer,
    powerless, biographical, identity_locked, local).

% Reject the conciliar texts as inherently flawed or heretical, arguing for a return to pre-conciliar clarity. They are excluded from mainstream interpretive discourse and often face ecclesiastical sanctions.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_factions, excluded,
    organized, generational, constrained, global).

% Embrace the 'spirit' of Vatican II as a call for radical reform, often pushing interpretations beyond what the texts explicitly state. They are also subject to magisterial correction when their interpretations diverge too far from the official line.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_factions, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allowed for supermajority votes at the Council by crafting ambiguous compromise formulations, thus preserving institutional unity and preventing schism among bishops with deeply divergent theological views.
% TRANSFER_FUNCTION: Transfers the burden of reconciling incompatible theological visions from the Council Fathers to subsequent generations of theologians and the faithful, while simultaneously transferring hermeneutical control and interpretive flexibility to the papal magisterium.
% ABSENT_VOICES: The 10-12% of Council Fathers who voted against certain texts, signaling unresolved theological incompatibility, are now largely absent from the official narrative, their concerns subsumed by the 'spirit of consensus' narrative. Their objections would highlight the inherent contradictions this reading emphasizes.
% DISAPPEARANCE_RATIONALE: If the composite, overdetermined nature of Vatican II's texts were universally acknowledged and its interpretive ambiguity removed, the entire post-conciliar hermeneutical project would collapse. The magisterium would lose a key tool for maintaining control, and various factions would be forced to confront irreconcilable differences, potentially leading to significant institutional fragmentation.
% FOUNDING_PROBLEM: The Catholic Church faced deep internal divisions and a need to engage with the modern world, requiring a Council to address theological, pastoral, and liturgical issues while maintaining unity among bishops with widely divergent views.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Council, independent theologians, and sociologists of religion corroborate that the problem of internal division and engagement with modernity was live. The continued hermeneutical debates and ongoing internal tensions within the Church attest to the persistence of the underlying issues, even if the 'solution' itself became part of the problem. This corroboration comes from outside the immediate beneficiaries of the current interpretive regime.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the ongoing intellectual and spiritual labor required to reconcile the texts' internal tensions, a burden borne by theologians and the faithful. Suppression (0.70) is high because the magisterium actively enforces its interpretive authority, marginalizing readings that too strongly emphasize either continuity or rupture. The theater ratio (0.40) reflects the performative maintenance of a 'hermeneutic of continuity' that often downplays or reinterprets genuine textual tensions to preserve a unified institutional narrative. The 10-12% rejection votes at the Council are key evidence for the embedded incompatibility.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the magisterium, the composite nature of the texts is a feature that allowed for unity and ongoing development. From the perspective of theologians and the faithful, it is a source of confusion and an extractive demand for interpretive labor. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The papal magisterium and institutional hierarchy are beneficiaries, as the ambiguity grants them flexibility and reinforces their interpretive authority. Theologians and the faithful are payers, bearing the cost of reconciling the texts and experiencing doctrinal confusion. Traditionalist and progressive factions are excluded, as their 'pure' readings are suppressed to maintain the compromise. The 'identity_locked' exit option for the magisterium and faithful reflects the deep, constitutive nature of their relationship to the Church's teaching.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_intent_ambiguity,
    'Was the ambiguity in the Vatican II texts a deliberate strategy to achieve consensus, or an unavoidable outcome of complex theological debate?',
    'Further historical research into conciliar archives, private correspondence of Council Fathers, and theological diaries to ascertain explicit intentions behind compromise formulations.',
    'If deliberate, it strengthens the ''tangled_rope'' classification by highlighting the intentional nature of the extractive mechanism. If unavoidable, it might shift the classification towards a ''rope'' with unforeseen negative consequences, or a ''piton'' if the original coordination function atrophied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_intent_ambiguity, empirical, 'Whether textual ambiguity was intentional compromise or accidental byproduct.').

omega_variable(
    hermeneutical_control_locus,
    'Is the ''hermeneutic of continuity'' a genuine interpretive principle or a tool for magisterial control over post-conciliar theological discourse?',
    'Analysis of magisterial interventions: do they genuinely reconcile tensions, or primarily suppress dissenting interpretations? Examine cases where ''continuity'' is asserted despite strong textual evidence for discontinuity.',
    'If primarily a tool for control, the suppression metric is higher and the constraint leans more towards a ''snare''. If a genuine principle, the ''tangled_rope'' classification is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hermeneutical_control_locus, conceptual, 'Nature of the ''hermeneutic of continuity'' as principle vs. control mechanism.').

omega_variable(
    rejection_votes_significance,
    'Do the 10-12% rejection votes on key conciliar texts signify embedded theological incompatibility, or merely minor disagreements within an overall consensus?',
    'Detailed historical-theological analysis of the content of the rejected amendments and the stated reasons of the dissenting bishops, comparing them to the final text.',
    'If they signify incompatibility, it strongly supports this ''composite overdetermination'' reading and the ''tangled_rope'' classification. If minor, it weakens the claim of inherent textual tension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rejection_votes_significance, empirical, 'Significance of dissenting votes in Vatican II.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement_basis(vati_tr_t1965, observed).
narrative_ontology:measurement(vati_tr_t1980, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement_basis(vati_tr_t1980, observed).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement_basis(vati_tr_t1995, observed).
narrative_ontology:measurement(vati_tr_t2010, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement_basis(vati_tr_t2010, observed).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement_basis(vati_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement_basis(vati_be_t1965, observed).
narrative_ontology:measurement(vati_be_t1980, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement_basis(vati_be_t1980, observed).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement_basis(vati_be_t1995, observed).
narrative_ontology:measurement(vati_be_t2010, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement_basis(vati_be_t2010, observed).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2024, 0.65).
narrative_ontology:measurement_basis(vati_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement_basis(vati_su_t1965, observed).
narrative_ontology:measurement(vati_su_t1980, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement_basis(vati_su_t1980, observed).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement_basis(vati_su_t1995, observed).
narrative_ontology:measurement(vati_su_t2010, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement_basis(vati_su_t2010, observed).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2024, 0.7).
narrative_ontology:measurement_basis(vati_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Vatican II magisterial authority kernel. It posits that the conciliar texts are overdetermined composites encoding incompatible visions, leading to ongoing hermeneutical struggle. It directly influences and is influenced by the 'continuity' and 'rupture' readings, as its existence explains their persistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
