% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: Prophetic Revelation Authority: Endogenous Reinterpretation Reading
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   The Manifesto (1890) reversed the Church's historical doctrine on
 *   celestial plurality (polygamy) to monogamy as the binding covenant norm.
 *   Under this reading — the endogenous reinterpretation reading — the
 *   reversal is interpreted as genuine divine revelation channeled through
 *   prophetic authority, not as federal capitulation. God commanded the
 *   change to preserve the Church's prophetic succession and to advance the
 *   covenant to its current stage. Federal pressure is read as the
 *   circumstantial occasion (external timing) but not the cause (internal
 *   motivation). The Church's institutional hierarchy maintains its
 *   legitimacy not by abandoning polygamy under duress but by demonstrating
 *   its ongoing prophetic authority: God still speaks, guides, and refines
 *   the Church's practice. This reading preserves theological coherence (the
 *   covenant evolves; revelation is continuous) at the cost of absorbing
 *   substantial institutional costs (practitioners lose sanctioned practice;
 *   the Church's prior doctrine is deprecated). Extractiveness is low under
 *   this reading because the constraint's beneficiary is divine authority
 *   (not a human institutional seat collecting rents) and because the
 *   extraction is justified as the legitimate cost of prophetic guidance, not
 *   as institutional capture. The claim/metric independence here is critical:
 *   the claim is mountain (natural law of revelation), but the metrics
 *   describe what appears from outside as institutional power (suppression
 *   requirement rising, theater ratio low, accessibility collapse high). The
 *   divergence is intentional and is exactly what the committer frame
 *   captures — this reading's internal coherence versus external skepticism.
 *
 * KEY AGENTS:
 *   - Church institutional hierarchy: agenda-setter; maintains prophetic authority doctrine; frames reversal as divine guidance
 *   - Church membership body: beneficiary + payer; gains spiritual coherence; loses historical practice; identity-locked
 *   - Federal government: excluded; applied external pressure read as catalyst not cause
 *   - Polygamist practitioners: payer; bears acute cost of practice abandonment; constrained exit
 *   - Prophetic theological tradition: beneficiary (vindicated as operative); non-agent; represents the doctrine maintained
 *   - Analytical observer: sees full structure; neither benefits nor pays
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.18).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.22).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, mountain).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "Prophetic Revelation Authority: Endogenous Reinterpretation Reading").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious_institutional_history/political_theology").

domain_priors:emerges_naturally(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'be28eada-300b-4747-b866-9accb09efd96').
narrative_ontology:cs_kernel_codification('be28eada-300b-4747-b866-9accb09efd96', fixed_text).
narrative_ontology:cs_authority_grounding('be28eada-300b-4747-b866-9accb09efd96', lineage).
narrative_ontology:cs_interpretation_layer_present('be28eada-300b-4747-b866-9accb09efd96').
narrative_ontology:cs_reading_relation('be28eada-300b-4747-b866-9accb09efd96', marriage_commitment_legitimacy__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('be28eada-300b-4747-b866-9accb09efd96', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('be28eada-300b-4747-b866-9accb09efd96', foundational, manifesto_divinely_commanded).
narrative_ontology:cs_axiom_status(manifesto_divinely_commanded, holdable).
narrative_ontology:cs_axiom_grounding('be28eada-300b-4747-b866-9accb09efd96', manifesto_divinely_commanded, theological).
narrative_ontology:cs_axiom('be28eada-300b-4747-b866-9accb09efd96', foundational, covenant_progressive_dispensation).
narrative_ontology:cs_axiom_status(covenant_progressive_dispensation, holdable).
narrative_ontology:cs_axiom_grounding('be28eada-300b-4747-b866-9accb09efd96', covenant_progressive_dispensation, theological).
narrative_ontology:cs_axiom('be28eada-300b-4747-b866-9accb09efd96', secondary, federal_pressure_catalyst_not_cause).
narrative_ontology:cs_axiom_status(federal_pressure_catalyst_not_cause, holdable).
narrative_ontology:cs_axiom_grounding('be28eada-300b-4747-b866-9accb09efd96', federal_pressure_catalyst_not_cause, empirically_contingent).
narrative_ontology:cs_reference_frame('be28eada-300b-4747-b866-9accb09efd96', living_prophetic_revelation_doctrine).
narrative_ontology:cs_drift_state('be28eada-300b-4747-b866-9accb09efd96', contemporary_institutional_analysis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('be28eada-300b-4747-b866-9accb09efd96', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_prophetic_authority).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_institutional_succession).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, ExtMetricName, E),
    domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(marriage_commitment_legitimacy__endogenous_reinterpretation_reading),
    narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) under this reading because the constraint's primary beneficiary is divine authority (which collects no institutional rents) and because the extraction is theologically justified as revelation cost, not as institutional capture. The measurements show a slight rise over the 40-year interval (0.08 → 0.18) as the reversal settles and practitioners gradually accommodate to the new doctrine — the extraction becomes more deeply embedded as initial resistance decays. Suppression starts low (0.10) because the constraint's force initially comes from religious authority, not coercive machinery; it rises modestly to 0.22 as dissenting practitioners and external skeptics must be managed (suppression of dissent becomes necessary to maintain the reading). Theater ratio remains very low (0.04 → 0.08) because the constraint's function is genuine: it solves the Church's problem of maintaining prophetic authority through a major doctrinal shift. There is minimal performative overhead. Accessibility collapse is high (0.91) because once the reading is adopted and internalized, alternatives collapse nearly completely — practitioners cannot simultaneously hold both polygamy and monogamy as divinely sanctioned. Resistance is low (0.15) because the reading's authority is religious, not coercive; those who resist either leave the Church (imperfect exit, high cost) or conform. The time grid is shared: every metric is authored at every examination point (0, 5, 10, 15, 25, 40) so temporal analysis has clean alignment.
 *
 * PERSPECTIVAL GAP:
 *   The Church institutional hierarchy (agenda-setter) experiences this constraint as genuine divine guidance — prophetic authority is their core identity, and the Manifesto confirms it. From their position, the constraint appears as mountain-like: immovable revelation that happened to align with external political change. Polygamist practitioners (payer) experience the same constraint as extraction — their sanctioned practice is suddenly invalidated, they bear acute costs, and the theological justification for the reversal is opaque to them. From their position, the constraint appears extractive, even if the Church hierarchy claims it is prophetic. Federal actors and external scholars (excluded/observer) experience it differently still: as institutional capture or pragmatic adaptation. The engine computes these divergences from the structural data (different power atoms, different exit options, different relationship to the prophetic authority claim). The authored claim does NOT resolve the divergence; the claim itself is what gets evaluated against the metrics. That divergence — between the agenda-setter's experience (mountain) and the payer's experience (extraction) and the external observer's skepticism (capture) — is the point of the committer frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Church institutional hierarchy (agenda-setter, institutional power): d ≈ 0.1 (near beneficiary end). They set the constraint, maintain its authority, and their institutional legitimacy depends on its operation. They are not trapped; they can exit prophetic authority claims (institutionalize instead of charismatize) but their identity would rupture. Identity-locked beneficiary. Polygamist practitioners (payer, moderate power): d ≈ 0.8 (near target end). They bear the acute cost of practice abandonment, their alternatives are constrained (leave and rupture identity, or conform and abandon existing plural relationships), their power is limited relative to the institutional hierarchy. Trapped or identity-locked target. Church membership body (beneficiary + payer, organized power): d ≈ 0.5 (symmetric). They gain prophetic continuity and institutional coherence; they also lose a prior sanctioned practice and must psychologically absorb the reversal as legitimate. The dual role captures their ambiguous position. Federal government (excluded, institutional power): not seated in the constraint structure under this reading — their pressure is read as external catalyst, not internal motivation. Their d would be computed if they were included, but they are explicitly excluded from the reading's discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution depends on the relationship between founding_problem and disappearance_verdict. Under this reading: founding_problem = 'How does the Church maintain prophetic legitimacy and theological coherence through external pressure?' founding_problem_status = 'live' (the problem of maintaining prophetic authority in a changing world persists). disappearance_verdict = 'contested' (parties dispute whether the Manifesto solves the founding problem or evades it). The mismatch (live founding problem + contested disappearance) flags the reading as potentially zombie or captive — the founding problem is not truly solved, the Manifesto is not really responding to it, the constraint persists through theological assertion rather than functional solution. The mandatrophy engine would flag this for review: does the Church's claim to ongoing prophetic authority withstand scrutiny, or is the authority becoming performative? Theater ratio (0.08) is low, which argues against piton classification (degraded function). But the low theater ratio combined with the omega ambiguity about divine command vs. rationalization suggests the constraint is in critical dependence on continued belief in the prophetic authority frame. If that belief erodes, the constraint collapses into one of the sibling readings (exogenous override or hybrid pragmatic) where extractiveness and suppression rise sharply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_command_vs_institutional_rationalization,
    'Is the Manifesto a genuine expression of divine will channeled through prophetic authority, or an institutional rationalization of politically-motivated doctrinal change?',
    'Post-hoc coherence analysis: does the reinterpretation preserve internal theological consistency or does it require ad-hoc theological moves to accommodate federal pressure? Does the prophetic tradition''s own prior teachings predict or foreclose this revelation? Do subsequent institutional behaviors align with the prophetic framing or with political accommodation?',
    'If divine command: this reading stands; extractiveness remains low; the constraint is a mountain of revelation. If rationalization: the reading collapses; extractiveness rises substantially (institutional capture); the constraint reclassifies to snare or tangled_rope per sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_command_vs_institutional_rationalization, conceptual, 'Whether the constraint''s framing as prophetic revelation is genuine or theological cover for institutional pressure response.').

omega_variable(
    federal_pressure_causality_attribution,
    'What is the causal role of federal legal/social pressure in the Manifesto''s issuance? Is pressure the catalyst (this reading) or the cause (exogenous_override_reading)?',
    'Timing and sequence analysis: did prophetic utterances about monogamy predate federal pressure (supporting endogenous origin)? Are there private communications showing prophetic deliberation independent of political pressure? Do later Church actions suggest the reversal was motivated by doctrinal reasoning or by political survival calculation?',
    'If pressure is mere catalyst: this reading''s low extractiveness holds. If pressure is the determining cause: extractiveness rises; the institutional beneficiary (Church hierarchy) shifts from divine authority to self-interested political actor; the constraint moves toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_pressure_causality_attribution, empirical, 'Whether federal pressure is the catalyst or primary cause of the doctrinal reversal.').

omega_variable(
    prophetic_succession_legitimacy_grounding,
    'On what basis does the prophetic authority structure claim legitimacy for reinterpreting (vs. replacing) the prior doctrine? Is theological continuity preserved or merely asserted?',
    'Internal theological coherence test: does the Church''s own theological tradition support reinterpretation of settled doctrine, or does it claim settled doctrine as unchangeable? Are there prior reversals that established precedent for prophetic reinterpretation? Do dissenting members within the Church accept the reinterpretation as legitimate under the same authority framework?',
    'If continuity is genuine: the reading''s theological coherence supports mountain classification. If continuity is asserted ad-hoc: the constraint''s extraction becomes the institutional power to redefine covenant meaning unilaterally; extractiveness rises; classification shifts toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prophetic_succession_legitimacy_grounding, conceptual, 'Whether prophetic reinterpretation maintains theological coherence or represents unilateral power to redefine doctrine.').

omega_variable(
    reading_kernel_identity_contest,
    'Which of the three readings (endogenous_reinterpretation, exogenous_override, hybrid_pragmatic) is the correct interpretation of the Manifesto''s meaning and origin?',
    'This omega documents the irreducible committer contest. No single evidentiary program can resolve which reading is correct without first choosing a framework (divine authority, institutional analysis, pragmatic skepticism). The three readings coexist as live positions held by different parties; empirical evidence can constrain but cannot eliminate this ambiguity.',
    'Each reading instantiates a different constraint with a different beneficiary structure, extraction profile, and type classification. The corpus contains all three readings as separate constraint stories, linked via network.affects_constraints. The contest is not resolved; it is modeled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_identity_contest, conceptual, 'Irreducible multivalent reading of the Manifesto''s meaning, origin, and legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 15, 0.07).
narrative_ontology:measurement(marr_tr_t25, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(marr_tr_t40, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 15, 0.16).
narrative_ontology:measurement(marr_be_t25, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement(marr_be_t40, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 15, 0.18).
narrative_ontology:measurement(marr_su_t25, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 25, 0.21).
narrative_ontology:measurement(marr_su_t40, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.08).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel marriage_commitment_legitimacy. The Manifesto's meaning and origin is contested across three structural interpretations: (1) endogenous_reinterpretation_reading (this story) — divine revelation; (2) exogenous_override_reading — federal coercion; (3) hybrid_pragmatic_reading — strategic institutional adaptation. Each reading instantiates a different constraint with a different epsilon value, beneficiary structure, and type classification. All three are modeled as separate stories linked by network.affects_constraints. The readings coexist as irreducible positions held by different parties; the corpus preserves the contest rather than resolving it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
