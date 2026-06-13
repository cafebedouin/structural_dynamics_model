% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment as State Militia Authority (Collective Right Reading)
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   The Second Amendment's text: 'A well regulated Militia, being necessary
 *   to the security of a free State, the right of the people to keep and bear
 *   Arms, shall not be infringed.' This constraint story instantiates ONE
 *   READING of a contested kernel: the collective-right reading holds that
 *   the amendment protects state authority to maintain militias, not
 *   individual ownership rights. The reading asserts that 'the people' in the
 *   militia context refers to the collective state body, not individuals;
 *   that the prefatory clause (militia necessity) governs the operative
 *   clause (right to bear arms); and that the amendment is fundamentally
 *   about federalism—preventing federal disarmament of state-controlled
 *   forces. This reading dominated constitutional law for much of the 20th
 *   century before being substantially displaced by individual-right
 *   jurisprudence. The reading is here treated as a genuine mountain (a
 *   stable interpretation with historical warrant) WHILE SIMULTANEOUSLY
 *   carrying beneficiary declarations (states, organized militia) that
 *   trigger false-summit evaluation—the omega variables document the
 *   irreducible ambiguity about whether this reading is the natural law or a
 *   constructed interpretation that benefits from claiming naturality.
 *
 * KEY AGENTS:
 *   - state_legislatures: institutional agenda-setters and beneficiaries; retain regulatory authority under this reading
 *   - organized_militia_state_forces: institutional beneficiaries; operate under state legislative control
 *   - individual_firearms_owners: excluded from constitutional protection; voice advocates for individual rights but is not party to the militia relationship
 *   - federal_judiciary: institutional agenda-setter; interprets the amendment and applies the collective-right reading to cases
 *   - originalist_constitutional_scholars: observers; provide historical and textual warrant for the collective-right reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.28).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.42).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, mountain).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment as State Militia Authority (Collective Right Reading)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional/legal/political").

domain_priors:emerges_naturally(second_amendment_scope__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '4de300ce-097f-4aad-b4dc-afcb83ed91aa').
narrative_ontology:cs_kernel_codification('4de300ce-097f-4aad-b4dc-afcb83ed91aa', fixed_text).
narrative_ontology:cs_authority_grounding('4de300ce-097f-4aad-b4dc-afcb83ed91aa', lineage).
narrative_ontology:cs_interpretation_layer_present('4de300ce-097f-4aad-b4dc-afcb83ed91aa').
narrative_ontology:cs_reading_relation('4de300ce-097f-4aad-b4dc-afcb83ed91aa', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('4de300ce-097f-4aad-b4dc-afcb83ed91aa', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_axiom('4de300ce-097f-4aad-b4dc-afcb83ed91aa', foundational, militia_is_state_institution).
narrative_ontology:cs_axiom_status(militia_is_state_institution, holdable).
narrative_ontology:cs_axiom_grounding('4de300ce-097f-4aad-b4dc-afcb83ed91aa', militia_is_state_institution, empirically_contingent).
narrative_ontology:cs_axiom('4de300ce-097f-4aad-b4dc-afcb83ed91aa', foundational, prefatory_clause_governs_operative).
narrative_ontology:cs_axiom_status(prefatory_clause_governs_operative, holdable).
narrative_ontology:cs_axiom_grounding('4de300ce-097f-4aad-b4dc-afcb83ed91aa', prefatory_clause_governs_operative, deontological).
narrative_ontology:cs_reference_frame('4de300ce-097f-4aad-b4dc-afcb83ed91aa', federalism_militia_authority_framework).
narrative_ontology:cs_drift_state('4de300ce-097f-4aad-b4dc-afcb83ed91aa', contemporary_post_heller_jurisprudence, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('4de300ce-097f-4aad-b4dc-afcb83ed91aa', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, organized_militia_state_forces).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, ExtMetricName, E),
    domain_priors:suppression_score(second_amendment_scope__collective_right_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(second_amendment_scope__collective_right_reading),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.28) because the constraint is narrowly scoped: it assigns a constitutional right to states and militia institutions, not to the broad population. There is no diffuse extraction mechanism—the constraint does not take something from many and give to few; rather, it assigns a constitutional power to a specific institutional set (states) and denies it to another (individuals). Suppression is MODERATE (0.42) because the collective-right reading must actively suppress the individual-right interpretation to maintain its coherence; the text is genuinely ambiguous, and sustained suppression of the individual-rights reading requires interpretive and rhetorical work. Theater is MODERATE-LOW (0.31): judicial opinions that adopt the collective-right reading do substantive historical and textual work, but the reading also serves a regulatory interest (states prefer broad authority; federal courts preferred doctrinal stability over individual rights for much of the 20th century), which introduces performance. Accessibility_collapse is HIGH (0.72): once the collective-right reading is understood and institutionalized, the alternative (individual-right reading) is linguistically and legally difficult to access from within that framework—the reading collapses awareness of the ambiguity. Resistance is HIGH (0.68): the individual-rights constituency has mounted sustained intellectual, political, and judicial resistance, especially since the 1970s; this reading has lost ground in contemporary doctrine despite its historical authority. The measurement series show slow gradual rise in extractiveness and theater over the interval, indicating the reading's increasing artificiality as contemporary doctrine moves away from it—the suppressive effort required to maintain it grows as the cultural/legal consensus shifts.
 *
 * PERSPECTIVAL GAP:
 *   State legislatures and federal judges adopting the collective-right reading experience this constraint as a legitimate, grounded interpretation that protects federalism and constitutional order. Individual-rights advocates and contemporary constitutional scholars experience it as a suppressed, historically contested reading that serves state power by denying individual constitutional protection. The engine's per-seat computation should reveal this: state beneficiary seats compute the constraint as a legitimate mountain, while individual-excluded seats compute it as a snare or tangled-rope structure that suppresses their claim to constitutional status. The gap is not a measurement error—it is the core signal the constraint story is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   State legislatures and organized militia forces are structural beneficiaries (d near 0.0): they hold the constitutional right and face no regulatory constraint from individuals. Individual firearms owners are structural excluded/targets (d near 1.0): they have no constitutional protection under this reading and can be regulated by states with no Second Amendment check. Federal judiciary sits near symmetric or analytical (d around 0.5): they interpret and apply the constraint; they are neither primary beneficiary nor primary target, though their rulings distribute benefits and burdens to the other seats. No directionality overrides are required—the structural derivation from beneficiary/victim + exit_options produces accurate d values across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids misclassification of suppression as coordination by clearly naming its extraction/assignment function: it assigns a constitutional right to states, not to individuals; it does not solve a collective-action problem that benefits all participants. If misread as a 'rope' (pure coordination), the constraint would appear to solve the founding problem of militia disarmament without extraction. But the reading's suppression (0.42) and the active resistance to it (0.68) make clear that coordination alone does not sustain the interpretation—the state interest in regulatory authority is doing structural work. This is not a snare (no hidden victims or coercive extraction of goods); it is a legitimate constitutional assignment of a right to one party and denial to another. The mandatrophy risk is different: as the constraint's historical warrant weakens and individual-rights precedent accumulates, the constraint drifts from mountain to piton—persistent not because it solves a live problem but because it carries institutional and scholarly inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is this constraint a genuine feature of constitutional law and the framers'' intent, or is it a constructed interpretive reading that benefits state regulatory authority and benefits from framing as natural law?',
    'Historical evidence: contemporaneous founding-era documents, militia organization records, state constitutional provisions, and framing-era jurisprudence would establish whether states and framers intended the amendment to protect militia authority alone or also individual rights. The resolution depends on empirical historical scholarship.',
    'If the collective-right reading is the historical norm and natural interpretation, it is a genuine mountain (state authority is constitutionally settled). If historical evidence shows the framing was ambiguous or the amendment was understood to protect individual participation in militia, the reading becomes a false summit—constructed to benefit state regulatory authority by claiming naturality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, empirical, 'Whether this reading is the natural constitutional law or a constructed beneficiary interpretation.').

omega_variable(
    militia_scope_ambiguity,
    'What constitutes the ''militia'' for purposes of this reading? Is it limited to formally organized state forces (National Guard), or does it include unorganized militia (able-bodied citizens)? The text does not specify.',
    'Jurisprudential clarification via test cases: if courts must define ''militia'' to apply this reading, the definition will reveal whether the reading is a clean mountain or a space where interpretation becomes regulatory choice.',
    'A narrow definition (only formal state forces) keeps the reading consistent and institutional. A broad definition (unorganized militia = citizenry) begins to collapse the distinction from an individual-right reading, suggesting the amendment always contemplated some individual firearm entitlement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_scope_ambiguity, conceptual, 'Ambiguity in the definition of ''militia'' under the collective-right reading.').

omega_variable(
    doctrine_reversal_likelihood,
    'This reading held substantial judicial authority in the 20th century but has been substantially displaced by individual-right precedent (e.g., District of Columbia v. Heller, 2008). Will the collective-right reading be revived, or is its period of dominance over?',
    'Judicial trajectory and constitutional amendment: if future Supreme Court compositions restore the collective-right reading to majority status, or if the amendment is formally revised, the status would shift. Otherwise, the reading enters a defensive, marginalized posture.',
    'If marginalized, the reading becomes increasingly a piton (maintained theatrically by federalists and originalists studying history, not by active judicial enforcement). If revived, it returns to mountain or tangled-rope status. The trajectory is not a hidden parameter—it is observable in case law and scholarly influence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_reversal_likelihood, empirical, 'The future doctrine status of the collective-right reading in constitutional jurisprudence.').

omega_variable(
    originalism_vs_living_constitution_framing,
    'Does this reading depend on a specific interpretive methodology (originalism) or is it equally defensible under other constitutional theories (living constitutionalism, purposivism)? If methodology-dependent, is the constraint''s coherence vulnerable to shifts in judicial philosophy?',
    'Comparative jurisprudential analysis: can living-constitutionalist judges defend the collective-right reading on contemporary grounds, or does it require historical originalism to be plausible? If only originalism defends it, the reading is fragile.',
    'If only originalism defends the reading, shifts in judicial philosophy away from originalism directly threaten its status. If multiple methodologies can support it, the reading has more structural resilience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_vs_living_constitution_framing, conceptual, 'Whether the collective-right reading is methodologically dependent or robust across interpretive theories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__collective_right_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(seco_tr_t5, second_amendment_scope__collective_right_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__collective_right_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(seco_tr_t15, second_amendment_scope__collective_right_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement(seco_tr_t25, second_amendment_scope__collective_right_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(seco_tr_t35, second_amendment_scope__collective_right_reading, theater_ratio, 35, 0.31).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__collective_right_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(seco_be_t5, second_amendment_scope__collective_right_reading, base_extractiveness, 5, 0.21).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__collective_right_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(seco_be_t15, second_amendment_scope__collective_right_reading, base_extractiveness, 15, 0.26).
narrative_ontology:measurement(seco_be_t25, second_amendment_scope__collective_right_reading, base_extractiveness, 25, 0.27).
narrative_ontology:measurement(seco_be_t35, second_amendment_scope__collective_right_reading, base_extractiveness, 35, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__collective_right_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(seco_su_t5, second_amendment_scope__collective_right_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__collective_right_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(seco_su_t15, second_amendment_scope__collective_right_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(seco_su_t25, second_amendment_scope__collective_right_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(seco_su_t35, second_amendment_scope__collective_right_reading, suppression_requirement, 35, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment scope is a contested kernel instantiating in three separate constraint stories, one per reading. Each reading produces a different constraint with different beneficiary/victim structures, different types, and different ε values. This story (collective_right_reading) interprets the amendment as protecting state militia authority and assigning no individual constitutional right. Sibling readings interpret the same text as protecting individual rights, either unconditionally (individual_right_reading) or conditioned on militia participation (civic_right_reading). The readings coexist as live judicial and scholarly positions; neither logically forecloses the others. The structural delta lies in beneficiary assignment and regulatory authority distribution. All three stories should be linked via network.affects_constraints to preserve the constraint family relationship and enable contention analysis across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
