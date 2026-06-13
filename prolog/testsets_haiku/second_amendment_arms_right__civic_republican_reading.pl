% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_civic_republican, []).

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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment Civic Republican Reading: Armed Citizenship as Republican Prerequisite
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The Second Amendment reads: 'A well regulated Militia, being necessary to
 *   the security of a free State, the right of the people to keep and bear
 *   Arms, shall not be infringed.' The civic republican reading interprets
 *   this as protecting armed citizenship organized in militia context as a
 *   structural prerequisite for republican self-governance—neither purely
 *   individual right (severed from civic duty) nor state monopoly authority
 *   (the collective reading), but a conditional right tied to civic
 *   participation. Citizens gain the right to bear arms because and insofar
 *   as they participate in the armed defense of the republic; the right
 *   carries an embedded duty. Regulatory authority is not nullified but
 *   reframed: qualifications, training, and eligibility standards become
 *   constitutional permissible as enforcement of the civic prerequisite, not
 *   restrictions on a pre-political individual claim. Extraction emerges
 *   through exclusion—populations denied militia standing are denied the
 *   right, and the constraint's persistence depends on maintaining the
 *   boundary between those whose civic status qualifies them and those who
 *   are excluded. This is one of three structurally distinct readings of the
 *   contested kernel 'second_amendment_arms_right'; the other two
 *   (individual_right and collective_right) instantiate different ε,
 *   different beneficiary structures, and different regulatory implications.
 *
 * KEY AGENTS:
 *   - armed_citizens_militia_context: Citizens within militia standing gain constitutional protection and incur civic duty.
 *   - republican_governance_authority: Sets and enforces militia standards; authority is constrained by civic participation norm but not eliminated.
 *   - excluded_or_disqualified_populations: Barred from both the right and militia participation on grounds of civic status.
 *   - individual_liberty_advocates: Structurally excluded from this reading's framework; their pre-political rights language is not recognized.
 *   - state_monopoly_advocates: Structurally excluded; their collective reading is explicitly rejected by the civic republican framing.
 *   - historical_militia_institutions: Benefit from a reading that ties arms rights to their participation and legitimacy.
 *   - constitutional_courts: Interpret the boundary between permissible civic qualification and unconstitutional disarmament.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.38).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.42).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment Civic Republican Reading: Armed Citizenship as Republican Prerequisite").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional/political").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, '474e227d-f892-4223-b30d-010fbd011559').
narrative_ontology:cs_kernel_codification('474e227d-f892-4223-b30d-010fbd011559', fixed_text).
narrative_ontology:cs_authority_grounding('474e227d-f892-4223-b30d-010fbd011559', lineage).
narrative_ontology:cs_interpretation_layer_present('474e227d-f892-4223-b30d-010fbd011559').
narrative_ontology:cs_reading_relation('474e227d-f892-4223-b30d-010fbd011559', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('474e227d-f892-4223-b30d-010fbd011559', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('474e227d-f892-4223-b30d-010fbd011559', foundational, armed_citizenship_prerequisite_for_republic).
narrative_ontology:cs_axiom_status(armed_citizenship_prerequisite_for_republic, holdable).
narrative_ontology:cs_axiom_grounding('474e227d-f892-4223-b30d-010fbd011559', armed_citizenship_prerequisite_for_republic, deontological).
narrative_ontology:cs_axiom('474e227d-f892-4223-b30d-010fbd011559', foundational, civic_duty_inseparable_from_arms_right).
narrative_ontology:cs_axiom_status(civic_duty_inseparable_from_arms_right, holdable).
narrative_ontology:cs_axiom_grounding('474e227d-f892-4223-b30d-010fbd011559', civic_duty_inseparable_from_arms_right, deontological).
narrative_ontology:cs_reference_frame('474e227d-f892-4223-b30d-010fbd011559', republican_militia_self_defense).
narrative_ontology:cs_drift_state('474e227d-f892-4223-b30d-010fbd011559', contemporary_professional_militarization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('474e227d-f892-4223-b30d-010fbd011559', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, armed_citizens_militia_context).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_self_governance_system).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, excluded_or_disqualified_populations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38): the constraint coordinates genuine civic self-defense function but excludes populations from both right and duty on the basis of civic status, creating a tiered access regime. Extraction is not the primary driver—the coordination problem is real and the beneficiaries (armed citizens and republican governance) genuinely solve it—but the mechanism carries asymmetric burden (those excluded are permanently barred). Suppression is moderate (0.42): the constraint requires active enforcement to maintain militia participation norms and to exclude those deemed ineligible, but it does not depend on preventing alternatives through coercion (an armed citizenry is the stated alternative to state monopoly, not something hidden). Theater is moderate-low (0.28): the civic participation language is partly performative (modern militia capacity is technically transformed, and most citizens do not actually participate in militia), but the constraint's function is not entirely theatrical—state militias do exist and do train, and civic republican theory does motivate governance frameworks in some jurisdictions. Resistance is high (0.71): the constraint faces substantial push-back from both individual-rights advocates (who read the right as pre-civic) and from state-centered actors (who would prefer monopoly). The measurement series show modest extraction drift upward in the early interval (observed T0-T10, extractiveness 0.32 → 0.37) as court decisions narrow militia-participation language and broaden individual-right protections, then plateauing as the reading stabilizes around a moderate compromise position. Theater ratio rises slightly (0.20 → 0.29 T0-T20) as the constraint's civic rhetoric faces modern militia obsolescence. All metrics share one time grid; every time point includes all three series. Cyclical dynamics are not pronounced in this constraint—the drift is secular rather than oscillating.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (republican governance authority) and the beneficiary (armed citizens in militia) should experience this constraint as coordination and duty—their d values should be mid-range (symmetric, around 0.5) because they both benefit from and bear costs for the armed-citizenship arrangement. The excluded populations should experience high d (near 1.0, targets) because they bear the cost of permanent exclusion with no benefit. Individual-rights advocates should experience the constraint differently from the militia-participants: the constraint limits their reading (d moderately high, ~0.6-0.7) because it binds the right to civic duty rather than releasing it as individual liberty. The engine derives d from beneficiary/victim declarations and exit options; the structural gap between militia-participants (beneficiaries with constrained exit due to civic obligation) and excluded populations (victims with trapped exit) drives the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: armed_citizens_militia_context and republican_self_governance_system collect the coordination benefit (armed populace check on tyranny, collective self-defense capacity) and the normative satisfaction of civic duty. Their d values derive from their beneficiary status and moderate exit options—constrained by civic duty but not coerced into it (organized agents). Victims: excluded_or_disqualified_populations bear the cost of permanent exclusion from both right and civic participation. Their d values derive from victim status and trapped exit (no path to qualify out of exclusion). The structural asymmetry is not economic (money does not change hands) but status-based: civic standing is allocated unevenly, and the right follows the allocation. The secondary victimhood of disqualified populations who want to participate (e.g., women excluded from militia in many historical contexts, or those excluded for felony conviction) is central to the extraction story—the constraint uses civic status as a gating mechanism, and once you are outside the status boundary, exit is not available.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (republic needs armed citizenry for self-defense against tyranny) was live in 1791 when militias were the primary armed force. By the 20th century, the founding problem is substantially dead: professional standing armies and national militaries have displaced citizen militias as the society's armed defense, making the civic-militia solution structurally obsolete. Modern state militias (National Guard) are integrated with federal control and trained for national service, not as a check on federal tyranny. Yet the Second Amendment's text persists, and the civic republican reading continues to be invoked, particularly when courts defend the militia-participation frame. This is a signature mandatrophy: the founding problem is gone, the regulation persists, and the constraint's legitimacy now rests on theater (invoking civic virtue language) and on the sunk cost of constitutional text rather than on the solution of a live problem. The classification as tangled_rope captures this: the constraint does coordinate armed participation (genuine function) and does extract through exclusion (asymmetric burden), and it requires active enforcement (militia training standards, eligibility criteria). But the extraction is increasingly visible as the coordination function decays—the theater rises and the genuine problem shrinks. An omega variable should document this obsolescence gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_participation_obsolescence,
    'Is the founding problem—armed citizenry as check on government tyranny—still live, or has professional militarization rendered citizen militia structurally obsolete?',
    'Historical analysis of militia function post-1950: are citizen militia systems actually deployed for self-defense against tyranny, or do they serve primarily ceremonial/support roles to professional militaries? Comparative institutional study of militia across democracies.',
    'If militia function is obsolete, the constraint should be reclassified as piton (theater maintaining institutional forms without solving the founding problem). If militia still carries check-on-tyranny function (e.g., in contexts of democratic backsliding), the tangled_rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_participation_obsolescence, empirical, 'Whether the civic republican founding problem remains live or has atrophied.').

omega_variable(
    civic_status_vs_rights_boundary,
    'Is the civic status boundary (who qualifies for militia participation) a legitimate coordinate of the constitutional right, or is it an illegitimate restriction on a pre-political right?',
    'Constitutional interpretation: does the Framers'' language (''the people'') include all persons or a subset defined by civic status? Can a right tied to civic participation be squared with post-civil-rights understandings of equal protection?',
    'If civic status is a legitimate coordinate, the constraint''s exclusions are features of the regulation, not bugs. If civic status is illegitimate, the constraint should be reclassified as snare (extraction through status exclusion). This is a reading-vs.-reading divergence; different judicial philosophies will resolve it differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_status_vs_rights_boundary, conceptual, 'Whether rights can legitimately be conditioned on civic status.').

omega_variable(
    militia_interpretation_instability,
    'What counts as ''well regulated militia'' under this reading, and is that interpretation stable across legal contexts?',
    'Track court decisions defining militia (National Guard vs. state militia vs. unorganized militia) over time; compare across federal, state, and circuit courts. Test whether the definition remains coherent or drifts toward individual-right or collective-right interpretations.',
    'If the militia definition drifts (particularly toward ''any able-bodied person'' or toward ''state military only''), the constraint''s classification would shift. Drift toward individual interpretation = movement toward rope (pure coordination). Drift toward collective interpretation = movement toward snare (extraction through state monopoly). A stable militia definition maintains tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_interpretation_instability, empirical, 'Institutional stability of the militia interpretation.').

omega_variable(
    suppression_internalization_dynamics,
    'Is the measured suppression (0.42) structural (legal barriers to disqualified populations) or internalized (those excluded have internalized their exclusion as legitimate)?',
    'Post-policy-change observation: if legal barriers to militia participation are removed (e.g., women admitted, felony disenfranchisement reconsidered), do previously excluded populations attempt to participate, or do they remain absent due to internalized exclusion?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests—the excluded carry the suppression with them even after barriers fall. If structural, removal of legal barriers would lower suppression and potentially reclassify the constraint (lower suppression + lower extractiveness = rope rather than tangled_rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_dynamics, empirical, 'Whether suppression is structural or internalized in the excluded population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__civic_republican_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(seco_tr_t5, second_amendment_arms_right__civic_republican_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(seco_tr_t10, second_amendment_arms_right__civic_republican_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(seco_tr_t15, second_amendment_arms_right__civic_republican_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__civic_republican_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(seco_tr_t25, second_amendment_arms_right__civic_republican_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(seco_tr_t30, second_amendment_arms_right__civic_republican_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(seco_be_t5, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(seco_be_t10, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(seco_be_t15, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(seco_be_t25, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 25, 0.39).
narrative_ontology:measurement(seco_be_t30, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(seco_su_t5, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(seco_su_t10, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement(seco_su_t15, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(seco_su_t25, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(seco_su_t30, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__civic_republican_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Second Amendment kernel 'second_amendment_arms_right'. All three readings interpret the same constitutional text but derive different constraints with different ε values, different beneficiary/victim structures, and different regulatory implications. The civic republican reading (this story) treats the right as tied to militia participation and civic duty (ε=0.38, tangled_rope). The individual reading treats the right as pre-political and not militia-dependent (higher ε, likely rope or snare depending on measurement). The collective reading treats the right as entirely state militia authority (different ε, likely snare or piton). Each reading is a separate constraint story linked via this network field. The readings coexist in contemporary constitutional dispute and influence one another—court decisions favoring one reading shift the operative environment for the others. This reading influences both siblings by defining the militia-participation alternative; a strong civic republican constitutional trend would constrain the individual reading and foreclose the pure-collective reading within a coherent single-framework commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__civic_republican_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
