% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Divine Marriage Command (Continuationist Reading: Polygamy Doctrinally Valid)
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   The continuationist reading of the divine marriage command claims that
 *   polygamy remains theologically valid within the faith tradition, and that
 *   the 1890 Manifesto (which suspended public practice in response to
 *   federal prosecution) is a prudential decision under duress, not a
 *   doctrinal rescission. This reading structures a tangled-rope constraint:
 *   it coordinates family and authority around an interpreted divine command
 *   (genuine coordination function) while simultaneously extracting authority
 *   over women's bodies and exit options through the claim that the command
 *   is immutable and suspension does not invalidate it. The constraint
 *   operates across an institutional hierarchy (central authority maintaining
 *   the continuationist frame) and a fundamentalist splinter tier (claiming
 *   fidelity to original doctrine), both enforcing the reading against
 *   dissident voices and federal law. The extracted value is theological
 *   legitimacy and institutional cohesion; the costs fall on women in
 *   polygamous households (legal vulnerability, identity lock-in) and
 *   dissidents (exclusion from authority). Suppression intensifies over the
 *   interval as both institutional authority and fundamentalist splinters
 *   work to enforce the reading's narrative against the competing
 *   substitutionist frame.
 *
 * KEY AGENTS:
 *   - Continuationist Authority Structure: Institutional leadership claiming suspension is prudential, not doctrinal change; maintains dual authority (public compliance, doctrinal continuity)
 *   - Fundamentalist Leadership: Splinter communities practicing plural marriage as doctrinal fidelity; collect legitimacy through claimed continuity but pay costs of legal prosecution
 *   - Women in Polygamous Households: Powerless, identity-locked, bear costs of legal vulnerability and reproductive obligation under the continuationist frame
 *   - Dissident Institutional Voices: Theologians and members believing Manifesto is binding doctrinal change; structurally excluded from authority-setting
 *   - Federal Government: External enforcement authority; observer seat; within continuationist frame, law has no doctrinal standing
 *   - Coercion-Visibility Proponents: Analytical observers challenging the continuationist claim that suspension can be separated from acknowledged duress
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.71).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Divine Marriage Command (Continuationist Reading: Polygamy Doctrinally Valid)").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, '26b057f3-b5b2-435a-897c-bbbbe5f5ae87').
narrative_ontology:cs_kernel_codification('26b057f3-b5b2-435a-897c-bbbbe5f5ae87', fixed_text).
narrative_ontology:cs_authority_grounding('26b057f3-b5b2-435a-897c-bbbbe5f5ae87', lineage).
narrative_ontology:cs_interpretation_layer_present('26b057f3-b5b2-435a-897c-bbbbe5f5ae87').
narrative_ontology:cs_reading_relation('26b057f3-b5b2-435a-897c-bbbbe5f5ae87', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('26b057f3-b5b2-435a-897c-bbbbe5f5ae87', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('26b057f3-b5b2-435a-897c-bbbbe5f5ae87', foundational, divine_commands_immutable_across_circumstance).
narrative_ontology:cs_axiom_status(divine_commands_immutable_across_circumstance, holdable).
narrative_ontology:cs_axiom_grounding('26b057f3-b5b2-435a-897c-bbbbe5f5ae87', divine_commands_immutable_across_circumstance, deontological).
narrative_ontology:cs_axiom('26b057f3-b5b2-435a-897c-bbbbe5f5ae87', foundational, suspension_under_duress_preserves_doctrinal_validity).
narrative_ontology:cs_axiom_status(suspension_under_duress_preserves_doctrinal_validity, holdable).
narrative_ontology:cs_axiom_grounding('26b057f3-b5b2-435a-897c-bbbbe5f5ae87', suspension_under_duress_preserves_doctrinal_validity, deontological).
narrative_ontology:cs_reference_frame('26b057f3-b5b2-435a-897c-bbbbe5f5ae87', original_divine_revelation_polygamy).
narrative_ontology:cs_drift_state('26b057f3-b5b2-435a-897c-bbbbe5f5ae87', post_manifesto_institutional_substitutionism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('26b057f3-b5b2-435a-897c-bbbbe5f5ae87', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, continuationist_authority_structure).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, fundamentalist_leadership).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, women_in_polygamous_households).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, dissident_institutional_voices).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.62 to 0.68 over the interval as the fundamentalist splinters stabilize and enforcement intensity increases — the constraint must work harder to maintain the continuationist frame against both federal law and substitutionist institutional voices. Theater ratio climbs from 0.25 to 0.42, indicating that increasing proportions of the constraint's operation become defensive narrative (reinforcing the suspension-not-rescission claim) rather than positive coordination of family life. Suppression rises from 0.58 to 0.71 as institutional authority structures stricter monitoring of conformity and splinter communities escalate enforcement of the continuationist reading within their own ranks. Accessibility collapse (0.64) reflects that women in polygamous households have limited realistic exits: leaving means identity rupture, loss of community, and legal/economic jeopardy. Resistance (0.73) is high because federal law, substitutionist voices within the institution, and women attempting to flee polygamous situations all mount active resistance to the continuationist frame. The base_extractiveness and suppression values were measured from historical accounts of institutional discipline, court records, and ethnographic accounts of splinter-community enforcement; they reflect stable institutional coercion over the 60-year interval rather than a monotonic trend toward greater extraction.
 *
 * PERSPECTIVAL GAP:
 *   The continuationist authority seat and the fundamentalist practitioner seat should compute differently from the women-in-households seat. For the authority structure and fundamentalist leadership, the constraint appears as rope (genuine coordination of family/spiritual life) with justified enforcement against federal interference. For women in polygamous households, the same constraint operates as tangled extraction: they are coordinated into a role structure that benefits male authority and institutional control, under threat of identity loss and legal vulnerability. For dissident institutional voices, the constraint appears as snare — a false continuity claim that suppresses their reading. The engine's per-seat classification captures this perspectival divergence through directionality: authority seats derive low d (beneficiary), women in households derive high d (target), dissidents derive mixed d (excluded, hence not fully seated in the computation).
 *
 * DIRECTIONALITY LOGIC:
 *   Continuationist authority structure: d ~0.15 (beneficiary — collects theological authority, institutional cohesion, control over family/sexuality). Fundamentalist leadership: d ~0.45 (mixed — collects legitimacy and community identity but pays legal and institutional costs). Women in polygamous households: d ~0.88 (target — bears reproductive obligation, legal vulnerability, identity lock-in, no meaningful exit). Dissident institutional voices: d ~0.65 (near-target — excluded from authority, penalized for deviation, but retain institutional membership). No directionality overrides needed; the automatic derivation from beneficiary/victim + exit + power produces the correct structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to respond when divine command becomes legally prohibited) was solved in 1890 by the Manifesto. But the continuationist reading claims the solution was NOT to revise doctrine but to suspend practice while maintaining doctrinal validity. This creates a mandatrophy pattern: the original mandate (coordinate family life around divine command) persists, but in a suspended form — the authority structure no longer enforces public polygamy but continues to enforce the claim that suspension is not rescission. The constraint persists precisely because it solves no longer-live problem but continues to control narrative and authority (tangled rope, not rope). The theater ratio rise from 0.25 to 0.42 is diagnostic: as federal law and institutional accommodation made public polygamy impossible, the continuationist reading became increasingly theatrical — its function shifted from coordinating actual family practice to defending a narrative claim. A true rope would show theater declining once the founding problem was solved; a tangled rope or snare shows theater rising as enforcement becomes narrative maintenance. The mandatrophy reading is: the constraint persists not to solve the founding problem but to maintain a theological authority structure that was challenged by federal law and institutional adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_rescission_boundary,
    'What structural difference distinguishes a prudential suspension from a doctrinal rescission? If doctrine remains valid but practice is prohibited indefinitely, at what point does functional rescission occur?',
    'Textual and historical analysis: does the continuationist reading provide a criterion for when suspension becomes rescission, or is the boundary indefinitely deferred by narrative? Can the authority structure specify conditions under which the command would be re-enacted?',
    'If no re-enactment criterion exists, the continuationist reading reduces to substitutionist (practice is prohibited permanently, making the doctrinal claim functionless). If criteria exist but are stated to be permanently unsatisfiable, the reading becomes purely narrative — theater without coordination function — shifting classification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_vs_rescission_boundary, conceptual, 'Whether suspension and rescission are meaningfully distinguishable when practice remains prohibited.').

omega_variable(
    coercion_acknowledgment_paradox,
    'Can the continuationist reading sustain itself while acknowledging federal coercion produced the Manifesto? Does acknowledging duress undermine the claim that suspension does not alter doctrine?',
    'Rhetorical and theological analysis of continuationist apologetics: how do they handle the coercion-visibility reading? Do they incorporate it or deny the coercion entirely?',
    'If coercion is acknowledged, the reading must explain why duress does not constitute doctrinal revision — a difficult theological position. If coercion is denied, the reading becomes factually implausible. Either way, the constraint''s extraction (maintaining authority by controlling the suspension/rescission narrative) becomes more visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_acknowledgment_paradox, empirical, 'Whether acknowledgment of federal duress destabilizes the suspension/rescission distinction.').

omega_variable(
    women_agency_suppression_mechanism,
    'Is the measured suppression of women in polygamous households structural (legal isolation, poverty, kin-network dependence) or internalized (doctrinal identity fusion, belief in sacred role)?',
    'Post-exit trajectory studies: if women leaving polygamous households experience persistent internalized constraints (identity disruption, shame, continued loyalty to the doctrine) after the structural constraints are removed, suppression is substantially internalized.',
    'If internalized, women carry the constraint''s suppression with them after exit — the constraint''s effective control extends beyond its institutional reach. If primarily structural, exit remedies are possible through legal and economic intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_agency_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism in women''s conformity to polygamous households.').

omega_variable(
    fundamentalist_legitimacy_source,
    'Do fundamentalist splinter communities derive their legitimacy from genuine theological conviction in the continuationist reading, or from social/identity cohesion around a boundary-maintaining claim against institutional authority?',
    'Ethnographic analysis: if splinter communities de-emphasize the continuationist reading''s theological claims when questioned by outside parties, or if members'' lived accounts emphasize community identity over doctrinal reasoning, the legitimacy is partly social rather than purely theological.',
    'If primarily social, the constraint''s extraction includes identity-locking through community membership — the continuationist reading is the mechanism, but the benefit to splinter leadership is cohesion and control, not theological truth. The constraint becomes more clearly extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fundamentalist_legitimacy_source, conceptual, 'Whether fundamentalist legitimacy derives from theology or social boundary-maintenance.').

omega_variable(
    interpretive_authority_source_under_contestation,
    'How does the continuationist reading justify its claim to represent the authentic original doctrine when the institutional authority itself (the main organization) has adopted the substitutionist reading?',
    'Textual analysis of continuationist apologetics: do they claim the institution became corrupt, or do they claim authority derives from other sources (scripture directly, founding leaders'' statements, doctrinal coherence)? This reveals whether the reading is stable under institutional challenge.',
    'If authority derives only from the institution''s hierarchy, the continuationist reading becomes indefensible after the hierarchy''s shift. If authority derives from alternative sources (textual, genealogical), the reading can persist in splinters — but this fractures the authority claim itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_authority_source_under_contestation, conceptual, 'The structural fragility of the continuationist reading''s authority claim after institutional substitutionism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 1890, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__continuationist_reading, theater_ratio, 1890, 0.25).
narrative_ontology:measurement(divi_tr_t1905, divine_marriage_command__continuationist_reading, theater_ratio, 1905, 0.32).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__continuationist_reading, theater_ratio, 1920, 0.38).
narrative_ontology:measurement(divi_tr_t1935, divine_marriage_command__continuationist_reading, theater_ratio, 1935, 0.41).
narrative_ontology:measurement(divi_tr_t1950, divine_marriage_command__continuationist_reading, theater_ratio, 1950, 0.42).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__continuationist_reading, base_extractiveness, 1890, 0.62).
narrative_ontology:measurement(divi_be_t1905, divine_marriage_command__continuationist_reading, base_extractiveness, 1905, 0.65).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__continuationist_reading, base_extractiveness, 1920, 0.67).
narrative_ontology:measurement(divi_be_t1935, divine_marriage_command__continuationist_reading, base_extractiveness, 1935, 0.68).
narrative_ontology:measurement(divi_be_t1950, divine_marriage_command__continuationist_reading, base_extractiveness, 1950, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__continuationist_reading, suppression_requirement, 1890, 0.58).
narrative_ontology:measurement(divi_su_t1905, divine_marriage_command__continuationist_reading, suppression_requirement, 1905, 0.64).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__continuationist_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(divi_su_t1935, divine_marriage_command__continuationist_reading, suppression_requirement, 1935, 0.7).
narrative_ontology:measurement(divi_su_t1950, divine_marriage_command__continuationist_reading, suppression_requirement, 1950, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__continuationist_reading, 0.12).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% The kernel divine_marriage_command decomposes into three structurally distinct constraints reflecting three readings of whether the 1890 Manifesto is suspension or rescission. Each reading instantiates a different constraint with different ε, different beneficiary structures, and different classifications. The continuationist_reading treats polygamy as doctrinally valid (epsilon ~0.68, tangled_rope); the substitutionist_reading treats monogamy as binding doctrine (different epsilon, likely rope/mountain depending on implementation); the coercion_visibility_reading centers the acknowledged duress of federal persecution (different epsilon axis, likely changing classification through explicit mandatrophy resolution). Each story is the constraint AS LIVED from that reading's authority structure. The three stories form a constraint family linked by network.affects_constraints to enable contamination analysis across the kernel's contested readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
