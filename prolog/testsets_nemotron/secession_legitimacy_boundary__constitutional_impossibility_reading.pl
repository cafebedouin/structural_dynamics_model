% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__constitutional_impossibility_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Constitutional Impossibility of Unilateral Secession
 *   domain: political/constitutional/federalism
 *
 * SUMMARY:
 *   This reading asserts that the constitutional order — understood as the
 *   1867 Confederation compact as amended — structurally precludes unilateral
 *   secession. The constraint is not a policy choice but a logical
 *   consequence of the constitutional architecture: the federation is an
 *   indissoluble whole whose continuity is the condition for any constituent
 *   part's legitimate authority. Separatist claims are not merely politically
 *   difficult but legally impossible; only a constitutional amendment under
 *   the amending formula (itself requiring substantial federal-provincial
 *   consensus) can alter the boundary. The reading treats the federal
 *   government not as an extractive party but as the constitutional trustee
 *   whose authority is coextensive with the constitutional order itself. The
 *   beneficiary groups named — federal_government, provinces_territories,
 *   constitutional_order — are not rent-collectors but the institutional
 *   vessels of the constitutional continuity this constraint protects.
 *
 * KEY AGENTS:
 *   - federal_government: Institutional trustee of constitutional continuity (institutional/arbitrage) — administers the amending formula and represents the whole
 *   - provinces_territories: Constitutional partners whose legitimacy derives from the same indissoluble compact (institutional/arbitrage) — each province's authority presupposes the federation's continuity
 *   - separatist_movements: Political actors claiming a right to exit outside the constitutional amending formula (organized/constrained) — their claim is structurally illegitimate under this reading
 *   - indigenous_nations: Treaty partners whose rights predate confederation but who are not secession claimants under this reading (organized/identity_locked) — their constitutional relationship is distinct from provincial secession
 *   - constitutional_courts: Analytical seat interpreting the constraint (analytical/analytical) — the Supreme Court's Reference re Secession (1998) is the authoritative articulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.08).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.72).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, mountain).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Impossibility of Unilateral Secession").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political/constitutional/federalism").

domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, '5d11465b-96b8-4aad-828c-2f65739ba95d').
narrative_ontology:cs_kernel_codification('5d11465b-96b8-4aad-828c-2f65739ba95d', formalized).
narrative_ontology:cs_authority_grounding('5d11465b-96b8-4aad-828c-2f65739ba95d', lineage).
narrative_ontology:cs_interpretation_layer_present('5d11465b-96b8-4aad-828c-2f65739ba95d').
narrative_ontology:cs_reading_relation('5d11465b-96b8-4aad-828c-2f65739ba95d', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('5d11465b-96b8-4aad-828c-2f65739ba95d', secession_legitimacy_boundary__grievance_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('5d11465b-96b8-4aad-828c-2f65739ba95d', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('5d11465b-96b8-4aad-828c-2f65739ba95d', foundational, constitutional_indivisibility).
narrative_ontology:cs_axiom_status(constitutional_indivisibility, holdable).
narrative_ontology:cs_axiom_grounding('5d11465b-96b8-4aad-828c-2f65739ba95d', constitutional_indivisibility, conventional).
narrative_ontology:cs_axiom('5d11465b-96b8-4aad-828c-2f65739ba95d', foundational, amending_formula_exclusivity).
narrative_ontology:cs_axiom_status(amending_formula_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('5d11465b-96b8-4aad-828c-2f65739ba95d', amending_formula_exclusivity, conventional).
narrative_ontology:cs_reference_frame('5d11465b-96b8-4aad-828c-2f65739ba95d', confederation_compact_1867).
narrative_ontology:cs_drift_state('5d11465b-96b8-4aad-828c-2f65739ba95d', contemporary_constitutional_order, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5d11465b-96b8-4aad-828c-2f65739ba95d', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, provinces_territories).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_order).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_indivisibility_principle).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, rule_of_law_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acts as constitutional trustee: administers the amending formula, represents the federation in international law, and bears responsibility for constitutional continuity. Does not 'collect' from the constraint — its authority is constituted by the constraint. Exit is arbitrage-grade because the federal government could theoretically propose dissolution via amendment but has no structural incentive to exit its own constitutional role.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Constitutional partners whose legitimate authority derives from the same indissoluble compact. Each province's constitutional status is secured by the federation's continuity. They benefit from the constraint because it guarantees the constitutional order that makes their authority possible. Exit options are arbitrage-grade: a province could initiate the amending formula but would be exercising its constitutional role, not exiting the constraint.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, provinces_territories, beneficiary,
    institutional, generational, arbitrage, national).

% Political movements claiming a right to unilateral secession. Under this reading, their claim has no constitutional standing — they are not parties to the constitutional compact but challengers to its logic. Their exit is constrained: they can pursue the amending formula (which requires federal and substantial provincial consent) or extra-constitutional action (which the constraint treats as illegitimate). They are 'excluded' not in the sense of being silenced but in the sense that their claimed role (secession claimant) is denied by the constraint's logic.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, separatist_movements, excluded,
    organized, biographical, constrained, regional).

% Treaty partners whose constitutional relationship predates Confederation. Under this reading, they are not secession claimants — their rights operate within the constitutional order (Section 35 rights, duty to consult, modern treaties). They are excluded from the secession question because the reading treats secession as a provincial-federal issue, not an Indigenous sovereignty issue. Their exit is identity_locked: their nationhood is constituted through treaty relationships that cannot be unilaterally severed without dissolving their own constitutional standing.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, indigenous_nations, excluded,
    organized, generational, identity_locked, national).

% The Supreme Court of Canada, whose Reference re Secession (1998) is the authoritative judicial articulation of this reading. The Court declared unilateral secession unconstitutional while holding that a clear referendum majority would create a political obligation to negotiate — but only within the constitutional amending formula. The Court's role is analytical: it interprets the constraint but does not bear its costs or collect its benefits.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_courts, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the foundational coordination problem of federal union: creating a stable, indissoluble political whole from historically distinct colonies/peoples, such that each part's authority is secured by the whole's continuity. The constraint coordinates by making the federation's continuity the condition of any part's legitimate authority — preventing the coordination failure of a dissoluble union where any part can hold the whole hostage.
% TRANSFER_FUNCTION: Moves no resources between parties. The constraint denies the premise that a transfer relationship exists between the federation and a would-be seceding unit. The federation does not extract from provinces; provinces do not pay for membership. The constitutional order is the framework within which all resource allocation occurs, not a party to allocation.
% ABSENT_VOICES: Indigenous nations who might claim that the constitutional_impossibility_reading's denial of their secession standing (while granting provinces a negotiated amendment path) constitutes an asymmetric denial of self-determination. Also absent: the international law perspective (earned sovereignty, remedial secession) which this reading treats as irrelevant to domestic constitutional validity. Both are structurally excluded by the reading's domestic constitutional frame.
% DISAPPEARANCE_RATIONALE: If the constitutional impossibility constraint vanished overnight, the federation would become a voluntary association dissoluble at any province's will. The amending formula would lose its gatekeeping function. Provincial governments could credibly threaten exit to extract concessions. The constitutional order would rearrange from a federal union into a confederal or treaty-based association — a fundamental regime change.
% FOUNDING_PROBLEM: The 1867 Confederation compact was built to solve the problem of creating a stable transcontinental federation from British North American colonies with distinct identities, economies, and fears of American absorption. The founding problem was: how to unite without allowing any part to dissolve the union when its interests diverged — the 'secession crisis' problem that had destroyed the American union in 1861.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (creating a stable indissoluble federation) is attested as still live by the federal government, all provincial governments, and the Supreme Court — none of which are mere beneficiaries in a narrow sense but the constitutional partners whose authority depends on the union's continuity. The 1998 Secession Reference explicitly affirms that the constitutional architecture was designed for permanence. No credible constitutional actor argues the founding problem is dead.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, ExtMetricName, E),
    domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.08) because the constraint does not transfer resources from separatists to the federation — it denies the premise that a transfer relationship exists. The federation does not 'extract' from a would-be seceding unit; the unit's constitutional status is constituted by the federation's continuity. Suppression is high (0.72) because the constraint is actively enforced through legal, political, and if necessary coercive means — but this suppression is not extractive; it is the suppression of a category error (treating constitutional membership as revocable at will). Accessibility collapse is very high (0.88) because once the constitutional logic is grasped, the alternative (unilateral exit) is not merely costly but conceptually incoherent within the framework. Resistance is low (0.12) because the resistance comes from actors whose claims the reading treats as conceptually mistaken, not from parties bearing extractive costs. Theater ratio is low (0.15) — the constitutional order's maintenance is functional, not performative; the amending formula is genuinely used (e.g., 1982 patriation, various bilateral amendments).
 *
 * PERSPECTIVAL GAP:
 *   From the separatist seat (under sibling readings), this constraint appears as a mountain that falsely naturalizes federal power — a false summit protecting extraction. From the constitutional_impossibility_reading seat, the mountain is genuine: the constraint is the constitutional logic itself, not a policy imposed on a pre-existing reality. The engine will compute per-seat types from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the constitutional order itself and its institutional vessels — federal_government and provinces_territories. They do not 'collect' from the constraint; they are constituted by it. Directionality for these seats is near-beneficiary (d ≈ 0.1) because the constraint secures the framework within which their authority operates. Separatist movements are not 'victims' — their claims are denied standing, not extracted from. The constitutional_courts seat is analytical (d = 0.5). Indigenous_nations under this reading hold a distinct constitutional relationship (treaty partnership) that is neither beneficiary nor victim of the secession constraint — they are excluded from the secession question entirely, which is an omega-level ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving constitutional continuity) remains live — the founding problem (creating a stable federal union from historically distinct colonies) has not been solved by making the union dissoluble. The arrangement has not atrophied into a piton; the amending formula remains the active mechanism for constitutional change. The theater ratio has crept up slightly as political rhetoric increasingly treats the constraint as a political choice rather than a logical necessity, but the structural function persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_treaty_relationship_ambiguity,
    'Does the constitutional_impossibility_reading''s treatment of Indigenous nations as ''excluded from the secession question'' constitute a structural omission that masks extractive dynamics?',
    'Analyze whether the reading''s denial of secession standing to Indigenous nations (while granting provinces a negotiated amendment path) creates an asymmetric extraction that the reading''s own logic cannot see. Compare with treaty_primacy_reading''s structural account.',
    'If the exclusion is extractive, the constraint reclassifies from mountain to snare or tangled_rope for the Indigenous_nations seat — the mountain claim would be a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_treaty_relationship_ambiguity, conceptual, 'Whether the reading''s constitutional logic silently extracts from Indigenous nations by denying them the amendment path available to provinces.').

omega_variable(
    natural_law_vs_constitutional_construction,
    'Is the constitutional indivisibility principle a genuine structural feature of federalism (natural law of political order) or a constructed constraint serving federal power?',
    'Comparative constitutional analysis: do all federal systems structurally preclude secession, or do some embed secession clauses (e.g., Ethiopia 1994, St. Kitts-Nevis)? If the latter, the ''natural law'' claim is falsified.',
    'If constructed, the mountain claim is a false summit; the constraint is a tangled_rope (coordination of federal stability + extraction of provincial exit option) or snare (pure extraction of exit option). FSM signature would trigger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constitutional_construction, conceptual, 'Whether the mountain''s natural-law claim survives comparative falsification.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does this reading genuinely foreclose the popular_sovereignty_reading, or do they operate in incommensurable frameworks that merely talk past each other?',
    'Test whether a single constitutional framework could coherently incorporate both the supremacy of the constitutional text and the self-legitimating character of a provincial referendum on secession. If no coherent framework exists, foreclosure is real; if frameworks are incommensurable, coexistence is the accurate relation.',
    'If foreclosure is real, the constitutional_impossibility_reading structurally displaces the popular_sovereignty_reading in any framework adopting its premises. If incommensurable, both remain live in different frameworks and the kernel is permanently contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Structural relationship between constitutional_impossibility_reading and popular_sovereignty_reading: foreclosure vs. incommensurability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 1867, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(secession_const_imposs_tr_t1867, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 1867, 0.1).
narrative_ontology:measurement(secession_const_imposs_tr_t1982, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 1982, 0.12).
narrative_ontology:measurement(secession_const_imposs_tr_t1995, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 1995, 0.14).
narrative_ontology:measurement(secession_const_imposs_tr_t2000, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(secession_const_imposs_tr_t2020, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(secession_const_imposs_tr_t2026, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(secession_const_imposs_be_t1867, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 1867, 0.05).
narrative_ontology:measurement(secession_const_imposs_be_t1982, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 1982, 0.06).
narrative_ontology:measurement(secession_const_imposs_be_t1995, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 1995, 0.07).
narrative_ontology:measurement(secession_const_imposs_be_t2000, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 2000, 0.07).
narrative_ontology:measurement(secession_const_imposs_be_t2020, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 2020, 0.08).
narrative_ontology:measurement(secession_const_imposs_be_t2026, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 2026, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(secession_const_imposs_su_t1867, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 1867, 0.6).
narrative_ontology:measurement(secession_const_imposs_su_t1982, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 1982, 0.68).
narrative_ontology:measurement(secession_const_imposs_su_t1995, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 1995, 0.72).
narrative_ontology:measurement(secession_const_imposs_su_t2000, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(secession_const_imposs_su_t2020, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(secession_const_imposs_su_t2026, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.1).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% The secession_legitimacy_boundary kernel decomposes into four constraint stories, each a distinct reading with different ε, beneficiaries/victims, and claimed_type. This reading (constitutional_impossibility) claims mountain with near-zero ε; popular_sovereignty_reading claims rope/tangled_rope with beneficiaries (provincial majorities) and victims (federal minorities); grievance_threshold_reading claims tangled_rope with beneficiaries (oppressed groups) and victims (federal authority); treaty_primacy_reading claims mountain/tangled_rope with beneficiaries (treaty holders) and victims (both federal and provincial claimants). All four linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
