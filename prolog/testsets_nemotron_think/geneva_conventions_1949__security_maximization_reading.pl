% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Security Maximization Reading of Geneva Conventions
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   The security maximization reading of the 1949 Geneva Conventions treats
 *   the treaties as peacetime aspirations that yield to operational necessity
 *   in asymmetric conflict. It expands the unlawful combatant category to
 *   deny POW status and habeas corpus, degrades civilian immunity via 'human
 *   shields' doctrine and collateral damage acceptance, normalizes indefinite
 *   detention without trial, and redefines coercive interrogation as
 *   non-torture. This reading is advanced by state security establishments to
 *   maximize operational freedom against irregular adversaries. The
 *   constraint is the legal framework that enables these suspensions. It
 *   claims to coordinate state responses to irregular warfare (coordination
 *   function) while extracting protections from the most vulnerable (transfer
 *   function). The engine will compute per-seat classifications from the
 *   structural data: the agenda-setter seat (state security) experiences low
 *   effective extraction, while the payer seats (protected civilians,
 *   detainees, unlawful combatants) experience high effective extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.85).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.9).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Security Maximization Reading of Geneva Conventions").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, '717b4dfc-d4c1-4eed-96cc-919edd4f1542').
narrative_ontology:cs_kernel_codification('717b4dfc-d4c1-4eed-96cc-919edd4f1542', formalized).
narrative_ontology:cs_authority_grounding('717b4dfc-d4c1-4eed-96cc-919edd4f1542', lineage).
narrative_ontology:cs_interpretation_layer_present('717b4dfc-d4c1-4eed-96cc-919edd4f1542').
narrative_ontology:cs_reading_relation('717b4dfc-d4c1-4eed-96cc-919edd4f1542', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('717b4dfc-d4c1-4eed-96cc-919edd4f1542', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('717b4dfc-d4c1-4eed-96cc-919edd4f1542', foundational, state_security_justifies_suspension_of_protections_in_asymmetric_conflict).
narrative_ontology:cs_axiom_status(state_security_justifies_suspension_of_protections_in_asymmetric_conflict, holdable).
narrative_ontology:cs_axiom_grounding('717b4dfc-d4c1-4eed-96cc-919edd4f1542', state_security_justifies_suspension_of_protections_in_asymmetric_conflict, instrumental).
narrative_ontology:cs_axiom('717b4dfc-d4c1-4eed-96cc-919edd4f1542', secondary, unlawful_combatants_lack_privileged_status_under_conventions).
narrative_ontology:cs_axiom_status(unlawful_combatants_lack_privileged_status_under_conventions, holdable).
narrative_ontology:cs_axiom_grounding('717b4dfc-d4c1-4eed-96cc-919edd4f1542', unlawful_combatants_lack_privileged_status_under_conventions, conventional).
narrative_ontology:cs_reference_frame('717b4dfc-d4c1-4eed-96cc-919edd4f1542', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('717b4dfc-d4c1-4eed-96cc-919edd4f1542', post_9_11_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('717b4dfc-d4c1-4eed-96cc-919edd4f1542', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, state_security_establishment).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, protected_civilians).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detainees).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, unlawful_combatants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, irregular_armed_groups).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, state_survival_primacy).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, operational_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls interpretation and application of the Geneva Conventions through military legal advisories, executive orders, and legislative frameworks. Asserts that asymmetric conflict and irregular warfare create operational necessities that justify suspending or narrowing protections. Benefits from expanded authority, reduced legal constraints, and enhanced operational freedom.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, state_security_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Civilians in conflict zones who lose protections against targeting, displacement, and collective punishment when the security maximization reading is applied. The 'human shields' doctrine and expanded collateral damage acceptance directly increase their vulnerability. No effective exit from the conflict zone or the legal framework that denies them recourse.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, protected_civilians, payer,
    powerless, immediate, trapped, local).

% Individuals held in indefinite detention without trial, denied POW status, and subjected to coercive interrogation normalized as non-torture. Their legal personality is erased by the unlawful combatant category expansion. Exit is impossible without external intervention.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detainees, payer,
    powerless, biographical, trapped, local).

% Irregular fighters captured by state forces who are denied Geneva protections entirely. They face trial by military commission, potential execution, and no habeas corpus. Their exit options are limited to cessation of hostilities or escape, both highly constrained.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, unlawful_combatants, payer,
    moderate, biographical, constrained, regional).

% ICRC, NGOs, and UN bodies that advocate for humanitarian ceiling reading. They are structurally excluded from decision-making on targeting and detention policies. Their monitoring access is restricted when states invoke operational necessity. They would object to the degradation of protections but have no formal seat at the table.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, humanitarian_organizations, excluded,
    organized, generational, constrained, global).

% ICJ, ICC, and regional human rights courts that adjudicate violations. Their jurisprudence increasingly contests the security maximization reading, but enforcement depends on state cooperation. They provide the analytical seat that maps the divergence between the reading and the treaty text.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_courts, observer,
    institutional, generational, analytical, global).

% Non-state armed groups whose non-compliance is cited as justification for the reading. They are simultaneously targets of the expanded state violence and excluded from the reciprocal framework. Their fighters are denied protections, but they also cannot access the legal mechanisms to challenge the reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, irregular_armed_groups, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__security_maximization_reading, irregular_armed_groups, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state military and intelligence operations against irregular threats by providing a legal framework that permits suspension of conventional protections, enabling unified command action without legal friction.
% TRANSFER_FUNCTION: Transfers legal protections, physical safety, and procedural rights from protected persons (civilians, detainees, irregular fighters) to the state security establishment in the form of expanded operational latitude, reduced accountability, and resource savings from bypassing due process.
% ABSENT_VOICES: The protected persons themselves — civilians in conflict zones, detainees in black sites, and irregular fighters — are structurally absent from the interpretation process. Their voices are filtered through state security classifications and humanitarian organizations that lack enforcement power. The reciprocity argument excludes them because they are deemed outside the convention's reciprocal bargain.
% DISAPPEARANCE_RATIONALE: If the security maximization reading vanished overnight, states would lose the legal basis for indefinite detention, coercive interrogation, expanded targeting rules, and the unlawful combatant category. Military operations would require compliance with full Geneva protections, humanitarian organizations would gain monitoring access, and courts would have clear standards for adjudication. The asymmetric warfare legal framework would collapse into the humanitarian ceiling or conditional reciprocity readings.
% FOUNDING_PROBLEM: The 1949 Conventions were designed for inter-state wars between regular armies. They did not anticipate asymmetric conflicts where non-state actors blend with civilians, disregard laws of war, and exploit state adherence to conventions as a tactical vulnerability.
% FOUNDING_PROBLEM_CORROBORATION: State security establishments attest the founding problem is live and worsening (e.g., US Department of Defense Law of War Manual, Israeli Supreme Court targeted killing judgments). Humanitarian organizations and international courts attest the founding problem is overstated and used as pretext (ICRC commentaries, ICJ Nuclear Weapons advisory opinion, ICC policy papers). The disagreement is structural: the beneficiaries of the reading define the problem as live; the payers define it as manufactured.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.85) because the reading removes core protections (humane treatment, fair trial, distinction/proportionality) from entire categories of people. Suppression is very high (0.9) because the reading's persistence depends on active enforcement: military orders, legal memos, classification regimes, and denial of court access. Theater ratio is moderate (0.4): states maintain performative compliance (issuing rules of engagement, conducting reviews) while the substantive protections are suspended. Accessibility collapse is high (0.8) because once the reading is accepted, alternative legal frameworks (humanitarian ceiling, conditional reciprocity) are treated as naively dangerous. Resistance is moderate (0.6) because humanitarian organizations, courts, and some states push back, but the reading's institutionalization in major military powers makes resistance costly.
 *
 * PERSPECTIVAL GAP:
 *   From the state security seat, the reading is a necessary coordination mechanism for survival against an enemy that exploits legal restraint. From the payer seats, the same structure is a snare that strips away the last protections against state violence. The engine computes this divergence from the declared roles, power, and exit options. The claimed_type (tangled_rope) reflects the reading's self-presentation as coordination-with-extraction; the metrics describe the extraction-heavy reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The state security establishment is the structural beneficiary: it collects expanded authority and reduced constraints (d near 0.0). Protected civilians, detainees, and unlawful combatants are the targets: they bear the full cost of suspended protections with no exit (d near 1.0). Humanitarian organizations are excluded: they would object but are kept out of the decision loop (d not computed for excluded). International courts are observers: they see the full structure but lack enforcement power (d = 0.5 analytical). Irregular armed groups are dual payer/excluded: they are targeted by the reading and also cited as its justification, but have no voice in its interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (asymmetric conflict exploiting conventional protections) is contested, not dead. The reading persists because the security establishment benefits from the expanded authority and because the payers lack coalition power to force revision. The constraint is not a piton (no theatrical maintenance of a dead function) — the function (countering irregular threats) is live, but the extraction has grown beyond what the coordination function requires. This is a tangled_rope where the coordination cover has thinned and the extraction has intensified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does the security_maximization_reading''s structural relationship to the geneva_conventions_1949 kernel differ from its sibling readings, and what classification consequences follow from the reading''s foreclosure of the humanitarian_ceiling_reading?',
    'Comparative analysis of the three readings'' constraint stories: map each reading''s ε, beneficiary/victim structure, and claimed_type. The foreclosure relation means that within any single state''s legal framework, adopting security_maximization logically excludes humanitarian_ceiling. This forces a binary choice that the conditional_reciprocity reading avoids.',
    'If foreclosure holds, the kernel cannot be a stable coordination point — it becomes a contested boundary where states must choose between maximal extraction and absolute constraint. This drives the kernel toward fragmentation or formal revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame structural delta between sibling readings of the Geneva Conventions kernel.').

omega_variable(
    operational_necessity_vs_reciprocity_boundary,
    'Is the security_maximization reading''s expansion of permissible violence (unlawful combatant category, collateral damage acceptance) structurally distinct from the conditional_reciprocity reading''s proportional degradation, or do they converge in practice?',
    'Case law comparison: analyze military commission jurisprudence, targeting policies, and detention frameworks in states that invoke each reading. Measure the actual protection floor for each victim category.',
    'If they converge, the two readings are functionally the same constraint with different justifications — the engine should classify them identically. If they diverge, conditional_reciprocity may be a weaker tangled_rope while security_maximization is a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_necessity_vs_reciprocity_boundary, empirical, 'Whether the two degradation justifications produce materially different protection outcomes.').

omega_variable(
    internalized_suppression_in_detainee_populations,
    'Does the suppression experienced by detainees and unlawful combatants include an internalized component (learned helplessness, normalization of rightslessness) that persists beyond the structural barriers?',
    'Longitudinal studies of released detainees'' legal consciousness and capacity to claim rights post-release. Compare with populations subjected to similar regimes historically.',
    'If internalized suppression is significant, the effective suppression for payer seats is higher than the structural measure suggests, and the constraint''s extraction persists even after formal legal restoration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_in_detainee_populations, empirical, 'Structural vs. internalized suppression mechanism for the most vulnerable payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gc1949_secmax_tr_t1949, geneva_conventions_1949__security_maximization_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gc1949_secmax_tr_t1977, geneva_conventions_1949__security_maximization_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement(gc1949_secmax_tr_t2001, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2001, 0.3).
narrative_ontology:measurement(gc1949_secmax_tr_t2024, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(gc1949_secmax_be_t1949, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 1949, 0.2).
narrative_ontology:measurement(gc1949_secmax_be_t1977, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 1977, 0.35).
narrative_ontology:measurement(gc1949_secmax_be_t2001, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2001, 0.7).
narrative_ontology:measurement(gc1949_secmax_be_t2024, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gc1949_secmax_su_t1949, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 1949, 0.3).
narrative_ontology:measurement(gc1949_secmax_su_t1977, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 1977, 0.45).
narrative_ontology:measurement(gc1949_secmax_su_t2001, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2001, 0.8).
narrative_ontology:measurement(gc1949_secmax_su_t2024, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__security_maximization_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__conditional_reciprocity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the geneva_conventions_1949 kernel. The security_maximization reading has the highest ε (0.85) because it treats the conventions as aspirational and suspends protections for operational necessity. The humanitarian_ceiling reading has near-zero ε (mountain-like). The conditional_reciprocity reading has intermediate ε (tangled_rope). The three stories form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
