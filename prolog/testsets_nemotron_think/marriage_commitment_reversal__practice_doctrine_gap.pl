% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Section 132 Doctrine-Practice Gap (1890-1904)
 *   domain: religious_institutional_history
 *
 * SUMMARY:
 *   Following the 1890 Manifesto, the LDS Church publicly suspended plural
 *   marriage practice while preserving Section 132 (the revelation on eternal
 *   and plural marriage) as binding doctrine. This created a structural
 *   ambiguity: leadership privately authorized 200+ post-Manifesto plural
 *   marriages (1890-1904) in Mexico, Canada, and secretly in the US, while
 *   publicly testifying that the practice had ceased. The ambiguity served as
 *   a dual-track legitimation strategy — maintaining theological coherence
 *   for members while performing compliance for federal authorities. The
 *   constraint extracts clarity and trust from the general membership and
 *   sacrifices fundamentalist dissenters to schism, while the institution
 *   survives legally and corporately.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.75).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.65).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.75).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Section 132 Doctrine-Practice Gap (1890-1904)").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious_institutional_history").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '3d1bf209-25ee-48f3-8f24-5121f19e5f75').
narrative_ontology:cs_kernel_codification('3d1bf209-25ee-48f3-8f24-5121f19e5f75', formalized).
narrative_ontology:cs_authority_grounding('3d1bf209-25ee-48f3-8f24-5121f19e5f75', extraction).
narrative_ontology:cs_interpretation_layer_present('3d1bf209-25ee-48f3-8f24-5121f19e5f75').
narrative_ontology:cs_reading_relation('3d1bf209-25ee-48f3-8f24-5121f19e5f75', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d1bf209-25ee-48f3-8f24-5121f19e5f75', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('3d1bf209-25ee-48f3-8f24-5121f19e5f75', foundational, doctrine_practice_separation_legitimate).
narrative_ontology:cs_axiom_status(doctrine_practice_separation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('3d1bf209-25ee-48f3-8f24-5121f19e5f75', doctrine_practice_separation_legitimate, conventional).
narrative_ontology:cs_axiom('3d1bf209-25ee-48f3-8f24-5121f19e5f75', secondary, institutional_continuity_justifies_ambiguity).
narrative_ontology:cs_axiom_status(institutional_continuity_justifies_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('3d1bf209-25ee-48f3-8f24-5121f19e5f75', institutional_continuity_justifies_ambiguity, instrumental).
narrative_ontology:cs_reference_frame('3d1bf209-25ee-48f3-8f24-5121f19e5f75', section_132_canonical_authority).
narrative_ontology:cs_drift_state('3d1bf209-25ee-48f3-8f24-5121f19e5f75', post_manifesto_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3d1bf209-25ee-48f3-8f24-5121f19e5f75', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, church_institution).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, church_leadership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_dissenters).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, institutional_continuity_requires_doctrinal_ambiguity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% First Presidency and Quorum of Twelve Apostles publicly affirm Section 132 as binding doctrine while privately authorizing post-Manifesto plural marriages in Mexico, Canada, and secretly in US territories. They manage the ambiguity to preserve institutional legitimacy externally and theological coherence internally. They control the interpretive apparatus and face no accountability mechanism.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, church_leadership, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, church_leadership, beneficiary).

% The corporate legal entity (Church of Jesus Christ of Latter-day Saints) retains property, avoids disincorporation under Edmunds-Tucker Act, and maintains statehood pathway for Utah by performing public compliance while the doctrinal principle remains intact. The ambiguity functions as institutional survival strategy.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, church_institution, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__practice_doctrine_gap, church_institution).

% Rank-and-file members experience betrayal and bewilderment: taught that plural marriage is an eternal, non-negotiable principle, they witness leadership suspending practice while insisting doctrine is unchanged. Exit is constrained by community ties, family, identity, and belief structure. They bear the cognitive and social cost of the ambiguity.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    organized, biographical, constrained, global).

% Members who refuse to accept the practice suspension as legitimate, continuing plural marriage based on Section 132 as originally understood. They face excommunication, social ostracization, and legal prosecution. Their exit from the mainstream church is identity-locked — leaving means abandoning the community that constitutes their self-concept. They become the schismatic fundamentalist movement.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_dissenters, payer,
    moderate, biographical, trapped, regional).

% US Congress, Justice Department, and Utah territorial officials monitor compliance with anti-polygamy laws. They accept the 1890 Manifesto as sufficient for statehood but remain suspicious of continued covert practice. Their pressure creates the external condition for the ambiguity but they do not control the internal doctrinal framing.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_authorities, observer,
    institutional, generational, analytical, national).

% Mormon colonies in Mexico and Canada where post-Manifesto plural marriages are openly performed with leadership sanction. Colonists are structurally dependent on leadership for ecclesiastical validation and land titles. They would object to being used as pressure valves but have no voice in the institutional calculus.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, mexico_canada_colonies, excluded,
    powerless, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains institutional continuity and legal survival of the church by preserving the doctrinal principle (Section 132) as eternal truth while suspending its public practice to satisfy federal demands — a dual-track legitimation that coordinates internal theological coherence with external political compliance.
% TRANSFER_FUNCTION: Moves the cost of doctrinal ambiguity onto the general membership (cognitive dissonance, betrayal) and fundamentalist dissenters (excommunication, persecution), while transferring the benefit of institutional survival, property retention, and statehood to the church corporation and its leadership.
% ABSENT_VOICES: Post-Manifesto plural wives in Mexico/Canada colonies (structurally excluded, dependent on leadership sanction); rank-and-file members who privately questioned but had no forum for dissent; prospective converts never informed of the doctrine-practice gap. The colonies are the most structurally excluded — they enact the suspended practice but have zero say in its framing.
% DISAPPEARANCE_RATIONALE: If the doctrine-practice gap vanished overnight — either by openly abandoning Section 132 or by resuming public plural marriage — the church would face either theological collapse (abandoning a canonized revelation) or immediate federal re-prosecution and disincorporation. The ambiguity IS the structural glue holding the institution together in this period.
% FOUNDING_PROBLEM: The 1890 Manifesto faced an existential dilemma: the federal government demanded abandonment of plural marriage as condition for Utah statehood and church survival, but Section 132 canonized plural marriage as an eternal, necessary principle for exaltation. Openly repudiating Section 132 would shatter theological legitimacy; openly defying federal law would destroy the institution.
% FOUNDING_PROBLEM_CORROBORATION: The federal threat (Edmunds-Tucker Act, disincorporation, imprisonment of leaders) is documented in congressional records and court cases — corroborated by non-Mormon historians (e.g., Sarah Barringer Gordon, Kathleen Flake). The theological bind is attested by internal leadership minutes (e.g., Wilford Woodruff's journal, First Presidency correspondence) showing they understood Section 132 as binding. The 'dead' status is corroborated by the 1904 Second Manifesto which explicitly prohibited new plural marriages worldwide, confirming the founding problem (federal coercion) was resolved by institutional capitulation, not doctrinal resolution.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the constraint sacrifices the epistemic and moral clarity of the entire membership for institutional flexibility. Suppression is moderate-high (0.65) because maintaining the gap requires active enforcement: excommunicating fundamentalists who take the doctrine literally, disciplining members who expose the gap, and managing a two-tier communication system. Theater ratio is high (0.7) because the public performance of compliance (Manifesto, congressional testimony) diverges substantially from private practice (authorized marriages, doctrinal reaffirmation in temple liturgy). Accessibility collapse is moderate (0.6) — alternatives exist (schism, exit) but are costly due to identity-lock. Resistance is moderate (0.55) — fundamentalist resistance is real but contained; general membership resistance is mostly internalized bewilderment.
 *
 * PERSPECTIVAL GAP:
 *   From the leadership seat, the ambiguity is a necessary coordination mechanism preserving the institution against existential threat — a tragic but rational compromise. From the general membership seat, it is a betrayal of the covenant clarity they were taught to stake their eternal salvation on. From the fundamentalist seat, it is apostasy masked as fidelity. The engine computes these divergent seat classifications from the structural data; the authored claim (tangled_rope) reflects the coordination-extraction hybrid that the leadership seat experiences as coordination and the payer seats experience as extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership (agenda_setter) sits near the beneficiary end (d ~ 0.15) — they control the ambiguity and capture institutional survival benefits. Church institution (beneficiary) is a non-agent entity that structurally receives the gains. General membership (payer) sits near the target end (d ~ 0.8) — identity-locked, constrained exit, bears cognitive and social costs. Fundamentalist dissenters (payer) are fully trapped (d ~ 0.95) — their self-concept is fused to the literal doctrine, exit means existential loss. Federal authorities (observer) are analytical (d ~ 0.5) — they exert external pressure but don't directly extract. Mexico/Canada colonies (excluded) are constrained (d ~ 0.7) — they enact the suspended practice but have no voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal existential threat) is dead — resolved by 1904 Second Manifesto and Utah statehood (1896). Yet the doctrinal principle (Section 132) remains canonized, and the ambiguity structure persists in contemporary fundamentalist schisms and mainstream temple liturgy. The constraint has outlived its founding mandate but persists through institutional inertia and identity-lock of fundamentalist communities. This is not a scaffold (no sunset clause) but a tangled_rope that has partially atrophied toward piton — the coordination function (federal compliance) is gone, but the extraction (membership clarity sacrificed for institutional flexibility) continues in derivative forms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the practice_doctrine_gap a distinct constraint from its sibling readings, or a framing of the same constraint?',
    'Test ε-invariance: if the exogenous_override_reading and endogenous_reinterpretation_reading produce materially different beneficiary/victim structures or extractiveness values when authored as separate stories, the kernel decomposes. Author all three as independent constraints and compare.',
    'If ε differs across readings, the kernel is a colloquial label covering multiple constraints — each gets its own story linked via network.affects_constraints. If ε is stable, the readings are interpretive frames on one constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel decomposes into multiple ε-invariant constraints.').

omega_variable(
    doctrine_practice_separability,
    'Can the doctrinal principle (Section 132) be meaningfully separated from its practice without dissolving the constraint''s theological legitimacy?',
    'Compare post-1904 mainstream LDS theology (Section 132 retained, plural marriage prohibited) with fundamentalist theology (Section 132 requires plural marriage). If mainstream theology coheres without practice, the separation is sustainable. If fundamentalist critique holds, the gap is inherently unstable.',
    'If inseparable, the gap is a temporary Snare that must eventually collapse into either open repudiation or open practice. If separable, the gap can persist indefinitely as a stable Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_practice_separability, conceptual, 'Whether the doctrine-practice gap is structurally sustainable or inherently unstable.').

omega_variable(
    institutional_vs_leadership_beneficiary,
    'Does the extraction accrue to the church as corporate institution or to the leadership cadre as a ruling group?',
    'Trace post-1904 asset control, succession patterns, and whether leadership decisions consistently maximize institutional longevity over leadership privilege. Compare with corporate sole structure.',
    'If leadership captures gains, the constraint is a Snare with leadership as beneficiary. If institution captures gains, it is a Tangled Rope with institutional survival as coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_leadership_beneficiary, empirical, 'Whether the beneficiary is the institution as such or the leadership personnel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (excommunication, legal prosecution) or internalized (members policing their own doubt because identity is fused to the institution)?',
    'Post-exit trajectory analysis: do former members who leave over the doctrine-practice gap continue to self-silence, or does suppression cease upon exit? Compare fundamentalist defectors vs. mainstream dissidents.',
    'If internalized, effective suppression is higher than structural measures suggest — the target carries the constraint internally after exit. This would increase χ for identity-locked agents beyond the structural derivation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for identity-locked payers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0, 0.5).
narrative_ontology:measurement(marr_tr_t2, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 2, 0.55).
narrative_ontology:measurement(marr_tr_t4, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 4, 0.6).
narrative_ontology:measurement(marr_tr_t6, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 6, 0.65).
narrative_ontology:measurement(marr_tr_t8, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 8, 0.68).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 10, 0.7).
narrative_ontology:measurement(marr_tr_t12, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 12, 0.7).
narrative_ontology:measurement(marr_tr_t14, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 14, 0.7).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(marr_be_t2, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 2, 0.6).
narrative_ontology:measurement(marr_be_t4, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 4, 0.65).
narrative_ontology:measurement(marr_be_t6, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(marr_be_t8, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(marr_be_t12, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 12, 0.75).
narrative_ontology:measurement(marr_be_t14, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 14, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(marr_su_t2, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(marr_su_t4, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(marr_su_t6, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(marr_su_t8, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(marr_su_t12, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(marr_su_t14, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 14, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__practice_doctrine_gap, 0.1).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_schism_persistence).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, mainstream_temple_liturgy_section_132).

% DUAL FORMULATION NOTE:
% This constraint is the practice_doctrine_gap reading of the marriage_commitment_reversal kernel. It decomposes the colloquial '1890 Manifesto' into structurally distinct claims: (1) exogenous_override — federal coercion forced reversal; (2) endogenous_reinterpretation — divine revelation authorized reversal; (3) practice_doctrine_gap — structural ambiguity where doctrine is preserved while practice is suspended. These three constraints have different ε values, different beneficiary/victim structures, and different temporal dynamics. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__practice_doctrine_gap, organized, 0.75).
constraint_indexing:directionality_override(marriage_commitment_reversal__practice_doctrine_gap, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
