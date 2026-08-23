% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Equality Clause — Expansive Universalist Reading
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   The expansive universalist reading of the equality clause treats 'all men
 *   are created equal' and the Equal Protection Clause as self-evident
 *   universal truths that apply to every human person regardless of the
 *   historical exclusions practiced by the framers. This reading originated
 *   in the founding generation's own recognition of the contradiction
 *   (Jefferson, Madison, the Declaration's own logic), was carried forward by
 *   the Reconstruction framers who explicitly repudiated the original
 *   compromise, and was fully realized in the 20th-century civil rights
 *   jurisprudence that made universal inclusion the constitutional baseline.
 *   The reading's distinctive structural move: historical exclusions are not
 *   binding precedent but hypocrisy to be corrected; the legitimacy threshold
 *   for rights expansion via judicial interpretation is low because the
 *   principle itself demands it. The claimed_type is mountain — the reading
 *   asserts that universal equality is a moral fact, not a political
 *   construction — but the declared beneficiaries (all_human_persons,
 *   historically_excluded_groups, future_marginalized_populations) trigger
 *   False Summit Mountain evaluation. The metrics describe the constraint's
 *   actual historical operation: high initial extraction and suppression
 *   (slavery, coverture, property qualifications) declining toward near-zero
 *   as the reading's logic was implemented; low and declining theater as
 *   performative exclusion gave way to substantive inclusion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.18).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.12).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.18).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, mountain).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Equality Clause — Expansive Universalist Reading").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:emerges_naturally(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, '04343c1e-814a-4061-aa84-32c73d60b9b6').
narrative_ontology:cs_kernel_codification('04343c1e-814a-4061-aa84-32c73d60b9b6', fixed_text).
narrative_ontology:cs_authority_grounding('04343c1e-814a-4061-aa84-32c73d60b9b6', lineage).
narrative_ontology:cs_interpretation_layer_present('04343c1e-814a-4061-aa84-32c73d60b9b6').
narrative_ontology:cs_reading_relation('04343c1e-814a-4061-aa84-32c73d60b9b6', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('04343c1e-814a-4061-aa84-32c73d60b9b6', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('04343c1e-814a-4061-aa84-32c73d60b9b6', foundational, universal_personhood_self_evident).
narrative_ontology:cs_axiom_status(universal_personhood_self_evident, holdable).
narrative_ontology:cs_axiom_grounding('04343c1e-814a-4061-aa84-32c73d60b9b6', universal_personhood_self_evident, deontological).
narrative_ontology:cs_axiom('04343c1e-814a-4061-aa84-32c73d60b9b6', foundational, historical_exclusions_are_hypocrisy_not_precedent).
narrative_ontology:cs_axiom_status(historical_exclusions_are_hypocrisy_not_precedent, holdable).
narrative_ontology:cs_axiom_grounding('04343c1e-814a-4061-aa84-32c73d60b9b6', historical_exclusions_are_hypocrisy_not_precedent, deontological).
narrative_ontology:cs_axiom('04343c1e-814a-4061-aa84-32c73d60b9b6', secondary, judicial_interpretation_authorized_for_expansion).
narrative_ontology:cs_axiom_status(judicial_interpretation_authorized_for_expansion, holdable).
narrative_ontology:cs_axiom_grounding('04343c1e-814a-4061-aa84-32c73d60b9b6', judicial_interpretation_authorized_for_expansion, instrumental).
narrative_ontology:cs_reference_frame('04343c1e-814a-4061-aa84-32c73d60b9b6', declaration_equality_principle).
narrative_ontology:cs_drift_state('04343c1e-814a-4061-aa84-32c73d60b9b6', contemporary_rights_jurisprudence, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('04343c1e-814a-4061-aa84-32c73d60b9b6', '2026-08-03T14:22:10Z').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, all_human_persons).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, future_marginalized_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, legislative_majorities).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, universal_moral_equality).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, equal_protection_principle).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, human_dignity_as_foundation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every human person is a bearer of the equality claim; the constraint's operation extends moral and legal standing to all without exception. No exit is needed — the constraint is the condition of inclusion itself.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, all_human_persons, beneficiary,
    moderate, generational, analytical, universal).

% Groups historically denied standing (enslaved persons, women, non-property-holders, racial minorities, LGBTQ+ persons) are the primary beneficiaries of the expansive reading; they also bear the cost of past exclusion and ongoing struggle to make the principle effective. Exit is identity-locked: their personhood is constituted through the very recognition the reading demands.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_excluded_groups, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__expansive_universalist, historically_excluded_groups, payer).

% Groups not yet recognized or not yet existing (future migrants, digital persons, post-human entities) benefit from the reading's low threshold for expansion — the principle anticipates them. No current exit dynamics apply.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, future_marginalized_populations, beneficiary,
    powerless, civilizational, analytical, universal).

% Courts and jurists who operationalize the reading by recognizing new classes of rights-holders; their legitimacy derives from the reading's authorization of expansive interpretation. They are constrained by professional norms, precedent, and political backlash.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, judicial_interpreters, agenda_setter,
    institutional, generational, constrained, national).

% Judges committed to the restrictive_originalist reading; they would object to the expansive reading's legitimacy but are structurally excluded from its interpretive framework. Their identity is fused to the originalist method — exit means abandoning their judicial philosophy.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, originalist_judges, excluded,
    institutional, generational, identity_locked, national).

% Democratic majorities that must fund and implement rights expansions ordered by courts; they bear fiscal and political costs but can exit by constitutional amendment, jurisdiction-stripping, or court-packing — mobile at the institutional level.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, legislative_majorities, payer,
    powerful, biographical, mobile, national).

% Scholars who analyze the reading's coherence, history, and implications; they neither collect nor pay but map the constraint's structural dynamics across the kernel's readings.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, constitutional_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the recognition of moral and legal personhood across all human beings, providing a stable foundation for rights-claims that would otherwise be contested or denied by historically contingent power arrangements.
% TRANSFER_FUNCTION: Transfers the burden of justification from the excluded (who no longer must prove they count) to the state (which must justify any differential treatment); moves the presumption of inclusion from a political achievement to a constitutional baseline.
% ABSENT_VOICES: The restrictive_originalist reading's adherents — originalist judges, conservative legal theorists, and political movements that treat historical exclusion as binding precedent — are structurally excluded from this reading's interpretive community. They exist in the world but not in this reading's framework.
% DISAPPEARANCE_RATIONALE: If the expansive universalist reading vanished overnight, the presumption of universal inclusion would collapse; historically excluded groups would lose the constitutional baseline that makes their rights-claims presumptively valid, and the burden of justification would shift back to them. The world of rights-law would rearrange around a narrower, historically anchored equality concept.
% FOUNDING_PROBLEM: The founding problem was the contradiction between the Declaration's 'all men are created equal' and the Constitution's original accommodation of slavery, coverture, and property qualifications — a hypocrisy that threatened the regime's moral legitimacy from inception.
% FOUNDING_PROBLEM_CORROBORATION: The contradiction is attested by the founding generation themselves (Jefferson's 'wolf by the ears', Madison's Federalist 10 and 54, the Three-Fifths Compromise debates), by the Reconstruction Amendments' framers who explicitly treated the original Constitution as defective on equality, and by the Civil Rights Movement's appeal to the Declaration as a promissory note — all sources outside the expansive reading's direct beneficiary set.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, ExtMetricName, E),
    domain_priors:suppression_score(equality_clause_scope__expansive_universalist, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equality_clause_scope__expansive_universalist),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.18 at interval end) reflects the reading's current operation: the presumption of universal inclusion means the state bears the burden of justifying any differential treatment, so extraction from the excluded is minimal. The historical trajectory shows extraction collapsing from 0.85 (founding, when the equality claim was actively violated for most humans) to 0.18 as the reading's logic was institutionalized. Theater ratio follows the same arc: high at founding (the equality declaration was performative while slavery persisted), declining as the constraint's operation came to match its claim. Suppression requirement mirrors this: the founding regime suppressed the equality claim for excluded groups (0.85); the expansive reading's institutionalization removed the structural barriers. Accessibility collapse is 0.92: once the universalist principle is understood, alternatives (graduated personhood, property-qualified citizenship, caste systems) collapse almost completely — they become conceptually incoherent within the reading's framework. Resistance is 0.05: the reading meets little active resistance today because its core claim has been absorbed into the constitutional consensus; remaining resistance is to specific applications, not the principle.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat classifications from the structural data. The beneficiary seats (all_human_persons, historically_excluded_groups, future_marginalized_populations) experience near-zero effective extraction — the constraint subsidizes their inclusion. The agenda_setter seat (judicial_interpreters) experiences low extraction with moderate directionality — they administer the constraint but are constrained by it. The payer seat (legislative_majorities) experiences modest effective extraction — they fund rights enforcement but have mobile exit. The excluded seat (originalist_judges) experiences the constraint as an external imposition they cannot exit without identity loss — their directionality is high despite institutional power. The observer seat (constitutional_theorists) computes analytically with zero extraction. This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (all_human_persons, historically_excluded_groups, future_marginalized_populations) receive the constraint's subsidy: universal standing is the default, no justification needed. Their d-values are near 0.0. Historically_excluded_groups also bear legacy costs (identity_locked exit), so their effective extraction is slightly higher but still negative (net subsidy). Judicial_interpreters (agenda_setter) have constrained exit and institutional power — d ~ 0.3: they benefit from the reading's authorization but are bound by it. Legislative_majorities (payer) have mobile exit and bear implementation costs — d ~ 0.6: they pay but can amend. Originalist_judges (excluded) are identity_locked to a competing reading — d ~ 0.9: the expansive reading extracts their interpretive authority. Constitutional_theorists (observer) have analytical exit — d = 0.5 (symmetric). The derivation chain reads these structural relationships from beneficiary/payer declarations + exit options + power levels.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's mandate (universal inclusion) has not atrophied — it remains live (founding_problem_status: live) because new margins of exclusion continually emerge (migrants, trans persons, digital persons, future entities). The reading's low legitimacy threshold for expansion is not rent-seeking but the principle's self-application. Mandatrophy would occur if the reading became a fixed catalog of protected classes rather than an open-ended principle — that has not happened. The theater_ratio decline from 0.65 to 0.08 tracks the mandate's fulfillment, not its decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_framing_kernel_reading,
    'Does the expansive universalist reading instantiate a distinct constraint from the kernel''s other readings, or is it merely a rhetorical emphasis within a single constraint?',
    'If the sibling readings produce different beneficiary/victim structures, different ε values, and different classification outcomes when authored as separate constraint stories, they are distinct constraints per ε-invariance. The engine''s per-seat classification divergence across the three readings will confirm or deny structural distinctness.',
    'If the readings are structurally distinct, the kernel is a family of three constraints linked by network.affects_constraints. If they collapse to one constraint with observer-dependent classification, the kernel framework is misapplied here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_kernel_reading, conceptual, 'Whether the kernel''s readings map to distinct ε-invariant constraints').

omega_variable(
    mountain_claim_vs_beneficiary_presence,
    'Can a constraint that declares universal beneficiaries (all_human_persons) and treats historical exclusion as hypocrisy be a genuine mountain, or is the beneficiary declaration itself evidence that the constraint is a constructed coordination mechanism (tangled_rope) masquerading as natural law?',
    'The False Summit Mountain signature evaluates: if the metric profile is mountain-consistent (low ε, low suppression, emerges_naturally=true, high accessibility_collapse, low resistance) AND beneficiaries are declared, FSM triggers reclassification to tangled_rope. The omega documents the irreducible ambiguity: is universal moral equality a discovered fact (mountain) or a constructed commitment that benefits identifiable agents (tangled_rope)?',
    'FSM reclassification would change the constraint''s structural identity from ''self-evident truth'' to ''coordination mechanism with asymmetric extraction'' — the beneficiaries (judicial interpreters, rights-advocacy institutions) would be revealed as coordinated actors, the payers (legislative majorities, originalist institutions) as extraction targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_claim_vs_beneficiary_presence, conceptual, 'Natural-law vs. constructed-status ambiguity for a mountain with declared beneficiaries').

omega_variable(
    historical_exclusion_as_hypocrisy_vs_precedent,
    'Is the expansive reading''s treatment of historical exclusions (as hypocrisy to be corrected, not binding precedent) a structural feature of the constraint or a contingent interpretive choice?',
    'Compare the three sibling readings: restrictive_originalist treats exclusions as binding precedent; progressive_textualist treats them as errors corrected only by democratic amendment; expansive_universalist treats them as hypocrisy corrected by any legitimate interpreter. If the three readings produce different ε values and different classification outcomes for the same historical episodes (e.g., Dred Scott, Bradwell, Obergefell), the treatment of history is a structural differentiator, not a rhetorical one.',
    'Confirms or denies that the kernel''s contest is structurally generative — that the disagreement about history produces different constraints, not just different opinions about one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_exclusion_as_hypocrisy_vs_precedent, empirical, 'Whether the readings'' divergent treatments of history produce structurally distinct constraints').

omega_variable(
    identity_lock_mechanism_for_excluded_groups,
    'For historically_excluded_groups, is identity_locked exit a structural feature of the constraint (personhood constituted through recognition) or a contingent psychological state?',
    'If the reading were replaced by a restrictive reading overnight, would the groups'' capacity to claim rights collapse (structural identity lock) or would they continue claiming rights on alternative grounds (contingent)? The answer determines whether identity_locked is a canonical fallback or requires override.',
    'If structural, the directionality derivation correctly assigns high d to historically_excluded_groups despite their beneficiary role — they cannot exit the constraint without losing the personhood it constitutes. If contingent, an override toward mobile/constrained would be warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_excluded_groups, conceptual, 'Whether identity_locked exit for excluded groups is structural or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 1776, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t1776, equality_clause_scope__expansive_universalist, theater_ratio, 1776, 0.65).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t1787, equality_clause_scope__expansive_universalist, theater_ratio, 1787, 0.72).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t1865, equality_clause_scope__expansive_universalist, theater_ratio, 1865, 0.4).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t1868, equality_clause_scope__expansive_universalist, theater_ratio, 1868, 0.35).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t1920, equality_clause_scope__expansive_universalist, theater_ratio, 1920, 0.28).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t1954, equality_clause_scope__expansive_universalist, theater_ratio, 1954, 0.2).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t1964, equality_clause_scope__expansive_universalist, theater_ratio, 1964, 0.15).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t2015, equality_clause_scope__expansive_universalist, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t2026, equality_clause_scope__expansive_universalist, theater_ratio, 2026, 0.08).

% Extraction over time
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t1776, equality_clause_scope__expansive_universalist, base_extractiveness, 1776, 0.85).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t1787, equality_clause_scope__expansive_universalist, base_extractiveness, 1787, 0.78).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t1865, equality_clause_scope__expansive_universalist, base_extractiveness, 1865, 0.45).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t1868, equality_clause_scope__expansive_universalist, base_extractiveness, 1868, 0.38).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t1920, equality_clause_scope__expansive_universalist, base_extractiveness, 1920, 0.32).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t1954, equality_clause_scope__expansive_universalist, base_extractiveness, 1954, 0.28).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t1964, equality_clause_scope__expansive_universalist, base_extractiveness, 1964, 0.22).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t2015, equality_clause_scope__expansive_universalist, base_extractiveness, 2015, 0.19).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t2026, equality_clause_scope__expansive_universalist, base_extractiveness, 2026, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t1776, equality_clause_scope__expansive_universalist, suppression_requirement, 1776, 0.8).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t1787, equality_clause_scope__expansive_universalist, suppression_requirement, 1787, 0.85).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t1865, equality_clause_scope__expansive_universalist, suppression_requirement, 1865, 0.55).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t1868, equality_clause_scope__expansive_universalist, suppression_requirement, 1868, 0.48).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t1920, equality_clause_scope__expansive_universalist, suppression_requirement, 1920, 0.38).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t1954, equality_clause_scope__expansive_universalist, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t1964, equality_clause_scope__expansive_universalist, suppression_requirement, 1964, 0.22).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t2015, equality_clause_scope__expansive_universalist, suppression_requirement, 2015, 0.15).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t2026, equality_clause_scope__expansive_universalist, suppression_requirement, 2026, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__expansive_universalist, 0.06).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% Equality clause scope kernel decomposes into three readings: expansive_universalist (this story), restrictive_originalist, progressive_textualist. ε values differ: expansive (0.18 at end) claims near-zero extraction as universal inclusion is realized; restrictive_originalist would author high ε (extraction from excluded groups is the constraint's function); progressive_textualist would author intermediate ε (democratic amendment process creates friction but limits judicial extraction). All three linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__expansive_universalist, institutional, 0.35).
constraint_indexing:directionality_override(equality_clause_scope__expansive_universalist, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
