% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty over Basic Law Interpretation
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the parliamentary_sovereignty_reading
 *   of the basic_law_interpretive_boundary kernel. It asserts that the
 *   Knesset, as the elected sovereign, holds ultimate authority to interpret
 *   and amend Basic Laws via simple majority, including the power to override
 *   judicial review. The reading treats parliamentary sovereignty as a
 *   fundamental constitutional principle (Mountain) emerging naturally from
 *   democratic theory — the elected legislature's constituent power is
 *   irreducible and not subject to judicial veto. Near-zero extraction
 *   (ε≈0.05) for majoritarian domestic policy reflects the reading's claim
 *   that the constraint coordinates rather than extracts. The only
 *   acknowledged external constraint is international treaty obligations. The
 *   Supreme Court is reduced to an advisory body. Minority groups bear
 *   diffuse costs (loss of binding judicial review) but the reading does not
 *   classify them as victims, claiming the democratic process itself protects
 *   their interests through legislative majorities that must face
 *   re-election.
 *
 * KEY AGENTS:
 *   - knesset_majority: Primary agenda_setter (institutional/constrained) — holds sovereign interpretive authority
 *   - democratic_majority: Primary beneficiary (organized/mobile) — their will is constitutionally sovereign
 *   - minority_groups: Payer/excluded (powerless/identity_locked) — lose binding judicial protection
 *   - supreme_court: Excluded (institutional/trapped) — advisory only, no binding power
 *   - international_treaty_bodies: Observer (institutional/analytical) — only external constraint acknowledged
 *   - constitutional_scholars: Observer (analytical/analytical) — epistemic infrastructure for contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.05).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.1).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, mountain).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Parliamentary Sovereignty over Basic Law Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:emerges_naturally(basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'e934c844-5f74-4cae-be01-ad9139f949ab').
narrative_ontology:cs_kernel_codification('e934c844-5f74-4cae-be01-ad9139f949ab', fixed_text).
narrative_ontology:cs_authority_grounding('e934c844-5f74-4cae-be01-ad9139f949ab', lineage).
narrative_ontology:cs_interpretation_layer_present('e934c844-5f74-4cae-be01-ad9139f949ab').
narrative_ontology:cs_reading_relation('e934c844-5f74-4cae-be01-ad9139f949ab', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e934c844-5f74-4cae-be01-ad9139f949ab', basic_law_interpretive_boundary__balanced_contestation_reading, influences).
narrative_ontology:cs_axiom('e934c844-5f74-4cae-be01-ad9139f949ab', foundational, parliamentary_sovereignty_is_fundamental).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_is_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('e934c844-5f74-4cae-be01-ad9139f949ab', parliamentary_sovereignty_is_fundamental, conventional).
narrative_ontology:cs_axiom('e934c844-5f74-4cae-be01-ad9139f949ab', foundational, judicial_review_is_advisory_only).
narrative_ontology:cs_axiom_status(judicial_review_is_advisory_only, holdable).
narrative_ontology:cs_axiom_grounding('e934c844-5f74-4cae-be01-ad9139f949ab', judicial_review_is_advisory_only, conventional).
narrative_ontology:cs_reference_frame('e934c844-5f74-4cae-be01-ad9139f949ab', constituent_parliamentary_sovereignty).
narrative_ontology:cs_drift_state('e934c844-5f74-4cae-be01-ad9139f949ab', post_mizrahi_bank_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e934c844-5f74-4cae-be01-ad9139f949ab', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, democratic_majority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_groups).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, democratic_legitimacy_principle).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, constituent_power_of_elected_legislature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate authority to interpret and amend Basic Laws by simple majority. Sets the constitutional agenda and can override any judicial interpretation. Constrained by coalition politics and international treaty obligations but faces no domestic veto on legislative will.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority, agenda_setter,
    institutional, generational, constrained, national).

% Their electoral will is sovereign in constitutional interpretation. The constraint ensures their chosen representatives have final say on constitutional meaning. Exit is mobile through democratic elections, though identity-linked to national polity.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, democratic_majority, beneficiary,
    organized, biographical, mobile, national).

% Lose binding judicial protection for minority rights under this reading. Their constitutional claims depend entirely on legislative grace. Cannot exit the polity (identity-locked as Israeli citizens/residents) and lack organized power to constrain Knesset majorities.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_groups, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_groups, excluded).

% Reduced to advisory role only. May issue interpretations but has no binding invalidation power. Trapped in institutional role — cannot exit the judicial function but is structurally denied the authority other readings claim for it.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court, excluded,
    institutional, generational, trapped, national).

% Monitor compliance with treaty obligations that bind Israel internationally. These are the only external constraints this reading acknowledges on Knesset sovereignty. Analytical seat — they observe but do not participate in domestic constitutional interpretation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_treaty_bodies, observer,
    institutional, generational, analytical, global).

% Analyze and debate the constitutional structure from outside the political process. Provide the epistemic infrastructure for competing readings but hold no formal authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the coordination problem of legitimate constitutional authority by vesting ultimate interpretive power in the elected legislature, providing a single, democratically accountable source for constitutional meaning.
% TRANSFER_FUNCTION: Moves interpretive authority over Basic Laws from any judicial body to the Knesset, making legislative will the final constitutional arbiter except where international treaty obligations bind.
% ABSENT_VOICES: Minority communities and civil society organizations that rely on judicial review for rights protection are structurally excluded from the constitutional interpretation process under this reading; they are present in Israeli society but denied a veto point in constitutional adjudication.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty over Basic Law interpretation disappeared, the Supreme Court's power of judicial review would become binding and the Knesset could no longer override constitutional interpretations by simple majority, fundamentally restructuring the Israeli constitutional order.
% FOUNDING_PROBLEM: The founding problem was establishing a democratic constitutional order in a state without a formal written constitution, where the elected legislature needed to retain sovereign constituent power to adapt Basic Laws to changing circumstances without judicial entrenchment.
% FOUNDING_PROBLEM_CORROBORATION: The parliamentary sovereignty reading is corroborated by the absence of a formal constitution, the Knesset's historical constituent authority since 1948, and the democratic principle that elected representatives hold ultimate authority. However, the Supreme Court's 1995 Mizrahi Bank decision and subsequent jurisprudence, along with legal scholars like Amnon Rubinstein and the Israel Democracy Institute, contest this reading by asserting that Basic Laws have supra-legislative status and that the founding problem included protection against majoritarian tyranny.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(basic_law_interpretive_boundary__parliamentary_sovereignty_reading),
    narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.05) because the reading claims the constraint coordinates democratic self-governance without extracting from the majority; any costs to minorities are framed as the price of democratic sovereignty, not extraction. Suppression is low (0.1) because the constraint operates as the default constitutional order — it requires no active enforcement machinery beyond the legislative process itself. Theater ratio is low but shows a modest rise during periods of constitutional crisis (2000s-2020s) when the Knesset performs legislative overrides to reassert sovereignty against judicial encroachment. Accessibility collapse is high (0.9) because the reading treats parliamentary sovereignty and judicial supremacy as mutually exclusive frameworks — accepting one collapses the alternative. Resistance is low (0.15) from the reading's internal perspective but rises in measurements during judicial activism periods, reflecting external challenge to the constraint's operation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (knesset_majority) and beneficiary (democratic_majority) seats experience this as a Mountain — a natural, unchanging democratic principle. The payer (minority_groups) seat experiences it as a Snare — a constraint that extracts their protective veto with no exit. The excluded seat (supreme_court) experiences it as a Piton — a degraded institutional role maintained by inertia. The engine computes this per-seat divergence from the structural data; the claimed_type (mountain) reflects the agenda_setter/beneficiary perspective, which is the reading's own frame.
 *
 * DIRECTIONALITY LOGIC:
 *   The knesset_majority and democratic_majority are structural beneficiaries (d near 0.0) — the constraint subsidizes their sovereign authority. Minority_groups are payers (d near 1.0) despite the reading's denial — they bear the cost of lost judicial veto, identity-locked in the polity with no exit. The supreme_court is excluded (trapped) — it bears institutional costs of reduced authority but cannot exit its role. International_treaty_bodies and constitutional_scholars are analytical observers (d=0.5). The directionality derivation from beneficiary/victim declarations plus exit options produces this gradient automatically; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (democratic constitutional order without judicial entrenchment) is contested, not dead. The parliamentary sovereignty reading argues the problem remains live — Israel still lacks a formal constitution and the Knesset's constituent power must remain unfettered. Judicial supremacy proponents argue the founding problem included rights protection against majoritarianism, making the current arrangement mandatrophic (solving a problem that either no longer exists or was never the only problem). The contested status prevents either pure coordination or pure extraction labeling — the constraint is a Mountain in the reading's frame but functions as a Tangled Rope or Snare from excluded seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the parliamentary_sovereignty_reading a genuine Mountain (fixed democratic principle) or a constructed constraint benefiting the Knesset majority?',
    'Historical-institutional analysis of Israel''s constitutional development: if the Knesset''s constituent authority was understood as unlimited from 1948 onward with judicial review as a later judicial invention, the Mountain claim holds; if the founding generation contemplated judicial review, the reading is a constructed constraint.',
    'If Mountain: classification stands, FSM does not trigger (no victims declared). If constructed: false_summit_mountain signature fires (beneficiaries declared on Mountain) and reclassifies to tangled_rope, revealing the reading as a coordination-extraction hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether parliamentary sovereignty is a natural democratic law or a constructed institutional arrangement.').

omega_variable(
    minority_costs_extraction_boundary,
    'Do the diffuse costs borne by minority_groups (loss of binding judicial review) constitute extraction under this reading, or are they the inherent cost of democratic sovereignty?',
    'Comparative constitutional analysis: examine whether other parliamentary sovereignty systems (UK, NZ) provide alternative minority protections that Israel lacks, isolating the specific cost of the interpretive boundary.',
    'If extraction: victims array should include minority_groups, triggering snare/tangled_rope gates and raising ε. If inherent cost: Mountain classification holds with near-zero ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_costs_extraction_boundary, conceptual, 'Whether minority-group costs are extractive or constitutive of the democratic arrangement.').

omega_variable(
    international_obligations_scope,
    'How do international treaty obligations — the sole acknowledged external constraint — structurally limit Knesset sovereignty in practice?',
    'Case study of Knesset legislation overridden or modified due to treaty body rulings (e.g., UN Human Rights Committee views, ECHR-adjacent obligations via incorporation).',
    'If treaty obligations create binding veto points, the ''near-zero ε'' claim fails and the constraint becomes tangled_rope (coordination + asymmetric extraction from treaty-bound policy domains). If symbolic only, Mountain holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_obligations_scope, empirical, 'Whether international obligations are genuine constraints or rhetorical concessions.').

omega_variable(
    supreme_court_compliance_trajectory,
    'If the Knesset legislatively overrides a Supreme Court interpretation, will the Court comply, creating a constitutional crisis that tests the reading''s descriptive accuracy?',
    'Observe the 2023-2024 judicial reform crisis and its aftermath: whether the Court accepts legislative override of its Basic Law interpretations or asserts binding review despite the override.',
    'If Court complies: reading describes reality (Mountain). If Court refuses: reading is aspirational, not descriptive — the actual constraint is judicial_supremacy_reading or balanced_contestation_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supreme_court_compliance_trajectory, empirical, 'Whether the reading matches the operative constitutional order or is a contested claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(basi_tr_t8, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 8, 0.05).
narrative_ontology:measurement(basi_tr_t16, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(basi_tr_t32, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 32, 0.08).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(basi_be_t8, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 8, 0.04).
narrative_ontology:measurement(basi_be_t16, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 16, 0.05).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 24, 0.05).
narrative_ontology:measurement(basi_be_t32, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 32, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(basi_su_t8, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 8, 0.08).
narrative_ontology:measurement(basi_su_t16, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 16, 0.15).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 24, 0.2).
narrative_ontology:measurement(basi_su_t32, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 32, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.08).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint and its two sibling readings form the basic_law_interpretive_boundary constraint family. The kernel is the interpretive boundary of Israel's Basic Laws. This reading (parliamentary_sovereignty) claims ε≈0.05 for majoritarian policy and Mountain type. The judicial_supremacy_reading claims substantial ε (binding judicial veto) and Tangled Rope type. The balanced_contestation_reading claims moderate ε and Tangled Rope. The three stories have different ε values, different stakeholder structures, and different claimed types — they are structurally distinct constraints linked by the kernel, not one constraint with measurement variance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
