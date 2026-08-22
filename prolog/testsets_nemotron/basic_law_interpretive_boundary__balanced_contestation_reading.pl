% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Balanced Contestation Reading of Basic Law Interpretive Authority
 *   domain: constitutional_law/judicial_review_theory
 *
 * SUMMARY:
 *   The balanced contestation reading of Israeli Basic Law interpretive
 *   authority posits that neither the Supreme Court nor the Knesset holds
 *   final authority. Courts interpret within a jurisdictional domain defined
 *   by justiciability doctrines, proportionality, and purposive
 *   interpretation; the legislature retains sovereign power to legislate and
 *   amend Basic Laws but operates under international obligations (human
 *   rights treaties) and norms of judicial independence that constrain pure
 *   majoritarianism. The executive mediates between them, both constrained by
 *   and benefiting from judicial review. This reading instantiates one
 *   constraint from the contested kernel 'basic_law_interpretive_boundary' —
 *   sibling readings are judicial_supremacy_reading (court-final) and
 *   parliamentary_sovereignty_reading (Knesset-final).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.38).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.22).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Balanced Contestation Reading of Basic Law Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional_law/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, '97cc1116-bbf7-45ca-a2ed-2fd86d91009b').
narrative_ontology:cs_kernel_codification('97cc1116-bbf7-45ca-a2ed-2fd86d91009b', formalized).
narrative_ontology:cs_authority_grounding('97cc1116-bbf7-45ca-a2ed-2fd86d91009b', lineage).
narrative_ontology:cs_interpretation_layer_present('97cc1116-bbf7-45ca-a2ed-2fd86d91009b').
narrative_ontology:cs_reading_relation('97cc1116-bbf7-45ca-a2ed-2fd86d91009b', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('97cc1116-bbf7-45ca-a2ed-2fd86d91009b', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('97cc1116-bbf7-45ca-a2ed-2fd86d91009b', foundational, constitutional_dialogue_as_legitimating_mechanism).
narrative_ontology:cs_axiom_status(constitutional_dialogue_as_legitimating_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('97cc1116-bbf7-45ca-a2ed-2fd86d91009b', constitutional_dialogue_as_legitimating_mechanism, conventional).
narrative_ontology:cs_axiom('97cc1116-bbf7-45ca-a2ed-2fd86d91009b', foundational, institutional_legitimacy_requires_mutual_restraint).
narrative_ontology:cs_axiom_status(institutional_legitimacy_requires_mutual_restraint, holdable).
narrative_ontology:cs_axiom_grounding('97cc1116-bbf7-45ca-a2ed-2fd86d91009b', institutional_legitimacy_requires_mutual_restraint, conventional).
narrative_ontology:cs_axiom('97cc1116-bbf7-45ca-a2ed-2fd86d91009b', secondary, basic_law_interpretive_authority_is_domain_variable).
narrative_ontology:cs_axiom_status(basic_law_interpretive_authority_is_domain_variable, holdable).
narrative_ontology:cs_axiom_grounding('97cc1116-bbf7-45ca-a2ed-2fd86d91009b', basic_law_interpretive_authority_is_domain_variable, empirically_contingent).
narrative_ontology:cs_reference_frame('97cc1116-bbf7-45ca-a2ed-2fd86d91009b', post_1992_constitutional_revolution_dialogue_model).
narrative_ontology:cs_drift_state('97cc1116-bbf7-45ca-a2ed-2fd86d91009b', post_2023_judicial_reform_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('97cc1116-bbf7-45ca-a2ed-2fd86d91009b', '2026-08-25T14:30:00Z').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_supreme_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, civil_society_ngos).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, international_legal_observers).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, policy_domain_losers_in_judicial_review).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_majorities_facing_judicial_checks).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, executive_agencies_constrained_by_judicial_interpretation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_dialogue_model).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__balanced_contestation_reading, institutional_legitimacy_through_mutual_restraint).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_authority_shared).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises judicial review over Basic Laws and ordinary legislation, developing interpretive doctrines (purposive interpretation, proportionality, unconstitutional constitutional amendments). Claims authority to strike down legislation but faces political backlash and override threats. Professional identity fused with constitutional guardian role; exit from this identity would mean abandoning the court's self-conception as protector of democratic fundamentals.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_supreme_court, agenda_setter,
    institutional, generational, identity_locked, national).

% Retains formal sovereign power to legislate and amend Basic Laws by simple majority. Experiences judicial invalidation as constraint on legislative agenda; responds with override legislation, judicial appointments reform, and public criticism. Coalition politics make exit from majoritarian logic difficult; constrained by international obligations and democratic norms that limit pure majoritarianism.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_legislature, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_legislature, payer).

% Subject to judicial review of administrative decisions and legislative compliance. Benefits from judicial legitimization of executive actions when courts uphold policy; pays compliance costs when courts strike down or restrict policy. Cabinet ministers publicly criticize judicial activism while relying on courts to validate controversial security and economic policies.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch, beneficiary).

% Interest groups, communities, or sectors whose preferred policies are blocked or modified by judicial review (e.g., settlement advocates facing proportionality review, Haredi parties facing equality jurisprudence, economic reformers facing property rights protections). Organize politically to demand override legislation or judicial reform; exit constrained by dependence on state policy channels.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, policy_domain_losers_in_judicial_review, payer,
    organized, biographical, constrained, national).

% Current coalition majorities that experience judicial invalidation of flagship legislation. Pay political capital and policy delay costs; respond with court-curbing measures. Exit from majoritarian logic constrained by coalition agreements and electoral mandates; identity as 'the sovereign' makes judicial checks feel like usurpation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_majorities_facing_judicial_checks, payer,
    organized, immediate, constrained, national).

% Ministries and regulatory bodies whose discretionary powers are narrowed by purposive interpretation and proportionality review. Pay compliance and redesign costs; benefit from legal certainty when courts provide clear standards. Professional bureaucratic identity resists both judicial micromanagement and political interference; exit constrained by statutory mandates.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, executive_agencies_constrained_by_judicial_interpretation, payer,
    organized, biographical, constrained, national).

% Human rights organizations, democracy watchdogs, and advocacy groups that use judicial review to advance rights claims and check majoritarian overreach. Benefit from court accessibility and expansive standing doctrines; mobilize internationally when domestic space narrows. Mobile exit options through transnational advocacy networks and foreign funding.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, civil_society_ngos, beneficiary,
    organized, generational, mobile, national).

% Venice Commission, UN treaty bodies, foreign constitutional courts, and comparative scholars who monitor Israeli constitutional dynamics. Provide external legitimacy benchmarks; their assessments influence domestic institutional legitimacy and international standing. Pure analytical seat — no direct stakes in outcomes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_legal_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured channel for resolving fundamental constitutional disagreements between courts and legislature without constitutional collapse — the dialogue model replaces winner-take-all supremacy contests with iterative negotiation over interpretive boundaries, preserving both institutional legitimacy and democratic responsiveness across policy domains.
% TRANSFER_FUNCTION: Moves interpretive authority and policy discretion between court and legislature across policy domains: courts gain final say on rights-proportionality-review in security and discrimination domains; legislature retains final say on socioeconomic policy and resource allocation; executive gains operational discretion within judicially defined boundaries. The transfer is domain-specific and reversible through political-judicial interaction.
% ABSENT_VOICES: Palestinian residents of occupied territories (subject to Israeli military court system with limited judicial review), Arab Israeli citizens (systematically underrepresented in judicial appointments and legislative coalitions), ultra-Orthodox communities (reject secular constitutional framework entirely), and future generations (bear long-term constitutional design consequences without voice). These groups would challenge the balanced contestation frame as masking structural exclusion.
% DISAPPEARANCE_RATIONALE: If the balanced contestation constraint vanished, the system would polarize into either judicial supremacy (courts unilaterally define Basic Law meaning with binding effect) or parliamentary sovereignty (Knesset overrides all judicial review by simple majority). The triadic negotiation space — where courts, legislature, and executive each hold veto points on the others' constitutional claims — would collapse into a binary dominance contest, restructuring Israeli constitutional politics entirely.
% FOUNDING_PROBLEM: The 1990s constitutional revolution (Basic Laws: Human Dignity, Freedom of Occupation) created judicial review without a clear constituent power or constitutional text defining its scope. The founding problem was: how to legitimate judicial invalidation of legislation in a system with parliamentary sovereignty tradition, no entrenched constitution, and deep societal divisions over identity, territory, and religion-state relations.
% FOUNDING_PROBLEM_CORROBORATION: Supreme Court justices (Barak, Beinisch, Hayut) attest the problem remains live — constitutional dialogue is ongoing work. Knesset speakers and coalition chairs (Levin, Rothman) attest the problem is dead — judicial review was an illegitimate usurpation now being corrected. Legal scholars (Shamgar, Rubinstein, Navot, Diskin) split: some see dialogue as stabilizing achievement, others as unstable truce. No external corroboration beyond Israeli constitutional discourse; international observers (Venice Commission) validate dialogue model normatively but do not attest to Israeli founding facts.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).
:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects domain-variable costs: rights claimants gain judicial protection (low extraction for them); legislative majorities lose policy autonomy (high extraction for them). Suppression (0.22) is modest — alternatives exist (political override, constitutional amendment, non-compliance) but carry high legitimacy costs. Theater (0.18) is low-moderate: judicial review performs genuine rights-protection function, but performative constitutionalism increases during political crises (2023 judicial reform crisis). Accessibility collapse (0.45) reflects that once the dialogue frame is understood, pure supremacy claims become harder to sustain but not impossible. Resistance (0.52) is significant — both institutions actively contest the boundary.
 *
 * PERSPECTIVAL GAP:
 *   Court seat experiences constraint as coordination (it provides interpretive structure for rights protection); legislative majority seat experiences it as extraction (judicial veto on democratic will); executive seat experiences it as mixed (constraint on discretion + legitimization of action). The engine computes this divergence from the structural data — the same constraint is tangled_rope from court seat, snare-adjacent from legislative majority seat, rope-adjacent from executive seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Court and Knesset are dual agenda_setters with identity_locked and constrained exit respectively — court's professional identity fused to guardian role; Knesset's majoritarian identity fused to sovereignty claim. Executive branch and organized payer groups are constrained payers with domain-specific benefits. Civil society NGOs are mobile beneficiaries with transnational exit. International observers are analytical. Directionality derives from beneficiary/victim declarations: court and Knesset each benefit in some domains and pay in others; organized groups pay when their policy preferences lose in judicial review; NGOs benefit from access to courts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimating judicial review without constituent power) remains contested — not dead. The arrangement has not atrophied into piton because all three institutions actively contest and renegotiate the boundary (2023 reform crisis proves vitality). Mandatrophy is unresolved: the dialogue model persists because no institution can impose its preferred resolution, not because the founding problem is solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dialogue_vs_paralysis,
    'Does balanced contestation produce productive institutional dialogue or decision paralysis on fundamental issues (territory, religion-state, minority rights)?',
    'Longitudinal study of policy outcomes in domains where court and legislature repeatedly interact (e.g., Haredi conscription, settlement legality, asylum policy) — measure time-to-resolution and policy stability.',
    'If paralysis dominates, the constraint functions as scaffold (transitional) or piton (inertial) rather than tangled_rope; if dialogue produces stable compromises, tangled_rope classification is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dialogue_vs_paralysis, empirical, 'Whether contestation generates functional negotiation or systemic gridlock.').

omega_variable(
    international_obligations_as_constraint,
    'Do international human rights obligations genuinely constrain Knesset majorities, or are they rhetorical resources deployed selectively?',
    'Compare Knesset compliance rates with UN treaty body recommendations vs. domestic political costs of non-compliance; track citation of international law in override legislation debates.',
    'If obligations are rhetorical only, the ''constrained by international obligations'' element of this reading is performative — extraction shifts toward parliamentary_sovereignty_reading dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_obligations_as_constraint, empirical, 'Whether international law operates as real constraint or legitimating discourse.').

omega_variable(
    kernel_reading_boundary_foreclosure,
    'Does the balanced contestation reading logically foreclose the judicial_supremacy_reading within a single constitutional framework, or do they coexist as competing interpretations?',
    'Analyze whether any sitting justice or legal scholar has explicitly held both readings simultaneously; track doctrinal evolution of ''unconstitutional constitutional amendments'' doctrine.',
    'If forecloses, the kernel has a binary structure (dialogue vs. supremacy); if coexists_with, the kernel supports stable pluralism. Determines reading_relations declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_foreclosure, conceptual, 'Logical relationship between balanced contestation and judicial supremacy readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (formal override mechanisms, appointment powers) or internalized (institutional self-censorship, anticipatory compliance)?',
    'Compare formal override usage rates vs. legislative drafting changes anticipating judicial review; interview legislative drafters and judicial clerks.',
    'If internalized suppression dominates, effective suppression is higher than structural measure suggests — the constraint operates partly through institutionalized anticipation rather than active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in inter-institutional constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 1992, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_tr_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 1992, 0.08).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_tr_t1995, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_tr_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_tr_t2006, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2006, 0.15).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_tr_t2015, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_tr_t2020, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_tr_t2023, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2023, 0.25).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_tr_t2025, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_be_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 1992, 0.15).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_be_t1995, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 1995, 0.22).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_be_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_be_t2006, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2006, 0.35).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_be_t2015, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_be_t2020, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_be_t2023, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2023, 0.45).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_be_t2025, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_su_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 1992, 0.1).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_su_t1995, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 1995, 0.15).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_su_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_su_t2006, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2006, 0.22).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_su_t2015, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_su_t2020, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2020, 0.25).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_su_t2023, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2023, 0.35).
narrative_ontology:measurement(basic_law_interpretive_boundary_balanced_su_t2025, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2025, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__balanced_contestation_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_judicial_appointments_process).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_human_dignity_proportionality_doctrine).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_override_legislation_mechanism).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the single colloquial label 'Israeli judicial review' into three structurally distinct constraints with different ε, different beneficiary/victim structures, and different coordination/extraction balances. The balanced contestation reading claims the boundary is inherently and productively contested; the judicial supremacy reading claims courts have final interpretive authority; the parliamentary sovereignty reading claims Knesset has final authority via simple majority. They are linked by shared kernel_id and cross-referenced in affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_boundary__balanced_contestation_reading, institutional, 0.35).
constraint_indexing:directionality_override(basic_law_interpretive_boundary__balanced_contestation_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
