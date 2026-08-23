% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Progressive Textualist Equality Clause Constraint
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The progressive textualist reading of the equality clause (primarily the
 *   Fourteenth Amendment's Equal Protection Clause and its Fifth Amendment
 *   due process counterpart) holds that the constitutional text contains a
 *   genuine equality principle whose scope of application expands, but only
 *   through the Article V amendment process — not through judicial
 *   reinterpretation. This reading positions itself between restrictive
 *   originalism (which denies the principle's universalizability) and
 *   expansive universalism (which treats the principle as self-executing and
 *   judicially enforceable in all its implications). The constraint is the
 *   interpretive rule itself: courts must defer to democratic amendment for
 *   scope expansion. It coordinates by providing a stable, legitimate pathway
 *   for constitutional change; it extracts by imposing delay costs on rights
 *   claimants who must await supermajority consensus. The constraint has
 *   operated since 1789 but its extractiveness peaked during the mid-20th
 *   century when judicial expansion was most actively contested and
 *   suppressed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.45).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.55).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.45).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Progressive Textualist Equality Clause Constraint").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, '1e1a9944-acad-4929-b712-009029a5a775').
narrative_ontology:cs_kernel_codification('1e1a9944-acad-4929-b712-009029a5a775', formalized).
narrative_ontology:cs_authority_grounding('1e1a9944-acad-4929-b712-009029a5a775', lineage).
narrative_ontology:cs_interpretation_layer_present('1e1a9944-acad-4929-b712-009029a5a775').
narrative_ontology:cs_reading_relation('1e1a9944-acad-4929-b712-009029a5a775', equality_clause_scope__restrictive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('1e1a9944-acad-4929-b712-009029a5a775', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_axiom('1e1a9944-acad-4929-b712-009029a5a775', foundational, equality_principle_universalizable_but_amendment_bound).
narrative_ontology:cs_axiom_status(equality_principle_universalizable_but_amendment_bound, holdable).
narrative_ontology:cs_axiom_grounding('1e1a9944-acad-4929-b712-009029a5a775', equality_principle_universalizable_but_amendment_bound, conventional).
narrative_ontology:cs_reference_frame('1e1a9944-acad-4929-b712-009029a5a775', textual_amendment_bound_equality).
narrative_ontology:cs_drift_state('1e1a9944-acad-4929-b712-009029a5a775', contemporary_judicial_activism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1e1a9944-acad-4929-b712-009029a5a775', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, originalist_judges).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, democratic_legitimacy_advocates).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, institutional_stability_proponents).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, marginalized_rights_claimants).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, progressive_litigants).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, groups_awaiting_equality_recognition).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, article_v_exclusivity_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, democratic_legitimacy_principle).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, constitutional_textualism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt and enforce the progressive textualist reading as binding precedent, requiring equality scope expansion to proceed through Article V amendment rather than judicial interpretation. They administer the constraint by dismissing equal protection claims that seek judicial expansion beyond ratified amendments.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, originalist_judges, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from the constraint's insistence that rights expansion carry democratic authorization. They view the amendment process as the only legitimate vehicle for constitutional change and gain rhetorical and institutional leverage from the constraint's enforcement.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, democratic_legitimacy_advocates, beneficiary,
    organized, generational, mobile, national).

% Value the constraint as a bulwark against rapid doctrinal oscillation. They benefit from the predictability and stability that comes from channeling equality disputes through the supermajoritarian amendment process rather than judicial majorities.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, institutional_stability_proponents, beneficiary,
    organized, civilizational, mobile, national).

% Bear the costs of delayed equality recognition while awaiting supermajority consensus. Their identity and dignity claims are bound to the constitutional order; exit means abandoning the constitutional promise itself. They experience the constraint as a structural barrier to timely justice.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, marginalized_rights_claimants, payer,
    powerless, biographical, identity_locked, national).

% Litigate for equality expansion but find their path blocked by the amendment-only rule. They can pursue legislative remedies or constitutional amendment campaigns, but both require overcoming the same supermajoritarian thresholds that the constraint entrenches. Their professional practice is shaped by this barrier.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, progressive_litigants, payer,
    moderate, biographical, constrained, national).

% Communities whose equality claims lack both judicial and amendment-path viability in the current political configuration. They bear the full extraction of the constraint with no realistic exit — neither courts nor Article V are accessible vehicles for their claims.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, groups_awaiting_equality_recognition, payer,
    powerless, generational, trapped, national).

% Would interpret the equality clause as self-executing universal principle requiring no amendment for new applications. They are structurally excluded from enforcing this reading when the progressive textualist constraint dominates judicial doctrine, though they dissent and write competing opinions.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, expansive_universalist_judges, excluded,
    institutional, generational, constrained, national).

% Analyze the constraint from outside the progressive textualist framework. They agree on judicial restraint but reject the premise that the text contains a universalizable equality principle. Their scholarship maps the constraint's genealogical and doctrinal boundaries.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, restrictive_originalist_scholars, observer,
    analytical, civilizational, analytical, global).

% Study the constraint's operation across historical periods, tracking how the amendment-only rule has shaped equality jurisprudence, social movements, and constitutional culture. They provide the empirical and theoretical baseline for evaluating the constraint's effects.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, democratically legitimate path for equality scope expansion, preventing judicial oligarchy and ensuring broad social consensus for new rights recognitions by requiring supermajority ratification.
% TRANSFER_FUNCTION: Delays the legal recognition of equality rights for marginalized groups until supermajority democratic consensus is achieved, transferring the cost of delay from the majority to the minority and from the present to the future.
% ABSENT_VOICES: Historically excluded groups (enslaved persons, women, non-property holders, indigenous nations) who were not represented in the original constitutional compact and whose descendants bear the delay costs of the amendment process. Also future generations whose equality claims have not yet entered the political agenda.
% DISAPPEARANCE_RATIONALE: If the amendment-only constraint vanished overnight, courts would become the primary vehicle for equality scope expansion. Rights recognition would accelerate for currently excluded groups, but the democratic legitimacy of each expansion would be contested, leading to recurring legitimacy crises and potential backlash-driven retrenchment.
% FOUNDING_PROBLEM: The founding problem was how to entrench a principle of equality in a constitution that simultaneously permitted slavery and excluded women from civic participation, while providing a mechanism for future expansion that would not depend on judicial discretion or temporary majorities.
% FOUNDING_PROBLEM_CORROBORATION: The text of Article V and the Federalist Papers (particularly Federalist 43 and 85) corroborate the amendment mechanism as the designed expansion path. The Reconstruction Amendments (13th, 14th, 15th) and the 19th Amendment corroborate that major equality expansions historically occurred through amendment. However, 20th-century civil rights jurisprudence (Brown v. Board, Reed v. Reed, Obergefell v. Hodges) is cited by expansive universalists as evidence that judicial expansion has been a legitimate and necessary historical practice. No single account commands consensus across the beneficiary/payer divide.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).
:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: the constraint does not directly transfer wealth but imposes significant time-costs on rights claimants. Suppression (0.55) is higher than extraction because the constraint's persistence depends on actively suppressing judicial expansion paths — courts must be institutionally disciplined to refuse equal protection claims that exceed ratified amendments. Theater ratio (0.20) is low: the amendment process is genuinely used (27 amendments, including major equality expansions) and not merely performative. Accessibility collapse (0.50) is moderate: alternative paths (statutory rights, state constitutions, international law) exist but are treated as supplementary, not substitutive, for federal constitutional equality. Resistance (0.70) is high: the constraint has faced sustained challenge from progressive legal movements, civil rights litigation, and academic criticism throughout its operation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (originalist judges) and beneficiary seats experience the constraint as coordination — a legitimate, stable framework for constitutional development. The payer seats experience it as extraction — a barrier that forces them to pay the price of democratic sluggishness for rights that the text's principle already promises. The engine computes this divergence from the declared roles, power, and exit options; the claimed_type (tangled_rope) reflects the author's structural judgment that both functions are genuinely present.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges and democratic legitimacy advocates are structural beneficiaries (d near 0.0): they gain institutional authority and rhetorical coherence from the constraint. Marginalized rights claimants and groups awaiting recognition are full targets (d near 1.0): they bear the delay costs with identity-locked or trapped exit. Progressive litigants are constrained payers (d ~ 0.7): they have professional exit options but their cause is structurally blocked. Expansive universalist judges are excluded (d not computed): their interpretive approach is suppressed by the constraint's dominance. The engine will derive directionality from these structural positions and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (entangling equality with a supermajoritarian amendment gate) remains contested — not dead. The Reconstruction Amendments solved the slavery-exclusion problem but created a new founding problem: whether the equality principle requires judicial enforcement against democratic majorities that refuse to amend. The progressive textualist reading answers 'no' — but this answer is precisely what the expansive universalist and restrictive originalist readings contest. The mandate has not atrophied; it is actively litigated in every major equality case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_access_bias,
    'Is the Article V amendment process structurally accessible to marginalized groups, or does its supermajoritarian design systematically exclude the very groups whose equality claims it gates?',
    'Historical analysis of amendment campaigns led by marginalized groups (e.g., ERA, DC statehood, voting rights amendments) compared to campaigns led by majority interests; political science modeling of amendment feasibility under polarized conditions.',
    'If the process is structurally biased, the constraint operates as a snare for marginalized groups — the coordination function is illusory for them. If accessible, the constraint is a genuine tangled_rope with real but costly coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_access_bias, empirical, 'Whether the democratic amendment path is genuinely open to those it makes wait.').

omega_variable(
    judicial_vs_amendment_stability,
    'Do equality expansions achieved through judicial interpretation prove less stable or legitimate than those achieved through amendment, as the progressive textualist claim predicts?',
    'Longitudinal study of rights recognitions: compare durability, public acceptance, and resistance to backlash for judicial vs. amendment-based equality expansions (e.g., Brown vs. 14th Amendment; Obergefell vs. hypothetical marriage equality amendment).',
    'If judicial expansions are equally or more stable, the coordination justification for the constraint weakens. If amendment-based expansions are distinctly more durable, the constraint''s coordination function is empirically validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_vs_amendment_stability, empirical, 'Empirical test of the constraint''s coordination justification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s suppression of judicial remedies structural (Article V design, judicial hierarchy) or internalized (progressive lawyers accepting amendment-only framing, social movements diverting energy to amendment campaigns)?',
    'Post-exit trajectory analysis: if suppression persists after judicial composition changes (e.g., progressive courts still defer to amendment-only logic), reclassify as partially internalized. Track movement strategy allocation between litigation and amendment advocacy.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the targets carry the suppression with them into their advocacy strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression in the equality clause constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecs_pt_tr_t1789, equality_clause_scope__progressive_textualist, theater_ratio, 1789, 0.1).
narrative_ontology:measurement(ecs_pt_tr_t1868, equality_clause_scope__progressive_textualist, theater_ratio, 1868, 0.15).
narrative_ontology:measurement(ecs_pt_tr_t1920, equality_clause_scope__progressive_textualist, theater_ratio, 1920, 0.18).
narrative_ontology:measurement(ecs_pt_tr_t1954, equality_clause_scope__progressive_textualist, theater_ratio, 1954, 0.25).
narrative_ontology:measurement(ecs_pt_tr_t1973, equality_clause_scope__progressive_textualist, theater_ratio, 1973, 0.22).
narrative_ontology:measurement(ecs_pt_tr_t2015, equality_clause_scope__progressive_textualist, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(ecs_pt_tr_t2024, equality_clause_scope__progressive_textualist, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(ecs_pt_be_t1789, equality_clause_scope__progressive_textualist, base_extractiveness, 1789, 0.3).
narrative_ontology:measurement(ecs_pt_be_t1868, equality_clause_scope__progressive_textualist, base_extractiveness, 1868, 0.35).
narrative_ontology:measurement(ecs_pt_be_t1920, equality_clause_scope__progressive_textualist, base_extractiveness, 1920, 0.4).
narrative_ontology:measurement(ecs_pt_be_t1954, equality_clause_scope__progressive_textualist, base_extractiveness, 1954, 0.55).
narrative_ontology:measurement(ecs_pt_be_t1973, equality_clause_scope__progressive_textualist, base_extractiveness, 1973, 0.5).
narrative_ontology:measurement(ecs_pt_be_t2015, equality_clause_scope__progressive_textualist, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(ecs_pt_be_t2024, equality_clause_scope__progressive_textualist, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ecs_pt_su_t1789, equality_clause_scope__progressive_textualist, suppression_requirement, 1789, 0.4).
narrative_ontology:measurement(ecs_pt_su_t1868, equality_clause_scope__progressive_textualist, suppression_requirement, 1868, 0.5).
narrative_ontology:measurement(ecs_pt_su_t1920, equality_clause_scope__progressive_textualist, suppression_requirement, 1920, 0.55).
narrative_ontology:measurement(ecs_pt_su_t1954, equality_clause_scope__progressive_textualist, suppression_requirement, 1954, 0.7).
narrative_ontology:measurement(ecs_pt_su_t1973, equality_clause_scope__progressive_textualist, suppression_requirement, 1973, 0.6).
narrative_ontology:measurement(ecs_pt_su_t2015, equality_clause_scope__progressive_textualist, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(ecs_pt_su_t2024, equality_clause_scope__progressive_textualist, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__progressive_textualist, 0.12).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, reconstruction_amendments).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, judicial_review_power).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, civil_rights_legislation).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, state_constitutional_equality).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, international_human_rights_law).

% DUAL FORMULATION NOTE:
% This constraint (progressive_textualist) is one of three readings in the equality_clause_scope kernel family. The restrictive_originalist reading forecloses universalizability; the expansive_universalist reading forecloses amendment-exclusivity. All three readings claim the same constitutional text but instantiate different constraints with different beneficiary/victim structures and different extractiveness profiles. They are linked through network.affects_constraints in each story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__progressive_textualist, powerless, 0.95).
constraint_indexing:directionality_override(equality_clause_scope__progressive_textualist, moderate, 0.7).
constraint_indexing:directionality_override(equality_clause_scope__progressive_textualist, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
