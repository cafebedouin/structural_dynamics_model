% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Functional Protection Floor: Common Article 3 Baseline for All Detainees
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   Under the functional protection reading of combatant status, all persons
 *   detained during armed conflict receive Common Article 3 minimum
 *   protections—prohibition of torture, enforced disappearance, and summary
 *   execution; access to medical care and fair trial procedures—regardless of
 *   whether they have been formally classified as combatants, civilians, or
 *   persons of undetermined status. This reading prioritizes immediate
 *   protection over status precision. The constraint removes status
 *   determination as a precondition for humane treatment, creating a
 *   universal floor below which no detention practice falls. The founding
 *   problem it solves (what protections apply to disputed-status detainees)
 *   remains live in contemporary non-international armed conflicts. The
 *   engine measures this as low extractiveness because the constraint
 *   primarily coordinates baseline protection and removes barriers to its
 *   application, rather than extracting asymmetric benefit.
 *
 * KEY AGENTS:
 *   - detained_persons_all_categories: The primary beneficiaries; their protection applies regardless of status determination. Exit is only through release or conflict cessation.
 *   - detaining_military_authorities: Pay the cost of immediate compliance with protections for all detainees. Their exit is constrained by treaty obligation; they cannot defer compliance pending status determination.
 *   - treaty_bodies_and_icrc: Agenda-setter that maintains the constraint through interpretation, monitoring, and case law. They set and enforce the standard.
 *   - state_centric_proponents: Institutional payers whose preferred reading (status-based differentiation) loses authority when the functional floor applies universally.
 *   - national_liberation_movements: Dual position: beneficiary (their captured combatants receive protections even if not formally recognized) and payer (they too must provide protections to detainees).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.15).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.22).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Functional Protection Floor: Common Article 3 Baseline for All Detainees").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, 'f7a2da93-9d4b-4c8e-909a-87e299027387').
narrative_ontology:cs_kernel_codification('f7a2da93-9d4b-4c8e-909a-87e299027387', formalized).
narrative_ontology:cs_authority_grounding('f7a2da93-9d4b-4c8e-909a-87e299027387', lineage).
narrative_ontology:cs_interpretation_layer_present('f7a2da93-9d4b-4c8e-909a-87e299027387').
narrative_ontology:cs_reading_relation('f7a2da93-9d4b-4c8e-909a-87e299027387', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7a2da93-9d4b-4c8e-909a-87e299027387', combatant_status_definition__national_liberation_reading, influences).
narrative_ontology:cs_axiom('f7a2da93-9d4b-4c8e-909a-87e299027387', foundational, humanity_precedes_status).
narrative_ontology:cs_axiom_status(humanity_precedes_status, holdable).
narrative_ontology:cs_axiom_grounding('f7a2da93-9d4b-4c8e-909a-87e299027387', humanity_precedes_status, deontological).
narrative_ontology:cs_axiom('f7a2da93-9d4b-4c8e-909a-87e299027387', foundational, protection_floor_applies_universally).
narrative_ontology:cs_axiom_status(protection_floor_applies_universally, holdable).
narrative_ontology:cs_axiom_grounding('f7a2da93-9d4b-4c8e-909a-87e299027387', protection_floor_applies_universally, conventional).
narrative_ontology:cs_reference_frame('f7a2da93-9d4b-4c8e-909a-87e299027387', common_article_three_universal_applicability).
narrative_ontology:cs_drift_state('f7a2da93-9d4b-4c8e-909a-87e299027387', contemporary_non_international_armed_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f7a2da93-9d4b-4c8e-909a-87e299027387', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, detained_persons_all_categories).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, international_humanitarian_law_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, national_liberation_movements).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, conflict_affected_populations).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, detaining_military_authorities).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, state_centric_proponents).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, national_liberation_movements).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, human_dignity_precedes_legal_status).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, detention_law_applies_universally).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All individuals held in captivity during armed conflict receive minimum protections: prohibition of torture, cruel treatment, enforced disappearance, and humiliating punishment; medical care; fair and regular trial procedures. These protections apply regardless of whether they are labeled combatants, fighters, civilians, or those of undetermined status. Their only exit from the constraint's scope is release or end of conflict.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detained_persons_all_categories, beneficiary,
    powerless, immediate, trapped, universal).

% Obligated to classify and treat all detainees according to Common Article 3 minimum standards without waiting for formal status determination. They bear the administrative, training, and facility costs of implementing protections for detainees of unknown or disputed status. The constraint removes status determination as a precondition for providing protections, which increases near-term compliance burden.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detaining_military_authorities, payer,
    institutional, generational, constrained, universal).

% Red Cross/Red Crescent movement, human rights monitors, treaty bodies, and academic specialists operate under the universal applicability of Common Article 3. They document compliance, advocate for consistent interpretation, and support capacity-building for detaining authorities. The functional protection floor reduces interpretive disputes about who qualifies for protection.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, international_humanitarian_law_community, beneficiary,
    organized, generational, mobile, universal).

% States favoring restrictive combatant definitions (Article 4 criteria only) view the functional floor as constraining their operational choices and preventing them from denying protections to non-state actors they classify as outside the scope of IHL. They are not excluded from treaty bodies but their preferred reading competes against this one; the functional reading denies them the legal cover to differentiate below the baseline.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, state_centric_proponents, payer,
    institutional, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, state_centric_proponents, excluded).

% Non-state armed groups, especially those claiming liberation status under AP I Article 1(4), receive baseline protections for their captured combatants under this reading even if not formally recognized as combatants. They also bear the cost of implementing protections for their detainees, creating reciprocal obligation. The functional floor removes status determination as a barrier but also extends duty to provide protections.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, national_liberation_movements, beneficiary,
    organized, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, national_liberation_movements, payer).

% Civilians caught in armed conflict benefit from the clarity of universal baselines: they know that if detained, they receive protections regardless of accusations or suspicions about their status. The functional floor reduces the risk of disappearance-level abuse arising from status disputes. Their exit from the constraint's scope is only through release or conflict cessation.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, conflict_affected_populations, beneficiary,
    powerless, immediate, trapped, universal).

% The International Committee of the Red Cross, treaty monitoring bodies, and international courts interpret and enforce Common Article 3 as a universal floor. They set the standard, monitor compliance, investigate violations, and advocate for consistent application. They maintain the constraint through interpretation, case law, and technical assistance.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, treaty_bodies_and_icrc, agenda_setter,
    institutional, generational, mobile, universal).

% Must adjudicate violations of Common Article 3 in domestic criminal law frameworks. The functional protection floor provides them with a universal standard against which to measure compliance and prosecute violations, but integration into purely national law creates tension when national sovereignty claims exemptions.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, national_courts_and_prosecutors, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__functional_protection_reading, international_humanitarian_law_community).
narrative_ontology:fixing_cost_class(combatant_status_definition__functional_protection_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline of humane treatment for all persons deprived of liberty during armed conflict, removing status determination as a precondition: all parties can rely on a single floor of obligations regardless of classification disputes, avoiding legal gaps where detained persons fall into status limbo.
% TRANSFER_FUNCTION: Transfers protective obligations FROM status-determination authority (traditional POW/combatant/civilian distinctions) TO all detaining powers equally, and transfers dignity/protection rights TO all detained persons regardless of their legal characterization. The transfer cost is compliance burden on detaining authorities; the transfer benefit is universally applied protection.
% ABSENT_VOICES: Detainees of disputed or unknown status themselves—those whose detention occurs precisely where classification is contested (captured irregular fighters, civilians suspected of combatancy, persons in failed states or non-international armed conflicts)—are the primary speakers this reading privileges, yet they have no seat at treaty negotiation. Authoritarian regimes that benefit from status ambiguity to avoid accountability are excluded by design.
% DISAPPEARANCE_RATIONALE: If the functional protection floor disappeared overnight, detaining authorities in dozens of armed conflicts would immediately alter detention practices: protections for disputed-status detainees would drop to whatever their state of origin provides (often zero), interrogation and solitary confinement of those awaiting classification would intensify, and legal accountability for torture of non-combatants would collapse in jurisdictions treating status-determination as a threshold. The constraint's removal would restructure incentives for classification speed and quality.
% FOUNDING_PROBLEM: Early modern law of war (19th century POW conventions) assumed symmetrical state-to-state conflict with clear combatant/civilian lines. By mid-20th century (post-1949), asymmetrical conflicts, civil wars, and decolonization produced regular situations where detained persons' legal status was ambiguous or contested—no forum had decided if they were POWs, unlawful combatants, or civilians. The founding problem was: what protections apply to someone detained when no authority has yet determined their status?
% FOUNDING_PROBLEM_CORROBORATION: The International Committee of the Red Cross, UN Human Rights mechanisms, and independent monitors (Amnesty International, Human Rights Watch) document ongoing cases of detained persons held without status determination in Yemen, Syria, Myanmar, Colombia, and other non-international armed conflicts—the founding problem remains empirically present. The International Court of Justice and ad-hoc tribunals have repeatedly reaffirmed that Common Article 3 applies to these gaps. States party to the Geneva Conventions attest to the problem through their reporting obligations. Independent legal scholars and human rights organizations outside treaty-beneficiary states corroborate that status ambiguity is systematically exploited to avoid protections.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).
:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extracted value is low (0.15 at interval end) because the constraint's primary function is protective coordination, not redistributive extraction. Suppression is modest (0.22) because the constraint's enforcement is mostly through treaty monitoring, capacity-building, and case law, not through coercive exclusion of alternatives. Theater ratio is low (0.18, rising slightly then stabilizing) because the constraint's core function (ensuring protections apply) remains substantially real; the theater component appears in status-determination performativity (authorities creating classification procedures to appear compliant while delaying actual protections). The measurement series shows slight initial rise in extractiveness and theater as the constraint encounters resistance from state-centric authorities, followed by stabilization as normative acceptance broadens. Suppression requirement remains stable and modest because the constraint suppresses alternative practices (status-based denial of protections) rather than suppressing resistance to the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   The detaining authority's seat and the detained person's seat should compute differently. From the authority's perspective, the constraint imposes immediate compliance costs (providing protections without first completing classification); from the detainee's perspective, the constraint removes barriers to protection and applies universally. The engine computes this as moderate directionality divergence: the authority is a structural payer (bears compliance costs) while the detainee is a structural beneficiary (receives protection). The legal/treaty community sits as a beneficiary-beneficiary (the constraint vindicates their interpretive authority and reduces disputes), while state-centric authorities sit as payers losing legal cover for status-based denial.
 *
 * DIRECTIONALITY LOGIC:
 *   The detaining authorities are structural payers (d toward 1.0): they incur compliance costs immediately, cannot defer by claiming status ambiguity, and lose discretion to withhold protections. Their exit is constrained (they are bound by treaty). Detained persons are structural beneficiaries (d toward 0.0): they gain protections without precondition, their exit from the constraint is only through release. The international humanitarian law community are beneficiaries (their interpretive authority is vindicated, status disputes are reduced): d near 0.0. State-centric authorities that prefer status-based restrictions are payers losing legal authority: d toward 1.0. National liberation movements are dual: beneficiary for their combatants' protection, payer for their obligation to protect. The functional protection floor structure creates asymmetry: the benefit (protection) flows downward to powerless detainees regardless of institutional status; the cost (compliance burden) flows to all detaining authorities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (what protections apply to disputed-status detainees) remains live and empirically documented in contemporary non-international armed conflicts. The constraint does not exhibit mandatrophy: its mandate has not outlived its function. The slight rise in theater ratio (0.12 to 0.19 then stabilization) reflects increasing status-determination performativity (authorities creating elaborate classification procedures to appear to comply while delaying actual protections)—not the constraint's mandate atrophying, but rather resistance adapting to the constraint by performing compliance. This is a sign of the constraint's strength, not decay: the fact that resistance must now adopt theater rather than openly deny protections indicates the functional floor's normative acceptance. No mandate shift detected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_determination_delay_mechanism,
    'When detaining authorities systematically delay formal status determination, does the functional protection floor prevent actual abuse, or does it create theater (formal protection on paper, systematic deprivation in practice)?',
    'Comparison of countries implementing immediate protections (presumed status-neutral) with those implementing protections after determination: measure actual torture, disappearance, and medical denial rates in both cohorts; interview released detainees about timing and substance of protections received.',
    'If theater-heavy, the constraint''s extractiveness may be higher than authored (authorities gain legal cover while delaying protections). If protections are substantially real, the constraint is functionally protective despite theater. This governs whether the constraint should be reclassified as partially Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_determination_delay_mechanism, empirical, 'Whether functional floor provisions result in actual protection or enable performative compliance while abuse continues.').

omega_variable(
    reciprocal_burden_on_non_state_actors,
    'Does the functional protection reading create sustainable reciprocal obligation, or does it impose asymmetric burden on non-state armed groups (who lack detention infrastructure) compared to state detaining authorities?',
    'Measure capacity and compliance for detention of captured fighters by state vs. non-state actors; assess whether non-state groups treated as obligated but incapable (and thus systematically violating) compared to state compliance rates.',
    'If asymmetric burden, the constraint may operationalize as coercive for non-state actors while discretionary for state authorities—reclassifying from Rope toward Snare for the non-state actor seat. If symmetric burden, the constraint remains coordinative for all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocal_burden_on_non_state_actors, empirical, 'Whether Common Article 3 obligations impose equal practical burden across state and non-state detaining actors.').

omega_variable(
    universal_floor_vs_special_status_bargaining,
    'Does anchoring protections to a universal floor reduce bargaining over status (functional reading advantage), or does it entrench status-determination dispute by removing negotiation leverage from non-state actors seeking POW recognition?',
    'Analyze historical cases where non-state groups sought formal combatant/POW status: pre- and post-1977 AP I patterns showing whether universal floor reduced or intensified status-recognition demands.',
    'If floor reduces bargaining leverage for non-state actors, the constraint may be functioning as de facto exclusion (denying them the higher protections of recognized combatant status while providing universal baseline). If bargaining is unchanged, the floor is non-interfering with status disputes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_floor_vs_special_status_bargaining, conceptual, 'Whether universal protection floor reduces or reshapes political economy of status determination.').

omega_variable(
    reading_foreclosure_ambiguity,
    'Does the functional protection floor logically foreclose the STATE_CENTRIC_READING (which denies protections to non-state actors), or do the two readings coexist with different parties holding each, neither foreclosing the other within their own frameworks?',
    'Examine whether a state accepting this reading''s premise (universal protections apply immediately) must logically reject the state-centric reading''s core (status determines protections). If a state could theoretically hold both (protections apply universally AND only state combatants qualify), they coexist; if holding both is contradictory, foreclosure is structural.',
    'Foreclosure would classify this reading as having fundamental logical incompatibility with the state-centric frame; coexistence means the readings divide along party lines (different states adopt each) without foreclosure. This governs the cs_structure.reading_relations entry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Whether the functional protection axiom logically forecloses state-centric status restriction or permits coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t0, combatant_status_definition__functional_protection_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(comb_tr_t0, observed).
narrative_ontology:measurement(comb_tr_t5, combatant_status_definition__functional_protection_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(comb_tr_t5, observed).
narrative_ontology:measurement(comb_tr_t10, combatant_status_definition__functional_protection_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(comb_tr_t10, observed).
narrative_ontology:measurement(comb_tr_t15, combatant_status_definition__functional_protection_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement_basis(comb_tr_t15, observed).
narrative_ontology:measurement(comb_tr_t20, combatant_status_definition__functional_protection_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(comb_tr_t20, observed).
narrative_ontology:measurement(comb_tr_t25, combatant_status_definition__functional_protection_reading, theater_ratio, 25, 0.19).
narrative_ontology:measurement_basis(comb_tr_t25, observed).
narrative_ontology:measurement(comb_tr_t30, combatant_status_definition__functional_protection_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement_basis(comb_tr_t30, observed).
narrative_ontology:measurement(comb_tr_t35, combatant_status_definition__functional_protection_reading, theater_ratio, 35, 0.18).
narrative_ontology:measurement_basis(comb_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(comb_be_t0, combatant_status_definition__functional_protection_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(comb_be_t0, observed).
narrative_ontology:measurement(comb_be_t5, combatant_status_definition__functional_protection_reading, base_extractiveness, 5, 0.11).
narrative_ontology:measurement_basis(comb_be_t5, observed).
narrative_ontology:measurement(comb_be_t10, combatant_status_definition__functional_protection_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement_basis(comb_be_t10, observed).
narrative_ontology:measurement(comb_be_t15, combatant_status_definition__functional_protection_reading, base_extractiveness, 15, 0.14).
narrative_ontology:measurement_basis(comb_be_t15, observed).
narrative_ontology:measurement(comb_be_t20, combatant_status_definition__functional_protection_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(comb_be_t20, observed).
narrative_ontology:measurement(comb_be_t25, combatant_status_definition__functional_protection_reading, base_extractiveness, 25, 0.16).
narrative_ontology:measurement_basis(comb_be_t25, observed).
narrative_ontology:measurement(comb_be_t30, combatant_status_definition__functional_protection_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement_basis(comb_be_t30, observed).
narrative_ontology:measurement(comb_be_t35, combatant_status_definition__functional_protection_reading, base_extractiveness, 35, 0.15).
narrative_ontology:measurement_basis(comb_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t0, combatant_status_definition__functional_protection_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(comb_su_t0, observed).
narrative_ontology:measurement(comb_su_t5, combatant_status_definition__functional_protection_reading, suppression_requirement, 5, 0.2).
narrative_ontology:measurement_basis(comb_su_t5, observed).
narrative_ontology:measurement(comb_su_t10, combatant_status_definition__functional_protection_reading, suppression_requirement, 10, 0.21).
narrative_ontology:measurement_basis(comb_su_t10, observed).
narrative_ontology:measurement(comb_su_t15, combatant_status_definition__functional_protection_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement_basis(comb_su_t15, observed).
narrative_ontology:measurement(comb_su_t20, combatant_status_definition__functional_protection_reading, suppression_requirement, 20, 0.23).
narrative_ontology:measurement_basis(comb_su_t20, observed).
narrative_ontology:measurement(comb_su_t25, combatant_status_definition__functional_protection_reading, suppression_requirement, 25, 0.23).
narrative_ontology:measurement_basis(comb_su_t25, observed).
narrative_ontology:measurement(comb_su_t30, combatant_status_definition__functional_protection_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(comb_su_t30, observed).
narrative_ontology:measurement(comb_su_t35, combatant_status_definition__functional_protection_reading, suppression_requirement, 35, 0.22).
narrative_ontology:measurement_basis(comb_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__functional_protection_reading, 0.1).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).

% DUAL FORMULATION NOTE:
% The combatant_status_definition kernel has three constraint readings: functional_protection_reading (this constraint—universal floor independent of status), state_centric_reading (status restricted to formal state military), and national_liberation_reading (status extended to organized non-state groups under AP I Article 1(4)). All three are readings of the same kernel; each instantiates a distinct constraint with different ε, beneficiary/victim structure, and type. The functional reading creates a universal floor that influences both siblings: neither can operate below Common Article 3 minimum even if they restrict or expand status. Link with affects_constraints shows structural interdependence without subsuming one reading into another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
