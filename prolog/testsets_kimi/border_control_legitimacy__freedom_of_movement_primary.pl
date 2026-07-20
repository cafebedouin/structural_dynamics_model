% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Freedom of Movement as Primary Limit on Border Closure Authority
 *   domain: political/international_law/migration
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_of_movement_primary
 *   reading of the border_control_legitimacy kernel. The constraint is the
 *   international legal norm that freedom of movement constitutes a
 *   fundamental human right and that territorial sovereignty does not entail
 *   authority to close borders. Under this reading, displaced citizens and
 *   workers are victimized by the categorical exclusion mechanisms that
 *   persist despite the norm; the border enforcement apparatus is
 *   delegitimized as an illegitimate exercise of state power; and state
 *   authority is structurally limited to jurisdictional regulation of the
 *   rights and obligations of those already present. The constraint operates
 *   as a tangled rope: it genuinely coordinates refugee protection through
 *   formal legal categories, but asymmetrically extracts from displaced
 *   workers who fall outside those categories, subjecting them to precarity,
 *   detention, deportation, and labor exploitation.
 *
 * KEY AGENTS:
 *   - displaced_citizens_workers (payer/powerless/trapped): Bear the costs of categorical exclusion and border closure despite the nominal human right to movement.
 *   - recognized_asylum_seekers (beneficiary/moderate/constrained): Receive protection and legal status through the regime's coordination function.
 *   - international_human_rights_regime (agenda_setter/institutional/analytical): Sets and administers the normative framework, interpreting treaties and monitoring state compliance.
 *   - destination_states (payer/institutional/constrained): Lose absolute border closure authority and must invest in jurisdictional regulation and asylum processing.
 *   - border_enforcement_apparatus (payer/organized/constrained): Bears the cost of delegitimization as their closure function is stripped of normative authority under this reading.
 *   - human_rights_ngo_monitor (observer/organized/mobile): Documents violations and advocates for compliance without collecting from the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.62).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.55).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Freedom of Movement as Primary Limit on Border Closure Authority").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, 'b6ddbc79-52ce-4ca2-aeb5-a592bef5b187').
narrative_ontology:cs_kernel_codification('b6ddbc79-52ce-4ca2-aeb5-a592bef5b187', formalized).
narrative_ontology:cs_authority_grounding('b6ddbc79-52ce-4ca2-aeb5-a592bef5b187', lineage).
narrative_ontology:cs_interpretation_layer_present('b6ddbc79-52ce-4ca2-aeb5-a592bef5b187').
narrative_ontology:cs_reading_relation('b6ddbc79-52ce-4ca2-aeb5-a592bef5b187', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('b6ddbc79-52ce-4ca2-aeb5-a592bef5b187', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('b6ddbc79-52ce-4ca2-aeb5-a592bef5b187', foundational, freedom_of_movement_fundamental_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_fundamental_human_right, holdable).
narrative_ontology:cs_axiom_grounding('b6ddbc79-52ce-4ca2-aeb5-a592bef5b187', freedom_of_movement_fundamental_human_right, deontological).
narrative_ontology:cs_axiom('b6ddbc79-52ce-4ca2-aeb5-a592bef5b187', foundational, territorial_sovereignty_excludes_closure).
narrative_ontology:cs_axiom_status(territorial_sovereignty_excludes_closure, holdable).
narrative_ontology:cs_axiom_grounding('b6ddbc79-52ce-4ca2-aeb5-a592bef5b187', territorial_sovereignty_excludes_closure, conventional).
narrative_ontology:cs_reference_frame('b6ddbc79-52ce-4ca2-aeb5-a592bef5b187', universal_human_rights_framework).
narrative_ontology:cs_drift_state('b6ddbc79-52ce-4ca2-aeb5-a592bef5b187', contemporary_border_regime_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b6ddbc79-52ce-4ca2-aeb5-a592bef5b187', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, recognized_asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, displaced_citizens_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, international_human_rights_regime).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, destination_states).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek cross-border mobility for economic survival or displacement from conflict, climate, or state failure. Confronted by visa regimes, border militarization, detention, and deportation practices that obstruct movement. Even when they reach destination states, they often lack legal status, labor protections, or access to basic services, and are channeled into precarious, exploitable work. Their exit from this constraint is structurally blocked by citizenship-based territorial closure.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, displaced_citizens_workers, payer,
    powerless, immediate, trapped, global).

% Fall within the protection categories established by the international refugee regime. Receive legal recognition, temporary or permanent residence, and access to rights protections as a result of the constraint's categorical coordination function. Their situation depends on successfully navigating the narrow boundaries of the regime and surviving the journey to a jurisdiction that honors the norm.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, recognized_asylum_seekers, beneficiary,
    moderate, biographical, constrained, national).

% Comprises UN treaty bodies, regional human rights courts, and special rapporteurs that interpret freedom of movement as a binding human right, monitor state compliance, adjudicate individual complaints, and elaborate the normative standard that limits state closure authority. Gains institutional mandate, funding, and legitimacy from the continued elaboration and defense of this framework.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, international_human_rights_regime, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__freedom_of_movement_primary, international_human_rights_regime, beneficiary).

% Lose the authority to close borders absolutely under this normative framework. Must invest in jurisdictional regulationâdetermining rights, obligations, and integration pathways for those presentârather than exercising exclusion. Bear the fiscal and political costs of asylum processing, compliance monitoring, and the domestic backlash from sovereigntist opposition.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, destination_states, payer,
    institutional, generational, constrained, national).

% Comprises immigration police, border guards, coast guards, and deportation agencies whose traditional authority derives from territorial closure. Under the freedom-of-movement-primary reading, their core exclusionary function is delegitimized; they bear the cost of lost moral authority, legal challenges, budget reallocations, and potential dismantling as the norm shifts toward open movement and jurisdictional regulation.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_apparatus, payer,
    organized, biographical, constrained, national).

% Document violations of freedom of movement, litigate on behalf of migrants, and report to international bodies. Do not collect from the constraint but analyze its operation, expose gaps between norm and practice, and advocate for compliance.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, human_rights_ngo_monitor, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal legal framework that distinguishes refugees deserving of protection from other migrants, creates standardized procedures for asylum adjudication, and establishes minimum rights for lawfully present non-citizens, replacing unilateral state violence with multilateral legal process.
% TRANSFER_FUNCTION: Moves authority over entry and status determination from absolute state discretion to an international legal framework; moves risk, precarity, and rightlessness onto displaced workers who fall outside the narrow protection categories.
% ABSENT_VOICES: Unrecognized economic migrants, climate-displaced persons, and open-border advocates are structurally excluded from the legitimating conversation; states asserting absolute sovereignty are present in the discourse but delegitimized under this reading's framework.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, the international refugee protection system would collapse, non-refoulement obligations would dissolve, states would revert to pure discretion over entry and exclusion, and millions would lose existing legal protections while others might gain de facto freedom of movementâglobal migration governance would reorganize around bilateral coercion rather than multilateral rights.
% FOUNDING_PROBLEM: Mass displacement and statelessness after WWII required an international framework to protect refugees from return to persecution and to prevent a recurrence of the pre-war exclusion regime, without giving states absolute discretion to exclude.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the 1951 Refugee Convention negotiations corroborate the founding problem. Contemporary UNHCR and human rights NGOs attest the problem remains live. However, critical migration scholars and several Global South states argue the framework has been repurposed for Northern labor-market control and that the founding solution no longer matches current displacement drivers (climate, economic).
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__freedom_of_movement_primary, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the categorical structure of the refugee/migration regime creates a large population of rightless displaced workers who are excluded or precarized. Suppression (0.55) reflects the legal suppression of absolute state sovereignty alternatives and the continued reliance on state coercion to maintain categorical boundaries. Theater ratio (0.45) captures the growing gap between human rights discourse and the reality of border militarization and deportation. Resistance (0.70) is high because destination states and enforcement agencies vigorously contest the limitation on their closure authority. Accessibility collapse (0.45) is moderate: alternatives such as open labor mobility or purely bilateral migration management remain thinkable but are marginalized by the dominance of the human rights frame. The measurement series tracks the post-WWII elaboration of the regime, showing rising extraction and theater as the normative framework expanded without proportional enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the international human rights regime seat, the constraint is coordination: it rescues refugees from persecution and replaces war with law. From the displaced worker seat, the same framework is extractive: it creates the legal category of 'economic migrant' that justifies their exclusion and exploitability. From the destination state seat, it is a loss of sovereign authorityâa payer position. The engine computes this divergence from the structural data: same constraint, radically different directionality depending on whether the agent is protected by the categorical boundary or trapped outside it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (recognized asylum seekers, international human rights institutions) sit near the beneficiary end of directionality: the constraint subsidizes their legal standing and organizational mandate. Victims (displaced citizens/workers) sit near the full-target end: they bear the costs of the categorical exclusion the constraint operationalizes. Destination states and border enforcement apparatus sit in the upper mid-range: they lose closure authority and legitimacy, but retain sufficient power to resist and partially subvert the constraint. No directionality overrides are needed because the structural derivation from beneficiary/victim declarations plus exit options captures these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâprotecting refugees after WWIIâis partially solved but partially repurposed. The regime still protects some refugees, preventing a snare classification, but the categorical machinery has been extended to manage labor migration in ways that victimize displaced workers. The R5 genealogy (founding_problem_status: contested) flags this: the arrangement persists beyond its original function, but not so far beyond that the coordination function has atrophied into pure theater. This prevents mislabeling the constraint as a rope (which would ignore the asymmetric extraction) or as a snare (which would ignore the genuine refugee protection function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_exclusion_intent,
    'Does the international refugee/migration regime''s categorical distinction between refugees and economic migrants intentionally create a pool of rightless labor, or is the exclusion of displaced workers an enforcement failure in an otherwise protective framework?',
    'Historical analysis of treaty negotiation records and labor-market outcomes in destination states with high migrant flows; correlation between restrictive visa regimes and informal labor exploitation rates.',
    'If intentional, the constraint is a snare using coordination as cover; if a failure, it is a tangled rope with a genuine coordination function undermined by incomplete enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_exclusion_intent, empirical, 'Whether displaced worker exclusion is designed extraction or enforcement gap.').

omega_variable(
    state_compliance_internalization,
    'Has the freedom-of-movement norm been internalized by states as a genuine legal constraint, or do states engage in theatrical compliance (reporting, ritual condemnation) while operationalizing border closure?',
    'Cross-national compliance data: compare treaty ratification rates with actual border openness indices, deportation rates, and legal status regularization pathways.',
    'If internalized, resistance should decline over time and the constraint stabilizes as rope; if theatrical, theater_ratio rises and the constraint computes toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_compliance_internalization, empirical, 'Whether state adherence to the norm is genuine or performative.').

omega_variable(
    reading_stability,
    'This constraint instantiates the freedom_of_movement_primary reading of the border_control_legitimacy kernel. Would adopting the sovereignty_primary reading reclassify the displaced worker population from victims to excluded non-parties, and would the enforcement apparatus shift from payer to beneficiary?',
    'Generate the sibling constraint stories (sovereignty_primary and jurisdictional_sovereignty) and compare their computed seat classifications and directionality profiles.',
    'The kernel decomposition validates whether the victim/beneficiary structure is reading-dependent or stable across readings; if reading-dependent, the classification is irreducibly perspectival.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_stability, conceptual, 'Sensitivity of agent roles to kernel reading selection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bord_tr_t15, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 15, 0.22).
narrative_ontology:measurement(bord_tr_t30, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 30, 0.3).
narrative_ontology:measurement(bord_tr_t45, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 45, 0.38).
narrative_ontology:measurement(bord_tr_t60, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 60, 0.48).
narrative_ontology:measurement(bord_tr_t75, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 75, 0.45).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bord_be_t15, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(bord_be_t30, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(bord_be_t45, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(bord_be_t60, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(bord_be_t75, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(bord_su_t15, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(bord_su_t30, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(bord_su_t45, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(bord_su_t60, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(bord_su_t75, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the border_control_legitimacy kernel. It is decomposed from the colloquial concept of border sovereignty into three structurally distinct claims: freedom_of_movement_primary (this file), sovereignty_primary, and jurisdictional_sovereignty. Each has distinct epsilon values, victim/beneficiary structures, and axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
