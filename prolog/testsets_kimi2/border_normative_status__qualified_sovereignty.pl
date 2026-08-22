% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Qualified Sovereignty: Proportionate Border Control with Human Rights Obligations
 *   domain: political/international_law/migration
 *
 * SUMMARY:
 *   This constraint story instantiates the qualified_sovereignty reading of
 *   the border_normative_status kernel. Under this reading, states retain a
 *   conditional authority to control borders that is legitimate only when
 *   exercised proportionately to legitimate interests and consistently with
 *   human rights obligations. The constraint structures contemporary
 *   international migration law, creating a framework where exclusion is
 *   permissible but bounded. The expected structural delta places both
 *   excluded migrants and displaced citizens in the victim set, acknowledges
 *   the adjudication burden on states, and treats the coordination function
 *   (orderly international border management) as real but entangled with
 *   asymmetric extraction. The authored metrics and claimed type are
 *   independent: the claim is tangled_rope because the coordination function
 *   is structurally genuine, while the metrics describe an increasingly
 *   extractive and theatrical operation in practice.
 *
 * KEY AGENTS:
 *   - Destination governments (agenda_setter/beneficiary): exercise border authority, bear adjudication costs, collect sovereign legitimacy and control.
 *   - Excluded migrants (payer): bear detention, deportation, and denial under proportionality tests; structurally powerless with trapped exit.
 *   - Displaced citizens (payer): caught in legal gaps between sovereignty and protection; statelessness and climate displacement amplify extraction.
 *   - Human rights courts (observer): interpret and adjudicate compliance; lack enforcement power; analytical seat with global scope.
 *   - Pro-migrant advocacy NGOs (excluded): document abuses and provide legal aid; excluded from executive treaty-making forums.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.72).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.7).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.72).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Qualified Sovereignty: Proportionate Border Control with Human Rights Obligations").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, 'b7ee8972-d224-46ed-8c39-f1baed4e9715').
narrative_ontology:cs_kernel_codification('b7ee8972-d224-46ed-8c39-f1baed4e9715', formalized).
narrative_ontology:cs_authority_grounding('b7ee8972-d224-46ed-8c39-f1baed4e9715', lineage).
narrative_ontology:cs_interpretation_layer_present('b7ee8972-d224-46ed-8c39-f1baed4e9715').
narrative_ontology:cs_reading_relation('b7ee8972-d224-46ed-8c39-f1baed4e9715', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('b7ee8972-d224-46ed-8c39-f1baed4e9715', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_axiom('b7ee8972-d224-46ed-8c39-f1baed4e9715', foundational, state_authority_conditional_on_human_rights).
narrative_ontology:cs_axiom_status(state_authority_conditional_on_human_rights, holdable).
narrative_ontology:cs_axiom_grounding('b7ee8972-d224-46ed-8c39-f1baed4e9715', state_authority_conditional_on_human_rights, conventional).
narrative_ontology:cs_axiom('b7ee8972-d224-46ed-8c39-f1baed4e9715', secondary, proportionality_as_binding_adjudication_standard).
narrative_ontology:cs_axiom_status(proportionality_as_binding_adjudication_standard, holdable).
narrative_ontology:cs_axiom_grounding('b7ee8972-d224-46ed-8c39-f1baed4e9715', proportionality_as_binding_adjudication_standard, conventional).
narrative_ontology:cs_reference_frame('b7ee8972-d224-46ed-8c39-f1baed4e9715', sovereignty_rights_synthesis).
narrative_ontology:cs_drift_state('b7ee8972-d224-46ed-8c39-f1baed4e9715', contemporary_migration_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b7ee8972-d224-46ed-8c39-f1baed4e9715', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, destination_governments).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_citizens).
narrative_ontology:constraint_vindicates(border_normative_status__qualified_sovereignty, conditional_state_sovereignty).
narrative_ontology:constraint_vindicates(border_normative_status__qualified_sovereignty, proportionality_as_legitimacy_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise border control authority under international human rights law; set visa policies, detention criteria, and deportation procedures; bear adjudication costs and compliance monitoring; gain legitimacy and sovereign control over territory and population.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, destination_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, destination_governments, beneficiary).

% Seek entry or asylum but are denied under proportionality and legitimate interest tests; bear detention, deportation, family separation, and precarity; excluded from the forums where border norms are authored.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Include stateless persons and those displaced by conflict or climate; caught in legal gaps where sovereignty claims override protection needs; face prolonged displacement because destination states interpret proportionality narrowly.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_citizens, payer,
    powerless, immediate, trapped, global).

% Adjudicate individual complaints against state border practices; interpret proportionality and non-refoulement; lack direct enforcement power and rely on state compliance; their rulings create precedent that constrains or enables state action.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, human_rights_courts, observer,
    institutional, generational, analytical, global).

% Document pushbacks, provide legal aid, and campaign for less restrictive interpretations; structurally excluded from state-to-state treaty negotiations and bilateral migration deals; their courtroom presence is mediated and contingent on state admission.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, pro_migrant_advocacy_ngos, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__qualified_sovereignty, destination_governments).
narrative_ontology:fixing_cost_class(border_normative_status__qualified_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the coexistence of state territorial sovereignty and individual human rights by establishing a shared legal framework for border management, asylum adjudication, and non-refoulement across states.
% TRANSFER_FUNCTION: Transfers the direct costs of exclusion, detention, and displacement from states to excluded migrants and displaced citizens; transfers legitimacy and sovereign authority to destination governments; transfers adjudication and monitoring burdens to states and international bodies.
% ABSENT_VOICES: Excluded migrants and displaced persons are structurally absent from treaty-making and proportionality-determination processes; pro-migrant advocacy NGOs are present in courts but excluded from executive and inter-state bargaining forums where the constraint is actually authored.
% DISAPPEARANCE_RATIONALE: If the qualified sovereignty framework vanished, states would lose the legal vocabulary that legitimizes border exclusion; the vacuum would be filled either by unqualified sovereignty claims or by freedom-of-movement expansion, and the current distribution of authority, displacement, and protection would collapse and reorganize.
% FOUNDING_PROBLEM: How to reconcile the post-WWII state system of territorial sovereignty with the protection of individuals fleeing persecution and poverty, without either dissolving borders or permitting unfettered state cruelty.
% FOUNDING_PROBLEM_CORROBORATION: International lawyers and human rights institutions attest to the problem's persistence; critics from migration studies and global South governments attest that the 'problem' was framed by wealthy destination states to manage mobility on their terms. Corroboration is split along North-South and beneficiary-payer lines, with no neutral outside party commanding consensus.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the proportionality framework, while nominally protective, is interpreted by states to legitimate extensive exclusion, detention, and externalization. Suppression (0.70) reflects the active coercion required to maintain border control and the suppression of open-border alternatives. Theater ratio (0.55) captures the growing gap between rights rhetoric and pushback practice. Accessibility collapse (0.80) registers that, once the qualified-sovereignty frame is accepted, open borders become unthinkable in mainstream legal discourse. Resistance (0.60) comes from migrant movements, some states resisting human rights oversight, and civil society litigation. The measurement series share a single time grid (1990â2025) to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The destination-government seat experiences the constraint as a legitimate coordination mechanism that preserves order while imposing annoying legal burdens. The excluded-migrant and displaced-citizen seats experience it as a legitimizing structure for violence and immobility. The engine computes this divergence from the same structural data: low directionality for the institutional beneficiary, high directionality for the powerless trapped targets. The human-rights-court seat sits near analytical neutrality but leans toward coordination because its institutional existence depends on the framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Destination governments are declared beneficiaries because they collect sovereign authority and legitimated exclusion power; their directionality is toward the beneficiary end, reducing effective extraction. Excluded migrants and displaced citizens are declared victims (payers) because they bear the direct costs of exclusion and displacement; their directionality is toward the full-target end, amplifying effective extraction. The asymmetry is structural: the same legal norm that empowers states immobilizes migrants.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling in both directions. Against a rope reading, the declared victims and high extractiveness mark the asymmetric extraction that pure coordination cannot explain. Against a snare reading, the genuine coordination function (a shared legal framework preventing unfettered cruelty and interstate conflict over migration) and the real adjudication burden on states prevent reducing the structure to pure extraction. The theater ratio (0.55) signals that some of the coordination function has atrophied into performance, but not entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_as_mask,
    'Is the proportionality requirement a genuine limit on state power or a legitimizing theater for exclusionary outcomes?',
    'Comparative case-law analysis: if proportionality review systematically defers to state security or economic claims while rarely granting protection, the limit is theatrical; if it regularly overrides state decisions, it is substantive.',
    'If theatrical, theater_ratio is higher than base metrics suggest and the constraint drifts toward snare; if substantive, the coordination function is stronger and tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_as_mask, conceptual, 'Whether proportionality review is substantive or performative legitimacy').

omega_variable(
    human_rights_as_constraint_or_cover,
    'Do human rights obligations substantively constrain state border control, or do they provide a vocabulary for states to justify the same exclusions?',
    'Empirical tracking of state practice pre- and post-human-rights-treaty ratification: if exclusion rates and mortality at borders remain constant or rise after ratification, rights obligations function as cover.',
    'If cover, effective extraction is higher than structural measures indicate because the constraint pacifies resistance by framing exclusion as rights-compliant; if genuine constraint, extraction is partially offset by protection gains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_rights_as_constraint_or_cover, empirical, 'Whether human rights framing constrains or legitimizes exclusion').

omega_variable(
    adjudication_burden_real_cost,
    'Is the adjudication burden on states a meaningful cost that balances extraction, or is it absorbed through procedural delay and under-resourced tribunals?',
    'Budget and staffing analysis of migration tribunals and court systems relative to caseload; outcome data on asylum grant rates and processing times.',
    'If the burden is illusory, the constraint''s extraction is more asymmetric than it appears; if real, it constitutes a genuine cost to the beneficiary seat that partially offsets the coordination asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjudication_burden_real_cost, empirical, 'Whether state adjudication costs are material or illusory').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bnqs_tr_t0, border_normative_status__qualified_sovereignty, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bnqs_tr_t7, border_normative_status__qualified_sovereignty, theater_ratio, 7, 0.3).
narrative_ontology:measurement(bnqs_tr_t14, border_normative_status__qualified_sovereignty, theater_ratio, 14, 0.35).
narrative_ontology:measurement(bnqs_tr_t21, border_normative_status__qualified_sovereignty, theater_ratio, 21, 0.42).
narrative_ontology:measurement(bnqs_tr_t28, border_normative_status__qualified_sovereignty, theater_ratio, 28, 0.48).
narrative_ontology:measurement(bnqs_tr_t35, border_normative_status__qualified_sovereignty, theater_ratio, 35, 0.55).

% Extraction over time
narrative_ontology:measurement(bnqs_be_t0, border_normative_status__qualified_sovereignty, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bnqs_be_t7, border_normative_status__qualified_sovereignty, base_extractiveness, 7, 0.58).
narrative_ontology:measurement(bnqs_be_t14, border_normative_status__qualified_sovereignty, base_extractiveness, 14, 0.62).
narrative_ontology:measurement(bnqs_be_t21, border_normative_status__qualified_sovereignty, base_extractiveness, 21, 0.66).
narrative_ontology:measurement(bnqs_be_t28, border_normative_status__qualified_sovereignty, base_extractiveness, 28, 0.7).
narrative_ontology:measurement(bnqs_be_t35, border_normative_status__qualified_sovereignty, base_extractiveness, 35, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(bnqs_su_t0, border_normative_status__qualified_sovereignty, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bnqs_su_t7, border_normative_status__qualified_sovereignty, suppression_requirement, 7, 0.55).
narrative_ontology:measurement(bnqs_su_t14, border_normative_status__qualified_sovereignty, suppression_requirement, 14, 0.62).
narrative_ontology:measurement(bnqs_su_t21, border_normative_status__qualified_sovereignty, suppression_requirement, 21, 0.68).
narrative_ontology:measurement(bnqs_su_t28, border_normative_status__qualified_sovereignty, suppression_requirement, 28, 0.74).
narrative_ontology:measurement(bnqs_su_t35, border_normative_status__qualified_sovereignty, suppression_requirement, 35, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__freedom_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the border_normative_status kernel, decomposed per the Îµ-invariance principle because each reading carries a distinct beneficiary/victim structure and normative grounding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
