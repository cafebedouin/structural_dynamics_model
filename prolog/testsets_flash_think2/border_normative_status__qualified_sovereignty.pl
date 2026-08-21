% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Qualified State Sovereignty over Borders
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the international legal and ethical framework
 *   that acknowledges state sovereignty over borders but mandates its
 *   exercise be proportionate to legitimate interests and consistent with
 *   human rights. It is a reading of the 'border_normative_status' kernel,
 *   specifically the 'qualified_sovereignty' interpretation. The constraint
 *   attempts to coordinate state interests with universal human rights, but
 *   often results in asymmetric extraction from migrants due to states'
 *   active enforcement and occasional non-compliance with obligations. The
 *   metrics reflect a trend of increasing extractiveness and suppression as
 *   states face growing migration pressures, often paying lip service to
 *   human rights while implementing restrictive policies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.55).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.7).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.55).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Qualified State Sovereignty over Borders").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, '6c8b375b-df88-4cca-84e8-413100ab6d0c').
narrative_ontology:cs_kernel_codification('6c8b375b-df88-4cca-84e8-413100ab6d0c', formalized).
narrative_ontology:cs_authority_grounding('6c8b375b-df88-4cca-84e8-413100ab6d0c', lineage).
narrative_ontology:cs_interpretation_layer_present('6c8b375b-df88-4cca-84e8-413100ab6d0c').
narrative_ontology:cs_reading_relation('6c8b375b-df88-4cca-84e8-413100ab6d0c', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('6c8b375b-df88-4cca-84e8-413100ab6d0c', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_axiom('6c8b375b-df88-4cca-84e8-413100ab6d0c', foundational, state_sovereignty_is_qualified_by_human_rights).
narrative_ontology:cs_axiom_status(state_sovereignty_is_qualified_by_human_rights, holdable).
narrative_ontology:cs_axiom_grounding('6c8b375b-df88-4cca-84e8-413100ab6d0c', state_sovereignty_is_qualified_by_human_rights, conventional).
narrative_ontology:cs_axiom('6c8b375b-df88-4cca-84e8-413100ab6d0c', foundational, proportionality_and_necessity_test_applies_to_border_control).
narrative_ontology:cs_axiom_status(proportionality_and_necessity_test_applies_to_border_control, holdable).
narrative_ontology:cs_axiom_grounding('6c8b375b-df88-4cca-84e8-413100ab6d0c', proportionality_and_necessity_test_applies_to_border_control, conventional).
narrative_ontology:cs_reference_frame('6c8b375b-df88-4cca-84e8-413100ab6d0c', post_wwii_human_rights_regime).
narrative_ontology:cs_drift_state('6c8b375b-df88-4cca-84e8-413100ab6d0c', contemporary_migration_crises_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6c8b375b-df88-4cca-84e8-413100ab6d0c', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, states).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, citizens).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain primary authority over border control but are bound by international human rights law. They benefit from maintaining territorial integrity and national identity, but bear the burden of justifying border policies as necessary and proportionate.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, states, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the perceived security and stability of regulated borders, and the ability of their state to control entry. They may also bear indirect costs if border policies are inefficient or lead to human rights violations that damage international standing.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, citizens, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of border restrictions, including denied entry, detention, and dangerous journeys. Their movement is suppressed, and their human rights may be violated if state actions are disproportionate or inhumane.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, migrants, payer,
    powerless, immediate, trapped, global).

% Are particularly vulnerable, seeking protection from persecution or serious harm. They are identity-locked by their need for safety, and often face prolonged uncertainty, detention, and the risk of refoulement, even under qualified sovereignty norms.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, asylum_seekers, payer,
    powerless, immediate, identity_locked, global).

% Monitor state compliance with human rights obligations, document violations, and advocate for more humane and rights-respecting border policies. They bear the costs of sustained advocacy and legal challenges against state practices.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, human_rights_advocates, observer,
    organized, generational, analytical, global).

% Develop, interpret, and monitor the implementation of international human rights law relevant to borders. They exert normative pressure on states but often lack direct enforcement mechanisms, relying on state cooperation and public accountability.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, international_human_rights_bodies, agenda_setter,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__qualified_sovereignty, states).
narrative_ontology:fixing_cost_class(border_normative_status__qualified_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To reconcile the legitimate interests of states in controlling their borders with their international human rights obligations, providing a framework for proportionate and necessary border governance.
% TRANSFER_FUNCTION: Transfers a degree of security and stability to states and their citizens by allowing regulated entry, while transferring the burden of justification and the risk of denied entry or rights violations to migrants and asylum seekers.
% ABSENT_VOICES: Undocumented migrants and stateless persons are often excluded from formal legal and political processes, lacking a voice to challenge border policies directly. They would articulate the lived experience of disproportionate state action and the systemic failures of protection.
% DISAPPEARANCE_RATIONALE: If the norm of qualified sovereignty vanished, states would likely revert to a more absolute claim of sovereignty (increasing extraction and suppression), or, conversely, borders might become entirely open, leading to a rapid reorganization of global migration patterns and state functions. The current tension and legal framework would collapse.
% FOUNDING_PROBLEM: The post-WWII challenge of reconciling traditional state sovereignty over territory with the emerging international consensus on universal human rights, particularly in the context of displacement and migration.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, UN human rights committees, and human rights organizations consistently attest that this tension remains a central and unresolved challenge in international law and practice, with ongoing debates and violations.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.55, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it genuinely attempts to coordinate state interests (security, identity) with human rights, but this coordination is inherently asymmetric. States benefit from control, while migrants bear significant costs. Active enforcement is required both by states to control borders and by international bodies to monitor human rights compliance. The rising extractiveness and suppression over the interval reflect the increasing tension and states' tendency to prioritize control, often leading to a gap between declared obligations and actual practice, which manifests as a rising theater ratio.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states, this framework is a necessary balance that allows them to govern effectively while adhering to international norms. From the perspective of migrants and human rights advocates, it often functions as a cover for continued extraction and suppression, with states failing to meet their obligations. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   States and their citizens are beneficiaries, as the constraint legitimizes their control over borders while imposing limits. Migrants and asylum seekers are targets, bearing the direct costs of restricted movement and potential rights violations. Human rights advocates and international bodies act as observers and secondary agenda-setters, pushing for greater compliance and bearing the costs of advocacy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_adherence_ambiguity,
    'To what extent do states genuinely adhere to the principles of proportionality and necessity in their border control measures, versus using these principles as rhetorical cover for restrictive policies?',
    'Independent, granular audits of state border practices, including disaggregated data on detentions, asylum claim processing, and use of force, compared against international legal standards and expert testimony.',
    'If adherence is largely rhetorical, the constraint''s effective extractiveness and suppression are higher than currently measured, pushing it closer to a Snare. If adherence is genuine, the coordination function is stronger, reinforcing its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_adherence_ambiguity, empirical, 'Assessing the sincerity of state compliance with proportionality in border control.').

omega_variable(
    international_enforcement_efficacy,
    'How effective are international human rights bodies and legal mechanisms in compelling states to comply with their qualified sovereignty obligations?',
    'Analysis of state compliance rates with judgments from international courts (e.g., ECtHR) and recommendations from treaty bodies, correlated with changes in national legislation and border practices.',
    'If enforcement is weak, the constraint''s persistence relies more on state self-interest (extraction) than genuine coordination, potentially shifting its classification towards a Snare. Stronger enforcement would reinforce its Tangled Rope nature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_enforcement_efficacy, empirical, 'Measuring the actual impact of international human rights enforcement on state border practices.').

omega_variable(
    conceptual_framing_of_sovereignty,
    'Is the concept of ''qualified sovereignty'' a stable and coherent legal framework, or is it an inherently unstable compromise between conflicting normative claims?',
    'Conceptual analysis of legal scholarship and judicial decisions over time, examining whether the framework consistently resolves tensions or merely defers them. This is a conceptual omega, not empirical.',
    'If unstable, the framework is prone to ''drift'' towards either absolute sovereignty or greater freedom of movement, making its current classification as Tangled Rope a temporary state. If coherent, it represents a durable, albeit contested, coordination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_framing_of_sovereignty, conceptual, 'Conceptual stability of qualified sovereignty as a legal framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1990, border_normative_status__qualified_sovereignty, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(bord_tr_t1996, border_normative_status__qualified_sovereignty, theater_ratio, 1996, 0.25).
narrative_ontology:measurement(bord_tr_t2002, border_normative_status__qualified_sovereignty, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(bord_tr_t2008, border_normative_status__qualified_sovereignty, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(bord_tr_t2014, border_normative_status__qualified_sovereignty, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(bord_tr_t2020, border_normative_status__qualified_sovereignty, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(bord_be_t1990, border_normative_status__qualified_sovereignty, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(bord_be_t1996, border_normative_status__qualified_sovereignty, base_extractiveness, 1996, 0.48).
narrative_ontology:measurement(bord_be_t2002, border_normative_status__qualified_sovereignty, base_extractiveness, 2002, 0.5).
narrative_ontology:measurement(bord_be_t2008, border_normative_status__qualified_sovereignty, base_extractiveness, 2008, 0.52).
narrative_ontology:measurement(bord_be_t2014, border_normative_status__qualified_sovereignty, base_extractiveness, 2014, 0.54).
narrative_ontology:measurement(bord_be_t2020, border_normative_status__qualified_sovereignty, base_extractiveness, 2020, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1990, border_normative_status__qualified_sovereignty, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(bord_su_t1996, border_normative_status__qualified_sovereignty, suppression_requirement, 1996, 0.63).
narrative_ontology:measurement(bord_su_t2002, border_normative_status__qualified_sovereignty, suppression_requirement, 2002, 0.66).
narrative_ontology:measurement(bord_su_t2008, border_normative_status__qualified_sovereignty, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(bord_su_t2014, border_normative_status__qualified_sovereignty, suppression_requirement, 2014, 0.69).
narrative_ontology:measurement(bord_su_t2020, border_normative_status__qualified_sovereignty, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
