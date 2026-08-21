% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement as Integration Imperative
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'integration_reading' of the
 *   'federation_membership_kernel', focusing on the expansive interpretation
 *   of free movement as a fundamental right constitutive of EU citizenship
 *   and single market completion. Supranational authority, particularly the
 *   ECJ, interprets its scope broadly to maximize labor mobility and equal
 *   treatment, often overriding national labor market protections and
 *   imposing costs on receiving state welfare systems without direct fiscal
 *   compensation. The claimed type is 'tangled_rope' because it genuinely
 *   coordinates a single market but does so with significant asymmetric
 *   extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.78).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.85).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement as Integration Imperative").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, 'ca68e1ac-9259-4435-a5ad-62bf968104db').
narrative_ontology:cs_kernel_codification('ca68e1ac-9259-4435-a5ad-62bf968104db', formalized).
narrative_ontology:cs_authority_grounding('ca68e1ac-9259-4435-a5ad-62bf968104db', lineage).
narrative_ontology:cs_interpretation_layer_present('ca68e1ac-9259-4435-a5ad-62bf968104db').
narrative_ontology:cs_reading_relation('ca68e1ac-9259-4435-a5ad-62bf968104db', federation_membership_kernel__member_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('ca68e1ac-9259-4435-a5ad-62bf968104db', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('ca68e1ac-9259-4435-a5ad-62bf968104db', foundational, free_movement_as_absolute_right).
narrative_ontology:cs_axiom_status(free_movement_as_absolute_right, holdable).
narrative_ontology:cs_axiom_grounding('ca68e1ac-9259-4435-a5ad-62bf968104db', free_movement_as_absolute_right, deontological).
narrative_ontology:cs_axiom('ca68e1ac-9259-4435-a5ad-62bf968104db', foundational, single_market_completion_imperative).
narrative_ontology:cs_axiom_status(single_market_completion_imperative, holdable).
narrative_ontology:cs_axiom_grounding('ca68e1ac-9259-4435-a5ad-62bf968104db', single_market_completion_imperative, instrumental).
narrative_ontology:cs_reference_frame('ca68e1ac-9259-4435-a5ad-62bf968104db', maastricht_treaty_era_integration_vision).
narrative_ontology:cs_drift_state('ca68e1ac-9259-4435-a5ad-62bf968104db', contemporary_ecj_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ca68e1ac-9259-4435-a5ad-62bf968104db', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, eu_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, sending_member_states).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_member_states_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, national_labor_market_regulators).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The European Commission and the European Court of Justice (ECJ) actively interpret and enforce free movement rights, expanding their scope to deepen EU integration and single market completion. They benefit from increased supranational authority and a more unified economic space.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, eu_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Individuals who exercise their right to live and work in any EU member state, benefiting from expanded opportunities, equal treatment, and access to social benefits in their host country. Their mobility is directly enabled and protected by this expansive interpretation.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Member states from which citizens emigrate often benefit from reduced unemployment pressures, remittances sent home by their citizens, and the externalization of costs associated with educating and training labor that then contributes to other economies.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_member_states, beneficiary,
    institutional, generational, constrained, national).

% National welfare systems in destination countries bear the costs of providing social benefits, healthcare, and education to mobile EU citizens, often without direct fiscal compensation from the EU budget or sending states. This can strain public services and budgets.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_member_states_welfare_systems, payer,
    institutional, biographical, constrained, national).

% Workers in receiving member states who face increased competition for jobs, downward pressure on wages, or displacement in specific sectors due to the influx of mobile EU citizens. Their options for exit or resistance are often limited.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_labor, payer,
    powerless, immediate, trapped, local).

% National authorities responsible for labor market protection and social policy find their regulatory autonomy constrained and often overridden by ECJ rulings that prioritize free movement and equal treatment, limiting their ability to implement specific national policies.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, national_labor_market_regulators, payer,
    institutional, biographical, constrained, national).

% Overall national governments in receiving states bear the political and fiscal costs of managing the impacts of free movement, including public services strain and domestic political resistance, while having limited legal avenues to restrict it.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, member_state_governments, payer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates a single market for labor and services across the EU, preventing national barriers from fragmenting economic activity and ensuring a common standard of rights for EU citizens.
% TRANSFER_FUNCTION: Transfers labor, skills, and sometimes welfare costs from sending to receiving states; transfers significant policy authority from national governments to supranational EU institutions (ECJ, Commission) regarding migration and social rights.
% ABSENT_VOICES: National populations in receiving states concerned about welfare strain, public service capacity, or labor displacement often feel their voices are unheard or dismissed as anti-EU, as the legal framework prioritizes integration over local concerns.
% DISAPPEARANCE_RATIONALE: If the expansive interpretation of free movement vanished overnight, the single market for labor would fragment, national borders would immediately re-emerge as significant barriers, and the EU's foundational principle of integration would be fundamentally undermined, leading to a profound reorganization of economic and political relations within Europe.
% FOUNDING_PROBLEM: The original problem was to prevent future conflicts and foster economic prosperity in post-WWII Europe by removing internal barriers to trade and movement, creating a common market and eventually a political union.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and pro-integration academics attest that the founding problem of fragmentation and economic stagnation is still live, requiring continuous integration. Eurosceptic parties and some national economists argue the problem has evolved, and the current expansive interpretation creates new challenges (e.g., welfare tourism, brain drain) that were not part of the original mandate; legislative hearings and independent economic analyses from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and increasing (0.78 at end) because the expansive interpretation of free movement leads to uncompensated costs for receiving states and local labor, while benefiting mobile citizens and EU institutions. Suppression is also high (0.85) as national policy autonomy is consistently overridden by ECJ rulings, and member states have limited legal recourse to restrict free movement. Theater ratio is low (0.15) because the enforcement of free movement is highly functional and central to the EU project, not merely performative. Accessibility collapse is moderate-high (0.70) as national alternatives to EU-level free movement rules are largely foreclosed. Resistance is moderate (0.60) reflecting ongoing political and legal challenges from member states and national populations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of EU institutions, this expansive interpretation is a necessary and beneficial coordination mechanism for a functioning single market and deeper integration. From the perspective of receiving member states, their welfare systems, and local labor, the same structure operates as an extractive mechanism that imposes uncompensated costs and suppresses national policy choices. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions (ECJ, Commission) are clear beneficiaries, gaining authority and furthering integration. Mobile EU citizens are direct beneficiaries of expanded rights and opportunities. Sending member states benefit from reduced unemployment and remittances. Conversely, receiving member states' welfare systems, displaced local labor, and national labor market regulators are victims, bearing uncompensated costs and losing policy autonomy. Member state governments, overall, are payers, managing the political and fiscal consequences.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_fundamental_right,
    'Is free movement an absolute fundamental right, or is its scope legitimately bounded by national welfare state capacity and labor market protection?',
    'A political consensus among member states to amend EU treaties, or a landmark ECJ ruling that re-interprets the balance between free movement and national prerogatives.',
    'If bounded, the extractiveness on receiving states and local labor would decrease, and national suppression would lessen. If absolute, the current high extractiveness and suppression would be structurally justified by this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_fundamental_right, conceptual, 'Ambiguity regarding the absolute vs. bounded nature of free movement rights.').

omega_variable(
    fiscal_compensation_mechanism,
    'Would an EU-level fiscal transfer mechanism, compensating receiving states for welfare costs associated with free movement, alter the constraint''s classification?',
    'Implementation and evaluation of a robust EU-level fiscal compensation scheme, assessing its impact on national budgets and public services.',
    'If effective, such a mechanism would significantly reduce the extractiveness on receiving member states, potentially shifting the constraint closer to a ''rope'' by addressing the asymmetric cost burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_compensation_mechanism, empirical, 'Impact of potential fiscal compensation on the constraint''s extractive nature.').

omega_variable(
    impact_on_local_labor,
    'What is the precise, empirically verifiable economic impact of free movement on wages and employment for local labor in specific sectors and regions of receiving member states?',
    'Longitudinal, granular economic studies controlling for other variables (e.g., automation, global trade) to isolate the effect of free movement on local labor markets.',
    'Clear evidence of significant, widespread negative impact would strengthen the ''snare'' elements of the constraint for local labor, while evidence of neutral or positive impact would weaken it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_local_labor, empirical, 'Empirical uncertainty regarding the economic impact on local labor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 1992, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1992, federation_membership_kernel__integration_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(fede_tr_t1998, federation_membership_kernel__integration_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_kernel__integration_reading, theater_ratio, 2004, 0.13).
narrative_ontology:measurement(fede_tr_t2010, federation_membership_kernel__integration_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_kernel__integration_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement(fede_tr_t2022, federation_membership_kernel__integration_reading, theater_ratio, 2022, 0.15).

% Extraction over time
narrative_ontology:measurement(fede_be_t1992, federation_membership_kernel__integration_reading, base_extractiveness, 1992, 0.55).
narrative_ontology:measurement(fede_be_t1998, federation_membership_kernel__integration_reading, base_extractiveness, 1998, 0.62).
narrative_ontology:measurement(fede_be_t2004, federation_membership_kernel__integration_reading, base_extractiveness, 2004, 0.68).
narrative_ontology:measurement(fede_be_t2010, federation_membership_kernel__integration_reading, base_extractiveness, 2010, 0.73).
narrative_ontology:measurement(fede_be_t2016, federation_membership_kernel__integration_reading, base_extractiveness, 2016, 0.76).
narrative_ontology:measurement(fede_be_t2022, federation_membership_kernel__integration_reading, base_extractiveness, 2022, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1992, federation_membership_kernel__integration_reading, suppression_requirement, 1992, 0.6).
narrative_ontology:measurement(fede_su_t1998, federation_membership_kernel__integration_reading, suppression_requirement, 1998, 0.68).
narrative_ontology:measurement(fede_su_t2004, federation_membership_kernel__integration_reading, suppression_requirement, 2004, 0.75).
narrative_ontology:measurement(fede_su_t2010, federation_membership_kernel__integration_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(fede_su_t2016, federation_membership_kernel__integration_reading, suppression_requirement, 2016, 0.83).
narrative_ontology:measurement(fede_su_t2022, federation_membership_kernel__integration_reading, suppression_requirement, 2022, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, global_infrastructure).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, eu_single_market_regulation).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, eu_citizenship_rights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
