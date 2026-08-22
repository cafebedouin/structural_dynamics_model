% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Border Closure Authority (Freedom-of-Movement Reading)
 *   domain: political/international_law/migration
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   border_control_legitimacy. The freedom_of_movement_primary reading holds
 *   that freedom of movement is a fundamental human right and that
 *   territorial sovereignty does not entail border closure authority; state
 *   authority is legitimate only in jurisdictional regulation (who receives
 *   what rights once present), not in exclusion. From this seat, the standing
 *   arrangement of border closure regimes is structurally extractive: it
 *   displaces costs onto mobile populations while concentrating authority and
 *   economic rents in sovereign states and domestic constituencies. The
 *   sibling readings are sovereignty_primary (closure as absolute discretion)
 *   and jurisdictional_sovereignty (balancing closure with labor needs). This
 *   reading forecloses sovereignty_primary and coexists with
 *   jurisdictional_sovereignty as a live but competing framework.
 *
 * KEY AGENTS:
 *   - Sovereign states: Primary agenda-setter (institutional/constrained) â administer closure and collect legitimacy.
 *   - Border enforcement apparatus: Beneficiary (organized/constrained) â receives budget and mission from closure maintenance.
 *   - Domestic labor constituencies: Beneficiary (organized/constrained) â protected from labor competition.
 *   - Displaced citizens: Primary target (powerless/trapped) â barred from safe territory.
 *   - Migrant workers: Target (moderate/constrained) â forced into irregularity and debt.
 *   - Asylum seekers: Target (powerless/trapped) â blocked from protection procedures.
 *   - Human rights organizations: Observer (organized/analytical) â contest closure on rights grounds.
 *   - International law scholars: Observer (analytical/analytical) â reinterpret sovereignty as non-exclusionary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.82).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.85).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.82).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Border Closure Authority (Freedom-of-Movement Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, 'd2bce349-406b-4072-a694-489d0cbf749e').
narrative_ontology:cs_kernel_codification('d2bce349-406b-4072-a694-489d0cbf749e', formalized).
narrative_ontology:cs_authority_grounding('d2bce349-406b-4072-a694-489d0cbf749e', lineage).
narrative_ontology:cs_interpretation_layer_present('d2bce349-406b-4072-a694-489d0cbf749e').
narrative_ontology:cs_reading_relation('d2bce349-406b-4072-a694-489d0cbf749e', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('d2bce349-406b-4072-a694-489d0cbf749e', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('d2bce349-406b-4072-a694-489d0cbf749e', foundational, freedom_of_movement_fundamental_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('d2bce349-406b-4072-a694-489d0cbf749e', freedom_of_movement_fundamental_right, deontological).
narrative_ontology:cs_axiom('d2bce349-406b-4072-a694-489d0cbf749e', foundational, sovereignty_lacks_closure_authority).
narrative_ontology:cs_axiom_status(sovereignty_lacks_closure_authority, holdable).
narrative_ontology:cs_axiom_grounding('d2bce349-406b-4072-a694-489d0cbf749e', sovereignty_lacks_closure_authority, conventional).
narrative_ontology:cs_reference_frame('d2bce349-406b-4072-a694-489d0cbf749e', jurisdictional_presence_framework).
narrative_ontology:cs_drift_state('d2bce349-406b-4072-a694-489d0cbf749e', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d2bce349-406b-4072-a694-489d0cbf749e', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, sovereign_states).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, domestic_labor_constituencies).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_apparatus).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, displaced_citizens).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, migrant_workers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise border closure authority through visa regimes, passport controls, and deportation systems. Justify exclusion as constitutive of territorial sovereignty and public order. Collect political legitimacy and budgetary resources from the maintenance of the enforcement apparatus.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, sovereign_states, agenda_setter,
    institutional, generational, constrained, global).

% Staff immigration agencies, border guards, and detention facilities. Receive budgets, employment, and institutional mission from the maintenance of border closure. Do not set policy but depend structurally on its continuation.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_apparatus, beneficiary,
    organized, biographical, constrained, national).

% Workers in sectors that would face wage competition from expanded migration. Benefit indirectly from restricted labor supply and from political narratives that equate border closure with national welfare protection.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, domestic_labor_constituencies, beneficiary,
    organized, biographical, constrained, national).

% Stateless persons, refugees, and internally displaced persons who cannot return home and are barred from safe territories by border closure. Bear the costs of exclusion through precarity, camp detention, and lack of legal protection.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, displaced_citizens, payer,
    powerless, immediate, trapped, regional).

% Seek cross-border economic opportunity. Visa restrictions and closure regimes force reliance on dangerous irregular routes, debt to recruitment intermediaries, or exclusion from labor markets where their skills are demanded.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, migrant_workers, payer,
    moderate, biographical, constrained, global).

% Flee persecution and violence. Border closure and externalization policies such as pushbacks and third-country processing block access to asylum procedures, leaving them in legal limbo or at risk of refoulement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers, payer,
    powerless, immediate, trapped, regional).

% Monitor border violence, document pushbacks, and litigate for freedom-of-movement rights in international forums. Neither collect from nor pay into the constraint.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, human_rights_organizations, observer,
    organized, generational, analytical, global).

% Analyze the historical emergence of territorial sovereignty and debate whether closure authority is constitutive or contingent. Provide the interpretive framework that can reframe sovereignty as jurisdictional rather than exclusionary.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__freedom_of_movement_primary, sovereign_states).
narrative_ontology:fixing_cost_class(border_control_legitimacy__freedom_of_movement_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages territorial membership, security screening, and public order within bounded jurisdictions; coordinates the allocation of rights and protections among those present, though this reading holds that the exclusion function exceeds what genuine coordination requires and that security and order can be managed through jurisdictional regulation without closure.
% TRANSFER_FUNCTION: Transfers mobility rights, economic opportunity, and security from displaced persons and migrant workers to sovereign states and domestic constituencies, enforcing a territorial partition of the global labor market and refuge.
% ABSENT_VOICES: Migrants in transit and origin-country governments are structurally excluded from the policy discourse that sets closure rules; their objections enter only through human rights organizations or irregular practice.
% DISAPPEARANCE_RATIONALE: If border closure authority vanished overnight, millions of displaced persons would access territory and labor markets, sovereign states would lose a primary mechanism of membership control, and the global labor market and refuge system would reorganize around jurisdictional presence rather than exclusion.
% FOUNDING_PROBLEM: The modern state system required mechanisms to define territorial membership, manage security threats, and allocate public goods within bounded communities.
% FOUNDING_PROBLEM_CORROBORATION: International historians and political sociologists attest the Westphalian emergence of territorial control; human rights organizations and migration scholars outside the benefiting parties attest that the security and membership functions are now achievable without closure authority, corroborating the contested status.
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__freedom_of_movement_primary, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.82) is high because the constraint systematically denies mobility rights and economic opportunity to displaced persons and workers. Suppression (0.85) is higher still: the global passport and visa regime actively suppresses alternatives, making open movement practically unthinkable for most populations. Theater ratio (0.45) reflects the growing performative dimension of border control (spectacular wall-building, deterrent rhetoric) alongside continued irregular mobility. Accessibility collapse (0.75) captures the near-total collapse of open-movement alternatives in the global institutional imagination once the constraint is understood. Resistance (0.60) reflects active legal challenges, irregular migration, and rights advocacy, though these remain structurally disadvantaged. The measurement series share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The sovereign state seat experiences the constraint as necessary coordination for membership and order; the displaced citizen and migrant worker seats experience the same structure as violent exclusion. The engine computes this divergence from beneficiary versus payer declarations and exit asymmetry (states are constrained by the interstate system but retain agenda-setting power; migrants are trapped or constrained by enforcement). The analytical observer seats bridge the gap through legal and empirical argument, but do not pay or benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (sovereign_states, domestic_labor_constituencies, border_enforcement_apparatus) drive directionality toward the subsidy end for those agents. Victim declarations (displaced_citizens, migrant_workers, asylum_seekers) drive directionality toward the full-target end. The engine will compute high effective extraction for the powerless, trapped victims at regional or global scope, and damped or inverted extraction for the institutional and organized beneficiaries. No override is necessary because the structural derivation accurately captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve genuine coordination problems (security, membership allocation, public order in bounded communities) but its closure function has atrophied into systematic extraction. The freedom-of-movement reading treats this as mandatrophy: the original coordination problem can now be addressed through jurisdictional regulation and presence-based rights without exclusion, yet the closure authority persists because it benefits states and domestic constituencies. The founding problem is contested precisely because one side (beneficiaries) claims it is still live while the other (victims and observers) argues it is dead or solvable by other means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    closure_security_separability,
    'Can the security and public-order functions of border management be fully separated from the authority to exclude, or does genuine coordination require some closure capacity?',
    'Comparative analysis of jurisdictions with high mobility and low security (Schengen with external borders) versus closure regimes; natural experiment from pandemic border closures and subsequent reopening.',
    'If separable, the extraction component is larger than the coordination component and the constraint trends toward snare; if inseparable, tangled_rope remains accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closure_security_separability, empirical, 'Whether security coordination requires exclusion authority').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of migration alternatives structural (physical barriers, deportation, detention) or internalized (global normative acceptance of passport regimes and territorial closure)?',
    'Measure migration attempts and routes before and after barrier removal or policy liberalization; if flows surge immediately, suppression was structural; if norms shift slowly, internalized.',
    'Internalized suppression raises effective extraction beyond structural measures by normalizing closure; it also makes the constraint more resistant to legal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression of mobility').

omega_variable(
    coordination_extraction_proportion,
    'What proportion of border control activity serves genuine security and public-order coordination versus demographic and economic exclusion?',
    'Audit of enforcement resource allocation toward security threats versus economic migrants; comparative cost-benefit of closure versus jurisdictional regulation.',
    'High extraction proportion would push classification toward snare; demonstrated genuine coordination proportion sustains tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_proportion, empirical, 'Proportion of border control serving coordination vs exclusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bord_tr_t20, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 20, 0.25).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 40, 0.3).
narrative_ontology:measurement(bord_tr_t60, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 60, 0.38).
narrative_ontology:measurement(bord_tr_t80, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 80, 0.42).
narrative_ontology:measurement(bord_tr_t100, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bord_be_t20, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(bord_be_t60, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(bord_be_t80, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 80, 0.78).
narrative_ontology:measurement(bord_be_t100, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 100, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bord_su_t20, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(bord_su_t60, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(bord_su_t80, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 80, 0.83).
narrative_ontology:measurement(bord_su_t100, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 100, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the border_control_legitimacy kernel, decomposed from the colloquial label 'territorial sovereignty' into structurally distinct claims: freedom_of_movement_primary (this file), sovereignty_primary, and jurisdictional_sovereignty. The Îµ values and victim/beneficiary structures differ across readings because each instantiates a different normative commitment about what sovereignty entails.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
