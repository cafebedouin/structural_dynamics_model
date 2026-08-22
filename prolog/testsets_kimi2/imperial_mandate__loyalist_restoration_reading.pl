% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__loyalist_restoration_reading, []).

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
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Divine Mandate: Unmediated Imperial Sovereignty (Loyalist Restoration Reading)
 *   domain: political/constitutional/historical
 *
 * SUMMARY:
 *   This constraint story instantiates the loyalist_restoration_reading of
 *   the imperial_mandate kernel. The sibling bakufu_delegation_reading treats
 *   the same kernel as permitting institutional delegation; this reading
 *   treats divine mandate as requiring unmediated imperial sovereignty.
 *   Operationalized during the Meiji Restoration, the doctrine delegitimized
 *   the Tokugawa shogunate and the samurai class as usurpers, concentrating
 *   sovereignty in the emperor while restorationist oligarchs and modernizing
 *   elites governed in his name. The claim/metric independence is maintained:
 *   the reading is claimed as tangled_rope because it carries a genuine
 *   national-unification coordination function alongside asymmetric
 *   extraction from displaced intermediaries, while the metrics track the
 *   high extraction, high suppression, and rising theater of imperial rule
 *   under oligarchic management.
 *
 * KEY AGENTS:
 *   - restorationist_oligarchs: Primary agenda-setters (institutional/constrained) â craft the state order in the emperor's name and enforce the doctrine.
 *   - imperial_court: Primary beneficiary (institutional/trapped) â gains ritual centrality and nominal sovereignty but is functionally bound to ratify oligarchic decisions.
 *   - tokugawa_shogunate: Primary target (powerful/trapped) â deposed as usurpers; stripped of authority and territorial control.
 *   - samurai_class: Secondary target (organized/constrained) â hereditary status abolished; bear the cost of institutional rupture.
 *   - modernizing_elites: Secondary beneficiaries (moderate/constrained) â gain positions in the centralized military and bureaucracy.
 *   - traditional_confucian_scholars: Excluded voice (moderate/trapped) â had justified shogunal rule; silenced in the new dispensation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.82).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.83).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.83).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Divine Mandate: Unmediated Imperial Sovereignty (Loyalist Restoration Reading)").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political/constitutional/historical").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, 'b94c8202-de25-4f00-ba3e-5665676c7005').
narrative_ontology:cs_kernel_codification('b94c8202-de25-4f00-ba3e-5665676c7005', formalized).
narrative_ontology:cs_authority_grounding('b94c8202-de25-4f00-ba3e-5665676c7005', lineage).
narrative_ontology:cs_interpretation_layer_present('b94c8202-de25-4f00-ba3e-5665676c7005').
narrative_ontology:cs_reading_relation('b94c8202-de25-4f00-ba3e-5665676c7005', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('b94c8202-de25-4f00-ba3e-5665676c7005', foundational, divine_mandate_requires_direct_rule).
narrative_ontology:cs_axiom_status(divine_mandate_requires_direct_rule, holdable).
narrative_ontology:cs_axiom_grounding('b94c8202-de25-4f00-ba3e-5665676c7005', divine_mandate_requires_direct_rule, theological).
narrative_ontology:cs_axiom('b94c8202-de25-4f00-ba3e-5665676c7005', secondary, intermediary_governance_is_usurpation).
narrative_ontology:cs_axiom_status(intermediary_governance_is_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('b94c8202-de25-4f00-ba3e-5665676c7005', intermediary_governance_is_usurpation, deontological).
narrative_ontology:cs_reference_frame('b94c8202-de25-4f00-ba3e-5665676c7005', unified_imperial_sovereignty).
narrative_ontology:cs_drift_state('b94c8202-de25-4f00-ba3e-5665676c7005', meiji_oligarchic_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b94c8202-de25-4f00-ba3e-5665676c7005', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, modernizing_elites).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, restorationist_oligarchs).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, tokugawa_shogunate).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, samurai_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Craft the constitutional and administrative order in the emperor's name. They require the doctrine of unmediated sovereignty to delegitimize the shogunate and justify centralizing reforms, but are themselves constrained to rule through imperial fiction rather than open oligarchic claim.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, restorationist_oligarchs, agenda_setter,
    institutional, generational, constrained, national).

% Restored to political centrality as the fount of legitimacy and the nominal source of all sovereignty. The court gains prestige and budget but is functionally bound to ratify decisions made by the oligarchs; the emperor cannot exit the role without dissolving the polity's foundation.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_court, beneficiary,
    institutional, civilizational, trapped, national).

% Deposed as usurpers under the new doctrine. Lose all political authority, territorial control, and military capacity. Former retainers are dispersed; the house survives in diminished form but cannot reclaim its former role.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, tokugawa_shogunate, payer,
    powerful, biographical, trapped, national).

% Hereditary warrior status abolished as incompatible with direct imperial rule and equality before the throne. Lose tax-exempt stipends, right to bear arms, and social precedence. Bear the direct cost of institutional rupture through the abolition of feudal privileges.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, samurai_class, payer,
    organized, biographical, constrained, national).

% Gain new positions in the centralized ministries and military. Their authority derives from imperial appointment rather than domainal or feudal status. Benefit from the elimination of intermediary power holders.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, modernizing_elites, beneficiary,
    moderate, biographical, constrained, national).

% Had provided the ideological justification for shogunal rule and the bakuhan system. Under the new doctrine their intellectual framework is delegitimized; they are excluded from constitutional debate and educational authority as the state promulgates its own historical narrative.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, traditional_confucian_scholars, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__loyalist_restoration_reading, diffuse).
narrative_ontology:fixing_cost_class(imperial_mandate__loyalist_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves fragmented sovereignty by concentrating legitimacy in a single, unbroken imperial line; eliminates competing claims from regional military houses; provides a unified national actor for diplomatic recognition and modernization.
% TRANSFER_FUNCTION: Transfers administrative and military authority from hereditary intermediary bodies (shogunate, domain governments, samurai status) to centralized organs acting in the emperor's name; transfers material surplus from rural producers to state modernization projects.
% ABSENT_VOICES: Kokugaku nationalists who sought a purely nativist polity without Western constitutional forms; Tokugawa loyalists who viewed shogunal delegation as historically legitimate; peasant republicans who rejected monarchical sovereignty entirely. These voices were excluded from the Restoration settlement and the constitutional debates.
% DISAPPEARANCE_RATIONALE: The centralized state, the military oath, the constitution, and diplomatic recognition all hinge on the emperor as sovereign. If the doctrine vanished, the state would lose its Grundnorm; regional powers would reassert autonomy; the oligarchs would need a new legitimating principle.
% FOUNDING_PROBLEM: Late Tokugawa political fragmentation under the bakuhan system: multiple autonomous military and fiscal centers (shogunate, domains) prevented unified national response to foreign imperialism and domestic crisis.
% FOUNDING_PROBLEM_CORROBORATION: Restorationist oligarchs cite foreign threat and domestic paralysis. Post-war historians and comparative constitutional scholars outside the imperial-beneficiary framework confirm that the fragmentation problem was substantially resolved by the Meiji state's centralization by the 1890s, and that the doctrine persisted as ideological scaffolding thereafter. No non-beneficiary contemporary source corroborates the claim that fragmentation remained an active threat justifying the doctrine's continued intensity after the 1889 constitution.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imperial_mandate__loyalist_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__loyalist_restoration_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__loyalist_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__loyalist_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the doctrine strips all autonomous intermediary authority and redirects it to the center. Suppression is slightly higher (0.83) because the constraint's persistence required active military and ideological enforcement against shogunal restoration, samurai revolt, and liberal dissent. Theater_ratio rises to 0.5 because the claim of unmediated imperial governance became increasingly performative as actual governance migrated to oligarchic and bureaucratic hands. Accessibility_collapse is very high (0.88): once the doctrine was embedded in the constitution, state education, and state Shinto, alternatives became virtually unthinkable within the polity. Resistance is substantial (0.72) from the Satsuma Rebellion and the Freedom and People's Rights Movement, but ultimately suppressed.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (oligarchs, modernizers, court) experience the constraint as necessary national coordination and legitimate restoration; the payer seats (shogunate, samurai) experience it as usurpation and dispossession. The engine computes this divergence from the structural data: low directionality for the imperial court (subsidized by symbolic centrality) and oligarchs (empowered by legitimacy), high directionality for the samurai and shogunate (stripped of status and autonomy).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared are imperial_court, modernizing_elites, and restorationist_oligarchs: they receive legitimacy, careers, and governing authority respectively, yielding low derived directionality. Victims declared are tokugawa_shogunate and samurai_class: they lose autonomous political and military status, yielding high derived directionality. The modernizing elites sit at moderate power with constrained exit (dependent on the imperial state), while the samurai sit at organized power with constrained exit (status abolished, conscription imposed). The court is trapped in its symbolic role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â fragmented sovereignty under foreign pressure â was substantially solved by the 1890s, yet the doctrine intensified rather than sunset. This produces a mandatrophy signal (founding_problem_status: dead, disappearance_verdict: world_rearranges). The classification as tangled_rope rather than piton is supported by the continuing concentration of extraction in identifiable beneficiary seats and the active enforcement requirement; it is not mere inertial performance. The classification as tangled_rope rather than snare is supported by the genuine coordination function of unified sovereignty and diplomatic recognition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_naturality,
    'Does the imperial mandate derive from an unchangeable cosmological order, or is it a constructed legitimating narrative developed to justify the overthrow of the Tokugawa system?',
    'Comparative analysis of political-theology texts before and after the Meiji Restoration; examination of whether the unmediated-sovereignty doctrine appears in pre-Restoration imperial discourse or is synthesized in the 1860s.',
    'If the mandate is constructed, the constraint''s claimed authority is genealogically contingent and its extraction is politically motivated rather than cosmologically necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_naturality, conceptual, 'Whether the divine mandate is natural law or political construction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s persistence secured primarily by structural coercion (military, police, legal prohibition of alternatives) or by internalized ideological fusion (subjects identifying the imperial line with the nation itself)?',
    'Analysis of post-defeat behavior in 1945: if the constraint collapses immediately upon structural coercion removal, it was structural; if persistent identity patterns continue, it was partially internalized.',
    'If internalized, effective suppression exceeds structural measures and the constraint operates more like identity coordination; if purely structural, it is vulnerable to force majeure shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    coordination_extraction_separability,
    'Could the coordination benefit of unified national sovereignty have been achieved without delegitimizing and dissolving the samurai class and domain autonomy?',
    'Counterfactual comparison with constitutional monarchies that retained aristocratic intermediaries (e.g., Prussia, Britain) to determine if unified foreign policy required domestic class abolition.',
    'If separable, the constraint is tangled rope (coordination with asymmetric extraction); if inseparable, the extraction may be the necessary cost of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imperial_lr_tr_t0, imperial_mandate__loyalist_restoration_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(imperial_lr_tr_t11, imperial_mandate__loyalist_restoration_reading, theater_ratio, 11, 0.38).
narrative_ontology:measurement(imperial_lr_tr_t22, imperial_mandate__loyalist_restoration_reading, theater_ratio, 22, 0.45).
narrative_ontology:measurement(imperial_lr_tr_t33, imperial_mandate__loyalist_restoration_reading, theater_ratio, 33, 0.48).
narrative_ontology:measurement(imperial_lr_tr_t44, imperial_mandate__loyalist_restoration_reading, theater_ratio, 44, 0.5).

% Extraction over time
narrative_ontology:measurement(imperial_lr_be_t0, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(imperial_lr_be_t11, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 11, 0.68).
narrative_ontology:measurement(imperial_lr_be_t22, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 22, 0.75).
narrative_ontology:measurement(imperial_lr_be_t33, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 33, 0.79).
narrative_ontology:measurement(imperial_lr_be_t44, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 44, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(imperial_lr_su_t0, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(imperial_lr_su_t11, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 11, 0.68).
narrative_ontology:measurement(imperial_lr_su_t22, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 22, 0.76).
narrative_ontology:measurement(imperial_lr_su_t33, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 33, 0.8).
narrative_ontology:measurement(imperial_lr_su_t44, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 44, 0.83).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, bakufu_delegation_reading).

% DUAL FORMULATION NOTE:
% This constraint and bakufu_delegation_reading are dual formulations of the imperial_mandate kernel. They are not the same constraint viewed from different angles; they share the referent (imperial legitimacy) but instantiate structurally distinct constraints with mutually exclusive implications for sovereignty. The loyalist reading forecloses the delegation reading; their epsilon values and victim sets differ accordingly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
