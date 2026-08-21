% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Imperial Mandate: Loyalist Restoration Reading
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This constraint represents the 'loyalist restoration' reading of the
 *   imperial mandate, which asserts that divine mandate requires the emperor
 *   to exercise unmediated sovereignty, making active imperial governance
 *   inseparable from legitimacy. This reading delegitimizes any intermediary
 *   governance structures (like the shogunate) as usurpation and necessitates
 *   institutional rupture to restore direct imperial rule. The narrative
 *   context is the period leading up to and including the Meiji Restoration
 *   in Japan, where this reading gained ascendancy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.85).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.9).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, snare).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Imperial Mandate: Loyalist Restoration Reading").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, '2f547fc7-99c0-4ae5-98b7-09974d60f720').
narrative_ontology:cs_kernel_codification('2f547fc7-99c0-4ae5-98b7-09974d60f720', formalized).
narrative_ontology:cs_authority_grounding('2f547fc7-99c0-4ae5-98b7-09974d60f720', lineage).
narrative_ontology:cs_interpretation_layer_present('2f547fc7-99c0-4ae5-98b7-09974d60f720').
narrative_ontology:cs_reading_relation('2f547fc7-99c0-4ae5-98b7-09974d60f720', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('2f547fc7-99c0-4ae5-98b7-09974d60f720', foundational, unmediated_imperial_sovereignty).
narrative_ontology:cs_axiom_status(unmediated_imperial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('2f547fc7-99c0-4ae5-98b7-09974d60f720', unmediated_imperial_sovereignty, theological).
narrative_ontology:cs_axiom('2f547fc7-99c0-4ae5-98b7-09974d60f720', foundational, active_imperial_governance_is_legitimacy).
narrative_ontology:cs_axiom_status(active_imperial_governance_is_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2f547fc7-99c0-4ae5-98b7-09974d60f720', active_imperial_governance_is_legitimacy, conventional).
narrative_ontology:cs_reference_frame('2f547fc7-99c0-4ae5-98b7-09974d60f720', ancient_imperial_direct_rule).
narrative_ontology:cs_drift_state('2f547fc7-99c0-4ae5-98b7-09974d60f720', late_tokugawa_period, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('2f547fc7-99c0-4ae5-98b7-09974d60f720', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, loyalist_factions).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, shogunate_officials).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, daimyo_class).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, samurai_bureaucracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The direct beneficiaries of the restoration, reclaiming administrative and ritual authority. Their legitimacy is tied to the unmediated exercise of imperial sovereignty, making any alternative governance structure a usurpation. Exit means abandoning their foundational claim to power.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_court, agenda_setter,
    institutional, generational, identity_locked, national).

% Political and military groups advocating for direct imperial rule. They gain power and influence by dismantling existing intermediary structures and aligning themselves with the imperial court. Their options are to support the restoration or be marginalized.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, loyalist_factions, beneficiary,
    organized, biographical, constrained, national).

% The primary targets of the restoration, whose administrative and military authority is directly challenged and delegitimized. They face loss of power, property, and potentially life if they resist the imperial claim. Their exit options are surrender or armed conflict.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, shogunate_officials, payer,
    powerful, biographical, trapped, national).

% Regional lords who previously held significant autonomy under the shogunate. The restoration demands their direct allegiance and submission to imperial authority, reducing their local power. They can either comply or risk military suppression.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, daimyo_class, payer,
    moderate, biographical, constrained, regional).

% The administrative class that served the shogunate. Their professional identity and livelihood are tied to the existing delegated governance structure. The restoration requires them to either re-align with the new direct imperial administration or face unemployment and social displacement.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, samurai_bureaucracy, payer,
    moderate, biographical, identity_locked, national).

% External actors observing the internal political shifts, potentially seeking to establish diplomatic or trade relations with the newly unified imperial government. Their interest is in a stable, recognizable authority for engagement.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, foreign_powers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to unify national governance under a single, divinely sanctioned authority, resolving internal fragmentation and presenting a coherent front for modernization and foreign engagement.
% TRANSFER_FUNCTION: Transfers administrative and military authority, along with associated revenues and legitimacy, from the shogunate and daimyo class directly to the imperial court and its loyalist supporters.
% ABSENT_VOICES: Those who believe in a separation of ritual and administrative authority, or who advocate for a more decentralized, feudal system, are actively suppressed or marginalized. Their arguments for delegated governance are treated as illegitimate challenges to divine will.
% DISAPPEARANCE_RATIONALE: If the loyalist restoration reading of the imperial mandate vanished, the entire political structure would collapse. The shogunate's authority would be re-legitimized, regional powers would reassert autonomy, and the imperial court would revert to a purely ritualistic role, fundamentally altering the nation's governance.
% FOUNDING_PROBLEM: The perceived usurpation of imperial authority by the shogunate, leading to a fragmented and weakened state unable to respond effectively to internal and external pressures.
% FOUNDING_PROBLEM_CORROBORATION: Loyalist scholars and court historians attest to the ongoing problem of usurpation and the need for direct imperial rule. While the shogunate and its supporters contest this, the loyalist narrative is strongly supported by the imperial court itself and its allied factions.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(imperial_mandate__loyalist_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__loyalist_restoration_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high (0.85) because this reading demands a complete transfer of power and resources from existing feudal structures to the imperial center, imposing significant costs on the former ruling class. Suppression is very high (0.90) as the restoration required active military and political force to dismantle the shogunate and consolidate imperial power, suppressing all alternatives. Theater ratio is low (0.10) because the loyalist movement was genuinely committed to direct imperial rule, with little performative maintenance of a defunct system; the actions were direct and functional towards their goal.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the imperial court and loyalists, this is a necessary restoration of natural order and legitimate governance. From the perspective of the shogunate and daimyo, it is a violent usurpation and a snare designed to extract their power and resources. The engine's classification will reflect this divergence based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial court and loyalist factions are clear beneficiaries, gaining direct power and legitimacy. The shogunate officials, daimyo class, and samurai bureaucracy are direct targets, losing power and facing severe consequences for non-compliance. Foreign powers are observers, their directionality shaped by their strategic interests in a stable, unified Japan.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_naturalness,
    'Is the ''divine mandate'' a genuine natural law (Mountain) or a constructed political claim (Snare) that benefits identifiable agents?',
    'Historical and anthropological analysis of the mandate''s origins, evolution, and enforcement mechanisms, particularly examining periods of contestation and the role of coercion in its maintenance.',
    'If a constructed claim, the constraint''s extractiveness and suppression are fully attributable to human agency, reinforcing its Snare classification. If genuinely perceived as natural law by all parties, it would shift towards a False Summit Mountain, where beneficiaries exploit a perceived inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_naturalness, conceptual, 'Ambiguity between natural law and political construct for the divine mandate.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is imperial legitimacy derived solely from unmediated divine mandate, or can it be legitimately delegated to intermediary institutions?',
    'Analysis of historical precedents and philosophical arguments within the tradition: whether periods of delegated rule were considered legitimate or always viewed as usurpation.',
    'If delegation is legitimate, the ''loyalist_restoration_reading'' is a specific political interpretation rather than an inherent truth, making its enforcement more clearly extractive. If unmediated rule is the only legitimate form, the restoration is a return to natural order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Whether imperial legitimacy requires direct, unmediated rule or allows for delegation.').

omega_variable(
    modernization_necessity_vs_imperial_power,
    'Was the ''institutional rupture'' truly necessary for modernization and foreign engagement, or was it primarily a means to consolidate imperial power under the guise of national necessity?',
    'Comparative historical analysis with other nations undergoing modernization, examining whether similar reforms required such a complete dismantling of existing power structures, or if alternative paths existed.',
    'If modernization could have occurred with less institutional rupture, the high extractiveness and suppression of the restoration are more clearly attributable to power consolidation rather than functional necessity. If rupture was unavoidable, it partially justifies the costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernization_necessity_vs_imperial_power, empirical, 'The true motivation behind the institutional rupture: modernization or power consolidation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 1850, 1870).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1850, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1850, 0.2).
narrative_ontology:measurement(impe_tr_t1855, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1855, 0.18).
narrative_ontology:measurement(impe_tr_t1860, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1860, 0.15).
narrative_ontology:measurement(impe_tr_t1865, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1865, 0.12).
narrative_ontology:measurement(impe_tr_t1870, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1870, 0.1).

% Extraction over time
narrative_ontology:measurement(impe_be_t1850, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1850, 0.6).
narrative_ontology:measurement(impe_be_t1855, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1855, 0.7).
narrative_ontology:measurement(impe_be_t1860, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1860, 0.78).
narrative_ontology:measurement(impe_be_t1865, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1865, 0.82).
narrative_ontology:measurement(impe_be_t1870, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1870, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1850, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1850, 0.7).
narrative_ontology:measurement(impe_su_t1855, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1855, 0.78).
narrative_ontology:measurement(impe_su_t1860, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1860, 0.85).
narrative_ontology:measurement(impe_su_t1865, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1865, 0.88).
narrative_ontology:measurement(impe_su_t1870, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1870, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, imperial_mandate__bakufu_delegation_reading).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, meiji_restoration_land_tax_reform).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, samurai_class_abolition).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'imperial_mandate' kernel. The 'bakufu_delegation_reading' is a sibling constraint that posits a different structural relationship between the emperor and governance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
