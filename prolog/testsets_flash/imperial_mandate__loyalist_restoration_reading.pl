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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Imperial Mandate (Loyalist Restoration Reading)
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This constraint represents the 'loyalist restoration' reading of the
 *   imperial mandate, which asserts that the emperor's divine authority
 *   requires direct, unmediated exercise of sovereignty. This reading emerged
 *   during the Meiji Restoration in Japan, challenging the long-standing
 *   system of delegated authority under the shogunate. It necessitated
 *   significant institutional rupture and active suppression of existing
 *   power structures to establish a centralized imperial state capable of
 *   modernization and engagement with foreign powers. The constraint is
 *   claimed as a Tangled Rope because it genuinely aimed to coordinate
 *   national unity and modernization (beneficiaries: emperor, loyalist
 *   bureaucracy) but did so through the asymmetric extraction of power and
 *   legitimacy from the shogunate and feudal lords (victims).
 *
 * KEY AGENTS:
 *   - emperor: Primary agenda_setter (institutional/identity_locked) — asserts and embodies the mandate.
 *   - loyalist_bureaucracy: Primary beneficiary (organized/constrained) — gains power and legitimacy from direct imperial rule.
 *   - shogunate: Primary payer (institutional/trapped) — delegitimized and dismantled by the restoration.
 *   - daimyo_class: Payer (powerful/constrained) — loses regional autonomy and landholdings.
 *   - samurai_class: Payer (moderate/identity_locked) — loses status and livelihood tied to feudal lords.
 *   - foreign_powers: Observer (institutional/analytical) — strategically monitors the internal power shift.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.65).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.75).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Imperial Mandate (Loyalist Restoration Reading)").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, '0794d927-e20c-4902-abbd-1ec65606c77c').
narrative_ontology:cs_kernel_codification('0794d927-e20c-4902-abbd-1ec65606c77c', formalized).
narrative_ontology:cs_authority_grounding('0794d927-e20c-4902-abbd-1ec65606c77c', lineage).
narrative_ontology:cs_interpretation_layer_present('0794d927-e20c-4902-abbd-1ec65606c77c').
narrative_ontology:cs_reading_relation('0794d927-e20c-4902-abbd-1ec65606c77c', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('0794d927-e20c-4902-abbd-1ec65606c77c', foundational, imperial_sovereignty_unmediated).
narrative_ontology:cs_axiom_status(imperial_sovereignty_unmediated, holdable).
narrative_ontology:cs_axiom_grounding('0794d927-e20c-4902-abbd-1ec65606c77c', imperial_sovereignty_unmediated, deontological).
narrative_ontology:cs_axiom('0794d927-e20c-4902-abbd-1ec65606c77c', foundational, active_imperial_governance_legitimacy).
narrative_ontology:cs_axiom_status(active_imperial_governance_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0794d927-e20c-4902-abbd-1ec65606c77c', active_imperial_governance_legitimacy, conventional).
narrative_ontology:cs_reference_frame('0794d927-e20c-4902-abbd-1ec65606c77c', ancient_imperial_direct_rule).
narrative_ontology:cs_drift_state('0794d927-e20c-4902-abbd-1ec65606c77c', meiji_restoration_era, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('0794d927-e20c-4902-abbd-1ec65606c77c', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, emperor).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, loyalist_bureaucracy).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, shogunate).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, daimyo_class).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, samurai_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The divine sovereign whose unmediated rule is asserted as the source of all legitimate authority. Benefits from the concentration of power and symbolic capital, but is also bound by the mandate to actively govern.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, emperor, agenda_setter,
    institutional, generational, identity_locked, national).

% The administrative apparatus that directly serves the emperor, gaining power and legitimacy by displacing intermediary structures. Their careers and influence depend on the active, unmediated imperial rule.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, loyalist_bureaucracy, beneficiary,
    organized, biographical, constrained, national).

% The military government that historically exercised de facto power. Under this reading, their authority is illegitimate usurpation, and their institutional existence is threatened by the restoration of direct imperial rule.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, shogunate, payer,
    institutional, generational, trapped, national).

% Regional lords who held significant autonomy under the shogunate. Their power and landholdings are subject to direct imperial control and potential confiscation under the restorationist agenda.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, daimyo_class, payer,
    powerful, generational, constrained, regional).

% The warrior class whose status and livelihood were tied to service under the daimyo and shogunate. The delegitimization of these structures threatens their social standing and economic survival, forcing them to choose between loyalty to old masters or the new imperial order.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, samurai_class, payer,
    moderate, biographical, identity_locked, local).

% External actors whose engagement with the nation is complicated by the internal power struggle. They observe the shift in sovereignty with strategic interest, potentially seeking to exploit or influence the new imperial government.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, foreign_powers, observer,
    institutional, immediate, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies national authority under a single, divinely sanctioned sovereign, eliminating fragmented governance and providing a clear, centralized command structure for national policy and modernization efforts.
% TRANSFER_FUNCTION: Transfers political power, administrative control, and symbolic legitimacy from intermediary military and feudal structures (shogunate, daimyo) to the emperor and his direct bureaucracy.
% ABSENT_VOICES: The shogunate and daimyo, whose historical claims to delegated authority are actively suppressed and delegitimized. They would argue for the historical precedent of delegated rule and the stability it provided, but their voices are silenced by the restorationist narrative.
% DISAPPEARANCE_RATIONALE: If the loyalist restorationist reading of the imperial mandate vanished, the entire political structure would collapse. The shogunate's historical claims would reassert, regional powers would fragment, and the emperor's role would revert to a purely ritualistic one, necessitating a complete reorganization of governance.
% FOUNDING_PROBLEM: The fragmentation of national authority and the perceived weakness of the nation in the face of foreign encroachment, attributed to the usurpation of imperial power by military regimes.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political theorists outside the loyalist faction corroborate the historical problem of fragmented authority and foreign pressure. However, they contest whether the 'unmediated rule' solution was the only or most effective path, noting the significant internal conflict it generated.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(imperial_mandate__loyalist_restoration_reading, 'none', 1).

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
 *   The extractiveness (0.65) is high because the restoration involved a significant transfer of resources and power from the old order to the new. Suppression (0.75) is also high, reflecting the active military and political campaigns required to dismantle the shogunate and consolidate imperial power. The theater ratio (0.4) indicates that while the divine mandate had a genuine symbolic function, a substantial portion of the 'restoration' was performative justification for a power grab. The temporal measurements show a sharp increase in extractiveness and suppression during the initial phase of the Meiji Restoration (1860s-1870s) as the new order was established, followed by a slight stabilization as the new imperial state consolidated its power.
 *
 * PERSPECTIVAL GAP:
 *   The emperor and loyalist bureaucracy would experience this as a necessary and legitimate act of national coordination and restoration, aligning with their divine mandate. The shogunate, daimyo, and samurai class would experience it as a coercive seizure of power, a snare that dismantled their established roles and livelihoods. The engine's per-seat classification will reflect this divergence based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The emperor and loyalist bureaucracy are clear beneficiaries, as the constraint directly empowers them (low d). The shogunate, daimyo, and samurai class are clear targets, as the constraint actively disempowers and extracts from them (high d). Their identity-locked or constrained exit options further amplify their target status. Foreign powers are analytical observers, not directly subject to the constraint's internal dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it genuinely aimed to solve a coordination problem (national unity, modernization) but did so through asymmetric extraction and active enforcement against existing power structures. It prevents mislabeling as a pure Rope by acknowledging the victims and the coercive enforcement, and prevents mislabeling as a Snare by recognizing the genuine coordination function and the perceived national benefit by its proponents. The 'founding problem status' being 'live' from the loyalist perspective, but 'contested' by external observers, highlights the ongoing debate about the constraint's true function versus its historical justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_natural_vs_constructed,
    'Is the requirement for unmediated imperial sovereignty a genuine divine mandate (natural law) or a constructed political ideology used to justify a power shift?',
    'Analysis of historical and theological texts for consistency, and comparison with other cultures'' interpretations of divine right. Examination of the political and economic interests served by this specific interpretation.',
    'If genuinely natural, the constraint''s extractiveness might be re-evaluated as inherent cost of a Mountain. If constructed, it reinforces the Tangled Rope classification, highlighting the ideological cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_natural_vs_constructed, conceptual, 'Ambiguity between natural law and political construct for imperial mandate.').

omega_variable(
    necessity_of_rupture_vs_evolution,
    'Was institutional rupture (dismantling the shogunate) truly necessary for national unity and modernization, or could a more evolutionary path have achieved similar goals with less extraction?',
    'Counterfactual historical analysis comparing outcomes with alternative political reforms in similar contexts. Examination of contemporary debates among reformers regarding gradualism versus radical change.',
    'If rupture was not strictly necessary, the high suppression and extractiveness are less justifiable as coordination costs, pushing the classification closer to a Snare. If it was, it reinforces the coordination aspect of the Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_of_rupture_vs_evolution, empirical, 'Whether institutional rupture was a necessary condition for the stated goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 1850, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1850, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1850, 0.1).
narrative_ontology:measurement(impe_tr_t1860, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1860, 0.25).
narrative_ontology:measurement(impe_tr_t1870, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1870, 0.45).
narrative_ontology:measurement(impe_tr_t1880, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1880, 0.42).
narrative_ontology:measurement(impe_tr_t1890, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1890, 0.4).

% Extraction over time
narrative_ontology:measurement(impe_be_t1850, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1850, 0.4).
narrative_ontology:measurement(impe_be_t1860, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1860, 0.55).
narrative_ontology:measurement(impe_be_t1870, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1870, 0.7).
narrative_ontology:measurement(impe_be_t1880, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1880, 0.68).
narrative_ontology:measurement(impe_be_t1890, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1890, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1850, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1850, 0.3).
narrative_ontology:measurement(impe_su_t1860, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1860, 0.6).
narrative_ontology:measurement(impe_su_t1870, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1870, 0.8).
narrative_ontology:measurement(impe_su_t1880, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1880, 0.78).
narrative_ontology:measurement(impe_su_t1890, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1890, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'imperial_mandate' kernel. It asserts unmediated imperial rule, contrasting with the 'bakufu_delegation_reading' which posits delegated authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
