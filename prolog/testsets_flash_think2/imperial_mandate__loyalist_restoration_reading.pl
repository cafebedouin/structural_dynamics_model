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
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Imperial Mandate: Loyalist Restoration Reading
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This constraint represents the 'loyalist restoration' reading of the
 *   imperial mandate, which asserts that divine mandate requires the
 *   unmediated exercise of sovereignty by the emperor, with legitimacy
 *   inseparable from active imperial governance. This reading emerged during
 *   periods of political upheaval (e.g., Meiji Restoration in Japan) to
 *   justify the overthrow of existing delegated power structures (like the
 *   shogunate) and the centralization of authority under the emperor. It
 *   demands institutional rupture to restore direct rule and requires
 *   explicit imperial initiative for modernization and foreign engagement.
 *   The claimed type is 'snare' because, while it offers a coordination story
 *   (national unity), its primary function is the forceful extraction of
 *   power from existing elites and the suppression of alternatives.
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
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, snare).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Imperial Mandate: Loyalist Restoration Reading").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, 'd38a0e7a-97b7-432e-bbe3-6f8d7b3d22d1').
narrative_ontology:cs_kernel_codification('d38a0e7a-97b7-432e-bbe3-6f8d7b3d22d1', formalized).
narrative_ontology:cs_authority_grounding('d38a0e7a-97b7-432e-bbe3-6f8d7b3d22d1', lineage).
narrative_ontology:cs_interpretation_layer_present('d38a0e7a-97b7-432e-bbe3-6f8d7b3d22d1').
narrative_ontology:cs_reading_relation('d38a0e7a-97b7-432e-bbe3-6f8d7b3d22d1', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('d38a0e7a-97b7-432e-bbe3-6f8d7b3d22d1', foundational, imperial_sovereignty_is_direct).
narrative_ontology:cs_axiom_status(imperial_sovereignty_is_direct, holdable).
narrative_ontology:cs_axiom_grounding('d38a0e7a-97b7-432e-bbe3-6f8d7b3d22d1', imperial_sovereignty_is_direct, deontological).
narrative_ontology:cs_axiom('d38a0e7a-97b7-432e-bbe3-6f8d7b3d22d1', foundational, legitimacy_requires_active_governance).
narrative_ontology:cs_axiom_status(legitimacy_requires_active_governance, holdable).
narrative_ontology:cs_axiom_grounding('d38a0e7a-97b7-432e-bbe3-6f8d7b3d22d1', legitimacy_requires_active_governance, conventional).
narrative_ontology:cs_reference_frame('d38a0e7a-97b7-432e-bbe3-6f8d7b3d22d1', ancient_imperial_direct_rule).
narrative_ontology:cs_drift_state('d38a0e7a-97b7-432e-bbe3-6f8d7b3d22d1', bakufu_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('d38a0e7a-97b7-432e-bbe3-6f8d7b3d22d1', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, emperor).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_court_officials).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, loyalist_samurai_officials).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, shogunate_bakufu).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, daimyo_feudal_lords).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, common_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The divine sovereign whose unmediated rule is asserted as the only legitimate form of governance. Benefits from the centralization of power and resources, but is also bound by the mandate's requirements for active rule.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, emperor, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Advisors and administrators who gain direct access to power and influence through the restoration of direct imperial rule. They actively promote and enforce the loyalist interpretation of the mandate.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_court_officials, agenda_setter,
    organized, generational, constrained, national).

% Military and administrative figures who support the imperial restoration, gaining new positions, prestige, and resources in the centralized government. They are instrumental in dismantling the old feudal order.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, loyalist_samurai_officials, beneficiary,
    powerful, biographical, constrained, national).

% The former military government, whose authority is delegitimized and forcibly dismantled by the loyalist restoration. They are the primary target of the extraction of political power and resources.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, shogunate_bakufu, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, shogunate_bakufu, excluded).

% Regional feudal lords whose semi-autonomous domains and military power are absorbed into the centralized imperial state. They lose significant political and economic autonomy.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, daimyo_feudal_lords, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, daimyo_feudal_lords, excluded).

% Bear the costs of the institutional rupture, including civil conflict, new taxes, and conscription, as the state centralizes power. They have little agency in the political transformation.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, common_people, payer,
    powerless, immediate, trapped, local).

% Observe the internal political transformations, assessing the stability and implications for diplomatic and trade relations. Their actions can influence the external pressures on the imperial government.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, foreign_powers, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To unify the nation under a single, divinely sanctioned imperial authority, eliminating internal fragmentation and presenting a united front against perceived external threats and internal usurpation.
% TRANSFER_FUNCTION: Transfers political power, administrative control, military authority, and associated revenues from the decentralized feudal system (shogunate, daimyo) to the centralized imperial government.
% ABSENT_VOICES: The shogunate and daimyo, who would argue for the historical legitimacy of delegated rule and the stability of the existing order. Their voices are actively suppressed or excluded from the discourse of restoration.
% DISAPPEARANCE_RATIONALE: If the loyalist interpretation of the imperial mandate and its enforcement vanished, the newly centralized state would likely collapse into fragmentation, civil conflict, or a return to a decentralized feudal system, as the foundational claim for its authority would be gone.
% FOUNDING_PROBLEM: The perceived usurpation of legitimate imperial authority by military governments (shogunate), leading to internal fragmentation, perceived national weakness, and an inability to respond effectively to foreign pressures.
% FOUNDING_PROBLEM_CORROBORATION: Loyalist scholars, imperial court historians, and later, state-sponsored educational systems attest to the problem's historical and ongoing validity. External corroboration is limited, as it is a specific political interpretation of historical events.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.85) because this reading demands a complete transfer of power and resources from existing feudal structures to the imperial center. Suppression is very high (0.90) as it necessitates the active dismantling of the shogunate and daimyo domains, often through military force and political coercion, with no legitimate alternatives permitted. The theater ratio is low (0.10) because this reading emphasizes active, direct governance rather than mere symbolic performance, though imperial rituals remain important. Resistance is high (0.75) due to the significant opposition from those whose power is being usurped. Accessibility collapse is high (0.92) as all alternative forms of governance are delegitimized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the emperor and loyalists, this constraint is a necessary restoration of legitimate order, a 'rope' for national unity. From the perspective of the shogunate and daimyo, it is a 'snare' designed to extract their power and suppress their historical legitimacy. The engine's classification as 'snare' reflects the structural reality of coercion and extraction, despite the loyalist claim of coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The emperor and loyalist officials are clear beneficiaries, gaining centralized power and resources. The shogunate and daimyo are primary victims, losing their authority and domains. The common people are also victims, bearing the costs of conflict and centralization. Foreign powers act as observers, reacting to the new political order.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_naturalness,
    'Is the divine mandate a genuine, universally recognized truth, or a political construct used to justify power centralization?',
    'Comparative analysis of political theology across cultures, and historical examination of the mandate''s emergence and reinterpretation during periods of political change.',
    'If a political construct, the constraint''s naturalness claim is undermined, reinforcing its classification as a constructed snare. If genuinely universal, it might lend a ''mountain'' aspect to the underlying principle, though its application remains extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_naturalness, conceptual, 'Ambiguity regarding the inherent vs. constructed nature of the divine mandate.').

omega_variable(
    coordination_vs_extraction_legitimacy,
    'To what extent was the national unity achieved by the restoration a genuine coordination benefit for the populace, versus a cover for the extraction of power from existing elites?',
    'Socio-economic analysis of the post-restoration period, examining changes in commoners'' welfare, internal stability, and external security, compared to the costs borne by the former elites.',
    'If genuine coordination benefits were widespread and substantial, it might push the classification towards a ''tangled_rope''. If benefits were concentrated or negligible for the populace, it reinforces the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_legitimacy, empirical, 'The true balance between coordination and extraction functions.').

omega_variable(
    suppression_internalization,
    'Was the suppression of alternative power structures purely structural (military force, legal decrees), or did it lead to an internalization of imperial legitimacy among the populace and former elites?',
    'Analysis of post-restoration social movements, intellectual discourse, and regional rebellions. If resistance persists or re-emerges after initial structural suppression, internalization was incomplete.',
    'If internalized, the constraint''s effective suppression is higher and more durable than structural measures alone suggest, making it harder to dislodge even if direct enforcement lessens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression mechanism for imperial authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 1868, 1912).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement(impe_tr_t1878, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1878, 0.12).
narrative_ontology:measurement(impe_tr_t1888, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1888, 0.1).
narrative_ontology:measurement(impe_tr_t1898, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1898, 0.1).
narrative_ontology:measurement(impe_tr_t1908, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1908, 0.1).
narrative_ontology:measurement(impe_tr_t1912, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1912, 0.1).

% Extraction over time
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1868, 0.7).
narrative_ontology:measurement(impe_be_t1878, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1878, 0.78).
narrative_ontology:measurement(impe_be_t1888, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1888, 0.82).
narrative_ontology:measurement(impe_be_t1898, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1898, 0.84).
narrative_ontology:measurement(impe_be_t1908, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1908, 0.85).
narrative_ontology:measurement(impe_be_t1912, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1912, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1868, 0.75).
narrative_ontology:measurement(impe_su_t1878, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1878, 0.85).
narrative_ontology:measurement(impe_su_t1888, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1888, 0.88).
narrative_ontology:measurement(impe_su_t1898, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1898, 0.9).
narrative_ontology:measurement(impe_su_t1908, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1908, 0.9).
narrative_ontology:measurement(impe_su_t1912, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1912, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, national_unity_doctrine).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, samurai_class_privileges).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, foreign_policy_stance).

% DUAL FORMULATION NOTE:
% This constraint is the 'loyalist_restoration_reading' of the 'imperial_mandate' kernel, which contrasts with the 'bakufu_delegation_reading'. Each represents a distinct structural claim about imperial authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
