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
 *   human_readable: Imperial Mandate: Loyalist Restoration Reading (Meiji Era)
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This constraint represents the 'loyalist restoration' reading of the
 *   imperial mandate, prominent during the Meiji Restoration in Japan. It
 *   asserts that the emperor's divine mandate requires direct, unmediated
 *   exercise of sovereignty, making any intermediary governance (like the
 *   shogunate) an illegitimate usurpation. This reading necessitated
 *   institutional rupture to restore direct imperial rule and was
 *   instrumental in driving modernization and foreign engagement under
 *   explicit imperial initiative. The constraint is classified as a Snare due
 *   to its high extraction from the former ruling class and the active
 *   suppression required to dismantle the old order.
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
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, snare).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Imperial Mandate: Loyalist Restoration Reading (Meiji Era)").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, '0f3a7d48-3028-4848-9d4d-3dc85e395351').
narrative_ontology:cs_kernel_codification('0f3a7d48-3028-4848-9d4d-3dc85e395351', formalized).
narrative_ontology:cs_authority_grounding('0f3a7d48-3028-4848-9d4d-3dc85e395351', lineage).
narrative_ontology:cs_interpretation_layer_present('0f3a7d48-3028-4848-9d4d-3dc85e395351').
narrative_ontology:cs_reading_relation('0f3a7d48-3028-4848-9d4d-3dc85e395351', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('0f3a7d48-3028-4848-9d4d-3dc85e395351', foundational, unmediated_imperial_sovereignty).
narrative_ontology:cs_axiom_status(unmediated_imperial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('0f3a7d48-3028-4848-9d4d-3dc85e395351', unmediated_imperial_sovereignty, theological).
narrative_ontology:cs_axiom('0f3a7d48-3028-4848-9d4d-3dc85e395351', secondary, usurpation_of_governance_by_shogunate).
narrative_ontology:cs_axiom_status(usurpation_of_governance_by_shogunate, holdable).
narrative_ontology:cs_axiom_grounding('0f3a7d48-3028-4848-9d4d-3dc85e395351', usurpation_of_governance_by_shogunate, conventional).
narrative_ontology:cs_reference_frame('0f3a7d48-3028-4848-9d4d-3dc85e395351', ancient_imperial_direct_rule).
narrative_ontology:cs_drift_state('0f3a7d48-3028-4848-9d4d-3dc85e395351', tokugawa_shogunate_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('0f3a7d48-3028-4848-9d4d-3dc85e395351', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, loyalist_samurai_factions).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, bakufu_officials).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, daimyo_lords).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, traditional_samurai_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The emperor and his immediate advisors, who claim direct divine authority and seek to centralize all administrative power, delegitimizing any intermediary governance. Their legitimacy is fused with the concept of unmediated imperial rule.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_court, agenda_setter,
    institutional, generational, identity_locked, national).

% Samurai who support the direct imperial rule, seeing it as a restoration of proper order and a path to national strength. They benefit from the new power structure and the dismantling of the old shogunate system, gaining new administrative roles.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, loyalist_samurai_factions, beneficiary,
    organized, biographical, constrained, national).

% Officials of the former shogunate, whose administrative and military authority is directly challenged and dismantled by the loyalist movement. They face loss of power, status, and even life if they resist.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, bakufu_officials, payer,
    institutional, biographical, trapped, national).

% Feudal lords who previously held significant regional autonomy under the shogunate. Their domains are abolished, and they are integrated into a centralized state, losing their independent power base.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, daimyo_lords, payer,
    powerful, biographical, constrained, regional).

% The broader samurai class, whose traditional stipends and privileges are abolished as the new imperial government centralizes power and modernizes the military. Their identity is deeply tied to their warrior status, which is now obsolete.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, traditional_samurai_class, payer,
    moderate, biographical, identity_locked, local).

% Western nations observing Japan's internal political struggles, seeking to establish trade relations and influence. They are external to the internal mandate debate but their presence acts as a catalyst for the loyalist restoration's modernization agenda.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, foreign_powers, observer,
    institutional, immediate, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to unify the nation under a single, direct imperial authority to respond to internal fragmentation and external threats, coordinating national defense and modernization efforts.
% TRANSFER_FUNCTION: Transfers administrative and military authority, as well as economic resources, from decentralized feudal lords and the shogunate to the centralized imperial government and its loyalist supporters.
% ABSENT_VOICES: Any proponents of a constitutional monarchy or a more democratic system, whose ideas are suppressed by the absolutist claims of imperial sovereignty. Also, those who would argue for a continued, albeit reformed, shogunate system.
% DISAPPEARANCE_RATIONALE: If the loyalist restoration reading of the imperial mandate vanished, the Meiji Restoration would not have occurred in its historical form. Japan would likely have remained a decentralized feudal state, or evolved into a different form of governance, with profound implications for its response to Western powers and its subsequent history.
% FOUNDING_PROBLEM: The problem of a fragmented feudal state (Tokugawa Shogunate) unable to effectively respond to internal dissent and external pressure from Western powers, leading to national weakness and potential colonization.
% FOUNDING_PROBLEM_CORROBORATION: The imperial court and loyalist factions attest the problem is live, citing ongoing threats to national sovereignty. Historians and political scientists, from outside the benefiting parties, corroborate that the problem of national unity and external threat was indeed pressing, though they may dispute the necessity or nature of the 'restoration' as the solution.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because the loyalist reading fundamentally reallocated power and resources from the shogunate and daimyo to the imperial center, with significant costs to the former. Suppression is also very high (0.90) as the Meiji Restoration involved civil war, political purges, and the forceful dismantling of the samurai class and feudal domains. Theater ratio is low (0.10) because the 'restoration' was a genuine, active political project with real consequences, not merely performative maintenance of an atrophied function. The historical trajectory shows increasing extractiveness and suppression as the new imperial order consolidated power, while theatricality decreased as the new system became established.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the imperial court and loyalists, this was a necessary and legitimate 'restoration' of divine order. From the perspective of the shogunate and feudal lords, it was a violent usurpation and a snare. The engine's classification as a Snare reflects the high extraction and suppression inherent in this reading's implementation.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial court and loyalist samurai factions are clear beneficiaries, gaining power and legitimacy from the direct imperial rule. Bakufu officials, daimyo lords, and the traditional samurai class are victims, losing their power, status, and livelihoods. Foreign powers are observers, external to the internal mandate debate but influencing its context.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_naturalness,
    'Is the ''divine mandate'' a genuine natural law (Mountain) or a constructed political claim (Snare)?',
    'Analysis of historical and theological texts, cross-cultural comparison of sovereignty claims, and the degree of active enforcement required to maintain belief in the mandate.',
    'If genuinely a natural law, the constraint''s extractiveness would be negligible, reclassifying it as a Mountain. If a constructed claim, its high extractiveness and suppression are consistent with a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_naturalness, conceptual, 'Ambiguity between natural law and political construct for the divine mandate.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (military force, legal abolition of feudal domains) or internalized (ideological indoctrination, cultural pressure to conform to imperial will)?',
    'Post-restoration social stability analysis: if resistance persists after initial military suppression, internalized suppression is lower than structural measures suggest. If compliance is rapid and deep, internalized suppression is higher.',
    'If internalized suppression is a significant component, the constraint''s effective suppression is higher than the structural measure suggests, indicating deeper control. If purely structural, removal of force would lead to rapid collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the Meiji Restoration.').

omega_variable(
    mandate_legitimacy_source,
    'Is the emperor''s legitimacy primarily derived from divine right, or from the practical success of the loyalist restoration in modernizing and strengthening Japan?',
    'Historical analysis of public discourse and official pronouncements: if divine right claims diminish in favor of pragmatic justifications over time, the grounding shifts. If divine right remains paramount, it''s the primary source.',
    'If legitimacy shifts to pragmatic success, the constraint becomes more vulnerable to empirical challenges (e.g., military defeat, economic failure), potentially reducing its stability and increasing resistance. If divine right remains, it''s more resilient to such challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_legitimacy_source, conceptual, 'Source of imperial mandate legitimacy: divine right vs. pragmatic success.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 1868, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1868, 0.2).
narrative_ontology:measurement(impe_tr_t1875, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1875, 0.15).
narrative_ontology:measurement(impe_tr_t1882, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1882, 0.12).
narrative_ontology:measurement(impe_tr_t1890, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1890, 0.1).

% Extraction over time
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1868, 0.75).
narrative_ontology:measurement(impe_be_t1875, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1875, 0.8).
narrative_ontology:measurement(impe_be_t1882, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1882, 0.83).
narrative_ontology:measurement(impe_be_t1890, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1890, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1868, 0.8).
narrative_ontology:measurement(impe_su_t1875, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1875, 0.85).
narrative_ontology:measurement(impe_su_t1882, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1882, 0.88).
narrative_ontology:measurement(impe_su_t1890, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1890, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, meiji_constitution_reading).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, national_unity_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'imperial_mandate' kernel. Its sibling, 'bakufu_delegation_reading', posits a different distribution of imperial authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
