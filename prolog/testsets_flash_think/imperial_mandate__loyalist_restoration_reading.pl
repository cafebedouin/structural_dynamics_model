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
 *   This constraint story instantiates the 'loyalist restoration' reading of
 *   the imperial mandate kernel, prevalent during the late Edo and Meiji
 *   Restoration periods in Japan. This reading asserts that the emperor's
 *   divine mandate requires direct, unmediated exercise of sovereignty,
 *   making legitimacy inseparable from active imperial governance. It
 *   fundamentally delegitimizes intermediary governance structures like the
 *   shogunate, viewing them as usurpations. The metrics reflect the highly
 *   extractive and suppressive nature of attempting to enforce this reading
 *   against an entrenched military government, requiring significant
 *   institutional rupture.
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
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Imperial Mandate: Loyalist Restoration Reading").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, '0f5c1ac9-9a79-48c5-8742-6113999d62e0').
narrative_ontology:cs_kernel_codification('0f5c1ac9-9a79-48c5-8742-6113999d62e0', fixed_text).
narrative_ontology:cs_authority_grounding('0f5c1ac9-9a79-48c5-8742-6113999d62e0', lineage).
narrative_ontology:cs_interpretation_layer_present('0f5c1ac9-9a79-48c5-8742-6113999d62e0').
narrative_ontology:cs_reading_relation('0f5c1ac9-9a79-48c5-8742-6113999d62e0', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('0f5c1ac9-9a79-48c5-8742-6113999d62e0', foundational, imperial_sovereignty_unmediated).
narrative_ontology:cs_axiom_status(imperial_sovereignty_unmediated, holdable).
narrative_ontology:cs_axiom_grounding('0f5c1ac9-9a79-48c5-8742-6113999d62e0', imperial_sovereignty_unmediated, theological).
narrative_ontology:cs_axiom('0f5c1ac9-9a79-48c5-8742-6113999d62e0', foundational, governance_inseparable_from_legitimacy).
narrative_ontology:cs_axiom_status(governance_inseparable_from_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0f5c1ac9-9a79-48c5-8742-6113999d62e0', governance_inseparable_from_legitimacy, conventional).
narrative_ontology:cs_reference_frame('0f5c1ac9-9a79-48c5-8742-6113999d62e0', direct_imperial_rule_ancient_ideal).
narrative_ontology:cs_drift_state('0f5c1ac9-9a79-48c5-8742-6113999d62e0', bakufu_era_observation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('0f5c1ac9-9a79-48c5-8742-6113999d62e0', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, emperor).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, loyalist_officials).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, shogunate).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, daimyo_regional_lords).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, samurai_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, common_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The divine sovereign whose legitimacy is inseparable from active, unmediated governance. This reading positions the emperor as the sole legitimate source of political authority and administrative power, demanding direct rule.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, emperor, agenda_setter,
    institutional, civilizational, identity_locked, national).

% The traditional administrative and ceremonial body surrounding the emperor. Under this reading, the court would regain significant political influence and administrative roles, benefiting from the centralization of power.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_court, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, imperial_court, agenda_setter).

% Scholars, ideologues, and military figures who advocate for direct imperial rule and actively work to dismantle the shogunate. They would gain power and status in a restored imperial government.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, loyalist_officials, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, loyalist_officials, agenda_setter).

% The military government that has historically exercised de facto rule, delegating authority from the emperor. This reading directly delegitimizes their existence and seeks to dismantle their power structure, making them a primary target of extraction and suppression.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, shogunate, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, shogunate, excluded).

% Feudal lords who hold significant regional power, often under the shogunate's authority. A loyalist restoration would centralize power, reducing their autonomy and potentially confiscating their lands or revenues.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, daimyo_regional_lords, payer,
    powerful, biographical, constrained, regional).

% The warrior class, many of whom are tied to the shogunate or daimyo. A loyalist restoration would disrupt their traditional roles, potentially leading to loss of status, stipends, or employment, unless they align with the loyalist cause.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, samurai_class, payer,
    moderate, biographical, constrained, local).

% The general populace, who would bear the costs of political instability, conflict, and potentially new administrative burdens during a transition to direct imperial rule. They might also benefit from a more unified and stable government in the long term, depending on the outcome.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, common_people, payer,
    powerless, immediate, trapped, local).

% External nations observing the internal political struggles, potentially seeking to engage with a unified imperial government or exploit internal divisions. Their analytical perspective is detached from the internal legitimacy claims.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, foreign_powers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To centralize political authority and legitimacy under direct imperial rule, resolving the perceived fragmentation and usurpation of power by military governments and feudal lords, thereby creating a unified national polity.
% TRANSFER_FUNCTION: Transfers ultimate political power, administrative control, and associated economic resources (taxes, land rights) from the shogunate and various daimyo to the emperor and the imperial court, along with loyalist officials.
% ABSENT_VOICES: The shogunate and its direct supporters, who would argue for the historical legitimacy of delegated rule, the practical necessity of military governance, and the potential chaos of attempting direct imperial administration across a vast realm. Their voices are actively suppressed by the loyalist narrative.
% DISAPPEARANCE_RATIONALE: If this reading of the imperial mandate (demanding unmediated imperial rule) vanished, the entire political landscape would be fundamentally altered. The loyalist movement would lose its core ideological justification, potentially leading to the continued dominance of the shogunate, a different form of centralized rule, or prolonged civil conflict without a clear legitimizing principle.
% FOUNDING_PROBLEM: The perceived usurpation of the emperor's divine authority by military governments (shogunate) and the resulting fragmentation of political power, leading to a system where the true sovereign's will was mediated or ignored.
% FOUNDING_PROBLEM_CORROBORATION: Loyalist scholars, ideologues, and some segments of the populace attest that the problem of usurped imperial authority is still live and requires resolution. However, the shogunate and its allied historians would dispute this, asserting the historical legitimacy and practical necessity of their delegated rule.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.85) and suppression (0.90) reflect the immense political and social upheaval required to dismantle the existing feudal system and centralize power under the emperor. This reading demands a radical transfer of power and resources, which is met with strong resistance from the established order. The low theater ratio (0.10) indicates that this is a highly functional, active political project, not a performative one; its proponents genuinely seek to implement direct imperial rule. Accessibility collapse is high because alternatives (delegated rule) are actively delegitimized, and resistance is high because existing power structures fight back.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the loyalist beneficiaries, this constraint is a necessary restoration of natural order and legitimate governance, a 'Rope' or even 'Mountain' of divine will. From the perspective of the shogunate and daimyo, it is a 'Snare' or 'Tangled Rope' designed to strip them of power and wealth through coercion. The engine's classification will reflect the structural reality of extraction and suppression, independent of the claimed legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   The emperor, imperial court, and loyalist officials are clear beneficiaries, gaining power and resources. The shogunate, daimyo, and samurai class are the primary targets, from whom power and resources are extracted. The common people are largely payers, bearing the costs of conflict and transition, though they might be framed as beneficiaries of a unified state by loyalists. Foreign powers are observers, analyzing the situation without direct structural involvement in the mandate's internal dynamics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_unmediated_rule,
    'Is direct imperial rule a natural and inherent requirement of the divine mandate, or an interpretation that primarily benefits the imperial court and loyalist factions?',
    'Historical and theological analysis of pre-shogunate imperial governance, comparative studies of other ''divine right'' systems, and examination of the political interests of the loyalist movement.',
    'If it''s primarily an interpretation for benefit, the constraint''s ''naturalness'' claim is weakened, supporting a higher effective extraction and a more ''constructed'' classification. If genuinely inherent, it lends more weight to the ''Mountain'' aspect of the claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_unmediated_rule, conceptual, 'Ambiguity regarding the inherent vs. constructed nature of unmediated imperial sovereignty.').

omega_variable(
    feasibility_of_direct_governance,
    'Is direct imperial governance practically feasible across a large, diverse realm without significant delegation, or does it inherently require intermediary structures for effective administration?',
    'Empirical observation of the administrative capacity of the restored imperial government, analysis of historical precedents for direct rule, and assessment of the logistical challenges of centralized control.',
    'If direct rule proves infeasible, the constraint''s coordination function is undermined, potentially leading to a reclassification towards a ''Piton'' (if it becomes purely theatrical) or a ''Snare'' (if it maintains extraction without effective governance). If feasible, it strengthens the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feasibility_of_direct_governance, empirical, 'Practical feasibility of unmediated imperial administration.').

omega_variable(
    legitimacy_of_delegated_rule,
    'Is the shogunate''s historical delegation of authority a legitimate evolution of the imperial mandate, or a fundamental usurpation that must be rectified?',
    'Analysis of historical legal texts, political philosophy of the period, and the long-term stability and prosperity achieved under delegated rule versus direct rule. This is a deeply contested historical and political question.',
    'If delegated rule is deemed legitimate, the loyalist restoration reading becomes a ''Snare'' attempting to extract power from a valid alternative. If it''s a usurpation, the loyalist reading gains moral force, though its extractive nature remains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_delegated_rule, preference, 'Contested legitimacy of historical delegated governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 1850, 1870).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1850, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(impe_tr_t1855, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1855, 0.13).
narrative_ontology:measurement(impe_tr_t1860, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1860, 0.12).
narrative_ontology:measurement(impe_tr_t1865, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1865, 0.11).
narrative_ontology:measurement(impe_tr_t1870, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1870, 0.1).

% Extraction over time
narrative_ontology:measurement(impe_be_t1850, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1850, 0.6).
narrative_ontology:measurement(impe_be_t1855, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1855, 0.68).
narrative_ontology:measurement(impe_be_t1860, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1860, 0.75).
narrative_ontology:measurement(impe_be_t1865, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1865, 0.8).
narrative_ontology:measurement(impe_be_t1870, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1870, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1850, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1850, 0.65).
narrative_ontology:measurement(impe_su_t1855, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1855, 0.72).
narrative_ontology:measurement(impe_su_t1860, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1860, 0.8).
narrative_ontology:measurement(impe_su_t1865, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1865, 0.85).
narrative_ontology:measurement(impe_su_t1870, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1870, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, imperial_mandate__bakufu_delegation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'imperial_mandate' kernel. It directly challenges and seeks to replace the 'bakufu_delegation_reading' by asserting the necessity of unmediated imperial sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
