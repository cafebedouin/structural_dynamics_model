% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Imperial Mandate: Bakufu Delegation Reading
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This constraint describes the 'Bakufu Delegation' reading of the Imperial
 *   Mandate in pre-Meiji Japan, where the emperor's divine authority was
 *   acknowledged as the source of legitimacy, but actual governance was
 *   delegated to the Shogunate. The emperor became a ritual figure, while the
 *   Shogun exercised effective political and military power. This reading
 *   emphasizes the institutional continuity achieved through this bifurcation
 *   of sovereignty, with the samurai class serving as the legitimate
 *   governing stratum.
 *
 * KEY AGENTS:
 *   - emperor: Primary beneficiary (symbolic legitimacy) / Excluded (political power) — (institutional/identity_locked)
 *   - shogunate: Agenda setter (governance) / Beneficiary (power) — (institutional/constrained)
 *   - samurai_class: Beneficiary (social order, privilege) — (organized/constrained)
 *   - imperial_court: Payer (diminished status, lack of power) — (powerless/trapped)
 *   - loyalist_factions: Payer (suppression, marginalization) — (moderate/constrained)
 *   - common_people: Payer (taxation, conscription) — (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.6).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.7).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Imperial Mandate: Bakufu Delegation Reading").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, '3943a78e-4e2a-47c7-b403-eff78d587c7f').
narrative_ontology:cs_kernel_codification('3943a78e-4e2a-47c7-b403-eff78d587c7f', formalized).
narrative_ontology:cs_authority_grounding('3943a78e-4e2a-47c7-b403-eff78d587c7f', lineage).
narrative_ontology:cs_interpretation_layer_present('3943a78e-4e2a-47c7-b403-eff78d587c7f').
narrative_ontology:cs_reading_relation('3943a78e-4e2a-47c7-b403-eff78d587c7f', imperial_mandate__loyalist_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('3943a78e-4e2a-47c7-b403-eff78d587c7f', foundational, divine_mandate_delegable).
narrative_ontology:cs_axiom_status(divine_mandate_delegable, holdable).
narrative_ontology:cs_axiom_grounding('3943a78e-4e2a-47c7-b403-eff78d587c7f', divine_mandate_delegable, theological).
narrative_ontology:cs_axiom('3943a78e-4e2a-47c7-b403-eff78d587c7f', foundational, effective_governance_requires_delegation).
narrative_ontology:cs_axiom_status(effective_governance_requires_delegation, holdable).
narrative_ontology:cs_axiom_grounding('3943a78e-4e2a-47c7-b403-eff78d587c7f', effective_governance_requires_delegation, instrumental).
narrative_ontology:cs_reference_frame('3943a78e-4e2a-47c7-b403-eff78d587c7f', bifurcated_sovereignty_delegated_governance).
narrative_ontology:cs_drift_state('3943a78e-4e2a-47c7-b403-eff78d587c7f', late_edo_period, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('3943a78e-4e2a-47c7-b403-eff78d587c7f', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, shogunate).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_class).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_court).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, loyalist_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, emperor).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, common_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ritual head of state, whose divine lineage grants ultimate legitimacy to the entire political system. The emperor's actual political power is suppressed, confined to symbolic acts and the granting of titles. Any attempt to exercise direct governance is met with institutional resistance from the shogunate.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, emperor, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, emperor, excluded).

% The de facto governing authority, exercising military and administrative control over the realm. Its legitimacy is derived from imperial delegation, which it actively maintains and enforces. It benefits from the stability provided by the emperor's symbolic authority without direct interference in governance.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, shogunate, agenda_setter,
    institutional, generational, constrained, national).

% The warrior class that forms the administrative and military backbone of the shogunate. They benefit from the social order and their privileged position within the delegated system of governance. Their loyalty is to the shogunate, which in turn derives its authority from the emperor.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_class, beneficiary,
    organized, biographical, constrained, regional).

% The aristocratic elite surrounding the emperor, whose political influence has been systematically curtailed. They bear the cost of their diminished status and lack of direct power, often living in relative poverty compared to their historical prominence. Their attempts to reassert imperial authority are suppressed.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_court, payer,
    powerless, generational, trapped, local).

% Groups, often scholars or regional lords, who believe in the direct exercise of imperial power and oppose the shogunate's delegated authority. They face suppression and political marginalization for challenging the established order.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, loyalist_factions, payer,
    moderate, biographical, constrained, regional).

% The vast majority of the population, who are governed by the shogunate and its delegated authorities. They bear the costs of taxation and conscription, and their lives are largely unaffected by the theoretical debates over imperial vs. delegated authority, as long as order is maintained.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, common_people, payer,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable framework for governance by separating the sacred, legitimizing authority of the emperor from the practical, administrative authority of the shogunate, thereby allowing for institutional continuity across changes in effective rulers.
% TRANSFER_FUNCTION: Transfers effective political power and administrative control from the imperial court to the shogunate and the samurai class, while retaining the emperor's symbolic legitimacy as the ultimate source of authority.
% ABSENT_VOICES: Any direct imperial voice advocating for unmediated governance is suppressed; loyalist factions who believe in direct imperial rule are marginalized. Their arguments for a unified, active imperial sovereignty are excluded from the dominant political discourse.
% DISAPPEARANCE_RATIONALE: If the system of imperial delegation to the shogunate vanished, the entire political structure would collapse. The shogunate would lose its primary source of legitimacy, leading to widespread civil unrest, power vacuums, and potentially a direct imperial restoration or a new form of governance.
% FOUNDING_PROBLEM: The historical problem of maintaining political stability and effective governance across a vast and often turbulent realm, while respecting the sacrosanct, divine authority of the emperor, who was often too distant or ritualistically constrained to govern directly.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political theorists outside the direct beneficiaries corroborate that the problem of balancing sacred authority with practical governance was a persistent challenge in pre-modern Japan. The solution, while extractive, provided a long period of relative stability.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is a Tangled Rope because it genuinely coordinates (stable governance through delegation) but also involves significant asymmetric extraction. Extractiveness (0.6) is high due to the transfer of real power and resources from the imperial court to the shogunate. Suppression (0.7) is substantial, as any direct imperial political involvement or loyalist challenge is actively put down. Theater ratio (0.4) reflects the performative maintenance of imperial rituals and titles, which serve to legitimize the shogunate's rule, even as the emperor's actual power is negligible. The metrics show a slight increase in extractiveness and suppression over time, reflecting the hardening of the delegated system, before a slight decline towards the end of the Edo period.
 *
 * PERSPECTIVAL GAP:
 *   The shogunate and samurai class would experience this as a legitimate and necessary coordination mechanism for stable governance, deriving their authority from the emperor's mandate. The imperial court and loyalist factions, however, would experience it as a snare, a system of enforced extraction that usurps the emperor's rightful, active role in governance. The common people, while paying taxes and labor, might view it as a stable, if distant, authority.
 *
 * DIRECTIONALITY LOGIC:
 *   The shogunate and samurai class are clear beneficiaries, as they wield power and collect resources under this system (low d). The emperor is a beneficiary of symbolic legitimacy but a victim of political disempowerment (complex d, leaning towards target for political agency). The imperial court and loyalist factions are direct victims, bearing the costs of their suppressed political aspirations (high d). The common people are diffuse payers, bearing the general costs of governance (moderate d).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the system as pure extraction by acknowledging its genuine coordination function (providing stable governance). However, it also highlights the significant and actively enforced extraction from the imperial court and loyalist factions, preventing it from being seen as a pure Rope. The 'contested' status of the founding problem further points to a potential mandatrophy, where the original coordination need (stable governance) is now served by an arrangement that has become overly extractive for certain parties, leading to calls for restoration of direct imperial rule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'Is the shogunate''s legitimacy primarily derived from imperial delegation, or from its effective military and administrative control?',
    'Analysis of historical periods where imperial delegation was weak or contested, but shogunate rule persisted, or vice-versa. Examination of popular acceptance vs. elite justification.',
    'If legitimacy is primarily derived from effective control, the emperor''s role is more theatrical, increasing the constraint''s theater_ratio and extractiveness. If delegation is primary, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Ambiguity in the true source of the shogunate''s political legitimacy.').

omega_variable(
    imperial_agency_potential,
    'To what extent did the emperor retain latent political agency, even under shogunate rule, and how might this have been activated?',
    'Detailed historical analysis of imperial court maneuvers, symbolic resistance, and the conditions under which imperial edicts could gain political traction despite shogunate suppression.',
    'If significant latent agency existed, the suppression metric might be higher than measured, as more active force was required to contain it. If agency was truly negligible, the emperor''s role is more purely symbolic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imperial_agency_potential, empirical, 'The extent of the emperor''s suppressed political agency.').

omega_variable(
    reading_difference_locus,
    'What is the precise structural element on which the ''Bakufu Delegation'' reading differs from the ''Loyalist Restoration'' reading?',
    'Compare the core axioms and reference frames of both readings. The difference lies in whether the emperor''s divine mandate requires active, unmediated governance (Loyalist) or can be legitimately delegated (Bakufu).',
    'This omega clarifies the conceptual boundary between the two readings, ensuring they are modeled as distinct constraints rather than observer-dependent views of the same one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_difference_locus, conceptual, 'Clarifies the core disagreement between the Bakufu Delegation and Loyalist Restoration readings of the Imperial Mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 1192, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1192, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1192, 0.3).
narrative_ontology:measurement(impe_tr_t1333, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1333, 0.35).
narrative_ontology:measurement(impe_tr_t1467, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1467, 0.38).
narrative_ontology:measurement(impe_tr_t1600, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1600, 0.4).
narrative_ontology:measurement(impe_tr_t1750, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1750, 0.42).
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1868, 0.4).

% Extraction over time
narrative_ontology:measurement(impe_be_t1192, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1192, 0.5).
narrative_ontology:measurement(impe_be_t1333, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1333, 0.55).
narrative_ontology:measurement(impe_be_t1467, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1467, 0.58).
narrative_ontology:measurement(impe_be_t1600, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1600, 0.6).
narrative_ontology:measurement(impe_be_t1750, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1750, 0.62).
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1868, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1192, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1192, 0.6).
narrative_ontology:measurement(impe_su_t1333, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1333, 0.65).
narrative_ontology:measurement(impe_su_t1467, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1467, 0.68).
narrative_ontology:measurement(impe_su_t1600, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1600, 0.7).
narrative_ontology:measurement(impe_su_t1750, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1750, 0.72).
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1868, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'imperial_mandate' kernel. It describes the system of delegated authority to the shogunate, contrasting with the 'loyalist_restoration_reading' which emphasizes direct imperial rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
