% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Monotheistic Reading of Divine Legitimacy
 *   domain: ancient_history/religious_studies/political_economy_of_belief_systems
 *
 * SUMMARY:
 *   This constraint represents the Atenist monotheistic reading of divine
 *   legitimacy during the reign of Akhenaten. It asserts that divine
 *   authority flows exclusively through the pharaoh's revelation of Aten as
 *   the sole true deity, rendering all other gods false. This reading led to
 *   the dismantling of traditional temple economies, the suppression of
 *   polytheistic practices, and the pharaoh's establishment as the sole
 *   legitimate interpreter of the divine will. The constraint is structurally
 *   a snare, as it extracts immense power and wealth from the populace and
 *   traditional elites, enforced through severe suppression of alternatives.
 *
 * KEY AGENTS:
 *   - pharaoh_akhenaten: Primary beneficiary/agenda_setter (institutional/arbitrage) — consolidates all power.
 *   - amun_priesthood: Primary target/victim (organized/trapped) — suffers complete loss of power and identity.
 *   - common_worshippers: Primary target/victim (powerless/identity_locked) — forced to abandon traditional practices.
 *   - military_enforcers: Secondary agenda_setter (institutional/mobile) — actively suppresses dissent.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.92).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.95).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, snare).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Monotheistic Reading of Divine Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "ancient_history/religious_studies/political_economy_of_belief_systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, '884200b1-85fa-45ee-8dfa-2ee78143cd05').
narrative_ontology:cs_kernel_codification('884200b1-85fa-45ee-8dfa-2ee78143cd05', formalized).
narrative_ontology:cs_authority_grounding('884200b1-85fa-45ee-8dfa-2ee78143cd05', extraction).
narrative_ontology:cs_reading_relation('884200b1-85fa-45ee-8dfa-2ee78143cd05', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('884200b1-85fa-45ee-8dfa-2ee78143cd05', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('884200b1-85fa-45ee-8dfa-2ee78143cd05', foundational, aten_is_sole_deity).
narrative_ontology:cs_axiom_status(aten_is_sole_deity, holdable).
narrative_ontology:cs_axiom_grounding('884200b1-85fa-45ee-8dfa-2ee78143cd05', aten_is_sole_deity, theological).
narrative_ontology:cs_axiom('884200b1-85fa-45ee-8dfa-2ee78143cd05', foundational, pharaoh_is_sole_interpreter).
narrative_ontology:cs_axiom_status(pharaoh_is_sole_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('884200b1-85fa-45ee-8dfa-2ee78143cd05', pharaoh_is_sole_interpreter, theological).
narrative_ontology:cs_reference_frame('884200b1-85fa-45ee-8dfa-2ee78143cd05', pharaonic_divine_monopoly).
narrative_ontology:cs_drift_state('884200b1-85fa-45ee-8dfa-2ee78143cd05', post_akhenaten_restoration, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('884200b1-85fa-45ee-8dfa-2ee78143cd05', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, royal_family).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_nobility).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, common_worshippers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sole interpreter and prophet of Aten, consolidating all religious and political authority. Benefits immensely from the dismantling of rival power centers and the redirection of wealth to the new cult.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten, agenda_setter,
    institutional, biographical, arbitrage, national).

% Benefits from the enhanced status and wealth of the pharaoh, and the suppression of competing noble families and priestly estates. Their legitimacy is tied directly to Akhenaten's rule.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, royal_family, beneficiary,
    powerful, generational, constrained, national).

% Suffers complete loss of power, wealth, and influence as their temples are closed, their gods declared false, and their traditional roles abolished. Their very identity is under attack.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, payer,
    organized, generational, trapped, national).

% Loses traditional sources of power and patronage tied to the old cults. Forced to conform to the new religious order, often relocating to Akhetaten, the new capital, under close royal supervision.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_nobility, payer,
    powerful, generational, constrained, national).

% Forced to abandon traditional household gods and local cults, losing familiar spiritual practices and community structures. Their access to the divine is now mediated solely through the pharaoh, disrupting centuries of religious custom.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, common_worshippers, payer,
    powerless, biographical, identity_locked, local).

% Executes the pharaoh's decrees, dismantling old temples, defacing images of other gods, and suppressing dissent. Benefits from direct royal favor and increased authority in enforcing the new religious order.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, military_enforcers, agenda_setter,
    institutional, immediate, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes religious authority and belief around a single deity, theoretically unifying the kingdom under a single divine mandate and eliminating internal religious conflicts.
% TRANSFER_FUNCTION: Transfers all religious, economic, and political power from the traditional priesthoods and nobility to the pharaoh and his chosen administrators, along with the spiritual allegiance of the populace.
% ABSENT_VOICES: The silenced voices of the traditional priesthoods, local cult leaders, and common people who clung to their ancestral gods. Their dissent is actively suppressed, and their perspectives are excluded from any official discourse.
% DISAPPEARANCE_RATIONALE: If the Atenist decree vanished, the traditional Amun priesthood would immediately reassert its authority, old temples would reopen, and the populace would revert to polytheistic practices. The pharaoh's power would be severely curtailed, and the kingdom's religious and political landscape would revert to its previous state.
% FOUNDING_PROBLEM: The pharaoh perceived a threat to royal authority from the powerful and wealthy Amun priesthood, and a need to reassert the pharaoh's unique divine connection.
% FOUNDING_PROBLEM_CORROBORATION: Akhenaten's own inscriptions and decrees attest to the problem of priestly power and the need for a direct divine connection. Historians and archaeologists corroborate the historical context of the Amun priesthood's immense influence prior to Akhenaten's reign.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high because the constraint funnels all religious, economic, and political power to the pharaoh and his immediate circle, at the expense of all other institutions and individuals. Suppression is near-total, as the state actively persecutes rival cults, defaces monuments, and enforces conformity. Theater ratio is low because the enforcement is direct and brutal, with little pretense of coordination for the benefit of the suppressed. Accessibility collapse is high as all traditional religious alternatives are systematically eliminated. Resistance is high, reflecting the deep-seated opposition from those whose identities and livelihoods were tied to the old order.
 *
 * PERSPECTIVAL GAP:
 *   From Akhenaten's perspective, this was a necessary reform to restore true divine order and royal authority. From the perspective of the Amun priesthood and common worshippers, it was a tyrannical imposition that destroyed their spiritual world and economic stability. The engine's classification will reflect this divergence, showing a snare for the victims and a highly beneficial, self-serving structure for the pharaoh.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh Akhenaten and the royal family are full beneficiaries, as the constraint directly subsidizes their power and wealth. The Amun priesthood, traditional nobility, and common worshippers are full targets, bearing the costs of suppression, loss of identity, and economic disruption. Military enforcers are also beneficiaries, gaining power and favor by enforcing the new order.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a clear snare, not a tangled rope or scaffold. Its coordination story (unifying the kingdom under one god) is a thin cover for the massive, asymmetric extraction of power and resources by the pharaoh. There is no genuine collective action problem solved for the victims; their alternatives are suppressed, and their losses are concentrated. The high extractiveness and suppression, coupled with the clear beneficiaries and victims, prevent mislabeling it as a benign coordination mechanism. The founding problem (priestly power) is still 'live' from Akhenaten's perspective, but the solution is purely extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_divine_mandate_ambiguity,
    'Was Akhenaten''s revelation of Aten a genuine divine mandate, or a political maneuver to consolidate power?',
    'Theological or historical consensus on the nature of Akhenaten''s religious experience, if such evidence could ever be definitively established.',
    'If a genuine divine mandate, the constraint might be re-read as a mountain (natural law of the divine realm) from a theological perspective, though its earthly enforcement would remain extractive. If a political maneuver, its snare classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(true_divine_mandate_ambiguity, conceptual, 'Ambiguity regarding the source of Akhenaten''s religious claims.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (military force, legal decrees) or internalized (fear, ideological conversion)?',
    'Post-Akhenaten restoration: if polytheistic practices immediately resurfaced, suppression was primarily structural. If a significant Atenist minority persisted, some internalization occurred.',
    'If internalized, the constraint''s effective suppression was higher than the structural measure suggests, as some individuals carried the suppression with them even after the pharaoh''s death.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''atenist_monotheistic_reading'' of the ''divine_legitimacy_substrate'' kernel. What would change if the ''amun_polytheistic_reading'' or ''folk_syncretistic_reading'' were adopted?',
    'Analysis of historical records from the Amarna period and the post-Amarna restoration, comparing the structural features of each reading''s implementation.',
    'The ''amun_polytheistic_reading'' would shift power to the Amun priesthood and decentralize religious authority, likely resulting in a tangled_rope or rope. The ''folk_syncretistic_reading'' would further decentralize authority to local practices, likely resulting in a rope or even a mountain for local customs. This reading''s high extractiveness and suppression are specific to its monotheistic, pharaoh-centric claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of alternative readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divi_tr_t5, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(divi_tr_t10, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(divi_tr_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 17, 0.1).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(divi_be_t5, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(divi_be_t10, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 10, 0.9).
narrative_ontology:measurement(divi_be_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 17, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(divi_su_t5, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(divi_su_t10, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 10, 0.9).
narrative_ontology:measurement(divi_su_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 17, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'divine_legitimacy_substrate' kernel. Its high extractiveness and suppression are specific to the Atenist monotheistic claim, which actively suppresses alternative readings. The other readings (amun_polytheistic_reading, folk_syncretistic_reading) represent different structural arrangements of divine legitimacy with distinct extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
