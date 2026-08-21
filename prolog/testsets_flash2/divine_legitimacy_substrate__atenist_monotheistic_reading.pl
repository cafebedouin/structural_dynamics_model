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
 *   This constraint describes the Atenist monotheistic revolution under
 *   Pharaoh Akhenaten, where divine legitimacy was declared to flow
 *   exclusively through the pharaoh's revelation of Aten as the sole true
 *   deity. All other gods were deemed false, leading to the suppression of
 *   traditional cults, particularly that of Amun. This reading instantiates
 *   one specific interpretation of the 'divine_legitimacy_substrate' kernel,
 *   focusing on the radical centralization of power and belief.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.9).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.95).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, snare).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Monotheistic Reading of Divine Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "ancient_history/religious_studies/political_economy_of_belief_systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, '97899893-1a02-4f40-b503-b756140ebd16').
narrative_ontology:cs_kernel_codification('97899893-1a02-4f40-b503-b756140ebd16', formalized).
narrative_ontology:cs_authority_grounding('97899893-1a02-4f40-b503-b756140ebd16', lineage).
narrative_ontology:cs_reading_relation('97899893-1a02-4f40-b503-b756140ebd16', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('97899893-1a02-4f40-b503-b756140ebd16', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('97899893-1a02-4f40-b503-b756140ebd16', foundational, aten_sole_creator_deity).
narrative_ontology:cs_axiom_status(aten_sole_creator_deity, holdable).
narrative_ontology:cs_axiom_grounding('97899893-1a02-4f40-b503-b756140ebd16', aten_sole_creator_deity, theological).
narrative_ontology:cs_axiom('97899893-1a02-4f40-b503-b756140ebd16', foundational, pharaoh_sole_interpreter_of_aten).
narrative_ontology:cs_axiom_status(pharaoh_sole_interpreter_of_aten, holdable).
narrative_ontology:cs_axiom_grounding('97899893-1a02-4f40-b503-b756140ebd16', pharaoh_sole_interpreter_of_aten, theological).
narrative_ontology:cs_reference_frame('97899893-1a02-4f40-b503-b756140ebd16', pharaonic_divine_monopoly).
narrative_ontology:cs_drift_state('97899893-1a02-4f40-b503-b756140ebd16', post_akhenaten_restoration, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('97899893-1a02-4f40-b503-b756140ebd16', '').
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

% The sole interpreter and conduit of Aten's will, centralizing all religious and political authority. Benefits from the dismantling of rival power centers and the redirection of wealth to the royal court.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten, agenda_setter,
    institutional, biographical, arbitrage, national).

% Benefits from the elevated status and wealth derived from the pharaoh's unique position, becoming the primary recipients of state patronage and religious veneration alongside Aten.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, royal_family, beneficiary,
    powerful, generational, constrained, national).

% Suffers the complete dismantling of their temples, confiscation of their vast estates, and suppression of their religious practices. Their traditional role as intermediaries to the divine is abolished, leading to loss of power, wealth, and identity.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, payer,
    institutional, generational, trapped, national).

% Loses influence and patronage tied to traditional cults and local deities. Forced to conform to the new Atenist orthodoxy, often at the expense of their established social and economic networks.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_nobility, payer,
    powerful, biographical, constrained, regional).

% Deprived of traditional religious practices, access to local shrines, and the comfort of familiar deities. Forced to worship Aten through the pharaoh, which is a distant and abstract form of worship, leading to spiritual alienation and cultural disruption.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, common_worshippers, payer,
    powerless, immediate, identity_locked, local).

% Executes the pharaoh's decrees, dismantling temples, defacing monuments, and suppressing dissent. Benefits from direct royal favor and increased authority in enforcing the new religious order.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, military_enforcers, agenda_setter,
    organized, immediate, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes religious authority and belief around a single deity, theoretically unifying the kingdom under a singular divine mandate and eliminating internal religious conflicts.
% TRANSFER_FUNCTION: Transfers all religious, political, and economic power from the traditional priesthoods and local cults to the pharaoh and the royal court, along with the symbolic capital of divine favor.
% ABSENT_VOICES: The suppressed Amun priesthood and other traditional religious leaders, as well as the vast majority of common people whose spiritual needs were met by polytheistic practices, are silenced. Their objections would center on the destruction of sacred traditions and the imposition of an alien belief system.
% DISAPPEARANCE_RATIONALE: If the Atenist decree vanished, the traditional polytheistic system, particularly the cult of Amun, would immediately reassert itself. Temples would be rebuilt, old gods reinstated, and the pharaoh's unique divine status would revert to a more traditional role, fundamentally altering the political and religious landscape.
% FOUNDING_PROBLEM: The pharaoh perceived a threat to royal authority from the powerful and wealthy Amun priesthood, and sought to consolidate power by establishing a new, exclusive religious system directly controlled by the monarchy.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and archaeological evidence from the Amarna period, including the rapid dismantling of Amun's cult and the construction of Aten's city, corroborate the pharaoh's intent to centralize power and suppress rivals. The speed and scale of the changes indicate a direct challenge to an existing power structure.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is extremely high (0.9) because the system forcibly redirects all religious and economic capital to the pharaoh and the royal family, dismantling existing power structures. Suppression is near-total (0.95) due to the active, violent enforcement against traditional cults and the complete lack of alternatives for worship. Theater ratio is low (0.1) as the pharaoh genuinely believed in and actively promoted Aten, with little performative maintenance of a defunct system. Resistance is high (0.8) due to the widespread opposition from the populace and traditional elites, though this resistance was largely suppressed during Akhenaten's reign.
 *
 * PERSPECTIVAL GAP:
 *   From Akhenaten's perspective, this was a necessary and divinely ordained reform to purify religion and unify the state. From the perspective of the Amun priesthood and common worshippers, it was a tyrannical imposition that destroyed their spiritual world and economic livelihoods. The engine's classification will reflect the high extraction and suppression experienced by the victims, regardless of the pharaoh's stated intentions.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh Akhenaten and the royal family are the primary beneficiaries and agenda-setters, gaining absolute religious and political authority. The Amun priesthood, traditional nobility, and common worshippers are the victims, suffering immense loss of power, wealth, and spiritual comfort. Military enforcers are also beneficiaries, gaining power and favor through their role in implementing the new order.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_belief_vs_political_strategy,
    'To what extent was Akhenaten''s Atenism a genuine religious conviction versus a political strategy to consolidate power?',
    'Analysis of Akhenaten''s personal writings and artistic expressions for consistency and depth of religious devotion, compared with the political outcomes of his reforms.',
    'If primarily political, the extractiveness and suppression are more clearly intentional and less justified by genuine belief, strengthening the ''snare'' classification. If primarily religious, the constraint still functions as a snare for victims, but the agenda-setter''s directionality might be seen as less purely extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_belief_vs_political_strategy, conceptual, 'Ambiguity of Akhenaten''s motivation for Atenism.').

omega_variable(
    long_term_cultural_impact,
    'What would have been the long-term cultural and religious impact if Atenism had persisted beyond Akhenaten''s reign?',
    'Counterfactual historical analysis, comparing the trajectory of other monotheistic movements with the specific cultural context of ancient Egypt.',
    'If Atenism could have stabilized into a less extractive form, it might have evolved into a ''tangled_rope'' or even ''rope'' over generations. Its rapid collapse suggests its high extractiveness and suppression were unsustainable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_cultural_impact, empirical, 'Counterfactual persistence and evolution of Atenism.').

omega_variable(
    suppression_internalization,
    'Was the suppression of traditional beliefs purely structural (dismantling temples, banning rituals) or did it lead to internalized suppression among common worshippers?',
    'Archaeological evidence of hidden cult objects or private worship spaces, and textual analysis of non-royal funerary texts for continued references to traditional deities.',
    'If internalized, the effective suppression for common worshippers was even higher than the structural measures suggest, as they carried the suppression within their own belief systems, making exit even harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression mechanism for traditional beliefs.').


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
narrative_ontology:measurement(divi_be_t10, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 10, 0.88).
narrative_ontology:measurement(divi_be_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 17, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(divi_su_t5, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(divi_su_t10, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 10, 0.92).
narrative_ontology:measurement(divi_su_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 17, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
