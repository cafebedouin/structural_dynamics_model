% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__durable_separation_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Herem Command (Durable Separation Reading): Timeless Divine Mandate for Identity Preservation
 *   domain: religious_ethics/commitment_system
 *
 * SUMMARY:
 *   This constraint represents the 'durable separation' reading of the Herem
 *   command, which interprets it as a timeless divine mandate for the
 *   covenant community to preserve its identity through strict separation
 *   from designated outsiders. This reading leads to high extractiveness on
 *   individual autonomy, particularly regarding intermarriage, and can
 *   legitimize violence or severe social exclusion against those deemed
 *   'outside the covenant.' This is one reading of the 'herem_command_dt7'
 *   kernel, distinct from 'contextual_supersession_reading' and
 *   'allegorical_displacement_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.88).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.92).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, snare).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem Command (Durable Separation Reading): Timeless Divine Mandate for Identity Preservation").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "religious_ethics/commitment_system").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, '9d24d02c-cb34-49e6-a78a-2ee8301b545e').
narrative_ontology:cs_kernel_codification('9d24d02c-cb34-49e6-a78a-2ee8301b545e', fixed_text).
narrative_ontology:cs_authority_grounding('9d24d02c-cb34-49e6-a78a-2ee8301b545e', lineage).
narrative_ontology:cs_interpretation_layer_present('9d24d02c-cb34-49e6-a78a-2ee8301b545e').
narrative_ontology:cs_reading_relation('9d24d02c-cb34-49e6-a78a-2ee8301b545e', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('9d24d02c-cb34-49e6-a78a-2ee8301b545e', herem_command_dt7__allegorical_displacement_reading, forecloses).
narrative_ontology:cs_axiom('9d24d02c-cb34-49e6-a78a-2ee8301b545e', foundational, divine_mandate_timeless_and_universal).
narrative_ontology:cs_axiom_status(divine_mandate_timeless_and_universal, holdable).
narrative_ontology:cs_axiom_grounding('9d24d02c-cb34-49e6-a78a-2ee8301b545e', divine_mandate_timeless_and_universal, theological).
narrative_ontology:cs_axiom('9d24d02c-cb34-49e6-a78a-2ee8301b545e', foundational, identity_preservation_requires_categorical_separation).
narrative_ontology:cs_axiom_status(identity_preservation_requires_categorical_separation, holdable).
narrative_ontology:cs_axiom_grounding('9d24d02c-cb34-49e6-a78a-2ee8301b545e', identity_preservation_requires_categorical_separation, conventional).
narrative_ontology:cs_reference_frame('9d24d02c-cb34-49e6-a78a-2ee8301b545e', original_divine_command_unmodified).
narrative_ontology:cs_drift_state('9d24d02c-cb34-49e6-a78a-2ee8301b545e', contemporary_pluralistic_society, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9d24d02c-cb34-49e6-a78a-2ee8301b545e', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_leaders).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_members).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, designated_outsiders).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarried_individuals).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, dissenting_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the Herem command as a timeless divine mandate, ensuring strict adherence to separation from designated outsiders. They benefit from the preservation of communal identity and their authority as interpreters of divine will.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_leaders, agenda_setter,
    institutional, generational, identity_locked, local).

% Adhere to the command, believing it ensures their spiritual purity and collective identity. They benefit from a clear sense of belonging and divine favor, but bear the cost of restricted social interaction and potential internal conflict.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_members, beneficiary,
    moderate, biographical, identity_locked, local).

% Are categorically separated from the covenant community, often facing social exclusion, economic disadvantage, or even violence, based on their identity as 'outsiders' rather than any specific action. They have no recourse within the framework of the Herem command.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, designated_outsiders, payer,
    powerless, immediate, trapped, local).

% Face severe social and religious sanctions for violating the separation mandate through marriage with outsiders. They are forced to choose between their identity within the community and their family, often leading to ostracization or forced divorce.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, intermarried_individuals, payer,
    powerless, biographical, identity_locked, local).

% Question the timeless applicability or severity of the Herem command, often advocating for more inclusive interpretations. They face social pressure, accusations of heresy, and potential excommunication for challenging the established reading.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, dissenting_community_members, payer,
    moderate, biographical, constrained, local).

% Analyze the historical and textual context of the Herem command, often highlighting its ancient Near Eastern parallels and specific historical circumstances, which may challenge its timeless application.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, historical_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation of a distinct communal identity and religious purity by establishing clear boundaries and rules for interaction with designated outsiders, ensuring adherence to a perceived divine covenant.
% TRANSFER_FUNCTION: Transfers social and religious legitimacy, as well as a sense of divine favor, to the covenant community members, while extracting autonomy, social integration, and sometimes life itself from designated outsiders and those who violate the separation.
% ABSENT_VOICES: The designated outsiders themselves are entirely absent from the interpretive and enforcement process; their perspective on the justice or necessity of their categorical separation is never considered. Advocates for universal human rights and interfaith dialogue are also excluded.
% DISAPPEARANCE_RATIONALE: If the Herem command, as interpreted by this reading, vanished overnight, the covenant community's identity structure would collapse, intermarriage would become permissible, and the designated outsiders would no longer face categorical exclusion. The social and religious landscape would fundamentally reorganize around principles of inclusion and individual autonomy.
% FOUNDING_PROBLEM: The problem of maintaining a distinct, divinely chosen identity and preventing assimilation or spiritual contamination by surrounding cultures and peoples during the formation of the covenant community.
% FOUNDING_PROBLEM_CORROBORATION: Covenant community leaders and many members attest that the threat of assimilation and spiritual contamination remains live in contemporary society. Historical scholars, while acknowledging the ancient context, often point to the enduring human need for group identity and boundary maintenance as a corroborating factor, even if they dispute the divine mandate.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__durable_separation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(herem_command_dt7__durable_separation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the command, under this reading, demands significant sacrifices of individual liberty and social integration for the sake of communal purity and identity. Suppression is also very high (0.92) as the enforcement relies on strong social, religious, and sometimes physical coercion to maintain boundaries and punish transgressions. The theater ratio is low (0.1) because the command is actively and genuinely enforced, with little performative maintenance; its function is direct and severe. Accessibility collapse is high (0.75) as alternatives to strict adherence are severely curtailed, and resistance is moderate (0.4) due to the strong social and religious pressures against dissent.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of covenant community leaders, this constraint is a necessary 'rope' for identity preservation and divine obedience. From the perspective of designated outsiders and intermarried individuals, it is a severe 'snare' that extracts their autonomy and well-being. The engine's classification will reflect the high extractiveness and suppression, likely classifying it as a snare from most seats, despite the internal 'rope' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Covenant community leaders are clear agenda-setters and beneficiaries, deriving authority and identity from enforcing the command. Community members are beneficiaries of a strong collective identity but also payers through restricted autonomy. Designated outsiders and intermarried individuals are clear victims, bearing the full cost of exclusion and sanction. Dissenting members are payers who challenge the constraint from within.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_naturalness,
    'Is the Herem command, as interpreted by this reading, a genuine divine mandate (mountain) or a human-constructed constraint (snare) justified by an appeal to divine authority?',
    'Theological and philosophical analysis of divine command theory, combined with empirical study of the command''s historical and social function in different contexts. Resolution is primarily conceptual and theological.',
    'If a genuine divine mandate, its extractiveness might be re-evaluated as an inherent cost of obedience. If a human construct, its high extractiveness and suppression would firmly classify it as a snare, with the divine appeal serving as a legitimizing cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_mandate_naturalness, conceptual, 'Ambiguity between divine command as natural law and human interpretation as constructed constraint.').

omega_variable(
    identity_preservation_necessity,
    'Is the strict, categorical separation mandated by this reading genuinely necessary for the preservation of the covenant community''s identity, or are there alternative, less extractive means?',
    'Comparative sociological and anthropological studies of identity formation in other religious communities, examining the efficacy and costs of various boundary-maintenance strategies. Counterfactual historical analysis.',
    'If less extractive alternatives exist and are viable, the current level of extraction and suppression would be re-evaluated as excessive and unnecessary, reinforcing a snare classification. If strict separation is shown to be uniquely effective, it would temper the extractiveness assessment, though not eliminate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_preservation_necessity, empirical, 'Whether the level of separation is proportional to the goal of identity preservation.').

omega_variable(
    violence_legitimation_ambiguity,
    'Does this reading''s emphasis on categorical separation and divine mandate inherently legitimize violence or severe harm against designated outsiders, or is such harm a misapplication of the core principle?',
    'Textual analysis of interpretive traditions within this reading, combined with historical case studies of its application. Theological and ethical debate on the limits of divine command ethics.',
    'If violence is an inherent outcome, the constraint''s extractiveness and suppression are even higher than measured, and its classification as a snare is reinforced. If it''s a misapplication, the core constraint might be less extractive, but the interpretive tradition still carries a high risk of harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_legitimation_ambiguity, conceptual, 'Whether the reading inherently legitimizes violence or if violence is a misapplication.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__durable_separation_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__durable_separation_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(here_tr_t60, herem_command_dt7__durable_separation_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(here_tr_t80, herem_command_dt7__durable_separation_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__durable_separation_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__durable_separation_reading, base_extractiveness, 20, 0.86).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__durable_separation_reading, base_extractiveness, 40, 0.87).
narrative_ontology:measurement(here_be_t60, herem_command_dt7__durable_separation_reading, base_extractiveness, 60, 0.88).
narrative_ontology:measurement(here_be_t80, herem_command_dt7__durable_separation_reading, base_extractiveness, 80, 0.88).
narrative_ontology:measurement(here_be_t100, herem_command_dt7__durable_separation_reading, base_extractiveness, 100, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__durable_separation_reading, suppression_requirement, 20, 0.91).
narrative_ontology:measurement(here_su_t40, herem_command_dt7__durable_separation_reading, suppression_requirement, 40, 0.92).
narrative_ontology:measurement(here_su_t60, herem_command_dt7__durable_separation_reading, suppression_requirement, 60, 0.92).
narrative_ontology:measurement(here_su_t80, herem_command_dt7__durable_separation_reading, suppression_requirement, 80, 0.92).
narrative_ontology:measurement(here_su_t100, herem_command_dt7__durable_separation_reading, suppression_requirement, 100, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
