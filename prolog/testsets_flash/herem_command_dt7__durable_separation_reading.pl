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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Herem Command (Durable Separation Reading)
 *   domain: religious_ethics/commitment_system
 *
 * SUMMARY:
 *   This constraint represents the 'durable separation' reading of the Herem
 *   command (DT7), which interprets it as a timeless divine mandate for the
 *   preservation of a distinct religious identity through strict boundaries
 *   and categorical separation from designated outsiders. This reading
 *   emphasizes the ongoing necessity of purity and the dangers of
 *   assimilation, often legitimizing severe social and, historically,
 *   physical exclusion. The constraint is framed as a snare due to its high
 *   extraction of individual autonomy and severe suppression of alternatives,
 *   with identifiable victims.
 *
 * KEY AGENTS:
 *   - religious_authorities: Agenda-setter (institutional/identity_locked) — interpret and enforce the command, benefiting from derived authority.
 *   - covenant_community_members: Beneficiary (organized/identity_locked) — gain identity cohesion but bear costs of strict boundaries.
 *   - intermarrying_members: Payer (powerless/trapped) — face severe ostracization for violating separation.
 *   - designated_outsiders: Payer (powerless/trapped) — are categorically separated and demonized.
 *   - secular_human_rights_advocates: Observer (institutional/analytical) — critique the command's application against universal rights.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.9).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.95).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, snare).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem Command (Durable Separation Reading)").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "religious_ethics/commitment_system").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, 'ce819ec7-06be-435f-843e-305db2bc737e').
narrative_ontology:cs_kernel_codification('ce819ec7-06be-435f-843e-305db2bc737e', fixed_text).
narrative_ontology:cs_authority_grounding('ce819ec7-06be-435f-843e-305db2bc737e', lineage).
narrative_ontology:cs_interpretation_layer_present('ce819ec7-06be-435f-843e-305db2bc737e').
narrative_ontology:cs_reading_relation('ce819ec7-06be-435f-843e-305db2bc737e', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('ce819ec7-06be-435f-843e-305db2bc737e', herem_command_dt7__allegorical_displacement_reading, forecloses).
narrative_ontology:cs_axiom('ce819ec7-06be-435f-843e-305db2bc737e', foundational, divine_mandate_for_categorical_separation_is_timeless).
narrative_ontology:cs_axiom_status(divine_mandate_for_categorical_separation_is_timeless, holdable).
narrative_ontology:cs_axiom_grounding('ce819ec7-06be-435f-843e-305db2bc737e', divine_mandate_for_categorical_separation_is_timeless, theological).
narrative_ontology:cs_axiom('ce819ec7-06be-435f-843e-305db2bc737e', foundational, identity_purity_requires_strict_boundary_enforcement).
narrative_ontology:cs_axiom_status(identity_purity_requires_strict_boundary_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('ce819ec7-06be-435f-843e-305db2bc737e', identity_purity_requires_strict_boundary_enforcement, conventional).
narrative_ontology:cs_reference_frame('ce819ec7-06be-435f-843e-305db2bc737e', original_divine_command_for_separation).
narrative_ontology:cs_drift_state('ce819ec7-06be-435f-843e-305db2bc737e', contemporary_globalized_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ce819ec7-06be-435f-843e-305db2bc737e', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_identity).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, religious_authorities).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarrying_members).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, designated_outsiders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the Herem command as a timeless divine mandate for the preservation of the covenant community's identity. They benefit from the authority derived from this interpretation and the clear boundaries it establishes.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, religious_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from a clear, divinely sanctioned identity and a sense of purity and separation from perceived threats. Their social cohesion is reinforced by adherence to the Herem command, but they also bear the cost of strict social boundaries.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_members, beneficiary,
    organized, generational, identity_locked, national).

% Face severe social ostracization, excommunication, or even violence for violating the categorical separation. Their autonomy in choosing partners is completely suppressed, and they are treated as a threat to the community's purity.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, intermarrying_members, payer,
    powerless, biographical, trapped, local).

% Are categorically separated and often demonized as a threat to the covenant community's identity. In historical contexts, this could lead to violence or displacement. Their very existence is framed as a challenge to the community's purity.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, designated_outsiders, payer,
    powerless, generational, trapped, regional).

% Observe and critique the Herem command's application, particularly its implications for individual autonomy, interfaith relations, and potential for violence. They advocate for universal human rights principles that often conflict with this reading.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, secular_human_rights_advocates, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation of a distinct religious and cultural identity by establishing clear boundaries and rules for membership and interaction with external groups, ensuring internal cohesion and purity.
% TRANSFER_FUNCTION: Transfers social and spiritual purity, as well as institutional authority, to the covenant community and its leaders, by extracting autonomy, social inclusion, and sometimes life from those who violate or are designated as outside the prescribed boundaries.
% ABSENT_VOICES: The voices of those who seek integration, intermarriage, or peaceful coexistence with designated outsiders are actively suppressed or excluded from the interpretive discourse. Their perspectives are framed as threats to the divine mandate.
% DISAPPEARANCE_RATIONALE: If the Herem command, as interpreted by this reading, vanished overnight, the covenant community would face an immediate identity crisis. Boundaries would blur, intermarriage would increase, and the authority structure built on enforcing separation would collapse, leading to a profound reorganization of social and religious life.
% FOUNDING_PROBLEM: The problem of maintaining a distinct, divinely chosen identity and preventing assimilation or contamination by surrounding cultures and beliefs, particularly during periods of settlement or cultural mixing.
% FOUNDING_PROBLEM_CORROBORATION: Religious authorities within the tradition attest that the problem of identity preservation and cultural assimilation remains live, citing contemporary challenges to religious distinctiveness. External sociological observers corroborate that identity maintenance is a persistent concern for many religious communities, though they dispute the necessity or morality of Herem as a solution.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).

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
 *   Extractiveness is very high (0.9) because this reading demands significant sacrifices of individual autonomy (e.g., choice of spouse) and can lead to severe consequences for designated outsiders. Suppression is also very high (0.95) as the command is presented as a divine, non-negotiable mandate, with strong social and institutional enforcement mechanisms that actively suppress dissent or alternative interpretations. Theater ratio is low (0.1) because the command's function, as understood by this reading, is genuinely enacted through strict enforcement, not merely performed. Resistance is high (0.7) due to the severe nature of the extraction and suppression, leading to internal and external challenges.
 *
 * PERSPECTIVAL GAP:
 *   Religious authorities and many covenant community members experience this as a necessary, divinely ordained structure for identity preservation, thus a form of identity coordination. However, intermarrying members and designated outsiders experience it as a severe snare, extracting their autonomy and well-being through coercive separation. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious authorities are clear beneficiaries (d=0.0-0.1) as they gain immense authority and legitimacy from enforcing this divine mandate. Covenant community members are also beneficiaries (d=0.1-0.2) through reinforced identity and cohesion, though they bear some costs. Intermarrying members and designated outsiders are clear targets (d=0.9-1.0), facing severe extraction and suppression with no viable exit. Secular human rights advocates are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of Herem is not subject to mandatrophy in the conventional sense, as its mandate is considered timeless and divinely ordained. The persistence is not due to inertia but to active, ideologically driven enforcement. The classification as a snare prevents mislabeling it as a 'natural' or 'necessary' part of religious identity, highlighting its coercive and extractive nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_human_interpretation,
    'Is the ''timeless divine mandate'' an intrinsic property of the Herem command, or a specific human interpretation that serves to legitimize identity-based exclusion?',
    'Comparative theological and historical analysis of interpretive traditions, focusing on the socio-political contexts in which this reading gained prominence versus alternative readings.',
    'If it''s primarily a human interpretation, the constraint''s ''divine'' grounding is a cover story, increasing its effective extractiveness and suppression by masking its constructed nature. If genuinely divine, the moral calculus shifts, though the structural extraction remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_vs_human_interpretation, conceptual, 'Ambiguity between divine command and human interpretive choice.').

omega_variable(
    identity_preservation_necessity,
    'Is the level of categorical separation and exclusion mandated by this reading genuinely necessary for the preservation of the covenant community''s identity, or are less extractive forms of identity maintenance possible?',
    'Sociological and anthropological studies of other religious communities that maintain distinct identities without such severe exclusionary practices, or historical analysis of periods where this community adopted less rigid boundaries.',
    'If less extractive forms are viable, the current level of extraction and suppression is revealed as excessive and unnecessary for the stated coordination function, pushing the classification more firmly towards snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_preservation_necessity, empirical, 'Necessity of severe separation for identity preservation.').

omega_variable(
    violence_legitimation_scope,
    'To what extent does this reading of Herem legitimize or encourage violence against designated outsiders in contemporary contexts, beyond historical applications?',
    'Analysis of contemporary sermons, theological treatises, and community practices that explicitly or implicitly invoke Herem in relation to non-members, and documented instances of violence or discrimination linked to such interpretations.',
    'If it actively legitimizes contemporary violence, the constraint''s suppression and extractiveness are amplified to their most extreme forms, with potential for physical harm as a direct outcome. If it is purely historical, the impact is primarily social and psychological.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(violence_legitimation_scope, empirical, 'Scope of violence legitimation in contemporary application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(here_tr_t25, herem_command_dt7__durable_separation_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(here_tr_t50, herem_command_dt7__durable_separation_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(here_tr_t75, herem_command_dt7__durable_separation_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__durable_separation_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(here_be_t25, herem_command_dt7__durable_separation_reading, base_extractiveness, 25, 0.88).
narrative_ontology:measurement(here_be_t50, herem_command_dt7__durable_separation_reading, base_extractiveness, 50, 0.9).
narrative_ontology:measurement(here_be_t75, herem_command_dt7__durable_separation_reading, base_extractiveness, 75, 0.91).
narrative_ontology:measurement(here_be_t100, herem_command_dt7__durable_separation_reading, base_extractiveness, 100, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(here_su_t25, herem_command_dt7__durable_separation_reading, suppression_requirement, 25, 0.92).
narrative_ontology:measurement(here_su_t50, herem_command_dt7__durable_separation_reading, suppression_requirement, 50, 0.95).
narrative_ontology:measurement(here_su_t75, herem_command_dt7__durable_separation_reading, suppression_requirement, 75, 0.94).
narrative_ontology:measurement(here_su_t100, herem_command_dt7__durable_separation_reading, suppression_requirement, 100, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Herem command (DT7) kernel. Other readings (contextual_supersession_reading, allegorical_displacement_reading) offer alternative interpretations with different structural implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
