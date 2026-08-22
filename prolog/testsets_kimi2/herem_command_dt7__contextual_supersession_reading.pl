% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__contextual_supersession_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Command under Contextual Supersession
 *   domain: biblical_hermeneutics/religious_ethics/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the contextual_supersession_reading of
 *   the contested kernel herem_command_dt7 (Deuteronomy 7). Under this
 *   reading, herem was a historically-bounded directive for ancient Israel's
 *   settlement period, morally superseded by prophetic universalism or
 *   Christian covenant ethics. The constraint coordinates modern religious
 *   communities around a non-violent hermeneutic that retains the text
 *   without reinstate its ethnic violence. Two sibling readings exist:
 *   durable_separation_reading (timeless mandate for ethnic separation) and
 *   allegorical_displacement_reading (spiritual typology with no historical
 *   ethnic referents). This reading structurally forecloses durable
 *   separation while coexisting with allegorical displacement.
 *
 * KEY AGENTS:
 *   - mainstream_religious_authorities: Primary agenda_setter (institutional/mobile) â administers the supersession hermeneutic
 *   - inclusive_religious_communities: Primary beneficiary (organized/mobile) â benefits from delegitimized ethnic separation
 *   - interethnic_families: Secondary beneficiary (moderate/mobile) â liberated from intermarriage bans
 *   - fundamentalist_enclave_members: Primary target/payer (powerless/trapped) â bears residual coercion
 *   - biblical_scholars: Analytical observer (analytical/analytical) â corroborates historical horizon
 *   - separatist_enclave_leaders: Excluded voice (moderate/constrained) â enforces the literal reading outside this consensus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.18).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.28).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command under Contextual Supersession").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "biblical_hermeneutics/religious_ethics/commitment_system_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, '2e763cca-080d-4d67-b626-1b0d0d7a52ac').
narrative_ontology:cs_kernel_codification('2e763cca-080d-4d67-b626-1b0d0d7a52ac', fixed_text).
narrative_ontology:cs_authority_grounding('2e763cca-080d-4d67-b626-1b0d0d7a52ac', lineage).
narrative_ontology:cs_interpretation_layer_present('2e763cca-080d-4d67-b626-1b0d0d7a52ac').
narrative_ontology:cs_reading_relation('2e763cca-080d-4d67-b626-1b0d0d7a52ac', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('2e763cca-080d-4d67-b626-1b0d0d7a52ac', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('2e763cca-080d-4d67-b626-1b0d0d7a52ac', foundational, herem_temporally_bounded).
narrative_ontology:cs_axiom_status(herem_temporally_bounded, holdable).
narrative_ontology:cs_axiom_grounding('2e763cca-080d-4d67-b626-1b0d0d7a52ac', herem_temporally_bounded, empirically_contingent).
narrative_ontology:cs_axiom('2e763cca-080d-4d67-b626-1b0d0d7a52ac', foundational, universal_covenant_ethic).
narrative_ontology:cs_axiom_status(universal_covenant_ethic, holdable).
narrative_ontology:cs_axiom_grounding('2e763cca-080d-4d67-b626-1b0d0d7a52ac', universal_covenant_ethic, deontological).
narrative_ontology:cs_reference_frame('2e763cca-080d-4d67-b626-1b0d0d7a52ac', ancient_settlement_mandate).
narrative_ontology:cs_drift_state('2e763cca-080d-4d67-b626-1b0d0d7a52ac', prophetic_universalism_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('2e763cca-080d-4d67-b626-1b0d0d7a52ac', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, inclusive_religious_communities).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, interethnic_families).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, fundamentalist_enclave_members).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, prophetic_universalism).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, christian_covenant_ethic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer denominational and interpretive traditions that teach herem as a historically-limited mandate for ancient Israel's settlement; set curricula, liturgical readings, and ethical guidelines that locate the command in a superseded covenantal phase.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, mainstream_religious_authorities, agenda_setter,
    institutional, generational, mobile, global).

% Communities that embrace interethnic membership and universalist ethics; benefit from a canonical framework that retains Deuteronomy 7 without requiring its literal reinstatement, allowing continuity with scripture without ethnic separation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, inclusive_religious_communities, beneficiary,
    organized, generational, mobile, global).

% Families formed across ethnic lines that would be proscribed under a literal herem regime; the supersession reading removes the theological barrier to their belonging.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, interethnic_families, beneficiary,
    moderate, biographical, mobile, local).

% Members of isolated religious communities where leaders still enforce herem-style ethnic separation, intermarriage bans, and shunning of outsiders; they bear the psychological and social costs of a command the broader tradition has superseded.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, fundamentalist_enclave_members, payer,
    powerless, biographical, trapped, local).

% Historical-critical scholars who analyze the Deuteronomistic history and ancient Near Eastern conquest ideology; their work provides the empirical horizon that grounds the bounded-context reading.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, biblical_scholars, observer,
    analytical, civilizational, analytical, global).

% Leaders of separatist enclaves who continue to enforce ethnic boundary rules derived from herem; they are not participants in the supersession consensus and would reject its legitimacy, but their enforcement creates the narrow victim set that persists under this reading.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, separatist_enclave_leaders, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates continuity with a canonical conquest text across radically changed ethical and political conditions, by locating the command's obligating force in a non-repeatable historical episode.
% TRANSFER_FUNCTION: Moves authority over ethnic boundary maintenance from the literal text to historical-prophetic interpretation; moves affected persons from a category marked for separation to a category eligible for inclusion.
% ABSENT_VOICES: Separatist enclave leaders and durable-separation literalists are excluded from the supersession consensus; they would argue for the ongoing obligating force of the text but are not seated in the mainstream interpretive conversation. Allegorical-displacement advocates are partially marginalized in historical-critical discourse.
% DISAPPEARANCE_RATIONALE: If the contextual supersession framework vanished, mainstream communities would lose their primary theological mechanism for retaining Deuteronomy 7 without reinstate ethnic violence; literalist readings would gain legitimacy, and interethnic families would face renewed exclusionary pressure.
% FOUNDING_PROBLEM: How to maintain covenantal continuity with a text that commands ethnic conquest and separation after the historical conditions of conquest have ended and universalist ethics have emerged.
% FOUNDING_PROBLEM_CORROBORATION: Prophetic literature (Isaiah, Micah) and early Christian sources attest the universalist trajectory from seats outside the modern inclusive community's direct benefit; historical-critical biblical scholarship corroborates the non-repeatable ancient context. Durable-separation readers contest that the problem was ever solved by supersession, asserting instead that boundary maintenance remains the solution.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).
:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the supersession reading removes herem's obligating force for modern ethnic separation; only residual enclave enforcement produces extraction. Suppression is low-moderate (0.28) because the reading operates by theological consensus and canonical interpretation rather than coercion. Theater ratio is moderate (0.38): public reaffirmation of the text's historical boundedness can substitute for deeper ethical engagement with its violence. Accessibility collapse (0.45) reflects that literalist alternatives remain theologically available but are socially costly in mainstream communities. Resistance (0.42) comes from separatist holdouts and fundamentalist enclaves that reject supersession. The temporal series trace herem's trajectory from active conquest enforcement to residual performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (mainstream authorities) experiences the constraint as a successful coordination mechanism that preserves scripture and peace simultaneously. The beneficiary seats (inclusive communities, interethnic families) experience liberation and inclusion. The payer seat (fundamentalist enclave members) experiences the same textual heritage as a source of ongoing coercion. The excluded separatist leaders experience the supersession reading as illegitimate theological drift. These divergences are structurally derived from the same kernel read through different commitments.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainstream religious authorities and inclusive communities are structural beneficiaries of the supersession framework (low d), receiving theological coherence and ethnic inclusion. Interethnic families are direct beneficiaries (low d). Fundamentalist enclave members are the sole structural targets (high d), as they bear the costs of residual enforcement that the broader tradition has delegitimized. Biblical scholars occupy an analytical seat near symmetric d. Separatist enclave leaders are excluded from the consensus; if seated, they would read as high-d targets of the supersession reading's delegitimating force.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the contextual supersession reading, the herem kernel would likely be classified as a snare (under durable separation) or scaffold (under contextual reading in its ancient phase). The mandatrophy risk here is mislabeling the modern interpretive rope as either a timeless snare (importing ancient extraction into the present) or a naturalized mountain (treating the supersession as an inevitable moral law rather than a contested interpretive achievement). The classification as rope preserves the genuine coordination functionâmanaging a violent text non-violentlyâwhile the low metrics honestly report minimal modern extraction. The residual victim set prevents false benignity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    herem_kernel_reading_location,
    'This constraint instantiates the contextual_supersession_reading of kernel herem_command_dt7. A durable_separation_reading would treat herem as a timeless mandate, massively expanding the victim set; an allegorical_displacement_reading would deny historical ethnic referents entirely. Which structural elementâhistorical referent, temporal scope, or ethical continuityâis the actual locus of disagreement?',
    'Historical-critical and theological analysis of the text''s original context versus its canonical reception; comparison of epsilon and victim structures across the three sibling constraints.',
    'If the historical referent is denied, the contextual reading collapses toward allegory; if temporal scope is expanded, it collapses toward durable separation. The classification of this constraint as low-extraction rope depends on maintaining the bounded-historical claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(herem_kernel_reading_location, conceptual, 'Committer-frame location of this reading within the herem kernel family').

omega_variable(
    residual_coercion_separation,
    'Does the residual fundamentalist enforcement of herem belong to this constraint as a failure mode of incomplete supersession, or to the durable_separation_reading instantiated in enclaves?',
    'Ethnographic study of enclave theologyâwhether enforcers cite historical boundedness or timeless mandateâand comparison of their epsilon profiles against the durable_separation_reading.',
    'If residual enforcement is durable separation in practice, this constraint''s victim set and extraction should be near zero, sharpening the rope classification. If it is uncompleted supersession, the constraint carries a transitional extraction residue that blurs the rope/scaffold boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_coercion_separation, empirical, 'Whether residual enclave coercion belongs to this reading or the sibling').

omega_variable(
    suppression_mechanism_enclave,
    'In fundamentalist enclaves where herem-style separation is enforced, is the suppression structural (isolation, shunning, economic control) or internalized (identity fusion with the separatist community)?',
    'Post-exit trajectory study of enclave leavers: if suppression persists after physical exit, it is partially internalized.',
    'Internalized suppression would mean effective extraction exceeds the structural measure; structural suppression would mean extraction drops sharply upon exit. This affects the directionality assigned to fundamentalist_enclave_members.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_enclave, empirical, 'Structural vs internalized suppression mechanism for residual enclave coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(here_tr_t10, herem_command_dt7__contextual_supersession_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(here_tr_t25, herem_command_dt7__contextual_supersession_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(here_tr_t50, herem_command_dt7__contextual_supersession_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(here_tr_t75, herem_command_dt7__contextual_supersession_reading, theater_ratio, 75, 0.35).
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__contextual_supersession_reading, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(here_be_t10, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(here_be_t25, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(here_be_t50, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(here_be_t75, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 75, 0.22).
narrative_ontology:measurement(here_be_t100, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 100, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(here_su_t10, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(here_su_t25, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(here_su_t50, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 50, 0.35).
narrative_ontology:measurement(here_su_t75, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 75, 0.28).
narrative_ontology:measurement(here_su_t100, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 100, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% The herem_command_dt7 kernel decomposes into three structurally distinct constraints: contextual_supersession_reading (this file), durable_separation_reading, and allegorical_displacement_reading. Each reading carries a distinct epsilon, beneficiary/victim structure, and classification derived from its unique referent and structural commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
