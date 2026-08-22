% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Herem as Timeless Divine Mandate for Durable Separation
 *   domain: biblical_hermeneutics/religious_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the durable_separation_reading of the
 *   herem_command_dt7 kernel: Deuteronomy 7 is read as a timeless divine
 *   mandate requiring covenant communities to maintain categorical separation
 *   from designated outsiders, especially through intermarriage prohibition
 *   and social boundary enforcement. The reading presents the arrangement as
 *   necessary identity preservation (coordination), but structurally enforces
 *   asymmetric extraction: outsiders are constructed as contamination
 *   threats, members lose intimate autonomy, and communal authority gains
 *   legitimating power. The constraint is actively enforced through
 *   theological interpretation, communal sanction, and historically through
 *   violence. The authored metrics describe an extractive, actively enforced
 *   structure; the claimed type (tangled_rope) names the hybrid
 *   coordination-extraction character.
 *
 * KEY AGENTS:
 *   - communal_authority (institutional/identity_locked): agenda-setter and beneficiary â interprets and enforces the herem mandate, derives legitimacy from unchanging text
 *   - boundary_adherent_members (moderate/identity_locked): beneficiary â receive identity coherence and social belonging from enforced separation
 *   - intermarriage_seeking_members (powerless/trapped): payer â bear extraction of marriage and family autonomy
 *   - non_covenant_outsiders (powerless/trapped): payer â designated as contamination threats, excluded from participation
 *   - liberal_religious_scholars (moderate/mobile): excluded â offer rival readings but lack authority in this framework
 *   - secular_observers (institutional/analytical): observer â classify the mandate as discriminatory from outside the theological system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.78).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.75).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem as Timeless Divine Mandate for Durable Separation").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "biblical_hermeneutics/religious_ethics").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, '82587285-ed73-4d42-9f29-153041c74a17').
narrative_ontology:cs_kernel_codification('82587285-ed73-4d42-9f29-153041c74a17', fixed_text).
narrative_ontology:cs_authority_grounding('82587285-ed73-4d42-9f29-153041c74a17', lineage).
narrative_ontology:cs_interpretation_layer_present('82587285-ed73-4d42-9f29-153041c74a17').
narrative_ontology:cs_reading_relation('82587285-ed73-4d42-9f29-153041c74a17', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('82587285-ed73-4d42-9f29-153041c74a17', herem_command_dt7__allegorical_displacement_reading, forecloses).
narrative_ontology:cs_axiom('82587285-ed73-4d42-9f29-153041c74a17', foundational, herem_as_timeless_ethnic_boundary).
narrative_ontology:cs_axiom_status(herem_as_timeless_ethnic_boundary, holdable).
narrative_ontology:cs_axiom_grounding('82587285-ed73-4d42-9f29-153041c74a17', herem_as_timeless_ethnic_boundary, theological).
narrative_ontology:cs_axiom('82587285-ed73-4d42-9f29-153041c74a17', foundational, divine_contamination_logic).
narrative_ontology:cs_axiom_status(divine_contamination_logic, holdable).
narrative_ontology:cs_axiom_grounding('82587285-ed73-4d42-9f29-153041c74a17', divine_contamination_logic, theological).
narrative_ontology:cs_reference_frame('82587285-ed73-4d42-9f29-153041c74a17', covenant_fidelity_as_separation).
narrative_ontology:cs_drift_state('82587285-ed73-4d42-9f29-153041c74a17', contemporary_pluralist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('82587285-ed73-4d42-9f29-153041c74a17', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, communal_authority).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, boundary_adherent_members).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarriage_seeking_members).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, non_covenant_outsiders).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, divine_command_theory).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, covenantal_exclusivism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the herem mandate as timeless divine law; derives institutional legitimacy, social control, and boundary-defining power from the unchanging text. Cannot abandon the reading without dissolving its own authority and identity.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, communal_authority, agenda_setter,
    institutional, generational, identity_locked, national).

% Receive clear identity boundaries, social cohesion, and divine legitimation for group membership. Support the separation mandate because it resolves their perceived assimilation threat, though they bear diffuse costs of reduced social flexibility.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, boundary_adherent_members, beneficiary,
    moderate, biographical, identity_locked, local).

% Have romantic and family ties to outsiders blocked by the mandate. Face communal shunning, loss of status, or spiritual threats if they transgress. Their intimate autonomy is the direct target of extraction.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, intermarriage_seeking_members, payer,
    powerless, biographical, trapped, local).

% Designated as contamination threats to covenant identity. Excluded from social, economic, and marital participation with the community. Subject to hostility legitimated by divine command, with no standing to contest the theological frame.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, non_covenant_outsiders, payer,
    powerless, biographical, trapped, regional).

% Offer contextual and allegorical rival readings of the same text. Are marginalized or delegitimized by the communal authority; their frameworks are ruled out as compromise or apostasy within the durable-separation system.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, liberal_religious_scholars, excluded,
    moderate, generational, mobile, national).

% Monitor the mandate's effects from outside the theological framework. Classify it as discriminatory or incitement under human-rights norms, but lack standing to alter the interpretive community's internal commitments.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, secular_observers, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__durable_separation_reading, diffuse).
narrative_ontology:fixing_cost_class(herem_command_dt7__durable_separation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a distinct religious-cultural identity across generations by enforcing bounded membership, regulating intermarriage, and preventing assimilation into surrounding populations through categorical social separation.
% TRANSFER_FUNCTION: Moves autonomy over marriage and intimate association away from individual members and over social participation away from outsiders, transferring legitimating power to communal authority and identity-security to adherent members.
% ABSENT_VOICES: Liberal theologians advocating contextual or allegorical readings are structurally excluded; outsiders designated as contamination threats have no standing to contest their classification; members who would choose intermarriage are silenced by communal sanction and theological guilt.
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight, the community's categorical boundary between insider and outsider would dissolve, intermarriage restrictions would collapse, communal authority would lose its core legitimating claim, and the social structure would reorganize around open association.
% FOUNDING_PROBLEM: Ancient Israel's vulnerability to religious syncretism and cultural assimilation in a polytheistic Levantine context; the need to maintain covenant identity across generations amid hostile or seductive neighboring cultures.
% FOUNDING_PROBLEM_CORROBORATION: Communal authority and boundary-adherent members attest the problem is live, citing ongoing assimilation threats. Liberal religious scholars, secular historians, and sociological evidence from outside the benefiting parties attest the problem was historically specific to Iron Age state formation and is now a cover for ethno-religious boundary maintenance.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.78) is high because the constraint strips intimate autonomy from members and social standing from outsiders in exchange for identity coherence that could be achieved through less extractive means. Suppression (0.75) is high because the mandate is enforced through divine-command legitimation (hard to contest from inside), communal shunning, and historically violence. Theater ratio (0.45) reflects moderate performative maintenance: separation rituals and boundary policing exceed what is strictly necessary for cultural continuity. Accessibility collapse (0.80) is high because once the divine-command frame is accepted, alternative arrangements appear theologically impossible. Resistance (0.50) is moderate: strong external resistance from secular and liberal seats, but low internal resistance due to identity-lock.
 *
 * PERSPECTIVAL GAP:
 *   The communal authority seat experiences the constraint as sacred trust and fiduciary coordination; the intermarriage-seeking member and outsider seats experience it as coercive extraction backed by transcendent threats. The engine computes this divergence from the structural data: identical textual content produces opposite directionality values depending on beneficiary-victim position and exit options (identity_locked vs trapped).
 *
 * DIRECTIONALITY LOGIC:
 *   Communal authority and boundary-adherent members are declared beneficiaries with identity-locked or constrained exit, yielding low directionality (beneficiary side). Intermarriage-seeking members and non-covenant outsiders are declared victims with trapped exit, yielding high directionality (target side). The transfer flows from payer autonomy to beneficiary identity and authority legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, this constraint could be misread as pure coordination (a rope preserving a minority culture) or pure extraction (a snare demonizing outsiders). The tangled_rope gate requires naming both the coordination function (identity preservation) and the victim set (outsiders, restricted members), preventing either reduction. If the founding problem (ancient assimilation threat) were unequivocally dead, the drift path would point toward piton or snare; here the founding problem is contested, so the hybrid classification holds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression enforced externally through communal sanctions and violence, or internalized through theological identity fusion that makes exit unthinkable?',
    'Post-exit trajectory observation: if suppression persists after the agent leaves the community, the mechanism is partially internalized.',
    'If internalized, effective extraction exceeds the structural measure because the target carries the constraint after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    coordination_extraction_separability,
    'Can the genuine coordination function of identity preservation be structurally separated from the outsider exclusion and autonomy extraction, or is the extraction inherent to the coordination?',
    'Comparative analysis of communities that maintain distinct identity without categorical herem-style separation.',
    'If separable, the constraint is a tangled rope with separable components; if inseparable, it collapses toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether identity coordination requires outsider exclusion').

omega_variable(
    kernel_authority_vs_text,
    'Does the constraint derive from the textual kernel itself, or from the interpretive authority''s choice to fix the durable-separation reading as the plain sense?',
    'Text-critical and reception-history analysis of whether the reading is compelled by the text or stabilized by institutional repetition.',
    'If authority-derived, the constraint is a commitment-system extraction structure riding on a fixed text; if text-compelled, the extraction is encoded in the kernel itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_authority_vs_text, conceptual, 'Source of the constraint''s binding force: text or authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(here_tr_t10, herem_command_dt7__durable_separation_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__durable_separation_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(here_tr_t30, herem_command_dt7__durable_separation_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__durable_separation_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(here_tr_t50, herem_command_dt7__durable_separation_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(here_be_t10, herem_command_dt7__durable_separation_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__durable_separation_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(here_be_t30, herem_command_dt7__durable_separation_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__durable_separation_reading, base_extractiveness, 40, 0.73).
narrative_ontology:measurement(here_be_t50, herem_command_dt7__durable_separation_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(here_su_t10, herem_command_dt7__durable_separation_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__durable_separation_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(here_su_t30, herem_command_dt7__durable_separation_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(here_su_t40, herem_command_dt7__durable_separation_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(here_su_t50, herem_command_dt7__durable_separation_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% This constraint is the durable-separation reading of the herem_command_dt7 kernel; siblings contextual_supersession_reading and allegorical_displacement_reading instantiate structurally distinct constraints from the same text, linked by shared kernel origin but divergent epsilon values and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
