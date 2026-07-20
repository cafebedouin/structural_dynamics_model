% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Herem as Timeless Durable Separation Mandate
 *   domain: biblical_hermeneutics/religious_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the durable_separation reading of the herem
 *   command in Deuteronomy 7, which treats the mandate to destroy and
 *   separate from Canaanite nations as a timeless divine obligation for
 *   covenant identity preservation. The reading produces severe extraction
 *   from outsiders and restricted members while coordinating in-group
 *   boundaries. It is structurally a tangled rope: a genuine coordination
 *   function for identity maintenance layered with asymmetric extraction
 *   through intermarriage prohibition, outsider dehumanization, and violence
 *   legitimation under divine command obedience.
 *
 * KEY AGENTS:
 *   - covenant_community_members (beneficiary/organized): receive identity coherence and bounded preservation through separation
 *   - intermarriage_autonomy_seekers (payer/powerless): bear the cost of foregone marriage and kinship ties under the mandate
 *   - non_covenant_outsiders (payer/powerless): designated as contaminating threats subject to exclusion or violence
 *   - boundary_administrators (agenda_setter/institutional): interpret and enforce the mandate, adjudicating membership limits
 *   - universalist_prophets (excluded/moderate): voices advancing covenantal expansion or universalism, marginalized within this reading
 *   - modern_critical_scholars (observer/analytical): academic analysts of the text and its ethical effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.82).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.79).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem as Timeless Durable Separation Mandate").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "biblical_hermeneutics/religious_ethics").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, '9cce8d79-be83-47ac-abde-b79ee7b1139c').
narrative_ontology:cs_kernel_codification('9cce8d79-be83-47ac-abde-b79ee7b1139c', fixed_text).
narrative_ontology:cs_authority_grounding('9cce8d79-be83-47ac-abde-b79ee7b1139c', lineage).
narrative_ontology:cs_interpretation_layer_present('9cce8d79-be83-47ac-abde-b79ee7b1139c').
narrative_ontology:cs_reading_relation('9cce8d79-be83-47ac-abde-b79ee7b1139c', herem_command_dt7__contextual_supersession_reading, influences).
narrative_ontology:cs_reading_relation('9cce8d79-be83-47ac-abde-b79ee7b1139c', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('9cce8d79-be83-47ac-abde-b79ee7b1139c', foundational, herem_timeless_covenant_boundary).
narrative_ontology:cs_axiom_status(herem_timeless_covenant_boundary, holdable).
narrative_ontology:cs_axiom_grounding('9cce8d79-be83-47ac-abde-b79ee7b1139c', herem_timeless_covenant_boundary, deontological).
narrative_ontology:cs_axiom('9cce8d79-be83-47ac-abde-b79ee7b1139c', foundational, outsider_contamination_necessity).
narrative_ontology:cs_axiom_status(outsider_contamination_necessity, holdable).
narrative_ontology:cs_axiom_grounding('9cce8d79-be83-47ac-abde-b79ee7b1139c', outsider_contamination_necessity, theological).
narrative_ontology:cs_reference_frame('9cce8d79-be83-47ac-abde-b79ee7b1139c', deuteronomic_covenant_fidelity).
narrative_ontology:cs_drift_state('9cce8d79-be83-47ac-abde-b79ee7b1139c', prophetic_universalist_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9cce8d79-be83-47ac-abde-b79ee7b1139c', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_members).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarriage_autonomy_seekers).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, non_covenant_outsiders).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, divine_command_theory).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, categorical_separation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive bounded membership and categorical separation as the mechanism of divine promise preservation; their collective identity is maintained through strict avoidance of intermarriage and ritual contamination with designated outsider groups.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_members, beneficiary,
    organized, generational, identity_locked, national).

% Community members whose desired marriages and kinship ties with outsiders are prohibited by the herem mandate; they bear the cost through social ostracism, forfeited relationships, and the threat of communal sanctions.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, intermarriage_autonomy_seekers, payer,
    powerless, biographical, trapped, local).

% Groups designated as contaminating threats to covenant fidelity; they are subject to dispossession, exclusion, or violence under the mandate and cannot alter their outsider status within the framework.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, non_covenant_outsiders, payer,
    powerless, generational, trapped, regional).

% Priestly and communal authorities who interpret the herem command, adjudicate membership boundaries, and organize enforcement; their authority and role are constituted by the mandate's continuation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, boundary_administrators, agenda_setter,
    institutional, generational, identity_locked, national).

% Voices within the tradition who advance covenantal inclusion or universal divine concern; they are marginalized or reclassified as dissenters because their claims contradict categorical separation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, universalist_prophets, excluded,
    moderate, generational, constrained, national).

% Academic analysts who examine the herem command as a historical and textual construct produced by specific editorial and political contexts; they do not participate in the constraint's operation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, modern_critical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves covenant community identity across generations by enforcing bounded membership, preventing religious and cultural assimilation through intermarriage, and maintaining categorical distinction between insiders and designated outsiders.
% TRANSFER_FUNCTION: Moves autonomy over marriage and kinship from community members and basic security from outsiders to the covenant community's collective identity maintenance, administered by boundary authorities.
% ABSENT_VOICES: Non-covenant outsiders are structurally excluded from the conversation; universalist prophetic voices and intermarriage aspirants within the community are marginalized or silenced as covenant-breakers.
% DISAPPEARANCE_RATIONALE: If the herem mandate vanished, intermarriage restrictions would lift, outsider categorization would lose divine warrant, community boundaries would become permeable, and the identity-preservation function would collapse; the social and theological order would reorganize around open kinship and universalist ethics.
% FOUNDING_PROBLEM: Post-exodus Israelite community formation required maintaining distinct monotheistic identity among polytheistic Canaanite populations; assimilation through intermarriage and ritual syncretism threatened covenantal existence.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist commentators within the lineage attest the problem is live and perpetual. Critical historians and contextual supersession readers attest the problem was historically bounded and is now dead; no neutral corroboration exists outside the benefiting tradition.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.82) is high because the mandate severely restricts intermarriage autonomy and legitimates violence against outsiders, transferring bodily and social security to the coordination function. Suppression (0.79) is high because the constraint depends on active communal enforcement, divine-threat legitimation, and the exclusion of rival universalist theologies. Theater_ratio (0.45) reflects that ritual performance of separation (textual recitation, liturgical distinction, purity observance) sustains a substantial share of the constraint's operation. Accessibility_collapse (0.78) captures the near-total cognitive and social closure of alternatives once the covenant framework is adopted: intermarriage becomes unthinkable, outsiders become ontologically dangerous. Resistance (0.55) registers both outsider hostility and insider prophetic dissent, though the divine-command frame suppresses organized opposition.
 *
 * PERSPECTIVAL GAP:
 *   The covenant_community seat computes the constraint as protective coordination preserving divine promise and group existence; the outsider and autonomy-seeker seats compute it as violent extraction. The engine derives this divergence from the same structural data: beneficiaries with identity-locked exit experience low directionality (subsidized coordination), while victims with trapped or constrained exit experience high directionality (amplified extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Covenant community members are structural beneficiaries: they collect identity coherence and divine-favor maintenance, with directionality near the beneficiary end. Boundary administrators set the agenda and enforce; their directionality is mixed but leans beneficiary due to authority accumulation. Intermarriage autonomy seekers and non-covenant outsiders are the targets: they pay with restricted autonomy and security, and their lack of exit options (trapped by identity or powerless by definition) pushes directionality toward the full-target end. The engine computes effective extraction as amplified for these trapped targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 genealogy interview records a contested founding problem: the mandate was built to solve Israelite identity formation among hostile polytheistic populations, but its status as a live problem is disputed by supersessionist and critical voices. The tangled_rope classification prevents mislabeling the constraint as pure extraction (snare) because identity preservation is a genuine coordination function for the beneficiary community; it also prevents mislabeling it as pure coordination (rope) because the victim set is expansive and the extraction is asymmetric. If the founding problem is dead and the constraint persists by inertia, the measurements' rising theater_ratio and extractiveness over time would flag a drift toward piton or snare, but the current structural data show active enforcement and live coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    timeless_vs_historically_bounded,
    'Is the herem command a timeless divine mandate for covenant communities across all epochs, or a historically-bounded directive tied to ancient Israel''s settlement?',
    'Textual criticism of Deuteronomic editorial layers, comparative reception history, and analysis of how the tradition itself re-applies or discontinues the command in Second Temple and rabbinic literature.',
    'If historically bounded, the constraint loses its claim to perpetuity and reclassifies toward scaffold or snare; if timeless, the durable_separation reading retains structural coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(timeless_vs_historically_bounded, conceptual, 'Whether herem is perpetually binding or epoch-specific.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is separation enforced primarily through external communal sanctions or through internalized identity fusion that persists after physical exit?',
    'Post-exit observation of formerly observant individuals: do separation norms persist autonomously or dissolve once communal surveillance is removed?',
    'If internalized, effective suppression exceeds structural metrics and the constraint operates as deep identity_coordination; if external, it is more readily identifiable as active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    sibling_reading_relationship,
    'Does the durable_separation reading foreclose the allegorical_displacement reading within a single hermeneutical framework, or can both senses coexist?',
    'Analysis of interpretive traditions that maintain both literal and spiritual senses (e.g., quadriga, Pardes) versus traditions that collapse the command to a single sense.',
    'If foreclosed, the kernel generates mutually incompatible constraints; if coexisting, the kernel supports plural stable readings without structural resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_relationship, conceptual, 'Logical relationship between literal and allegorical readings of herem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(here_tr_t5, herem_command_dt7__durable_separation_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(here_tr_t10, herem_command_dt7__durable_separation_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(here_tr_t15, herem_command_dt7__durable_separation_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__durable_separation_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(here_tr_t25, herem_command_dt7__durable_separation_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(here_tr_t30, herem_command_dt7__durable_separation_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(here_be_t5, herem_command_dt7__durable_separation_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(here_be_t10, herem_command_dt7__durable_separation_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(here_be_t15, herem_command_dt7__durable_separation_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__durable_separation_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(here_be_t25, herem_command_dt7__durable_separation_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement(here_be_t30, herem_command_dt7__durable_separation_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(here_su_t5, herem_command_dt7__durable_separation_reading, suppression_requirement, 5, 0.47).
narrative_ontology:measurement(here_su_t10, herem_command_dt7__durable_separation_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(here_su_t15, herem_command_dt7__durable_separation_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__durable_separation_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(here_su_t25, herem_command_dt7__durable_separation_reading, suppression_requirement, 25, 0.74).
narrative_ontology:measurement(here_su_t30, herem_command_dt7__durable_separation_reading, suppression_requirement, 30, 0.79).


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
