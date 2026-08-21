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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Herem Command: Durable Separation Reading
 *   domain: religious_ethics/commitment_system
 *
 * SUMMARY:
 *   This constraint represents the 'durable separation' reading of the Herem
 *   command, a biblical mandate interpreted as a timeless divine directive
 *   for the covenant community to preserve its identity through strict,
 *   categorical separation from designated outsiders. This reading
 *   legitimizes high extraction of autonomy and resources from those outside
 *   the group, and sometimes violence, as necessary for maintaining purity
 *   and divine favor. It is framed as a Tangled Rope because it provides a
 *   strong identity coordination function for the in-group, but at the cost
 *   of extreme extraction and suppression for outsiders and internal
 *   dissenters.
 *
 * KEY AGENTS:
 *   - covenant_community_members: Agenda-setter/Beneficiary (organized/identity_locked) — enforce and benefit from identity preservation.
 *   - designated_outsiders: Payer/Excluded (powerless/trapped) — primary targets of separation and violence.
 *   - internal_dissenters: Payer/Excluded (moderate/identity_locked) — question the mandate, face severe repercussions.
 *   - intermarriage_candidates: Payer (powerless/identity_locked) — face severe penalties for seeking external bonds.
 *   - religious_authorities: Agenda-setter (institutional/constrained) — interpret and enforce the timeless mandate.
 *   - secular_human_rights_advocates: Observer (organized/analytical) — critique the command from an external ethical framework.
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
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem Command: Durable Separation Reading").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "religious_ethics/commitment_system").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, '25a66ab2-1836-4383-9469-c5ea6dd58ada').
narrative_ontology:cs_kernel_codification('25a66ab2-1836-4383-9469-c5ea6dd58ada', fixed_text).
narrative_ontology:cs_authority_grounding('25a66ab2-1836-4383-9469-c5ea6dd58ada', lineage).
narrative_ontology:cs_interpretation_layer_present('25a66ab2-1836-4383-9469-c5ea6dd58ada').
narrative_ontology:cs_reading_relation('25a66ab2-1836-4383-9469-c5ea6dd58ada', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('25a66ab2-1836-4383-9469-c5ea6dd58ada', herem_command_dt7__allegorical_displacement_reading, forecloses).
narrative_ontology:cs_axiom('25a66ab2-1836-4383-9469-c5ea6dd58ada', foundational, divine_mandate_timeless).
narrative_ontology:cs_axiom_status(divine_mandate_timeless, holdable).
narrative_ontology:cs_axiom_grounding('25a66ab2-1836-4383-9469-c5ea6dd58ada', divine_mandate_timeless, theological).
narrative_ontology:cs_axiom('25a66ab2-1836-4383-9469-c5ea6dd58ada', foundational, identity_purity_absolute).
narrative_ontology:cs_axiom_status(identity_purity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('25a66ab2-1836-4383-9469-c5ea6dd58ada', identity_purity_absolute, deontological).
narrative_ontology:cs_reference_frame('25a66ab2-1836-4383-9469-c5ea6dd58ada', original_divine_command).
narrative_ontology:cs_drift_state('25a66ab2-1836-4383-9469-c5ea6dd58ada', contemporary_pluralistic_society, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('25a66ab2-1836-4383-9469-c5ea6dd58ada', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_members).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, designated_outsiders).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, internal_dissenters).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarriage_candidates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adhere to and enforce the Herem command, believing it preserves their distinct identity, purity, and divine favor. They benefit from the perceived security and cohesion of a bounded community, but bear the cost of strict adherence.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_members, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, covenant_community_members, beneficiary).

% Are the primary targets of the Herem command, facing categorical separation, exclusion, and potentially violence. They have no voice in the interpretation or application of the command and no viable exit from its effects.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, designated_outsiders, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, designated_outsiders, excluded).

% Are members of the covenant community who question the severity or timelessness of the Herem command. They face social ostracization, spiritual condemnation, and potential expulsion if their dissent becomes public or active.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, internal_dissenters, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, internal_dissenters, excluded).

% Are individuals within the covenant community who seek to marry outside the designated boundaries. They are subject to severe social and religious penalties, including excommunication, as their actions are seen as a direct threat to identity preservation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, intermarriage_candidates, payer,
    powerless, biographical, identity_locked, local).

% Are the interpreters and enforcers of the Herem command, responsible for maintaining doctrinal purity and community boundaries. Their authority is grounded in their role as custodians of the divine mandate, which they believe is timeless.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, religious_authorities, agenda_setter,
    institutional, generational, constrained, regional).

% Observe and critique the Herem command from a human rights perspective, highlighting its impact on individual autonomy, freedom of association, and the right to life. They seek to challenge the legitimacy of such commands in modern society.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, secular_human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__durable_separation_reading, covenant_community_members).
narrative_ontology:fixing_cost_class(herem_command_dt7__durable_separation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the distinct identity, cultural purity, and theological integrity of the covenant community by establishing clear, divinely mandated boundaries and rules for interaction with designated outsiders, thereby ensuring group cohesion and perceived divine favor.
% TRANSFER_FUNCTION: Transfers autonomy, social standing, resources, and sometimes lives from designated outsiders and internal dissenters to the covenant community, in exchange for the perceived benefits of identity preservation, spiritual purity, and collective security.
% ABSENT_VOICES: Designated outsiders, who are often dehumanized and denied agency, are structurally excluded from any discourse regarding the command. Internal dissenters are silenced by social pressure and fear of spiritual and communal repercussions.
% DISAPPEARANCE_RATIONALE: If the Herem command and its durable separation reading vanished overnight, the entire social, political, and theological structure of the covenant community, built on this foundational mandate, would undergo radical transformation. Identity boundaries would dissolve, intermarriage would become permissible, and the community's self-understanding and relationship with the divine would be fundamentally reconfigured.
% FOUNDING_PROBLEM: The perceived existential threat of assimilation, cultural dilution, and spiritual corruption for the covenant community, particularly during periods of settlement or interaction with other cultures, leading to a loss of distinct identity and divine favor.
% FOUNDING_PROBLEM_CORROBORATION: Religious authorities and many covenant community members attest that the founding problem of identity preservation and spiritual purity remains live and urgent in a pluralistic and secularizing world. External historians and sociologists might corroborate the historical context of identity formation but often dispute the 'timeless' nature or ongoing 'live' status of the problem in its original form, viewing its persistence as a mechanism of social control rather than a response to an active threat.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.90) is high due to the severe costs imposed on outsiders and dissenters, including loss of autonomy, social exclusion, and potential physical harm. Suppression (0.95) is extremely high, reflecting the divine mandate's coercive power, enforced through religious authority, social pressure, and historical violence, with virtually no legitimate exit for targets. The theater ratio (0.10) is low because the command is actively and genuinely enforced, not merely performed. Accessibility collapse (0.80) is high for those targeted by the separation, as alternatives to the mandated separation are severely limited or non-existent. Resistance (0.60) is moderate, as there is historical and contemporary resistance from both internal dissenters and external critics, but the command's adherents largely maintain its force.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the covenant community members and religious authorities, the Herem command is a necessary, divinely ordained mechanism for identity preservation and spiritual purity (a form of Rope or even Mountain). From the perspective of designated outsiders and internal dissenters, it is a highly extractive and suppressive Snare. The engine's computation of per-seat classification will reflect this divergence based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   Covenant community members are beneficiaries (d near 0.0) as they gain identity, cohesion, and perceived divine favor. Designated outsiders, internal dissenters, and intermarriage candidates are targets (d near 1.0) as they bear the full cost of separation, exclusion, and loss of autonomy. Religious authorities, while enforcing, also benefit from the authority derived from the mandate. Secular human rights advocates are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the Herem command as a pure coordination 'Rope' (as its adherents might claim) by highlighting the extreme asymmetric extraction and suppression it entails. It also avoids classifying it as a 'Piton' by recognizing the active enforcement and clear beneficiaries, despite its ancient origins. The 'Tangled Rope' classification captures both the internal identity-coordination function and the external extractive/suppressive function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    timeless_vs_contextual_mandate,
    'Is the Herem command a timeless divine mandate, or was it a historically-bounded directive for ancient Israel''s settlement period?',
    'Theological and historical-critical scholarship examining the textual and archaeological evidence for the command''s application across different historical periods, and its consistency with broader ethical developments within the religious tradition.',
    'If historically bounded, the ''durable separation'' reading would be reclassified as a Snare or Piton, as its original mandate would be dead. If timeless, its claimed coordination function for identity preservation would be strengthened, though its extractive nature would remain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(timeless_vs_contextual_mandate, conceptual, 'Ambiguity regarding the temporal scope of the divine mandate.').

omega_variable(
    literal_vs_allegorical_interpretation,
    'Are the ''nations'' and ''separation'' in the Herem command to be understood literally as ethnic groups and physical boundaries, or allegorically as spiritual enemies and internal moral warfare?',
    'Further theological and hermeneutical analysis, potentially influenced by contemporary ethical considerations and interfaith dialogue, to determine the primary interpretive framework intended by the sacred texts and their historical reception.',
    'An allegorical reading would drastically reduce the extractiveness and suppression, potentially reclassifying the constraint as a Rope (internal moral coordination) or even a Mountain (spiritual law), as the victim set would shift from physical entities to abstract concepts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literal_vs_allegorical_interpretation, conceptual, 'Ambiguity regarding the literal vs. allegorical interpretation of the command''s targets.').

omega_variable(
    divine_command_ethics_legitimacy,
    'Is the legitimation of violence and categorical exclusion through divine command ethics morally defensible in a contemporary ethical framework?',
    'Ongoing philosophical and ethical debate, interfaith dialogue, and the development of human rights norms. This is a preference-based question that may not have a universally accepted ''resolution''.',
    'If deemed morally indefensible, the constraint''s legitimacy would collapse for external observers, reinforcing its Snare-like qualities, even if internal adherents maintain its validity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_command_ethics_legitimacy, preference, 'Ethical defensibility of divine command ethics for violence and exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(here_tr_t400, herem_command_dt7__durable_separation_reading, theater_ratio, 400, 0.1).
narrative_ontology:measurement(here_tr_t800, herem_command_dt7__durable_separation_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement(here_tr_t1200, herem_command_dt7__durable_separation_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(here_tr_t1600, herem_command_dt7__durable_separation_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(here_tr_t2000, herem_command_dt7__durable_separation_reading, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(here_be_t400, herem_command_dt7__durable_separation_reading, base_extractiveness, 400, 0.87).
narrative_ontology:measurement(here_be_t800, herem_command_dt7__durable_separation_reading, base_extractiveness, 800, 0.88).
narrative_ontology:measurement(here_be_t1200, herem_command_dt7__durable_separation_reading, base_extractiveness, 1200, 0.89).
narrative_ontology:measurement(here_be_t1600, herem_command_dt7__durable_separation_reading, base_extractiveness, 1600, 0.9).
narrative_ontology:measurement(here_be_t2000, herem_command_dt7__durable_separation_reading, base_extractiveness, 2000, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(here_su_t400, herem_command_dt7__durable_separation_reading, suppression_requirement, 400, 0.92).
narrative_ontology:measurement(here_su_t800, herem_command_dt7__durable_separation_reading, suppression_requirement, 800, 0.93).
narrative_ontology:measurement(here_su_t1200, herem_command_dt7__durable_separation_reading, suppression_requirement, 1200, 0.94).
narrative_ontology:measurement(here_su_t1600, herem_command_dt7__durable_separation_reading, suppression_requirement, 1600, 0.95).
narrative_ontology:measurement(here_su_t2000, herem_command_dt7__durable_separation_reading, suppression_requirement, 2000, 0.95).


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
