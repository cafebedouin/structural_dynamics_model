% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Revolutionary Vanguard Reading of Jihad (Fard 'Ayn via Takfir)
 *   domain: theological/political
 *
 * SUMMARY:
 *   The revolutionary vanguard reading of the Quranic jihad corpus declares
 *   armed struggle an immediate individual obligation (fard 'ayn) incumbent
 *   on every Muslim without state authorization. It bypasses classical
 *   jurisprudential safeguardsâimam authority, proportionality,
 *   non-combatant immunityâthrough takfir (declaring Muslim rulers
 *   apostate) and emergency jurisprudence. The constraint is CLAIMED as a
 *   restoration of authentic prophetic obligation (Rope/Mountain framing by
 *   its adherents) while the authored metrics describe a structure of pure
 *   extraction: identifiable victims (apostate rulers, occupiers, and
 *   civilians reclassified via collective guilt), high suppression of
 *   alternative scholarly voices, and active enforcement through
 *   decentralized vanguard networks. This divergence is deliberate and is the
 *   measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - revolutionary_vanguard: Primary beneficiary/agenda_setter (powerful/identity_locked/global) â captures extraction through legitimacy, recruits, and territorial control
 *   - apostate_rulers: Primary target (institutional/trapped/national) â bear delegitimization, overthrow, and assassination
 *   - foreign_occupiers: Secondary target (institutional/constrained/global) â bear violence regardless of classical combatant status
 *   - civilians_collective_guilt: Diffuse target (powerless/trapped/local) â civilian immunity collapsed by collective guilt doctrine
 *   - classical_jurists: Excluded voice (institutional/trapped/global) â structurally bypassed by emergency doctrine overriding classical safeguards
 *   - muslim_communities: Identity-locked payers (moderate/identity_locked/national) â suffer coercion and instability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.88).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.85).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Revolutionary Vanguard Reading of Jihad (Fard 'Ayn via Takfir)").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "theological/political").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, 'f929aaf8-7888-4873-b263-d9c4ce20504f').
narrative_ontology:cs_kernel_codification('f929aaf8-7888-4873-b263-d9c4ce20504f', fixed_text).
narrative_ontology:cs_authority_grounding('f929aaf8-7888-4873-b263-d9c4ce20504f', lineage).
narrative_ontology:cs_interpretation_layer_present('f929aaf8-7888-4873-b263-d9c4ce20504f').
narrative_ontology:cs_reading_relation('f929aaf8-7888-4873-b263-d9c4ce20504f', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('f929aaf8-7888-4873-b263-d9c4ce20504f', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_axiom('f929aaf8-7888-4873-b263-d9c4ce20504f', foundational, takfir_of_muslim_rulers_permissible).
narrative_ontology:cs_axiom_status(takfir_of_muslim_rulers_permissible, holdable).
narrative_ontology:cs_axiom_grounding('f929aaf8-7888-4873-b263-d9c4ce20504f', takfir_of_muslim_rulers_permissible, theological).
narrative_ontology:cs_axiom('f929aaf8-7888-4873-b263-d9c4ce20504f', foundational, individual_jihad_without_state_authority).
narrative_ontology:cs_axiom_status(individual_jihad_without_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('f929aaf8-7888-4873-b263-d9c4ce20504f', individual_jihad_without_state_authority, theological).
narrative_ontology:cs_reference_frame('f929aaf8-7888-4873-b263-d9c4ce20504f', prophetic_mobilization_era).
narrative_ontology:cs_drift_state('f929aaf8-7888-4873-b263-d9c4ce20504f', post_colonial_state_crisis, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('f929aaf8-7888-4873-b263-d9c4ce20504f', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, foreign_occupiers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_collective_guilt).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, muslim_communities).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, emergency_jurisprudence_validity).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, takfir_as_political_tool).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims interpretive authority over Quranic jihad injunctions, bypassing classical state and scholarly institutions. Declares Muslim rulers apostate and foreign occupiers legitimate targets. Benefits from political authority, recruitment, and resource extraction in territories under its control. Exit is treated as apostasy.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard, agenda_setter,
    powerful, generational, identity_locked, global).

% Muslim state rulers declared apostate by vanguard takfir. They bear the cost of assassination, overthrow, and delegitimization. Their classical institutional authority is voided by the constraint.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers, payer,
    institutional, immediate, trapped, national).

% Foreign military or civilian presence declared occupation by vanguard theology. Targeted for violent resistance regardless of formal combatant status under classical jurisprudence.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, foreign_occupiers, payer,
    institutional, immediate, constrained, global).

% Civilian populations under apostate rule or in occupier nations who are reclassified as combatants via collective guilt doctrines. They cannot exit the target category individually; their civilian immunity is voided by emergency jurisprudence.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_collective_guilt, payer,
    powerless, immediate, trapped, local).

% Traditional scholars who maintain that jihad requires state authority, proportionality, and non-combatant immunity. Their voices are structurally excluded by the emergency doctrine that overrides classical jurisprudential safeguards.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_jurists, excluded,
    institutional, generational, trapped, global).

% Ordinary Muslims who suffer instability, coercion, and ideological pressure to support or join the vanguard. Their religious identity is leveraged to lock them into the constraint; dissent risks takfir.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, muslim_communities, payer,
    moderate, biographical, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__revolutionary_vanguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a transnational decentralized vanguard for armed struggle by providing a theological framework that eliminates state monopoly on violence authorization and unifies dispersed fighters under a single jurisprudential banner.
% TRANSFER_FUNCTION: Moves life, property, political authority, and legitimacy from declared apostate rulers, foreign occupiers, and collectively-guilted civilians to the revolutionary vanguard. Also transfers the obligation to fight from state institutions to individual Muslims.
% ABSENT_VOICES: Classical jurists and state religious authorities who maintain jihad requires imam authority and safeguards; Muslim civilians who reject collective guilt; anti-colonial scholars who oppose occupation but reject takfir methods. They are structurally excluded by emergency jurisprudence that overrides classical consensus requirements.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, the vanguard's theological foundation for bypassing state authority would collapse. Classical state-centric jurisprudence would reassert its monopoly on violence authorization, takfir declarations would lose their performative force, and the targeted populations would no longer be subject to collective guilt reclassification. The transnational jihadist organizational form would lose its primary legitimating architecture.
% FOUNDING_PROBLEM: The perceived failure of post-colonial Muslim states to resist foreign occupation and implement Islamic governance, creating a legitimacy crisis that classical institutional jurisprudence could not resolve.
% FOUNDING_PROBLEM_CORROBORATION: Anti-colonial scholars outside the vanguard corroborate the problem of foreign occupation and authoritarian governance, but they do NOT corroborate the specific takfir and emergency jurisprudence solution. Classical jurists and human rights organizations contest both the problem framing and the solution. No neutral party outside the benefiting vanguard fully corroborates the genealogy.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is very high (0.88) because the constraint extracts life, security, property, and political authority from its victims through violent action, with no reciprocal benefit to them. Suppression is high (0.85) because the constraint must actively suppress classical jurists, state religious authorities, and alternative readings to persist. Theater ratio is high (0.72) because a substantial share of vanguard activity is performativeâmediatized violence, theological proclamations, and state-building theater designed to demonstrate legitimacy rather than achieve strategic goals. Accessibility collapse is high (0.80) for both trapped victims (collective guilt prevents individual exit) and identity-locked members (alternatives collapse under takfir threat). Resistance is high (0.78) because the constraint faces active military, scholarly, and communal opposition. The measurement series shows extraction and theater peaking mid-interval and slightly declining as territorial control is lost, while remaining structurally high.
 *
 * PERSPECTIVAL GAP:
 *   The revolutionary vanguard seat experiences the constraint as a liberating restoration of authentic obligation (low effective extraction, possibly negative Ïâsubsidized by meaning and purpose). The apostate ruler and foreign occupier seats experience full-target extraction (high d, high Ï). The civilian seat under collective guilt experiences near-total extraction with no exit. The classical jurist seat, though excluded, experiences the constraint as suppression of their authority. The engine computes this divergence from structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: the revolutionary_vanguard collects legitimacy, recruits, and territorial revenue; they are the sole structural beneficiary. Victim declarations: apostate_rulers bear loss of authority and life; foreign_occupiers bear violence; civilians_collective_guilt bear the collapse of their protected status. Muslim_communities are not declared in victims but described as payers because they bear diffuse costs of instability without being the primary extraction target. The directionality derivation maps revolutionary_vanguard to low d (beneficiary) and all victim groups to high d (target). Identity_locked exit for the vanguard and communities amplifies effective extraction for locked-in members.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring an identifiable victim set and active enforcement. Without these, the vanguard's claimâthat this is merely coordination of resistanceâwould dominate. The victim set (apostate rulers, occupiers, and civilians via collective guilt) establishes that the coordination story is cover for extraction. The high theater ratio confirms that performative maintenance of legitimacy substitutes for functional governance. If classified as Rope, the collective guilt mechanism and takfir would be invisible; as Snare, they are structurally exposed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint one legitimate reading among many of the jihad kernel, or does it claim exclusivity?',
    'Comparative theological analysis of the three sibling readings across classical and modern jurisprudential corpora.',
    'If exclusive, its high extraction is presented as divine obligation; if one reading among many, its extraction is contingent on interpretive choice and political context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether the revolutionary vanguard reading claims exclusivity or pluralism').

omega_variable(
    civilian_combatant_boundary,
    'Is the collapse of civilian immunity via collective guilt structurally inherent to this reading or an operational distortion?',
    'Textual analysis of vanguard jurisprudential manifestos versus operational military manuals.',
    'If inherent, victim set and extractiveness are maximally high; if distortion, base extraction may be lower than implemented reality suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_combatant_boundary, conceptual, 'Whether collective guilt is core or distortion').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (violence against dissenting jurists and states) or internalized (true belief among vanguard members)?',
    'Post-defection narratives and external enforcement documentation compared to fighter testimonies.',
    'If internalized, effective suppression exceeds structural measure; if structural only, the constraint is more fragile to leadership removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    state_bypass_contingency,
    'Is the bypass of state authority a necessary theological feature or a contingent response to specific post-colonial state failures?',
    'Historical analysis of vanguard movements in contexts with and without perceived apostate states.',
    'If contingent, the constraint may revert to expansionist_legalist dynamics under legitimate state authority, reducing extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_bypass_contingency, empirical, 'Contingency of state authority bypass').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(jiha_tr_t8, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(jiha_tr_t16, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(jiha_tr_t24, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 24, 0.58).
narrative_ontology:measurement(jiha_tr_t32, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 32, 0.7).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 40, 0.78).
narrative_ontology:measurement(jiha_tr_t48, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 48, 0.72).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(jiha_be_t8, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(jiha_be_t16, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(jiha_be_t24, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(jiha_be_t32, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 32, 0.86).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 40, 0.9).
narrative_ontology:measurement(jiha_be_t48, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 48, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(jiha_su_t8, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(jiha_su_t16, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(jiha_su_t24, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(jiha_su_t32, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 32, 0.88).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 40, 0.92).
narrative_ontology:measurement(jiha_su_t48, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 48, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, expansionist_legalist_reading).

% DUAL FORMULATION NOTE:
% The jihad_quranic_corpus kernel decomposes into three structurally distinct constraints per the epsilon-invariance principle. This reading (revolutionary_vanguard) shares the fixed textual kernel with its siblings but diverges on authority, victim set, and enforcement structure. Epsilon values differ significantly across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
