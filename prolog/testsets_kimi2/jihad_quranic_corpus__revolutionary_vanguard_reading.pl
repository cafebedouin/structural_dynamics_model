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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Revolutionary Vanguard Reading of Jihad: Individual Obligation Against Apostate Rulers
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   The jihad_quranic_corpus kernel contains multiple structurally distinct
 *   readings of Quranic and prophetic texts governing armed struggle. The
 *   revolutionary_vanguard_reading instantiates a constraint in which jihad
 *   becomes an immediate individual obligation (fard 'ayn) incumbent on every
 *   Muslim without imam authorization, directed against Muslim rulers
 *   declared apostate through takfir and against occupying non-Muslim forces.
 *   Classical jurisprudential safeguardsâstate monopoly on declaring jihad,
 *   invitation to Islam before attack, proportionality, and non-combatant
 *   immunityâare overridden by emergency jurisprudence. Apostate Muslim
 *   populations, civilian populations under collective-guilt doctrines, and
 *   occupying forces enter the victim set. The vanguard leadership sets the
 *   agenda and collects allegiance and operational resources, while vanguard
 *   fighters are identity-locked into the obligation. This reading forecloses
 *   both the defensive_spiritual_reading (which privileges internal struggle
 *   and non-combatant immunity) and the expansionist_legalist_reading (which
 *   requires imam authority and classical conditions).
 *
 * KEY AGENTS:
 *   - revolutionary_vanguard_leadership: Primary beneficiary/agenda_setter (powerful/mobile) â sets takfir, collects allegiance and resources
 *   - vanguard_fighters: Primary payer/compelled enforcers (moderate/identity_locked) â bear individual obligation under theological duress
 *   - apostate_muslim_populations: Primary target/payer (powerless/trapped) â declared apostate, lose classical protection
 *   - civilian_populations_under_collective_guilt: Secondary target/payer (powerless/trapped) â reclassified as combatants
 *   - occupying_military_forces: Target/payer (institutional/constrained) â attacked regardless of classical jus ad bellum
 *   - classical_jurists: Excluded authority (institutional/constrained) â bypassed by emergency jurisprudence
 *   - analytical_observer: Analytical seat (analytical/analytical) â tracks doctrinal innovation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.85).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.9).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Revolutionary Vanguard Reading of Jihad: Individual Obligation Against Apostate Rulers").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, '5a5a395e-8f81-4e22-a91a-90348b0aa98d').
narrative_ontology:cs_kernel_codification('5a5a395e-8f81-4e22-a91a-90348b0aa98d', fixed_text).
narrative_ontology:cs_authority_grounding('5a5a395e-8f81-4e22-a91a-90348b0aa98d', lineage).
narrative_ontology:cs_interpretation_layer_present('5a5a395e-8f81-4e22-a91a-90348b0aa98d').
narrative_ontology:cs_reading_relation('5a5a395e-8f81-4e22-a91a-90348b0aa98d', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('5a5a395e-8f81-4e22-a91a-90348b0aa98d', jihad_quranic_corpus__expansionist_legalist_reading, forecloses).
narrative_ontology:cs_axiom('5a5a395e-8f81-4e22-a91a-90348b0aa98d', foundational, individual_jihad_obligation_without_imam).
narrative_ontology:cs_axiom_status(individual_jihad_obligation_without_imam, holdable).
narrative_ontology:cs_axiom_grounding('5a5a395e-8f81-4e22-a91a-90348b0aa98d', individual_jihad_obligation_without_imam, theological).
narrative_ontology:cs_axiom('5a5a395e-8f81-4e22-a91a-90348b0aa98d', foundational, takfir_as_legitimate_boundary_mechanism).
narrative_ontology:cs_axiom_status(takfir_as_legitimate_boundary_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('5a5a395e-8f81-4e22-a91a-90348b0aa98d', takfir_as_legitimate_boundary_mechanism, theological).
narrative_ontology:cs_reference_frame('5a5a395e-8f81-4e22-a91a-90348b0aa98d', pre_scholastic_islamic_authority).
narrative_ontology:cs_drift_state('5a5a395e-8f81-4e22-a91a-90348b0aa98d', contemporary_muslim_world, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('5a5a395e-8f81-4e22-a91a-90348b0aa98d', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leadership).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_fighters).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_muslim_populations).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilian_populations_under_collective_guilt).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_military_forces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues takfir declarations against Muslim rulers and occupiers, proclaims emergency jurisprudence overriding classical safeguards, and asserts the right to authorize jihad without state or imam approval. Collects operational allegiance, material resources, and transnational recruitment flows from the decentralized vanguard.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leadership, agenda_setter,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leadership, beneficiary).

% Individually obligated by fard 'ayn doctrine to take up arms against apostate regimes and occupiers without state authorization. Exit is blocked by theological identity fusionârejecting the obligation risks being declared apostate and losing communal belonging and salvific certainty.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_fighters, payer,
    moderate, biographical, identity_locked, national).

% Live under Muslim rulers declared apostate by vanguard takfir. The doctrine removes classical jurisprudential protections that shielded Muslim civilians from fellow-Muslim violence, exposing them to dispossession, subjugation, or lethal attack as legitimate targets.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_muslim_populations, payer,
    powerless, immediate, trapped, regional).

% Non-combatant civilians in apostate-ruled or occupied territories reclassified as combatants through collective-guilt doctrine. Their daily activitiesâpaying taxes to the regime, serving in bureaucracyâare reinterpreted as acts of war, removing classical immunity.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, civilian_populations_under_collective_guilt, payer,
    powerless, immediate, trapped, local).

% Foreign military forces designated as occupiers of Muslim land. Targeted for attack under the doctrine regardless of classical jus ad bellum requirements such as prior invitation to Islam or proportionality constraints. Their status as lawful targets is structurally presumed.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_military_forces, payer,
    institutional, immediate, constrained, national).

% Traditional authorities whose state-monopoly doctrine on declaring jihad and whose safeguards on non-combatant immunity are bypassed by emergency jurisprudence. Would object to the dissolution of imam authority and takfir of Muslim rulers but are structurally excluded from the vanguard's legitimating discourse.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_jurists, excluded,
    institutional, generational, constrained, global).

% Academic and policy analysts who track the doctrinal innovation from classical jurisprudence to vanguard emergency frameworks. Observe the displacement of state authority and the expansion of victim sets without participating in the theological discourse.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leadership).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__revolutionary_vanguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates decentralized militant action by self-authorized cells and individuals against declared apostate regimes and occupiers, substituting theological obligation and takfir for state command structure or classical imam authority.
% TRANSFER_FUNCTION: Moves lives, property, political sovereignty, and theological legitimacy from apostate Muslim populations, civilian populations under collective guilt, and occupying military forces to the revolutionary vanguard leadership and its armed apparatus.
% ABSENT_VOICES: Classical jurists who insist on imam authority and proportionality; Muslim publics who reject takfir of rulers; state security apparatuses whose monopoly on violence is eliminated; non-Muslim civilians whose classical immunity is denied. They are excluded through takfir or dismissed as illegitimate.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, the vanguard's authority to declare takfir and compel individual jihad would collapse; fighters would revert to classical state-monopoly frameworks or local defensive arrangements; civilian immunity would be restored; the transnational militant structure would lose its theological operating system and recruitment engine.
% FOUNDING_PROBLEM: Muslim-majority territories under foreign occupation and post-colonial rule by regimes perceived as un-Islamic, with classical state institutions failing to mount effective resistance or establish Islamic governance.
% FOUNDING_PROBLEM_CORROBORATION: External historians and political scientists document colonial occupation and post-colonial authoritarianism in Muslim-majority states, corroborating the grievance narrative from outside the vanguard's beneficiary set. However, these same external observers dispute that the vanguard's doctrine is the necessary or legitimate response, and affected state governments contest the characterization of their rule as apostasy.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.85, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.85) reflects authorization to take life, property, and sovereignty from declared apostates and occupiers without classical proportionality limits. Suppression (0.90) captures the active suppression of state authority, classical jurisprudential safeguards, and alternative readings of jihad. Theater ratio (0.40) registers the performative dimension of takfir declarations and emergency jurisprudential reasoning that masks political goals in theological form. Accessibility collapse (0.80) is high because acceptance of the doctrine collapses the classical alternative framework entirely for the adherent. Resistance (0.75) reflects strong counter-mobilization by states, classical religious institutions, and targeted populations. The measurement series tracks the doctrine's intensification from marginalia to dominant vanguard framework over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (vanguard leadership) experiences the constraint as a restoration of authentic authority and liberatory theology; the payer seats (apostate populations, civilians, fighters, occupying forces) experience it as coercive extraction of life, security, and theological autonomy. The engine computes this divergence from the structural dataâthe authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The vanguard leadership is the structural beneficiary (d near the beneficiary end) because the constraint generates allegiance, resources, and operational control flowing to it. The apostate populations, civilians, and occupying forces are structural targets (d near the full-target end) because the constraint authorizes their dispossession and death. Vanguard fighters sit high on the target axis despite their enforcement role because the individual obligation extracts their lives and obedience under identity-locked exit; their directionality derives from the victim declaration plus identity_locked exit. Classical jurists are excluded and thus outside the directionality computation for extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling by requiring explicit victim identification and high suppression. A naive reading might classify the doctrine as coordination (uniting Muslims against occupation). The snare classification is warranted because the coordination story is cover: the doctrine persists through active suppression of classical safeguards and alternatives, and the extraction is asymmetricâleadership collects while populations pay. The founding problem (defense against occupation and tyranny) may have been live historically, but the constraint's current operation extracts far beyond that defensive scope, and its persistence depends on suppressing the very juridical alternatives that would limit it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'This constraint is the revolutionary_vanguard_reading of kernel jihad_quranic_corpus. Would adopting the defensive_spiritual_reading or expansionist_legalist_reading remove the apostate Muslim and civilian victim sets and restore imam authority?',
    'Cross-reading structural comparison of victim sets, authority structures, and combatant-immunity rules across the three sibling constraints.',
    'If sibling readings eliminate these victim sets and restore authority conditions, the current epsilon is reading-dependent and the kernel decomposes into at least two constraints with non-overlapping victim populations and authority structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the revolutionary reading is structurally distinct from sibling readings or merely an intensification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external enforcement by the vanguard organization) or internalized (theological damnation and identity fusion that persist after physical departure)?',
    'Post-exit trajectory analysis of defectors and disengaged fighters: if obligation-anxiety and identity-bound distress persist after removal from vanguard-controlled territory, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggestsâthe target carries the suppression beyond physical exit, strengthening snare classification and complicating deradicalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism for compelled fighters.').

omega_variable(
    emergency_doctrine_scope,
    'Does the emergency jurisprudence override apply only to the classical imam-authority requirement, or does it also override proportionality and civilian-immunity constraints?',
    'Systematic review of vanguard jurisprudential texts (maqmamat, fatwas, manifestos) claiming emergency exception, coded for which classical constraints are explicitly suspended.',
    'If the emergency doctrine overrides all safeguards, extraction is maximized and the snare classification is reinforced; if only authority is bypassed, some protective constraints remain and epsilon would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_doctrine_scope, empirical, 'Scope of classical safeguard override by emergency jurisprudence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jiha_tr_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(jiha_tr_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(jiha_tr_t50, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(jiha_be_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(jiha_be_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(jiha_be_t50, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(jiha_su_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(jiha_su_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 40, 0.87).
narrative_ontology:measurement(jiha_su_t50, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, identity_coordination).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__expansionist_legalist_reading).

% DUAL FORMULATION NOTE:
% The jihad_quranic_corpus decomposes into three structurally distinct constraints because the kernel label conflates readings with non-overlapping victim sets, authority requirements, and combatant-immunity rules. The revolutionary_vanguard_reading is the most extractive member of the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
