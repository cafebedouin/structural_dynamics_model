% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Quran 9:5 Contextual-Defensive Reading
 *   domain: religious/jurisprudential/political
 *
 * SUMMARY:
 *   This constraint instantiates the contextual-defensive reading of Quran
 *   9:5, a contested kernel in Islamic jurisprudence. The reading holds that
 *   the verse addresses specific 7th-century Medinan treaty-breaking by
 *   polytheist tribes, does not abrogate prior peaceful verses, and
 *   establishes a conditional authorization of defensive warfare bound by
 *   treaty fidelity. It functions as a tangled rope: it coordinates peaceful
 *   pluralism and international treaty order for integrationist
 *   Muslim-majority states while asymmetrically extracting security and life
 *   from treaty-violating parties. The constraint requires active hermeneutic
 *   and state-level enforcement to suppress the competing
 *   abrogating-universal reading.
 *
 * KEY AGENTS:
 *   - integrationist_muslim_majority_states (beneficiary / institutional / constrained)
 *   - peaceful_non_muslim_treaty_partners (beneficiary / moderate / constrained)
 *   - treaty_violating_polytheist_tribes (payer / powerless / trapped)
 *   - islamic_integrationist_jurists (agenda_setter / institutional / constrained)
 *   - abrogationist_militant_movements (excluded / organized / constrained)
 *   - international_human_rights_institutions (observer / institutional / analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.48).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.65).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.48).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, tangled_rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Quran 9:5 Contextual-Defensive Reading").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "religious/jurisprudential/political").

domain_priors:requires_active_enforcement(quran_9_5_scope__contextual_defensive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, '9c525887-c5de-4608-9706-b05830f221ba').
narrative_ontology:cs_kernel_codification('9c525887-c5de-4608-9706-b05830f221ba', fixed_text).
narrative_ontology:cs_authority_grounding('9c525887-c5de-4608-9706-b05830f221ba', lineage).
narrative_ontology:cs_interpretation_layer_present('9c525887-c5de-4608-9706-b05830f221ba').
narrative_ontology:cs_reading_relation('9c525887-c5de-4608-9706-b05830f221ba', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('9c525887-c5de-4608-9706-b05830f221ba', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('9c525887-c5de-4608-9706-b05830f221ba', foundational, peace_verses_unabrogated).
narrative_ontology:cs_axiom_status(peace_verses_unabrogated, holdable).
narrative_ontology:cs_axiom_grounding('9c525887-c5de-4608-9706-b05830f221ba', peace_verses_unabrogated, conventional).
narrative_ontology:cs_axiom('9c525887-c5de-4608-9706-b05830f221ba', foundational, treaty_fidelity_supersedes_expansion).
narrative_ontology:cs_axiom_status(treaty_fidelity_supersedes_expansion, holdable).
narrative_ontology:cs_axiom_grounding('9c525887-c5de-4608-9706-b05830f221ba', treaty_fidelity_supersedes_expansion, deontological).
narrative_ontology:cs_reference_frame('9c525887-c5de-4608-9706-b05830f221ba', medinan_treaty_order).
narrative_ontology:cs_drift_state('9c525887-c5de-4608-9706-b05830f221ba', contemporary_international_order, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9c525887-c5de-4608-9706-b05830f221ba', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, peaceful_non_muslim_treaty_partners).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, treaty_violating_polytheist_tribes).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, treaty_based_coexistence).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, defensive_jihad_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derive domestic and international legitimacy from a jurisprudential framework that permits defensive warfare only after treaty violation. Use this reading to justify participation in the UN charter order and to delegitimize militant expansionist movements internally. Exit is constrained by the need to maintain Islamic legitimacy while integrating into global treaty regimes.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, beneficiary,
    institutional, generational, constrained, global).

% Live under formal or informal treaty protections with Muslim-majority polities. The contextual-defensive reading structurally prioritizes their security as long as treaties are honored, shielding them from the authorization of warfare that the abrogating reading would permit. Their exit is constrained by geopolitical dependency.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, peaceful_non_muslim_treaty_partners, beneficiary,
    moderate, biographical, constrained, regional).

% Once they breach a treaty with a Muslim polity that adopts this reading, they become the sole legitimate target of military authorization under 9:5. They bear the lethal extraction of the constraint. Exit is trapped because the violation itself triggers the authorization, and there is no remaining protective framework.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, treaty_violating_polytheist_tribes, payer,
    powerless, immediate, trapped, regional).

% Maintain the hermeneutic apparatusâchronology of revelation, asbab al-nuzul, and usul al-fiqhâthat bounds 9:5 to its Medinan treaty-breaking context. They enforce methodological rules against abrogation of peaceful verses. Their exit is constrained by the tradition they inhabit; abandoning the contextual reading would collapse their scholarly authority.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, islamic_integrationist_jurists, agenda_setter,
    institutional, generational, constrained, global).

% Read 9:5 as a universal offensive command and are structurally excluded from the interpretive community that adopts the contextual-defensive reading. They would contest the narrow scope but are marginalized by state fatwa councils and official curricula that enforce the contextual narrative.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, abrogationist_militant_movements, excluded,
    organized, biographical, constrained, regional).

% Monitor whether the contextual-defensive reading is applied consistently to restrict warfare or invoked opportunistically. They compile reports on state conduct, treaty compliance, and civilian protection, treating the reading as a legal-hermeneutic fact with geopolitical consequences.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, international_human_rights_institutions, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable interpretive framework that distinguishes legitimate defensive warfare from aggression, enabling treaty-based coexistence between Muslim and non-Muslim polities by restricting martial authorization to demonstrable prior treaty violation.
% TRANSFER_FUNCTION: Transfers security and legitimacy from treaty-violating parties to integrationist states and peaceful treaty partners by criminalizing breach and authorizing defensive response only after aggression, while extracting life and security from the violator.
% ABSENT_VOICES: Abrogationist scholars and militant movements who read 9:5 as a standing universal offensive command are structurally excluded; they would argue for expansionist application but are marginalized by the hermeneutic and state apparatus enforcing contextual scope.
% DISAPPEARANCE_RATIONALE: If the contextual-defensive binding disappeared, integrationist states would lose a primary jurisprudential tool for legitimizing defensive restraint and treaty fidelity; peaceful treaty partners would face uncertainty about whether Muslim polities remain bound by coexistence norms; abrogationist readings would gain institutional space, rearranging the geopolitical and communal order in pluralist Muslim-majority contexts.
% FOUNDING_PROBLEM: The 7th-century Medinan polity faced sudden treaty-breaking by surrounding polytheist tribes, requiring a targeted revelation that clarified when existing peaceful treaties could be set aside and hostilities resumed without dissolving the broader Quranic norm of treaty fidelity and defensive proportionality.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of early Islam and international legal scholars outside the integrationist Muslim-majority state beneficiary set corroborate the historical occasion of specific treaty violations in the Medinan period; they attest the founding problem is historically localized, though they dispute the verse's contemporary legal binding force.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the constraint authorizes lethal force, but only against parties who have already breached treaties, severely limiting the victim set. Suppression is substantial (0.65) because the contextual reading must actively exclude and delegitimize the abrogating-universal reading through state curricula, fatwa councils, and hermeneutic policing. Theater is moderate (0.32): much of the constraint's operation is functional (genuine treaty protection), but a growing share is performative maintenance of Islamic legitimacy in an international order that already prohibits offensive war. Accessibility collapse is moderate (0.55): once a jurist or state commits to this reading, the abrogationist alternative is methodologically excluded. Resistance is high (0.70) because abrogationist movements and scholars actively contest the reading.
 *
 * PERSPECTIVAL GAP:
 *   The integrationist state seat experiences the constraint as a subsidy to legitimacy and treaty order; the peaceful non-Muslim partner experiences it as a protective shield; the treaty-violator experiences it as a lethal authorization with no exit; the abrogationist experiences it as a suppressive cage that denies the plain lexical scope of the verse. The jurist seat experiences it as methodological fidelity to chronology and occasion-of-revelation, sitting near symmetric because they both maintain and are bound by the hermeneutic apparatus.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrationist states and peaceful treaty partners are structural beneficiaries: the constraint subsidizes their security and diplomatic stability (low d). Treaty-violating parties are the sole structural targets: the constraint authorizes force against them specifically (high d). Islamic integrationist jurists are near-symmetric: they administer the constraint but are also methodologically imprisoned by it. Abrogationist movements are excluded rather than coordinated; their exclusion is the object of the suppression machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâspecific 7th-century treaty-breaking by surrounding tribesâis dead. However, the constraint has not atrophied into a piton because it has been repurposed as a general jurisprudential framework for defensive war and treaty fidelity in the modern international order. If it were purely inertial, we would expect a high theater ratio and no live coordination function; instead, the coordination function (peaceful pluralism, defensive legitimacy) is actively served, even as modern state practice drifts toward international law norms that partially supersede the classical framework. The status=dead plus verdict=world_rearranges flags a zombie/capture hypothesis for downstream review, but the narrative evidence points to functional repurposing rather than pure inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_specificity_boundary,
    'Does the contextual-defensive reading of 9:5 establish a time-bound historical command or a recurrent legal category applicable to any future treaty violation?',
    'Comparative jurisprudential analysis across madhabs and contemporary state conduct; survey of fatwa corpora to determine whether 9:5 is invoked as a general legal category or solely as historical precedent.',
    'If purely historical, the constraint''s modern extraction is near-zero and it functions closer to a scaffold or rope; if recurrent, it is an active enforcement mechanism with live victims and sustained extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_specificity_boundary, conceptual, 'Ambiguity between historical event and eternal legal norm').

omega_variable(
    abrogationist_contestation,
    'Is the suppression of the abrogating-universal reading within this framework a matter of hermeneutic methodology or political coercion?',
    'Analysis of institutional control over religious education, state licensing of preachers, and penalties for advocating offensive jihad in integrationist Muslim-majority states.',
    'If hermeneutic, suppression is lower and the constraint remains a tangled rope; if political, suppression is higher and the constraint leans toward snare characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogationist_contestation, empirical, 'Whether suppression of rival readings is methodological or coercive').

omega_variable(
    sibling_reading_nexus,
    'If the contextual-defensive reading''s core axioms were adopted by a state actor, would that structurally foreclose the abrogating-universal reading''s institutional presence, or merely influence its legitimacy conditions?',
    'Observation of state behavior in Muslim-majority countries that adopt integrationist constitutions: do they constitutionally ban abrogationist parties or merely marginalize them through bureaucratic means?',
    'Determines whether the relation to the abrogating reading is forecloses or influences; affects the coupling analysis and contamination propagation across the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_nexus, conceptual, 'Downstream structural pressure on sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qura_tr_t10, quran_9_5_scope__contextual_defensive, theater_ratio, 10, 0.22).
narrative_ontology:measurement(qura_tr_t20, quran_9_5_scope__contextual_defensive, theater_ratio, 20, 0.25).
narrative_ontology:measurement(qura_tr_t30, quran_9_5_scope__contextual_defensive, theater_ratio, 30, 0.28).
narrative_ontology:measurement(qura_tr_t40, quran_9_5_scope__contextual_defensive, theater_ratio, 40, 0.3).
narrative_ontology:measurement(qura_tr_t50, quran_9_5_scope__contextual_defensive, theater_ratio, 50, 0.32).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(qura_be_t10, quran_9_5_scope__contextual_defensive, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(qura_be_t20, quran_9_5_scope__contextual_defensive, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(qura_be_t30, quran_9_5_scope__contextual_defensive, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(qura_be_t40, quran_9_5_scope__contextual_defensive, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(qura_be_t50, quran_9_5_scope__contextual_defensive, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__contextual_defensive, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(qura_su_t10, quran_9_5_scope__contextual_defensive, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(qura_su_t20, quran_9_5_scope__contextual_defensive, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(qura_su_t30, quran_9_5_scope__contextual_defensive, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(qura_su_t40, quran_9_5_scope__contextual_defensive, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(qura_su_t50, quran_9_5_scope__contextual_defensive, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% The natural-language label 'Quran 9:5 scope' conflates three structurally distinct claims: a universal abrogating command (high extraction, universal scope), a contextual defensive command (moderate extraction, conditional scope), and a time-bound political directive (low extraction, historical scope). Each reading has different epsilon, victim/beneficiary structure, and classification. This file instantiates the contextual-defensive reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
