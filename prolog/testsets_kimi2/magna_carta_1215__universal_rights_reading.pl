% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta Clause 39 Universal Due Process Reading
 *   domain: constitutional/legal/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the universal_rights_reading of the
 *   Magna Carta 1215 kernel: the interpretive doctrine that Clause 39 ('No
 *   free man shall be seized or imprisoned... except by the lawful judgment
 *   of his peers or by the law of the land') binds all state power
 *   transhistorically and protects all persons from arbitrary detention and
 *   punishment. The reading treats 'free men' as a term of art that expands
 *   to universal personhood through common law development. It is contested
 *   by legal historians (baronial_privilege_reading) and by
 *   living-constitutional theorists (living_document_reading). Structurally,
 *   the constraint coordinates expectations of due process while
 *   concentrating interpretive authority in the judiciary and suppressing the
 *   feudal, limited historical understanding of the text.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary: Agenda-setter and authority beneficiary (institutional/generational) â administers the universal reading through precedent.
 *   - rights_bearing_individuals: Primary beneficiary (powerless/constrained) â gain procedural protections against arbitrary state power.
 *   - state_executive: Primary payer (powerful/constrained) â loses discretionary detention and punishment authority.
 *   - legal_historians: Excluded voice (moderate/analytical) â possess documentary evidence contradicting the universal reading but are structurally absent from constitutional jurisprudence.
 *   - human_rights_advocates: Secondary beneficiary (organized/mobile) â leverage the ancient pedigree in litigation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.6).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.55).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta Clause 39 Universal Due Process Reading").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional/legal/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, 'ca46e724-78a9-4300-999f-e5e6c9480ea1').
narrative_ontology:cs_kernel_codification('ca46e724-78a9-4300-999f-e5e6c9480ea1', fixed_text).
narrative_ontology:cs_authority_grounding('ca46e724-78a9-4300-999f-e5e6c9480ea1', lineage).
narrative_ontology:cs_interpretation_layer_present('ca46e724-78a9-4300-999f-e5e6c9480ea1').
narrative_ontology:cs_reading_relation('ca46e724-78a9-4300-999f-e5e6c9480ea1', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca46e724-78a9-4300-999f-e5e6c9480ea1', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('ca46e724-78a9-4300-999f-e5e6c9480ea1', foundational, transhistorical_universal_binding).
narrative_ontology:cs_axiom_status(transhistorical_universal_binding, holdable).
narrative_ontology:cs_axiom_grounding('ca46e724-78a9-4300-999f-e5e6c9480ea1', transhistorical_universal_binding, deontological).
narrative_ontology:cs_axiom('ca46e724-78a9-4300-999f-e5e6c9480ea1', foundational, clause_39_universal_personhood_scope).
narrative_ontology:cs_axiom_status(clause_39_universal_personhood_scope, holdable).
narrative_ontology:cs_axiom_grounding('ca46e724-78a9-4300-999f-e5e6c9480ea1', clause_39_universal_personhood_scope, deontological).
narrative_ontology:cs_reference_frame('ca46e724-78a9-4300-999f-e5e6c9480ea1', transhistorical_rights_charter).
narrative_ontology:cs_drift_state('ca46e724-78a9-4300-999f-e5e6c9480ea1', post_historical_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ca46e724-78a9-4300-999f-e5e6c9480ea1', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, rights_bearing_individuals).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, human_rights_advocates).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, constitutional_judiciary).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, state_executive).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, common_law_ancient_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Clause 39 as binding precedent for universal due process across common law jurisdictions. Derives institutional authority from guardianship of ancient liberties and the continuity narrative. Cannot abandon the Magna Carta lineage without destabilizing the legitimacy of judicial review over executive detention.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, constrained, global).

% Invoke the Magna Carta lineage in habeas corpus and due process claims against arbitrary detention. Benefit from judicial review of executive action. Cannot practically exit the state legal system or the common law precedent structure that frames their rights.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, rights_bearing_individuals, beneficiary,
    powerless, biographical, constrained, global).

% Must justify detention and punishment through procedurally recognized legal process rather than discretionary decree. Bears political and administrative costs of judicial oversight and habeas litigation. Routinely seeks bypasses through immigration detention, national security designation, and emergency powers.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, state_executive, payer,
    powerful, immediate, constrained, national).

% Possess documentary and linguistic evidence that Magna Carta was a feudal contract limited to a narrow elite. Their findings are cited in textbooks but are systematically excluded from constitutional jurisprudence because they destabilize the transhistorical narrative required by modern due process doctrine.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legal_historians, excluded,
    moderate, civilizational, analytical, global).

% Cite Magna Carta in litigation and advocacy to extend due process protections and challenge arbitrary state action. Leverage the ancient parchment pedigree to add legitimating weight to modern rights claims against contemporary governments.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, human_rights_advocates, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__universal_rights_reading, constitutional_judiciary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a historically grounded, cross-temporal standard that coordinates expectations between individuals and state power by establishing a shared procedural norm against arbitrary detention and punishment.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy from the executive to the judiciary, and moves historical capital from the 1215 text to modern rights claimants and judicial institutions.
% ABSENT_VOICES: Legal historians and medievalists who document the charter's feudal, baronial limits are structurally excluded from constitutional jurisprudence; their absence allows the universal reading to appear historically self-evident rather than doctrinally constructed.
% DISAPPEARANCE_RATIONALE: Common law constitutionalism relies on Magna Carta as a foundational constraint. If the universal due process reading vanished, habeas corpus doctrine and judicial review of executive detention would lose their transhistorical anchor, requiring re-founding on purely statutory or philosophical grounds, while executive power would expand into the legitimating vacuum.
% FOUNDING_PROBLEM: How to constrain arbitrary royal power over individuals in the absence of a written constitution by appealing to an ancient, revered legal text.
% FOUNDING_PROBLEM_CORROBORATION: Human rights advocates and judicial institutions attest the problem of arbitrary executive power is still live. Legal historians attest that the original 1215 arrangement solved a baronial, not universal, problem, and that modern arbitrary power persists in immigration and security domains despite the constraint; no neutral party outside the benefiting legal institutions fully corroborates the universal efficacy claim.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60 at interval end) is substantial because the universal reading empowers the judiciary and human rights advocates with historically derived authority that outruns the text's original scope, while constraining executive discretion. Suppression (0.55) reflects the active exclusion of feudal-historical interpretations from binding constitutional reasoning and the suppression of non-judicial justice alternatives. Theater_ratio (0.45) captures the performative invocation of an 810-year-old parchment to legitimize modern outcomes where functional protection is partial. Accessibility_collapse (0.60) indicates that once inside the common law system, alternatives to Magna-Carta-based due process are difficult to articulate. Resistance (0.35) is moderate: executives routinely seek bypasses (immigration, security), but frontal rejection of Magna Carta is rare. Temporal measurements show extraction and theater rising through the constitutional era (1679â1948) and plateauing as the reading became normalized.
 *
 * PERSPECTIVAL GAP:
 *   The constitutional judiciary experiences this constraint as a source of institutional legitimacy and interpretive power â a rope-like inheritance they steward. The state executive experiences it as an extraction of discretionary authority. Rights-bearing individuals experience it as protective coordination, though its effectiveness is uneven. Legal historians experience it as a suppression of documentary truth. These divergences are structurally encoded: the same text operates as subsidy for the judiciary, cost for the executive, and shield (partial) for individuals.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (rights_bearing_individuals, human_rights_advocates, constitutional_judiciary) receive low directionality: the constraint subsidizes their protection or authority. Victims (state_executive) receive high directionality: the constraint extracts discretionary power. The legal_historians, though not declared in victim or beneficiary arrays, are excluded observers with analytical exit. No override is needed because the structural derivation matches the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem â limiting arbitrary royal power over a narrow feudal elite â has been generalized to a universal human rights claim. Whether this generalization is a legitimate expansion or a mandatrophic drift depends on whether one accepts the transhistorical rights premise. The universal reading asserts the founding problem was always broader; the baronial reading asserts the problem was solved for barons and later appropriated. The contested founding_problem_status prevents simple mandatrophy resolution: the constraint is either a living scaffold that grew into its true shape, or a piton performing ancient lineage for modern authority. The authored metrics lean toward tangled_rope because the universal claim generates real coordination (habeas corpus) alongside asymmetric authority concentration and historical-suppression costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magna_carta_historical_authenticity,
    'Does Clause 39 of the 1215 Magna Carta structurally support a universal due process reading, or is this reading a retrospective legal fiction imposed on a feudal text?',
    'Interdisciplinary historical-legal analysis comparing the semantic range of ''liber homo'' in thirteenth-century legal Latin against the doctrinal expansions performed by seventeenth- and twentieth-century common law courts.',
    'If the universal reading is a fiction, the constraint''s legitimacy derives from interpretive tradition rather than textual continuity, shifting classification toward living_document_reading or exposing the current constraint as extraction through false lineage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magna_carta_historical_authenticity, conceptual, 'Authenticity of universal due process derivation from feudal text').

omega_variable(
    universal_protection_enforcement_gap,
    'To what extent does the universal due process constraint actually constrain contemporary state executives versus being bypassed through immigration detention, national security designation, or emergency powers?',
    'Empirical audit of habeas corpus grant rates, executive detention statistics, and judicial deference patterns across national security and immigration contexts in common law jurisdictions.',
    'A wide enforcement gap would indicate the constraint operates more as theatrical legitimacy than functional coordination; a narrow gap would support the coordination narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_protection_enforcement_gap, empirical, 'Gap between universal due process claim and actual executive constraint').

omega_variable(
    kernel_reading_containment,
    'Does the universal_rights_reading foreclose the baronial_privilege_reading as a live historical interpretation, or do they occupy separate discursive domains such that foreclosure is illusory?',
    'Analysis of whether legal training and precedent systems structurally prevent baronial readings from entering constitutional reasoning even when historically valid.',
    'If structurally foreclosed in legal discourse despite historical validity, the constraint functions as an identity-locked interpretive regime; if merely coextensive, the readings are parallel constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_containment, conceptual, 'Whether universal reading structurally suppresses baronial reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__universal_rights_reading, theater_ratio, 1215, 0.05).
narrative_ontology:measurement(magn_tr_t1500, magna_carta_1215__universal_rights_reading, theater_ratio, 1500, 0.15).
narrative_ontology:measurement(magn_tr_t1679, magna_carta_1215__universal_rights_reading, theater_ratio, 1679, 0.3).
narrative_ontology:measurement(magn_tr_t1789, magna_carta_1215__universal_rights_reading, theater_ratio, 1789, 0.38).
narrative_ontology:measurement(magn_tr_t1948, magna_carta_1215__universal_rights_reading, theater_ratio, 1948, 0.42).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_1215__universal_rights_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__universal_rights_reading, base_extractiveness, 1215, 0.15).
narrative_ontology:measurement(magn_be_t1500, magna_carta_1215__universal_rights_reading, base_extractiveness, 1500, 0.25).
narrative_ontology:measurement(magn_be_t1679, magna_carta_1215__universal_rights_reading, base_extractiveness, 1679, 0.4).
narrative_ontology:measurement(magn_be_t1789, magna_carta_1215__universal_rights_reading, base_extractiveness, 1789, 0.55).
narrative_ontology:measurement(magn_be_t1948, magna_carta_1215__universal_rights_reading, base_extractiveness, 1948, 0.65).
narrative_ontology:measurement(magn_be_t2025, magna_carta_1215__universal_rights_reading, base_extractiveness, 2025, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__universal_rights_reading, suppression_requirement, 1215, 0.2).
narrative_ontology:measurement(magn_su_t1500, magna_carta_1215__universal_rights_reading, suppression_requirement, 1500, 0.35).
narrative_ontology:measurement(magn_su_t1679, magna_carta_1215__universal_rights_reading, suppression_requirement, 1679, 0.55).
narrative_ontology:measurement(magn_su_t1789, magna_carta_1215__universal_rights_reading, suppression_requirement, 1789, 0.65).
narrative_ontology:measurement(magn_su_t1948, magna_carta_1215__universal_rights_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(magn_su_t2025, magna_carta_1215__universal_rights_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, living_document_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Magna Carta 1215 kernel, decomposed per the epsilon-invariance principle because the universal due process reading and the feudal contract reading have structurally distinct epsilon values, beneficiary sets, and enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
