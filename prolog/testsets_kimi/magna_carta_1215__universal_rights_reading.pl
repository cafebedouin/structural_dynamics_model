% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta Clause 39 Universal Due Process Constraint
 *   domain: constitutional/law/history
 *
 * SUMMARY:
 *   The Magna Carta (1215) Clause 39 constraint under the universal rights
 *   reading treats the charter as a transhistorical rights precedent in which
 *   'free men' encompasses all persons, and the clause emits a universal due
 *   process constraint binding all state power. This reading expands the
 *   protected set from feudal barons to all individuals, constraining
 *   arbitrary detention and extrajudicial punishment. It is contested within
 *   a kernel that includes a narrow baronial privilege reading and a living
 *   document reading. The constraint coordinates political order around
 *   procedural legitimacy while asymmetrically extracting discretion from
 *   executive and crown power.
 *
 * KEY AGENTS:
 *   - rights_bearers: Universal beneficiary class â all persons protected from arbitrary state power (powerless/universal/constrained).
 *   - legal_profession: Secondary beneficiary â organized actors who gain from interpretive complexity and procedural advocacy (organized/national/constrained).
 *   - crown: Primary target â historical monarchical power and modern executive successors whose arbitrary authority is procedurally constrained (powerful/national/constrained).
 *   - executive_agents: Target â state bureaucrats and security actors whose operational discretion is limited by due process requirements (institutional/national/constrained).
 *   - common_law_judiciary: Agenda setter â courts that interpret and enforce the due process constraint, deriving institutional authority (institutional/national/constrained).
 *   - legal_historians: Observer â analysts who document the anachronism between the 1215 feudal category and modern universal claims (analytical/global/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.72).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.78).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta Clause 39 Universal Due Process Constraint").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional/law/history").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, '1af66c9b-e057-425d-ad9a-8e867154c500').
narrative_ontology:cs_kernel_codification('1af66c9b-e057-425d-ad9a-8e867154c500', fixed_text).
narrative_ontology:cs_authority_grounding('1af66c9b-e057-425d-ad9a-8e867154c500', lineage).
narrative_ontology:cs_interpretation_layer_present('1af66c9b-e057-425d-ad9a-8e867154c500').
narrative_ontology:cs_reading_relation('1af66c9b-e057-425d-ad9a-8e867154c500', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('1af66c9b-e057-425d-ad9a-8e867154c500', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('1af66c9b-e057-425d-ad9a-8e867154c500', foundational, universal_personhood_due_process).
narrative_ontology:cs_axiom_status(universal_personhood_due_process, holdable).
narrative_ontology:cs_axiom_grounding('1af66c9b-e057-425d-ad9a-8e867154c500', universal_personhood_due_process, deontological).
narrative_ontology:cs_axiom('1af66c9b-e057-425d-ad9a-8e867154c500', foundational, transhistorical_rights_continuity).
narrative_ontology:cs_axiom_status(transhistorical_rights_continuity, holdable).
narrative_ontology:cs_axiom_grounding('1af66c9b-e057-425d-ad9a-8e867154c500', transhistorical_rights_continuity, conventional).
narrative_ontology:cs_reference_frame('1af66c9b-e057-425d-ad9a-8e867154c500', universal_personhood_due_process).
narrative_ontology:cs_drift_state('1af66c9b-e057-425d-ad9a-8e867154c500', contemporary_rights_regime, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1af66c9b-e057-425d-ad9a-8e867154c500', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, rights_bearers).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, legal_profession).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, crown).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, executive_agents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All persons subject to state power who claim protection from arbitrary detention and punishment under the universal due process reading of Clause 39; they benefit from the constraint's expansive coverage but have no direct exit from state jurisdiction.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, rights_bearers, beneficiary,
    powerless, generational, constrained, universal).

% Legal practitioners who benefit from the procedural complexity and interpretive tradition of due process, which sustains professional demand, status, and a steady flow of advocacy work.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legal_profession, beneficiary,
    organized, generational, constrained, national).

% Historical monarchical authority and its modern executive successors whose arbitrary power to detain and punish is directly constrained by the due process requirement; they bear the cost of procedural limits and lost discretion.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, crown, payer,
    powerful, generational, constrained, national).

% State bureaucrats, security agencies, and law enforcement whose operational discretion is limited by procedural safeguards, evidentiary rules, and judicial oversight; they pay in reduced autonomy and speed of action.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, executive_agents, payer,
    institutional, generational, constrained, national).

% Courts and jurists who interpret and enforce the due process constraint, deriving institutional authority from their role as gatekeepers of legitimate state power, while remaining bound by precedent and interpretive tradition.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, common_law_judiciary, agenda_setter,
    institutional, civilizational, constrained, national).

% Scholars who document the historical limitations of the 1215 category 'free men' and contest the universal reading as anachronistic; they observe the interpretive expansion as a modern construction rather than historical fact.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents arbitrary state detention and punishment by requiring judgment according to the law of the land and due process, coordinating political order around procedural legitimacy rather than executive will.
% TRANSFER_FUNCTION: Transfers discretion over imprisonment and punishment from executive power to legal process and adjudicating courts; transfers interpretive authority over rights boundaries from the crown to the judiciary and legal profession.
% ABSENT_VOICES: Feudal subjects outside the 'free men' category â serfs, unfree laborers, and women in many historical contexts â are structurally absent from the original charter; modern universal readings absorb them retrospectively without their historical voice in the bargain.
% DISAPPEARANCE_RATIONALE: If the universal due process constraint disappeared, arbitrary detention would become immediately procedurally permissible, courts would lose their checking authority against executive power, and the modern rights order built upon this precedent would destabilize.
% FOUNDING_PROBLEM: Baronial rebellion against arbitrary royal imprisonment and taxation without feudal consent; the immediate problem was King John's abuse of seigneurial power against specific nobles.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians attest the founding problem was narrow baronial privilege and specific feudal grievances; the modern universal rights reading is not corroborated by the charter's historical parties but by later constitutional movements who repurposed the text.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is substantial because the constraint removes vast discretionary authority from state power and reallocates it to procedural process; it is not maximal because the coordination benefit (preventing arbitrary violence) is genuine. Suppression (0.78) is high: the constraint's operation depends on actively suppressing arbitrary executive action through courts, habeas corpus, and sanctions. Theater ratio (0.48) reflects centuries of performative citation of Magna Carta in political rhetoric that exceeds the original text's scope. Accessibility collapse (0.62) is moderately high: once the universal reading is accepted, alternatives (pure executive prerogative) become illegitimate in the legal order. Resistance (0.45) reflects persistent executive and security-state pushback against due process limits. Temporal series show rising extraction and suppression as the universal reading expanded from feudal barons to all persons, with theater increasing as the text was ritually invoked.
 *
 * PERSPECTIVAL GAP:
 *   Rights-bearers experience the constraint as protective coordination because it shields them from arbitrary state violence. The crown and executive agents experience it as extractive because it strips their discretionary power and imposes procedural costs. The judiciary occupies a mediating seat: it administers the constraint and gains authority, but is also bound by interpretive tradition. Legal historians see a constructed myth that naturalizes modern rights in a medieval text. The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights-bearers are declared beneficiaries with powerless/constrained/universal â structural derivation pushes d toward the beneficiary end (low d, subsidized by the constraint). The legal profession is a beneficiary with organized/constrained/national â also low d. Crown and executive_agents are declared victims with powerful/institutional and constrained/national â high d, full targets. The common_law_judiciary is agenda_setter (not in beneficiary/victim arrays) with institutional/constrained; without override it receives the canonical fallback for institutional, which sits near symmetric. This accurately reflects their dual position as both empowered by and bound to the interpretive tradition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â baronial rebellion against King John's arbitrary imprisonment of specific nobles â is dead. The constraint persists eight centuries later as a generalized due process norm. This creates mandatrophy risk: if the coordination function (preventing arbitrary detention) were dead, the constraint would be a piton. But the coordination function remains live for rights-bearers today; arbitrary detention is still a real threat, and the constraint still solves it. Thus the persistence is not purely inertial. However, the divergence between the original narrow feudal problem and the current universal scope means a substantial portion of the constraint's scope is carried by interpretive tradition rather than live problem-solving. The temporal measurement series captures this accumulation: base_extractiveness rises as the reading expands, suggesting layered extraction atop a genuine coordination core.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    free_men_universal_scope,
    'Does the 1215 text''s ''free men'' refer to a universal human category or a narrow, status-bound feudal class?',
    'Historical philology and legal-historical analysis of thirteenth-century English status categories and charter usage.',
    'If ''free men'' is a narrow feudal class, the universal reading is an anachronistic construction and the constraint''s scope is a modern invention; this would shift the constraint toward a snare or piton (extractive myth) or at least sever its textual authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_men_universal_scope, empirical, 'Ambiguity of the ''free men'' category in the original 1215 text').

omega_variable(
    state_disposition_asymmetry,
    'Is the constraint''s limitation on executive discretion genuinely extractive victimization of state power, or symmetric coordination that legitimates the state?',
    'Comparative analysis of state legitimacy and compliance costs under due process regimes versus arbitrary rule.',
    'If the state is a genuine victim of extraction, the tangled rope classification holds; if the constraint is symmetrically costless to legitimate state power, the classification shifts toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_disposition_asymmetry, preference, 'Whether the state pays an asymmetric cost or gains coordination benefit').

omega_variable(
    interpretive_layer_inflation,
    'Has the interpretive layer (common law judiciary) expanded the constraint beyond its coordination function into professional rent-seeking?',
    'Economic analysis of legal profession rents and judicial institutional expansion relative to baseline rights protection needs.',
    'Would clarify what portion of the measured extractiveness is inherent to due process coordination versus captured by the interpretive agenda-setter.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_layer_inflation, empirical, 'Whether judicial interpretation has inflated extraction beyond coordination need').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__universal_rights_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1300, magna_carta_1215__universal_rights_reading, theater_ratio, 1300, 0.15).
narrative_ontology:measurement(magn_tr_t1500, magna_carta_1215__universal_rights_reading, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_1215__universal_rights_reading, theater_ratio, 1689, 0.35).
narrative_ontology:measurement(magn_tr_t1789, magna_carta_1215__universal_rights_reading, theater_ratio, 1789, 0.4).
narrative_ontology:measurement(magn_tr_t1948, magna_carta_1215__universal_rights_reading, theater_ratio, 1948, 0.45).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_1215__universal_rights_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__universal_rights_reading, base_extractiveness, 1215, 0.25).
narrative_ontology:measurement(magn_be_t1300, magna_carta_1215__universal_rights_reading, base_extractiveness, 1300, 0.3).
narrative_ontology:measurement(magn_be_t1500, magna_carta_1215__universal_rights_reading, base_extractiveness, 1500, 0.35).
narrative_ontology:measurement(magn_be_t1689, magna_carta_1215__universal_rights_reading, base_extractiveness, 1689, 0.5).
narrative_ontology:measurement(magn_be_t1789, magna_carta_1215__universal_rights_reading, base_extractiveness, 1789, 0.6).
narrative_ontology:measurement(magn_be_t1948, magna_carta_1215__universal_rights_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(magn_be_t2025, magna_carta_1215__universal_rights_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__universal_rights_reading, suppression_requirement, 1215, 0.2).
narrative_ontology:measurement(magn_su_t1300, magna_carta_1215__universal_rights_reading, suppression_requirement, 1300, 0.3).
narrative_ontology:measurement(magn_su_t1500, magna_carta_1215__universal_rights_reading, suppression_requirement, 1500, 0.35).
narrative_ontology:measurement(magn_su_t1689, magna_carta_1215__universal_rights_reading, suppression_requirement, 1689, 0.5).
narrative_ontology:measurement(magn_su_t1789, magna_carta_1215__universal_rights_reading, suppression_requirement, 1789, 0.6).
narrative_ontology:measurement(magn_su_t1948, magna_carta_1215__universal_rights_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(magn_su_t2025, magna_carta_1215__universal_rights_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, living_document_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the magna_carta_1215 kernel. The universal_rights_reading, baronial_privilege_reading, and living_document_reading are structurally distinct constraints sharing a historical label but differing in scope, beneficiary/victim structure, and epsilon. They are linked as a constraint family per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
