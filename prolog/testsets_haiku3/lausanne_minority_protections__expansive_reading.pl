% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Treaty Minority Protections (Expansive Reading)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The Treaty of Lausanne (1923) includes provisions protecting religious
 *   minorities in Turkey. The expansive reading interprets these provisions
 *   as guaranteeing functional continuity of pre-1923 religious institutional
 *   structures: ecclesiastical self-governance, property rights, and
 *   theological education. The constraint operates as a coordination
 *   mechanism coordinating the expectation that minorities retain
 *   institutional autonomy; it is not extractive because no party collects
 *   rents from the arrangement. However, it is vulnerable: if the restrictive
 *   reading gains dominance, the entire institutional protection collapses.
 *   The expansive reading is a moderately coordinated rope that depends
 *   entirely on its ability to persist as the authoritative interpretation
 *   against competing readings that would dissolve it.
 *
 * KEY AGENTS:
 *   - Recognized religious minorities (Christians, Jews): beneficiaries of institutional autonomy guarantees; trapped in territory and dependent on treaty enforcement
 *   - Turkish state government: agenda-setter; holds interpretive authority and enforcement capacity; incentivized to select reading that maximizes state control
 *   - European human rights bodies (ECHR, Council of Europe): observers; provide external legitimacy for expansive reading; limited enforcement
 *   - Guarantor states (France, Italy, Greece): observers; retain formal treaty enforcement rights but rarely exercise them
 *   - Turkish nationalist constituencies: excluded from formal interpretation but shape domestic political pressure toward restrictive reading
 *   - Minority theological schools: powerless beneficiaries; identity-locked; face closure under restrictive reading
 *   - Minority institutional leadership (patriarchs, metropolitans): beneficiaries; constrained; lose authority under restrictive reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.28).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.42).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Treaty Minority Protections (Expansive Reading)").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, 'c5518828-752b-48b9-b4b8-ca86bf4c5031').
narrative_ontology:cs_kernel_codification('c5518828-752b-48b9-b4b8-ca86bf4c5031', fixed_text).
narrative_ontology:cs_authority_grounding('c5518828-752b-48b9-b4b8-ca86bf4c5031', lineage).
narrative_ontology:cs_interpretation_layer_present('c5518828-752b-48b9-b4b8-ca86bf4c5031').
narrative_ontology:cs_reading_relation('c5518828-752b-48b9-b4b8-ca86bf4c5031', lausanne_minority_protections__restrictive_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5518828-752b-48b9-b4b8-ca86bf4c5031', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('c5518828-752b-48b9-b4b8-ca86bf4c5031', foundational, institutional_autonomy_binding_obligation).
narrative_ontology:cs_axiom_status(institutional_autonomy_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c5518828-752b-48b9-b4b8-ca86bf4c5031', institutional_autonomy_binding_obligation, deontological).
narrative_ontology:cs_axiom('c5518828-752b-48b9-b4b8-ca86bf4c5031', foundational, treaty_text_protects_substantive_not_merely_procedural_autonomy).
narrative_ontology:cs_axiom_status(treaty_text_protects_substantive_not_merely_procedural_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('c5518828-752b-48b9-b4b8-ca86bf4c5031', treaty_text_protects_substantive_not_merely_procedural_autonomy, deontological).
narrative_ontology:cs_reference_frame('c5518828-752b-48b9-b4b8-ca86bf4c5031', pre_1923_institutional_continuity).
narrative_ontology:cs_drift_state('c5518828-752b-48b9-b4b8-ca86bf4c5031', contemporary_nationalized_turkey, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c5518828-752b-48b9-b4b8-ca86bf4c5031', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, recognized_religious_minorities).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_theological_schools).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_institutional_leadership).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, treaty_based_minority_rights_doctrine).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, institutional_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Christian and Jewish minorities in Turkey holding the right to self-administer religious affairs, maintain property, and operate theological schools. Their institutional continuity depends entirely on the treaty's enforceability and the state's honor of the reading that guarantees functional autonomy. They cannot exit the territory without abandoning ancestral institutions and cannot sue in domestic courts for treaty enforcement.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, recognized_religious_minorities, beneficiary,
    moderate, generational, trapped, national).

% Holds the interpretive authority over how the treaty applies domestically. Under the expansive reading, it is bound to permit institutional autonomy and property rights; under the restrictive reading it retains full discretion. It administers the constraint through domestic law and practice. It has the de facto power to implement either reading but faces diplomatic costs if it selects the restrictive reading too visibly.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_state_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Monitor Turkish treatment of minorities through European Court of Human Rights and Council of Europe mechanisms. They can issue rulings and recommendations that influence which reading gains legitimacy, but cannot directly enforce treaty interpretation. They provide external validation of the expansive reading's interpretation of minority rights.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, european_human_rights_bodies, observer,
    institutional, generational, analytical, continental).

% France, Italy, Greece, and other Lausanne guarantor signatories retain formal rights to enforce the treaty on behalf of minorities but have rarely exercised them after 1950. Their potential intervention creates diplomatic leverage for the expansive reading but their passivity enables restrictive reading practice.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, guarantor_states, observer,
    powerful, generational, constrained, continental).

% Oppose the expansive reading as a constraint on Turkish sovereignty and national cultural homogeneity. They advocate for the restrictive reading that subjects minorities to general law. They are excluded from formal treaty interpretation but shape domestic political pressure against minority institutional autonomy.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_nationalist_constituencies, excluded,
    organized, generational, mobile, national).

% Operate under the expansive reading's guarantee of the right to form clergy and conduct theological education. Under the restrictive reading they face closure or forced integration into state educational systems. Their institutional identity and religious continuity are inseparable from this constraint.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_theological_schools, beneficiary,
    powerless, generational, identity_locked, local).

% Patriarchs, metropolitans, and rabbinical councils exercise self-governance rights under the expansive reading. They administer religious discipline, ordain clergy, manage property, and represent their communities. The reading guarantees their authority; loss of the reading leaves them subject to state appointments and direct state control.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_institutional_leadership, beneficiary,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__expansive_reading, diffuse).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__expansive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-WWI problem of preserving minority religious institutions through a transitional treaty framework. The expansive reading coordinates the expectation that pre-1923 institutional structures (theological schools, ecclesiastical hierarchy, property ownership) will continue functioning under minority self-administration rather than being absorbed into Turkish state uniformity.
% TRANSFER_FUNCTION: Moves the obligation to respect minority institutional autonomy from the multinational Ottoman system (dissolved 1923) to the new Turkish nation-state. The constraint transfers from the international guarantors (France, Italy, Greece) to Turkey the duty to enforce; from the state to the minorities the right to self-govern. The minorities transfer property rights claims and clergy formation authority into the treaty framework rather than holding them under domestic law.
% ABSENT_VOICES: Turkish nationalist constituencies that oppose the expansive reading as a constraint on sovereignty are excluded from formal treaty interpretation bodies and from the decision machinery of minority institutions themselves. Turkish secular modernizers who view theological schools as backward are not seated. Representatives of populations displaced or expelled in the 1920s (Armenian, Greek, Anatolian Muslims) are absent from the contemporary implementation; they would dispute whether any reading adequately honors what was lost.
% DISAPPEARANCE_RATIONALE: If the expansive reading and its enforcement were to disappear overnight, minority theological schools would face immediate state pressure toward closure or integration into secular education; ecclesiastical hierarchies would lose the authority to appoint clergy and administer discipline independently; minority property claims would enter the domain of general Turkish property law with no special protection; the institutional continuity of pre-1923 religious governance would collapse within a generation as the state absorbed or dissolved autonomous structures. The Turkish state's ability to unify religious institutions under state supervision would advance markedly.
% FOUNDING_PROBLEM: The Treaty of Lausanne (1923) was built to protect religious minority institutions from absorption into a new Turkish nation-state that had just destroyed the Ottoman multinational framework. Minorities feared forced assimilation, loss of property, closure of theological schools, and state control of clergy appointment. The expansive reading interprets the treaty as guaranteeing that these pre-1923 institutional structures would persist in functional form.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Treaty document the explicit intent to protect minority institutions (Davison, McCarthy, Mackie). The European Court of Human Rights in Kokkinakis and subsequent cases affirms that the founding problem (protecting religious freedom and institutional autonomy) remains live. However, Turkish state officials and secular modernizers argue the founding problem was to establish a uniform nation-state and that institutional autonomy was always meant to be circumscribed by national law. The contest is documented in academic analysis (Ergün, Smyrnelis) and reflected in the divergent readings themselves.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures vulnerability rather than active extraction. The expansive reading extracts institutional autonomy costs on the state (0.28 by interval end): the state must permit self-governance structures it views as inefficient or regressive. It does not extract from minorities because they gain autonomy and institutions. The temporal series rises from 0.12 to 0.28 as nationalist pressure accumulates and the state increasingly views the reading as a constraint on modernization. Suppression requirement rises from 0.15 to 0.42 because maintaining the expansive reading against the restrictive reading requires active diplomatic and legal enforcement; without it, domestic power gravitates toward state control. Theater ratio rises from 0.08 to 0.31 as the state performs compliance (procedural recognition of institutional leadership) while gradually eroding substantive autonomy (appointments subject to state veto, property claims slow to adjudicate, schools face accreditation requirements that functionally subordinate them to state curriculum). The measurement series share one aligned time grid.
 *
 * PERSPECTIVAL GAP:
 *   The minority institutional leadership seat experiences the constraint as essential to survival; the state government seat experiences it as an unwanted limit on sovereignty. The payer (Turkish state) and beneficiary (minority institutions) seats should diverge sharply in the engine's computation: minorities should compute as clearly benefiting from a rope that protects them; the state should compute as paying for coordination costs it views as unjustified. The treaty's formal status as international law should modulate the state's effective exit options — reinterpretation requires diplomatic cover, which makes arbitrage constrained rather than pure. If computed correctly, the state seat should show higher χ than the minority seat, reflecting asymmetric extraction of sovereignty costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Recognized religious minorities are straightforward beneficiaries (d near 0.0–0.15): they gain institutional autonomy without bearing extraction costs; exit is trapped (cannot leave territory without abandoning institutions), which locks in their directionality. The Turkish state is more complex: formally it is a payer (constrained to permit autonomy it views as costly) but holds the interpretive agenda (agenda_setter role). Its d should be near the target end (0.75–0.90) because the treaty constraint effectively extracts institutional deference costs; however, its arbitrage exit option (reinterpret the treaty to the restrictive reading) pulls it downward artificially. No directionality override is needed if exit is modeled correctly — the ability to reinterpret constrains the extraction; it is exit, not beneficiary status. Guarantor states are observers (d near 0.5: they formalized the constraint but rarely enforce it; passive benefit from stability, no active cost). Excluded constituencies (nationalists) are outside the d framework entirely — they are not parties to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The expansive reading avoids mandatrophy through constant re-legitimation: European human rights bodies validate the reading's interpretation regularly; minority institutions actively exercise the autonomy the reading guarantees; guarantor states retain the option to enforce (even if unused). The founding problem (protecting institutions from assimilation) remains live, which prevents the reading from being purely inertial. However, the temporal measurement series show rising theater ratio, indicating that substantive autonomy is eroding while procedural recognition (theater) increases — a pattern consistent with the constraint entering a pre-piton state. If theater continues rising toward 0.50–0.60 while suppression_requirement stays high, the reading will show mandatrophy signatures: the constraint persists not because it solves the founding problem but because dismantling it requires costly diplomatic action.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_interpretation_authority,
    'Who has the authoritative power to interpret what the Lausanne protections require: the Turkish state domestically, European human rights courts, guarantor states through diplomacy, or the treaty text itself?',
    'Formal treaty amendment or a definitive ruling by an international tribunal that settles the interpretation authority hierarchy. Short of that, observing whether ECHR rulings or guarantor state pressure changes Turkish state practice would indicate where effective authority lies.',
    'If the Turkish state retains interpretive authority, the restrictive reading will eventually dominate through domestic legislative change. If European courts or guarantor states hold authority, the expansive reading can persist. The location of authority determines whether this constraint is rope (genuinely coordinated) or extractive (imposed from outside).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_interpretation_authority, empirical, 'Which institutional seat holds effective authority to interpret Lausanne protections.').

omega_variable(
    institutional_autonomy_vs_general_law_boundary,
    'Is institutional autonomy structurally separable from general Turkish law, or is it ultimately subject to general law with only procedural exceptions?',
    'Observing whether minority institutions can operate outside general law in practice: if theological schools accept state curriculum requirements, ecclesiastical courts apply state-defined law, and property is registered in state systems, autonomy is procedural not substantive. If institutions operate genuinely independent systems, autonomy is separable.',
    'Separation favors the expansive reading as rope; integration into general law favors the restrictive reading. The boundary is the crux of whether the constraint coordinates genuine institutional difference or merely governs appearance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_autonomy_vs_general_law_boundary, conceptual, 'Whether institutional autonomy is separable from Turkish state law or ultimately subordinate.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the rise in suppression_requirement driven by structural state enforcement machinery (deportations, school closures, property seizures) or by internalized suppression (minorities self-limiting their claims, accepting procedural forms while losing substance)?',
    'Historical analysis of specific incidents: state actions that directly forbid institutional practice (structural suppression), vs. cases where minorities preemptively limit their claims to maintain institutional survival (internalized suppression through captured expectations).',
    'Structural suppression indicates active state coercion; internalized indicates the reading is being eroded by minorities'' rational adaptation to rising costs. Both produce the rising suppression_requirement metric but have different mechanisms and different implications for repair.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether rising suppression reflects structural coercion or internalized constraints on minority claims.').

omega_variable(
    reading_contest_sibling_distinction,
    'What structural features distinguish this expansive reading from its restrictive sibling, and how does each reading operationalize the boundary it claims?',
    'Detailed analysis of actual cases: when minorities attempt institutional action (clergy appointment, property transfer, school operation), which reading''s framework prevails? The reading that governs actual outcomes is the one with effective authority, regardless of formal treaty language.',
    'If the restrictive reading governs outcomes, this expansive reading is inoperative regardless of its formal claim. The measurement of extractiveness and suppression would need to be reframed to reflect the reading that actually holds authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_sibling_distinction, empirical, 'Whether the expansive reading''s claimed distinction from the restrictive reading is operationally real.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t0, lausanne_minority_protections__expansive_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(laus_tr_t0, observed).
narrative_ontology:measurement(laus_tr_t20, lausanne_minority_protections__expansive_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(laus_tr_t20, observed).
narrative_ontology:measurement(laus_tr_t40, lausanne_minority_protections__expansive_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(laus_tr_t40, observed).
narrative_ontology:measurement(laus_tr_t60, lausanne_minority_protections__expansive_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement_basis(laus_tr_t60, observed).
narrative_ontology:measurement(laus_tr_t80, lausanne_minority_protections__expansive_reading, theater_ratio, 80, 0.31).
narrative_ontology:measurement_basis(laus_tr_t80, observed).
narrative_ontology:measurement(laus_tr_t100, lausanne_minority_protections__expansive_reading, theater_ratio, 100, 0.31).
narrative_ontology:measurement_basis(laus_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(laus_be_t0, lausanne_minority_protections__expansive_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(laus_be_t0, observed).
narrative_ontology:measurement(laus_be_t20, lausanne_minority_protections__expansive_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(laus_be_t20, observed).
narrative_ontology:measurement(laus_be_t40, lausanne_minority_protections__expansive_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement_basis(laus_be_t40, observed).
narrative_ontology:measurement(laus_be_t60, lausanne_minority_protections__expansive_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement_basis(laus_be_t60, observed).
narrative_ontology:measurement(laus_be_t80, lausanne_minority_protections__expansive_reading, base_extractiveness, 80, 0.28).
narrative_ontology:measurement_basis(laus_be_t80, observed).
narrative_ontology:measurement(laus_be_t100, lausanne_minority_protections__expansive_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement_basis(laus_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t0, lausanne_minority_protections__expansive_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(laus_su_t0, observed).
narrative_ontology:measurement(laus_su_t20, lausanne_minority_protections__expansive_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement_basis(laus_su_t20, observed).
narrative_ontology:measurement(laus_su_t40, lausanne_minority_protections__expansive_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement_basis(laus_su_t40, observed).
narrative_ontology:measurement(laus_su_t60, lausanne_minority_protections__expansive_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(laus_su_t60, observed).
narrative_ontology:measurement(laus_su_t80, lausanne_minority_protections__expansive_reading, suppression_requirement, 80, 0.42).
narrative_ontology:measurement_basis(laus_su_t80, observed).
narrative_ontology:measurement(laus_su_t100, lausanne_minority_protections__expansive_reading, suppression_requirement, 100, 0.42).
narrative_ontology:measurement_basis(laus_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__expansive_reading, 0.12).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Lausanne minority protections kernel. The expansive reading (this story) interprets protections as guaranteeing institutional autonomy. The restrictive reading interprets them as limited to individual rights subject to general law. The guarantor reading interprets them as enforced through international mechanisms. The three readings share the same treaty text but operationalize different constraint structures. Each story has its own ε (extractiveness measured relative to its own reading's referent), beneficiary/victim structure (different agents are beneficiaries under each reading), and claimed type. The ε-invariance principle requires separate stories: a unified story trying to average over readings would be observing-dependent, violating the invariance rule. Link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__expansive_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
