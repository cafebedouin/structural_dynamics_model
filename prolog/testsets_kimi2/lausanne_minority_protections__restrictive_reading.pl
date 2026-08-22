% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Restrictive Reading of Lausanne Minority Protections (Institutional Denial)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This constraint story captures the restrictive reading of the Lausanne
 *   Treaty minority protections (Articles 37-45) as operationalized in
 *   Turkish domestic law. Under this reading, Lausanne guarantees only
 *   individual worship rightsâprayer, ritual observance, and private
 *   conscienceâwhile institutional autonomy, communal property ownership,
 *   theological education, and independent legal personality are classified
 *   as ordinary domestic matters subject to general Turkish law. The result
 *   is a standing arrangement in which non-Muslim minority communities (Greek
 *   Orthodox, Armenian, Jewish, Syriac) are systematically denied the
 *   institutional infrastructure required for communal survival, while the
 *   Turkish state consolidates sovereign control over religious foundations,
 *   seminaries, and ecclesiastical governance. This is ONE reading of a
 *   contested kernel; the expansive and guarantor readings are separate
 *   constraints.
 *
 * KEY AGENTS:
 *   - Turkish state apparatus: agenda-setter and beneficiary (institutional power, arbitrage exit) â consolidates control over minority institutional capacity.
 *   - Non-Muslim religious institutions: primary payer (organized, constrained exit) â denied legal personality and self-governance.
 *   - Minority religious leaders: payer (moderate, identity-locked exit) â foreclosed from training successors.
 *   - Minority property foundations: payer (moderate, constrained exit) â assets seized or state-administered.
 *   - International human rights monitoring: observer (institutional, analytical exit) â rules but cannot enforce.
 *   - Domestic expansive interpreters: excluded (moderate, constrained exit) â silenced in domestic legal process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.85).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.78).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Restrictive Reading of Lausanne Minority Protections (Institutional Denial)").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, '71c36dcb-2761-4885-a2f0-b3c2f14efbd4').
narrative_ontology:cs_kernel_codification('71c36dcb-2761-4885-a2f0-b3c2f14efbd4', fixed_text).
narrative_ontology:cs_authority_grounding('71c36dcb-2761-4885-a2f0-b3c2f14efbd4', extraction).
narrative_ontology:cs_interpretation_layer_present('71c36dcb-2761-4885-a2f0-b3c2f14efbd4').
narrative_ontology:cs_reading_relation('71c36dcb-2761-4885-a2f0-b3c2f14efbd4', lausanne_minority_protections__expansive_reading, coexists_with).
narrative_ontology:cs_reading_relation('71c36dcb-2761-4885-a2f0-b3c2f14efbd4', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('71c36dcb-2761-4885-a2f0-b3c2f14efbd4', foundational, individual_worship_as_ceiling).
narrative_ontology:cs_axiom_status(individual_worship_as_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('71c36dcb-2761-4885-a2f0-b3c2f14efbd4', individual_worship_as_ceiling, conventional).
narrative_ontology:cs_axiom('71c36dcb-2761-4885-a2f0-b3c2f14efbd4', foundational, domestic_jurisdiction_over_religious_institutions).
narrative_ontology:cs_axiom_status(domestic_jurisdiction_over_religious_institutions, holdable).
narrative_ontology:cs_axiom_grounding('71c36dcb-2761-4885-a2f0-b3c2f14efbd4', domestic_jurisdiction_over_religious_institutions, conventional).
narrative_ontology:cs_reference_frame('71c36dcb-2761-4885-a2f0-b3c2f14efbd4', state_sovereignty_framework).
narrative_ontology:cs_drift_state('71c36dcb-2761-4885-a2f0-b3c2f14efbd4', contemporary_echr_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('71c36dcb-2761-4885-a2f0-b3c2f14efbd4', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, non_muslim_religious_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_religious_leaders).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_property_foundations).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, uniform_citizenship_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Lausanne Treaty provisions narrowly through domestic legislation and judiciary to reserve control over minority religious institutions, administering foundation properties via state-appointed boards and denying independent legal personality to churches and synagogues under general Turkish association and foundation law.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, beneficiary).

% Churches, synagogues, and other communal bodies denied autonomous legal personality; leadership elections require state approval; unable to govern internal affairs under their own canonical rules; subject to bureaucratic oversight that treats them as ordinary associations rather than religious communities.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, non_muslim_religious_institutions, payer,
    organized, generational, constrained, national).

% Theological educators and clergy barred from operating seminaries or schools to train successors; their vocational identity is fused with institutional continuity, which the state blocks by closing the sole permissible training pathways (e.g., Halki Seminary).
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_religious_leaders, payer,
    moderate, biographical, identity_locked, national).

% Religious foundation properties and historic community assets administered by state-appointed boards or seized under general property law; unable to freely hold, transfer, or restore properties; revenue from assets often diverted to state treasury or non-community purposes.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_property_foundations, payer,
    moderate, generational, constrained, national).

% European Court of Human Rights and related bodies issue rulings finding violations of religious-freedom protections; lacks effective enforcement leverage when Turkey asserts domestic-jurisdiction reservations and does not implement adverse judgments.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, international_human_rights_monitoring, observer,
    institutional, generational, analytical, continental).

% Turkish legal scholars, minority advocates, and bar associations arguing for institutional Lausanne protections are systematically sidelined from judicial appointments, constitutional court deliberations, and policy formation; their briefs receive no institutional uptake.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, domestic_expansive_interpreters, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__restrictive_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate peaceful coexistence by applying uniform domestic law to all religious bodies, eliminating foreign-protected privilege and subordinating communal organization to standard association and foundation statutes.
% TRANSFER_FUNCTION: Moves institutional sovereignty, property control, and educational authority from non-Muslim minority religious communities to the Turkish state apparatus, under the legal form of equal domestic regulation.
% ABSENT_VOICES: Minority religious institutions seeking autonomous legal personality, guarantor states invoking Lausanne supervisory clauses, and domestic jurists advocating expansive institutional readings are structurally excluded from the operative interpretive framework.
% DISAPPEARANCE_RATIONALE: If the restrictive reading vanished and Lausanne were interpreted to require institutional autonomy, minority communities would regain independent legal personality, control over foundation properties, and capacity to train clergy; the state's monopoly over religious institutional life would fragment and minority communal structures would reorganize.
% FOUNDING_PROBLEM: The post-Ottoman settlement needed to secure survival and dignity of non-Muslim minorities within a new nationalist Turkish state after the collapse of the millet system.
% FOUNDING_PROBLEM_CORROBORATION: International legal historians and the Greek and Armenian communities attest the founding problem required institutional continuity, not merely individual worship. The Turkish state asserts the problem was limited to preventing foreign interference in domestic affairs. No neutral external corroboration supports the state's narrow reading as the sole legitimate interpretation; the ECHR and UN treaty bodies have repeatedly found the institutional dimension violated.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85 at interval end) because the constraint systematically strips minority communities of institutional capacityâproperty, education, legal personhoodâtransferring that sovereignty to the state. Suppression is high (0.78) because the arrangement depends on active judicial and administrative enforcement: courts reject foundation claims, bureaucracy appoints boards, police and prosecutors enforce association-law penalties against unauthorized seminary activity. Theater ratio is moderate-high (0.48) because the state maintains extensive legalistic discourse about 'uniform citizenship' and 'equality before the law' to present the extraction as neutral governance rather than targeted minority control. Accessibility collapse is substantial (0.75): once inside the Turkish domestic legal framework, minority institutions have no viable alternative pathway to secure autonomous existence; foreign guarantor intervention has proven ineffective. Resistance is moderate (0.60): minorities litigate and international bodies rule against Turkey, but the state persists.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state apparatus) experiences the constraint as legitimate sovereignty and legal uniformity; the engine should compute a low directionality and damped extraction for this seat. The payer seats (minority institutions, leaders, property foundations) experience the same legal framework as targeted erasure of communal existence; the engine should compute high directionality and amplified extraction. The observer seat (international monitoring) sits at analytical distance with near-zero extraction. The divergence is structural, not perspectival error.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state apparatus is the structural beneficiary: it sets the interpretive agenda, collects institutional control, and enjoys arbitrage-grade exit (can alter interpretation unilaterally). Directionality is near the beneficiary pole. Minority institutions and leaders are structural targets: they bear the costs of property loss, educational foreclosure, and legal personality denial, with constrained or identity-locked exit. Directionality is near the full-target pole. The international observer has analytical exit and no extraction. No override is needed because beneficiary/victim declarations plus exit options capture the structural relationship accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as coordination (rope) because the coordination storyâuniform law for all citizensâis cover for asymmetric extraction that falls exclusively on non-Muslim minorities. It prevents mislabeling as a mountain because the constraint is manifestly constructed through treaty interpretation, legislation, and administrative practice, not an irreducible feature of political order. It prevents piton classification because the state apparatus actively benefits and maintains the constraint with intent; there is no inertia-only decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lausanne_textual_scope,
    'Does the Lausanne treaty text, interpreted by its ordinary meaning in 1923 and its travaux prÃ©paratoires, structurally extend beyond individual worship to encompass institutional autonomy, property rights, and theological education?',
    'Philological and historical-legal analysis of the negotiating record, comparative minority-regime architecture of the interwar period, and subsequent state practice by other Lausanne parties.',
    'If institutional rights are textually grounded, the restrictive reading is extraction disguised as textual fidelity (snare confirmed). If the text is genuinely limited to worship, the extraction is lower and the classification shifts toward a contested coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lausanne_textual_scope, conceptual, 'Uncertainty about whether Lausanne''s textual kernel includes institutional rights.').

omega_variable(
    guarantor_state_obligation_status,
    'Are the Lausanne guarantor powers (United Kingdom, France, Italy, Japan, etc.) still legally obligated to enforce minority protections through diplomatic or judicial means, or has that mechanism lapsed into historical formality?',
    'Analysis of subsequent treaty practice, UN and Council of Europe proceedings, and any extant diplomatic instruments invoking the guarantor clause.',
    'If guarantor obligations are live, minority institutions have an external exit option that lowers accessibility collapse and suppression; if lapsed, the state''s domestic-jurisdiction claim is structurally unchecked.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(guarantor_state_obligation_status, empirical, 'Uncertainty about the legal vitality of Lausanne guarantor-state enforcement.').

omega_variable(
    property_seizure_intent,
    'Are minority foundation property seizures and state administration of vakÄ±f assets unintended side-effects of secularizing general property law, or targeted extraction from non-Muslim communal wealth?',
    'Comparative analysis of enforcement patterns: whether Muslim religious foundations (evkaf) experience identical administrative capture and revenue diversion, or whether asymmetry tracks minority status.',
    'If enforcement is symmetrical, part of the extraction metric reflects general state secularization rather than minority-targeted snare; if asymmetrical, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_seizure_intent, empirical, 'Uncertainty about whether property extraction targets minorities or applies uniformly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t0, lausanne_minority_protections__restrictive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(laus_tr_t20, lausanne_minority_protections__restrictive_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(laus_tr_t40, lausanne_minority_protections__restrictive_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(laus_tr_t60, lausanne_minority_protections__restrictive_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(laus_tr_t80, lausanne_minority_protections__restrictive_reading, theater_ratio, 80, 0.45).
narrative_ontology:measurement(laus_tr_t100, lausanne_minority_protections__restrictive_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(laus_be_t0, lausanne_minority_protections__restrictive_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(laus_be_t20, lausanne_minority_protections__restrictive_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(laus_be_t40, lausanne_minority_protections__restrictive_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(laus_be_t60, lausanne_minority_protections__restrictive_reading, base_extractiveness, 60, 0.75).
narrative_ontology:measurement(laus_be_t80, lausanne_minority_protections__restrictive_reading, base_extractiveness, 80, 0.8).
narrative_ontology:measurement(laus_be_t100, lausanne_minority_protections__restrictive_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t0, lausanne_minority_protections__restrictive_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(laus_su_t20, lausanne_minority_protections__restrictive_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(laus_su_t40, lausanne_minority_protections__restrictive_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(laus_su_t60, lausanne_minority_protections__restrictive_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(laus_su_t80, lausanne_minority_protections__restrictive_reading, suppression_requirement, 80, 0.76).
narrative_ontology:measurement(laus_su_t100, lausanne_minority_protections__restrictive_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
