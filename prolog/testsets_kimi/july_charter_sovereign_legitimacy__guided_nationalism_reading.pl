% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: July Charter Guided Nationalism Reading: Religious Identity as Sovereign Legitimacy
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   Post-revolutionary charter drafted by religious-nationalist forces
 *   establishes that sovereign legitimacy derives from the nation's Islamic
 *   identity, not from secular popular will or military guardianship alone.
 *   This reading coordinates the majority religious population and state
 *   institutions under a shared sacred-national framework while structurally
 *   extracting political standing from secular civil society and religious
 *   minorities. The constraint is claimed by its authors as necessary
 *   post-revolutionary order; its critics read it as majoritarian capture
 *   institutionalized as constitutional theology. The metrics and claim are
 *   authored independently: the structural data describe high extraction and
 *   suppression, while the claimed type recognizes the genuine coordination
 *   function in post-revolutionary state-building.
 *
 * KEY AGENTS:
 *   - religious_nationalist_drafters: agenda_setter (institutional/mobile) â architects of the charter who embed religious identity as sovereignty ground
 *   - clerical_establishment: beneficiary (institutional/mobile) â gains constitutional status and resources
 *   - military_establishment: beneficiary (institutional/constrained) â shielded from secular oversight under nationalist-religious guard mandate
 *   - secular_civil_society: payer (moderate/constrained) â bears legal restrictions on secular political platforming
 *   - religious_minorities: payer (powerless/trapped) â subject to constitutional second-class status
 *   - international_human_rights_monitor: observer (institutional/analytical) â external documentation seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.72).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.78).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "July Charter Guided Nationalism Reading: Religious Identity as Sovereign Legitimacy").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'db43f532-356d-446e-b695-4a82517b0ebd').
narrative_ontology:cs_kernel_codification('db43f532-356d-446e-b695-4a82517b0ebd', fixed_text).
narrative_ontology:cs_authority_grounding('db43f532-356d-446e-b695-4a82517b0ebd', lineage).
narrative_ontology:cs_interpretation_layer_present('db43f532-356d-446e-b695-4a82517b0ebd').
narrative_ontology:cs_reading_relation('db43f532-356d-446e-b695-4a82517b0ebd', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('db43f532-356d-446e-b695-4a82517b0ebd', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('db43f532-356d-446e-b695-4a82517b0ebd', foundational, state_sovereignty_grounded_in_religious_identity).
narrative_ontology:cs_axiom_status(state_sovereignty_grounded_in_religious_identity, holdable).
narrative_ontology:cs_axiom_grounding('db43f532-356d-446e-b695-4a82517b0ebd', state_sovereignty_grounded_in_religious_identity, theological).
narrative_ontology:cs_axiom('db43f532-356d-446e-b695-4a82517b0ebd', foundational, minority_citizenship_conditional_on_majority_religious_norms).
narrative_ontology:cs_axiom_status(minority_citizenship_conditional_on_majority_religious_norms, holdable).
narrative_ontology:cs_axiom_grounding('db43f532-356d-446e-b695-4a82517b0ebd', minority_citizenship_conditional_on_majority_religious_norms, deontological).
narrative_ontology:cs_reference_frame('db43f532-356d-446e-b695-4a82517b0ebd', islamic_nationalist_popular_sovereignty).
narrative_ontology:cs_drift_state('db43f532-356d-446e-b695-4a82517b0ebd', contemporary_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('db43f532-356d-446e-b695-4a82517b0ebd', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_drafters).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, clerical_establishment).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_establishment).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and ratified the charter, embedding religious-nationalist identity as the supreme source of sovereignty. Control constitutional amendment gates, judicial appointments, and the state apparatus that interprets religious identity clauses. Their political survival is fused with the charter's persistence.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_drafters, agenda_setter,
    institutional, generational, mobile, national).

% Gains constitutional status for religious norms, a formal consultative or review role over legislation, and state resources for religious education and institutions. Their authority is amplified by the charter's elevation of religious identity over neutral procedural legitimacy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, clerical_establishment, beneficiary,
    institutional, generational, mobile, national).

% Retains institutional autonomy, budgetary independence, and a constitutional mandate to protect national-religious sovereignty. While not the formal sovereign under this reading, it is shielded from secular civilian oversight and integrated into the nationalist framework as a guardian of the faith and the state.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_establishment, beneficiary,
    institutional, generational, constrained, national).

% Subject to legal restrictions on association, expression, and electoral platforming that challenge religious sovereignty. Constitutional bars prevent secular parties from contesting state legitimacy on neutral grounds; activists face charges of apostasy or foreign agitation. Domestic exit is possible only by silencing dissent.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    moderate, biographical, constrained, national).

% Constitutionally relegated to protected but unequal status: excluded from high office, restricted in public worship and education, and subject to family-law regimes imposed by the majority religious identity. Emigration severs community and economic ties, making exit structurally costly.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, biographical, trapped, national).

% Documents differential citizenship and reports constitutional discrimination to treaty bodies. Lacks enforcement power over the charter but provides external frames that domestic opposition and global institutions reference.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_human_rights_monitor, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconstitutes sovereign legitimacy and state institutions after revolution by unifying the populace under a shared religious-nationalist identity, replacing the overthrown regime's legitimacy formula with an indigenous sacred frame.
% TRANSFER_FUNCTION: Moves constitutional standing, political rights, and public recognition from secular citizens and religious minorities to religious-nationalist institutions and the clerical establishment.
% ABSENT_VOICES: Secular democrats, liberal constitutionalists, and pluralist religious scholars who advocated for a neutral or inclusive constituent process; they were excluded from the drafting assembly or marginalized as foreign-aligned.
% DISAPPEARANCE_RATIONALE: Without the charter, the religious-nationalist sovereignty formula collapses; secular forces would reorganize around pluralist constitutionalism, minorities would claim equal citizenship, the military would face a legitimacy crisis, and the clerical establishment would lose constitutionalized authority.
% FOUNDING_PROBLEM: Post-revolutionary legitimacy vacuum: the overthrown regime's authority was discredited, colonial frameworks were rejected, and no pre-existing constitutional order commanded popular consent.
% FOUNDING_PROBLEM_CORROBORATION: External constitutional historians and international monitors attest the vacuum was genuine but argue the charter closed it via majoritarian exclusion rather than inclusive pluralism; domestic secular opposition corroborates the exclusionary closure from a non-beneficiary seat.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the constitutional transfer of rights and recognition from secular and minority seats to religious-nationalist institutions. Suppression (0.78) is high because the framework actively bars alternative secular legitimacy claims and minority equality demands through constitutional text and courts. Theater ratio (0.45) captures the increasing performative quality of religious-nationalist symbolism as governance drifts from the reference frame. Accessibility collapse (0.75) is high because once the charter is operative, legal avenues for secular equality collapse into constitutional impossibility. Resistance (0.68) reflects sustained but legally disarmed opposition from secularists and minorities.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (drafters, clerics, military) experience the constraint as restorative coordination â reconstituting the state on authentic foundations. The payer seats (secular civil society, minorities) experience it as structural extraction that removes their citizenship standing. The engine computes this divergence from the beneficiary/victim declarations and the differential exit options (mobile/constrained vs constrained/trapped).
 *
 * DIRECTIONALITY LOGIC:
 *   Religious-nationalist drafters and clerical establishment are structural beneficiaries with mobile exit within the national frame; their directionality sits near the beneficiary pole. Religious minorities are trapped by identity and structural bars; their directionality sits near the full-target pole. Secular civil society is constrained but not identity-locked in the same way, sitting between moderate and high target. The military is a beneficiary but with constrained exit because it is bound to defend the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling: if this were pure coordination (rope), the minority and secular victimization would be invisible; if pure extraction (snare), the genuine post-revolutionary coordination problem (legitimacy vacuum, institutional collapse) would be denied. The mandate (founding problem: post-revolutionary order) is contested but not dead, so mandatrophy is not declared, though drift is substantial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_sovereignty_vs_minority_rights,
    'Does grounding sovereign legitimacy in religious identity inherently require the constitutional subordination of religious minorities and secular citizens, or can it be interpreted as a symbolic national identity without legal hierarchy?',
    'Comparative constitutional analysis of states with religious identity clauses: measuring legal incidence on minority rights across regimes with identical constitutional formulas.',
    'If subordination is structurally entailed, extraction is inseparable from the coordination function; if symbolic, the extraction is a contingent overreading that could be peeled off.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_sovereignty_vs_minority_rights, conceptual, 'Whether religious sovereignty necessarily implies minority subordination.').

omega_variable(
    military_role_under_guided_nationalism,
    'Does the guided nationalism reading structurally subordinate the military to religious-nationalist civilian institutions, or does it preserve military autonomy behind a veneer of ideological alignment?',
    'Institutional analysis of charter provisions on military appointment, budget, and jurisdictional autonomy; comparison with the military custodian reading''s textually grounded claims.',
    'If military autonomy is preserved, the guided nationalism reading''s extraction is compounded by a latent coercive layer not visible in its own framing; if subordinated, the reading achieves genuine civilian supremacy at the cost of religious exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_role_under_guided_nationalism, empirical, 'Military institutional autonomy under the religious-nationalist frame.').

omega_variable(
    founding_problem_corroboration_adequacy,
    'Was the post-revolutionary legitimacy vacuum genuine enough to justify a closed constitutional settlement, or did the charter preempt a pluralist constituent process that could have achieved inclusive coordination?',
    'Historical analysis of the revolutionary moment''s threat level and the inclusivity of the constituent assembly relative to the population''s ideological distribution.',
    'If the vacuum was existential and inclusive process impossible, the constraint carries more coordination weight; if the assembly was deliberately majoritarian, extraction was baked into the founding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_corroboration_adequacy, preference, 'Contingency of the founding closure versus deliberate majoritarian capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_charter_gn_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(july_charter_gn_tr_t2, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(july_charter_gn_tr_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(july_charter_gn_tr_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 6, 0.4).
narrative_ontology:measurement(july_charter_gn_tr_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 8, 0.43).
narrative_ontology:measurement(july_charter_gn_tr_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(july_charter_gn_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(july_charter_gn_be_t2, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 2, 0.6).
narrative_ontology:measurement(july_charter_gn_be_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 4, 0.64).
narrative_ontology:measurement(july_charter_gn_be_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(july_charter_gn_be_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 8, 0.7).
narrative_ontology:measurement(july_charter_gn_be_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 10, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(july_charter_gn_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(july_charter_gn_su_t2, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 2, 0.65).
narrative_ontology:measurement(july_charter_gn_su_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 4, 0.7).
narrative_ontology:measurement(july_charter_gn_su_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 6, 0.74).
narrative_ontology:measurement(july_charter_gn_su_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 8, 0.77).
narrative_ontology:measurement(july_charter_gn_su_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 10, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
