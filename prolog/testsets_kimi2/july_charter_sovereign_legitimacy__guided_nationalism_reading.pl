% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: Islamic-Nationalist Sovereign Legitimacy Framework (Guided Nationalism Reading)
 *   domain: constitutional_law/political_transitions/post_revolutionary
 *
 * SUMMARY:
 *   This constraint story instantiates the guided_nationalism_reading of the
 *   contested kernel july_charter_sovereign_legitimacy. The kernel is a
 *   post-revolutionary constitutional charter whose sovereignty clause is
 *   read three ways: as establishing religious identity as the legitimacy
 *   ground (this reading), as mandating secular democratic civilian authority
 *   (secular_democratic_reading), or as ratifying military custodianship
 *   (military_custodian_reading). In this reading, the charter embeds an
 *   Islamic-nationalist framework that coordinates the post-revolutionary
 *   state around religious identity while asymmetrically extracting political
 *   standing from secular civil society and religious minorities. The
 *   constraint is authored as a tangled rope: it solves a genuine
 *   post-revolutionary coordination problem (legitimacy vacuum) but does so
 *   through enforced identity hierarchy.
 *
 * KEY AGENTS:
 *   - religious_nationalist_coalition: Primary agenda_setter (institutional/identity_locked) â sets constitutional rules and derives concentrated legitimacy
 *   - state_religious_institutions: Primary beneficiary (institutional/constrained) â gains constitutional interpretive authority
 *   - secular_civil_society: Primary target (organized/constrained) â bears legal and political exclusion
 *   - religious_minorities: Secondary target (powerless/trapped) â bears discriminatory subordination
 *   - military_establishment: Secondary actor (institutional/constrained) â accepts subordination in exchange for autonomy
 *   - comparative_constitutional_scholars: Analytical observer (analytical/analytical) â tracks divergence from international norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.78).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.82).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "Islamic-Nationalist Sovereign Legitimacy Framework (Guided Nationalism Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/political_transitions/post_revolutionary").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'f8eea6ab-74ac-4d54-8490-aa3058fd34e0').
narrative_ontology:cs_kernel_codification('f8eea6ab-74ac-4d54-8490-aa3058fd34e0', fixed_text).
narrative_ontology:cs_authority_grounding('f8eea6ab-74ac-4d54-8490-aa3058fd34e0', lineage).
narrative_ontology:cs_interpretation_layer_present('f8eea6ab-74ac-4d54-8490-aa3058fd34e0').
narrative_ontology:cs_reading_relation('f8eea6ab-74ac-4d54-8490-aa3058fd34e0', july_charter_sovereign_legitimacy__secular_democratic_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8eea6ab-74ac-4d54-8490-aa3058fd34e0', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('f8eea6ab-74ac-4d54-8490-aa3058fd34e0', foundational, islamic_identity_sovereign_source).
narrative_ontology:cs_axiom_status(islamic_identity_sovereign_source, holdable).
narrative_ontology:cs_axiom_grounding('f8eea6ab-74ac-4d54-8490-aa3058fd34e0', islamic_identity_sovereign_source, theological).
narrative_ontology:cs_reference_frame('f8eea6ab-74ac-4d54-8490-aa3058fd34e0', islamic_nationalist_constitutional_order).
narrative_ontology:cs_drift_state('f8eea6ab-74ac-4d54-8490-aa3058fd34e0', post_enactment_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f8eea6ab-74ac-4d54-8490-aa3058fd34e0', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_coalition).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, state_religious_institutions).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_establishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the constitutional drafting assembly and amendment processes. Embedded religious identity as the sovereign legitimacy ground, enabling filtering of political competition through religious loyalty tests. Derives authority from claiming to represent the authentic religious nation. Cannot abandon the framework without dissolving its own legitimacy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_coalition, agenda_setter,
    institutional, generational, identity_locked, national).

% Granted constitutional authority to review legislation for conformity with religious principles and to advise on policy compatibility. Institutional budgets and staffing expand under the charter. Fused to the state structure and cannot easily revert to pre-charter social roles.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, state_religious_institutions, beneficiary,
    institutional, generational, constrained, national).

% Political parties, NGOs, and media advocating secular or civic-nationalist governance are subject to legal restrictions, constitutional exclusion clauses, and surveillance. Political participation is contingent on accepting the religious sovereignty framework. Resists through litigation, protest, and international advocacy, facing escalating legal penalties.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    organized, biographical, constrained, national).

% Excluded from full constitutional membership by the religious identity requirement; subject to discriminatory personal-status laws and limited political representation. Emigration is often the only viable exit, though economic barriers trap many. Objections are structurally dismissed as incompatible with the sovereignty clause.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, biographical, trapped, national).

% Retains operational autonomy and budgetary allocations under the charter's stability provisions. Accepts nominal subordination to the religious-nationalist civilian authority in exchange for institutional privileges. Challenging the sovereignty framework would trigger a legitimacy crisis it may not survive.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_establishment, beneficiary,
    institutional, generational, constrained, national).

% Advocates for civic nationalism and secular constitutional frameworks who were structurally excluded from the drafting assembly and subsequent constitutional review processes. Their proposals were ruled out of order as incompatible with the religious sovereignty principle.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, liberal_constitutionalists, excluded,
    organized, biographical, constrained, national).

% Analyze the charter through comparative law and international human rights frameworks. Document the divergence between the religious sovereignty provisions and universal citizenship norms. Have no institutional power over the charter but provide the analytical vocabulary for external pressure.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified national identity and legitimate governmental authority in a post-revolutionary context by grounding sovereignty in the religious identity of the majority, providing a shared symbolic framework for state institutions and reducing fragmentation among the revolutionary coalition.
% TRANSFER_FUNCTION: Transfers political legitimacy and constitutional standing from secular and minority populations to religious-nationalist institutions; moves authority to interpret the constitutional order from secular jurists to religious scholars embedded in the state.
% ABSENT_VOICES: Liberal constitutionalists and civic-nationalist jurists who would argue for popular sovereignty detached from religious identity were structurally excluded from the drafting assembly and amendment processes; their exclusion is enforced by the religious legitimacy filter which treats secular constitutional proposals as categorically incompatible with the sovereignty clause.
% DISAPPEARANCE_RATIONALE: If the Islamic-nationalist sovereignty clause vanished, the constitutional order would lose its primary legitimacy anchor; secular civil society would re-enter constitutional politics, religious minorities would claim equal citizenship standing, and the current coalition would face a legitimacy crisis requiring either democratic re-foundation or military intervention.
% FOUNDING_PROBLEM: Post-revolutionary legitimacy vacuum: the collapse of the prior regime left no agreed sovereignty formula; the plurality of the population holds religious identity as a primary social bond, and the revolutionary coalition needed a unifying principle to prevent state collapse or civil war.
% FOUNDING_PROBLEM_CORROBORATION: The religious-nationalist coalition attests the problem is live and justifies the charter. Secular civil society and international human rights monitors attest the legitimacy vacuum could have been filled by civic nationalism or transitional democracy; they corroborate that the religious-framing was a coalition choice, not an inevitable response. Comparative constitutional scholars outside the beneficiary set note that similar post-revolutionary states adopted secular frameworks, indicating the Islamic-nationalist solution was path-dependent rather than structurally necessary.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.78) is high because the constraint systematically transfers constitutional standing from secular and minority seats to religious-nationalist institutions. Suppression (0.82) is higher because the arrangement's persistence depends on actively excluding secular democratic alternatives and policing religious boundaries. Theater_ratio (0.50) reflects substantial performative maintenance: public religious symbolism, constitutional rhetoric, and ritual legitimation that exceeds functional governance requirements. Accessibility_collapse (0.72) captures the near-total delegitimization of secular governance alternatives within the constitutional order. Resistance (0.68) reflects ongoing but contained opposition from secular civil society and international pressure. The measurement series share one time grid (0â10) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The religious-nationalist coalition and state religious institutions experience this constraint as coordination: it ended the post-revolutionary legitimacy vacuum and established a governable order anchored in popular identity. The secular civil society and religious minority seats experience the same structure as extraction: their citizenship is conditional, their political voice filtered, and their exit options truncated. The military experiences a hybrid: benefits from stability but constrained by a legitimacy framework it did not author. The engine computes this divergence from the structural data (beneficiary/victim roles, exit options, power levels) rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (religious_nationalist_coalition, state_religious_institutions) sit near d=0.0: the constraint subsidizes their authority and institutional expansion. Victims (secular_civil_society, religious_minorities) sit near d=1.0: the constraint extracts their political standing and legal equality. The military_establishment sits at moderate-low d (~0.25): it receives institutional benefits but is nominally subordinated to the religious authority. Comparative_constitutional_scholars sit at analytical exit with no directionality computation. No overrides are needed because the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 genealogy prevents mislabeling this constraint as a rope or scaffold. The founding problem â post-revolutionary legitimacy vacuum â was real, but the solution (religious identity as sovereign ground) was one path-dependent choice among several, not the only possible coordination mechanism. The absence of a sunset clause and the active enforcement against secular alternatives indicate the arrangement is not transitional. The metric profile (high extraction, high suppression, rising theater) distinguishes it from a scaffold, while the presence of identifiable victims and the asymmetric cost/benefit distribution distinguish it from a rope. It is not a snare because the coordination function (post-revolutionary state integration) is structurally genuine and not merely cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of secular alternatives primarily structural (legal barriers, electoral engineering) or internalized (self-censorship, majority identity fusion)?',
    'Survey of secular activists on exit intentions; post-exit speech patterns of emigrated dissidents.',
    'If internalized, effective suppression exceeds the structural measure and resistance may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism').

omega_variable(
    military_subordination_sincerity,
    'Does the military establishment genuinely accept subordination to religious-nationalist legitimacy, or is it tactically acquiescing pending activation of the custodian reading?',
    'Analysis of military statements during legitimacy crises; comparison of budgetary and operational autonomy trends.',
    'If tactical, the constraint''s enforcement depends on a brittle alliance and could reclassify toward military_custodian_reading under stress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_subordination_sincerity, empirical, 'Whether military subordination is sincere or tactical').

omega_variable(
    founding_problem_necessity,
    'Was the post-revolutionary legitimacy vacuum structurally unresolvable without religious identity hierarchy, or did the coalition impose this solution to exclude competitors?',
    'Comparative case analysis of post-revolutionary states that adopted secular vs religious sovereignty frameworks.',
    'If secular alternatives were viable, the coordination story is weaker and extraction is the dominant function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_necessity, conceptual, 'Whether religious identity hierarchy was structurally necessary or strategically imposed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(july_tr_t2, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 2, 0.34).
narrative_ontology:measurement(july_tr_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement(july_tr_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 8, 0.46).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 10, 0.5).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(july_be_t2, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 2, 0.64).
narrative_ontology:measurement(july_be_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(july_be_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 8, 0.75).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 10, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(july_su_t2, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 2, 0.74).
narrative_ontology:measurement(july_su_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 4, 0.77).
narrative_ontology:measurement(july_su_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 8, 0.83).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 10, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
