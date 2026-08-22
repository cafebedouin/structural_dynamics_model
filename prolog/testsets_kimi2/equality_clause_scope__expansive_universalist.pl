% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Expansive Universalist Equality Clause Scope
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the expansive universalist reading of the
 *   equality clause kernel: the claim that equality is a self-evident,
 *   universal moral truth applying to all humans regardless of historical
 *   exclusions. The reading treats the constitutional text as codifying a
 *   pre-legal normative reality rather than creating a contingent political
 *   bargain. It frames historical exclusions as hypocrisy to be corrected,
 *   not as binding precedent that constrains scope. This produces a universal
 *   beneficiary set and authorizes low-threshold judicial expansion of rights
 *   through interpretation rather than amendment. The constraint is CLAIMED
 *   as mountain (natural, self-evident, independent of enforcement) while the
 *   authored metrics reflect its actual operation as a contested legal
 *   construct with rising extraction, suppression, and theater over time â
 *   a deliberate claim/metric divergence to enable false-summit detection.
 *
 * KEY AGENTS:
 *   - historically_excluded_groups: Primary beneficiary (organized/identity_locked) â gains standing and recognition through judicial adoption of the universalist frame.
 *   - status_quo_defenders: Primary target (powerful/constrained) â bears the cost of invalidated privileges and narrowing legal space for hierarchy.
 *   - expansive_judiciary: Agenda_setter (institutional/constrained) â administers the interpretive framework and enforces invalidation of exclusionary practices.
 *   - originalist_jurists: Excluded voice (institutional/constrained) â structurally marginalized in the interpretive majority despite formal presence in dissent.
 *   - legal_philosophers: Analytical observer (analytical/analytical) â tracks the divergence between natural-law self-presentation and constructed legal effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.28).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.45).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.28).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, mountain).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Expansive Universalist Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy").

domain_priors:emerges_naturally(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, 'a1d82311-c987-4cbb-aaec-d1f19a690059').
narrative_ontology:cs_kernel_codification('a1d82311-c987-4cbb-aaec-d1f19a690059', fixed_text).
narrative_ontology:cs_authority_grounding('a1d82311-c987-4cbb-aaec-d1f19a690059', lineage).
narrative_ontology:cs_interpretation_layer_present('a1d82311-c987-4cbb-aaec-d1f19a690059').
narrative_ontology:cs_reading_relation('a1d82311-c987-4cbb-aaec-d1f19a690059', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('a1d82311-c987-4cbb-aaec-d1f19a690059', equality_clause_scope__progressive_textualist, influences).
narrative_ontology:cs_axiom('a1d82311-c987-4cbb-aaec-d1f19a690059', foundational, equality_as_self_evident_universal_truth).
narrative_ontology:cs_axiom_status(equality_as_self_evident_universal_truth, holdable).
narrative_ontology:cs_axiom_grounding('a1d82311-c987-4cbb-aaec-d1f19a690059', equality_as_self_evident_universal_truth, deontological).
narrative_ontology:cs_axiom('a1d82311-c987-4cbb-aaec-d1f19a690059', foundational, historical_exclusion_as_hypocrisy).
narrative_ontology:cs_axiom_status(historical_exclusion_as_hypocrisy, holdable).
narrative_ontology:cs_axiom_grounding('a1d82311-c987-4cbb-aaec-d1f19a690059', historical_exclusion_as_hypocrisy, deontological).
narrative_ontology:cs_reference_frame('a1d82311-c987-4cbb-aaec-d1f19a690059', universal_human_equality).
narrative_ontology:cs_drift_state('a1d82311-c987-4cbb-aaec-d1f19a690059', contemporary_human_rights_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a1d82311-c987-4cbb-aaec-d1f19a690059', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, status_quo_defenders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups defined by race, gender, disability, and other status markers who gain legal standing and social recognition as courts adopt the expansive universalist reading. Their political identity is constituted through the fight for inclusion under this principle; exit would mean abandoning the normative framework that validates their equal dignity.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_excluded_groups, beneficiary,
    organized, generational, identity_locked, universal).

% Individuals and institutions whose social, economic, or political advantages depend on maintaining exclusionary boundaries. They bear the cost of judicial decisions invalidating discriminatory practices and face narrowing legal space for hierarchy. Their opposition is framed as resistance to natural law but is structurally the defense of inherited privilege.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, status_quo_defenders, payer,
    powerful, biographical, constrained, national).

% Federal judges and justices who invoke the universalist equality principle to strike down discriminatory statutes and practices. They administer the interpretive framework, treating historical exclusions as deviations to be corrected rather than as binding constitutional meaning. Their authority derives from the claim that they are applying self-evident truths rather than making policy.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, expansive_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Judges and scholars who read the equality clause within an eighteenth-century framework of limited application. They are structurally excluded from the interpretive majority when the universalist reading dominates specific doctrinal areas, though they remain vocal in dissent.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, originalist_jurists, excluded,
    institutional, generational, constrained, national).

% Academic observers who analyze whether the universalist reading is grounded in the constitutional text, natural law philosophy, or political morality. They track the divergence between the reading's self-presentation and its constructed legal effects.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, legal_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:fixing_cost_class(equality_clause_scope__expansive_universalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, non-negotiable normative foundation for resolving competing rights claims and social status conflicts without recourse to majoritarian dominance or brute power.
% TRANSFER_FUNCTION: Transfers legal standing, social recognition, and political legitimacy from historically dominant exclusory classes to historically excluded groups by judicially invalidating exclusionary practices as breaches of a universal human condition.
% ABSENT_VOICES: Restrictive originalists are present in dissent but structurally marginalized in the interpretive majority; non-Western legal traditions and radical abolitionist critiques that reject the liberal equality frame entirely are absent from the constitutional conversation.
% DISAPPEARANCE_RATIONALE: If this reading vanished, constitutional jurisprudence would lose its primary engine for invalidating discriminatory legislation; historically excluded groups would lose their strongest doctrinal anchor, and the interpretive framework would revert to narrower textual or originalist boundaries â the architecture of rights and status would rearrange.
% FOUNDING_PROBLEM: How to legitimate a post-hierarchical political order after rejecting hereditary rank and divine right, while preventing tyranny of the majority over disfavored minorities and securing equal dignity for all persons.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights historians, critical legal scholars, and social movement theorists from outside the judiciary attest that structural exclusion persists and requires this normative corrective; originalist jurists dispute that the clause was designed to address this problem, corroborating the contested nature of the genealogy.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, ExtMetricName, E),
    domain_priors:suppression_score(equality_clause_scope__expansive_universalist, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equality_clause_scope__expansive_universalist),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28) is moderate-low because the constraint is primarily rights-protective, but it imposes real costs on status_quo_defenders by dismantling exclusionary institutions. Suppression (0.45) reflects the active judicial suppression of exclusionary laws and alternative hierarchies. Theater_ratio (0.40) captures the growing performative gap between courts' universalist rhetoric and persistent material inequality. Accessibility_collapse (0.88) is high because, within liberal legal discourse, hierarchical alternatives become nearly unthinkable once the universalist frame is accepted. Resistance (0.55) is moderate-to-high due to sustained originalist and status-quo opposition. The metrics are authored independently of the mountain claim to allow the engine to detect false-summit divergence.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat, the constraint appears as a discovered moral floor that limits state and private power â a mountain. From the payer seat, it appears as an activist judicial construct that reorders social hierarchies without democratic mandate â a snare or tangled_rope. The agenda_setter seat experiences it as legitimate interpretive authority grounded in timeless principle. The engine computes these divergences from the same structural data; the authored mountain claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically_excluded_groups occupy the beneficiary end (low d): the constraint subsidizes their legal standing and recognition. Status_quo_defenders occupy the target end (high d): the constraint extracts their privilege and enforces compliance with inclusionary norms. The expansive_judiciary sits near the middle but leans beneficiary: they administer the constraint and gain institutional authority from its expansion, though they do not personally capture the extracted value. Originalist_jurists are excluded from the beneficiary structure and experience high directional drag toward the target end when the universalist reading dominates their docket. The engine will compute divergent per-seat types from this asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The universalist reading prevents mislabeling by anchoring its coordination function in a non-contingent normative foundation. If the founding problem (hierarchy and exclusion) were dead, the constraint would risk piton status. However, the authored founding_problem_status is live, corroborated by extra-judicial observers, and the measurement series shows rising extraction and suppression rather than atrophy. This blocks piton classification. The reading also avoids pure snare classification because it names a genuine coordination function (resolving rights conflicts without brute power) and because the beneficiary set is universal in aspiration, even if asymmetric in application.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_or_constructed_norm,
    'Is the equality principle a discovered feature of moral reality independent of human construction, or is it a normative device that advances the interests of historically excluded groups under the guise of natural law?',
    'Comparative legal history examining whether universalist equality constraints emerge independently across disconnected legal cultures, or cluster around specific political movements and their beneficiaries.',
    'If constructed, the FSM override reclassifies the constraint from mountain to tangled_rope or snare, revealing the natural-law framing as legitimization for asymmetric extraction from status_quo_defenders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_or_constructed_norm, conceptual, 'Natural law versus constructed norm ambiguity').

omega_variable(
    judicial_expansion_mechanism,
    'Does expansive rights expansion via judicial interpretation rest on legitimate textual method, or does it rely on extralegal moral philosophy smuggled through interpretive rhetoric?',
    'Corpus analysis of judicial opinions measuring the ratio of independent moral argument to textual and precedential argument in landmark equality decisions.',
    'If the method is primarily extralegal, the constraint''s authority_grounding shifts from lineage to extraction, altering the drift_state classification and coupling analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_expansion_mechanism, empirical, 'Judicial method versus moral philosophy in equality expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__expansive_universalist, theater_ratio, 0, 0.15).
narrative_ontology:measurement(equa_tr_t10, equality_clause_scope__expansive_universalist, theater_ratio, 10, 0.2).
narrative_ontology:measurement(equa_tr_t20, equality_clause_scope__expansive_universalist, theater_ratio, 20, 0.24).
narrative_ontology:measurement(equa_tr_t30, equality_clause_scope__expansive_universalist, theater_ratio, 30, 0.28).
narrative_ontology:measurement(equa_tr_t40, equality_clause_scope__expansive_universalist, theater_ratio, 40, 0.32).
narrative_ontology:measurement(equa_tr_t50, equality_clause_scope__expansive_universalist, theater_ratio, 50, 0.36).
narrative_ontology:measurement(equa_tr_t60, equality_clause_scope__expansive_universalist, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__expansive_universalist, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(equa_be_t10, equality_clause_scope__expansive_universalist, base_extractiveness, 10, 0.08).
narrative_ontology:measurement(equa_be_t20, equality_clause_scope__expansive_universalist, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(equa_be_t30, equality_clause_scope__expansive_universalist, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(equa_be_t40, equality_clause_scope__expansive_universalist, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(equa_be_t50, equality_clause_scope__expansive_universalist, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(equa_be_t60, equality_clause_scope__expansive_universalist, base_extractiveness, 60, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__expansive_universalist, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(equa_su_t10, equality_clause_scope__expansive_universalist, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(equa_su_t20, equality_clause_scope__expansive_universalist, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(equa_su_t30, equality_clause_scope__expansive_universalist, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(equa_su_t40, equality_clause_scope__expansive_universalist, suppression_requirement, 40, 0.36).
narrative_ontology:measurement(equa_su_t50, equality_clause_scope__expansive_universalist, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(equa_su_t60, equality_clause_scope__expansive_universalist, suppression_requirement, 60, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% This story is one of three structurally distinct constraints decomposed from the natural-language label 'equality clause scope.' Each reading instantiates a different beneficiary structure, exit option, and epsilon. The expansive universalist reading naturalizes the principle as self-evident truth; the restrictive originalist reading treats it as historically bounded; the progressive textualist reading channels change through democratic amendment. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
