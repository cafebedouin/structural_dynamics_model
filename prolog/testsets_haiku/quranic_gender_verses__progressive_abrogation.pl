% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__progressive_abrogation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Qur'anic Gender Verses under Progressive Abrogation Reading
 *   domain: religious/jurisprudential/gender
 *
 * SUMMARY:
 *   The progressive abrogation reading of Qur'anic gender verses interprets
 *   later egalitarian principles (especially verse 49:13 on universal human
 *   dignity) as superseding earlier gender-differentiated rulings (4:11 on
 *   inheritance, 2:282 on testimony, 4:34 on guardianship) through the
 *   classical hermeneutical principle of naskh (abrogation). This reading is
 *   one interpretive framing of a contested kernel: the same gender verses
 *   can be read as timeless male-hierarchical ordinance (literal_hierarchical
 *   reading), as historically-situated steps toward justice requiring
 *   contextual reinterpretation (contextual_egalitarian reading), or as part
 *   of an incomplete egalitarian trajectory (progressive_abrogation
 *   reading—this one). The progressive abrogation reading differs
 *   structurally from its siblings in its mechanism (chronological
 *   supersession via naskh rather than contextual analogy or literal command)
 *   and its implication (earlier rulings are positively overridden, not
 *   merely reinterpreted). This reading extracts authority from
 *   traditionalist scholars whose literalist reading is delegitimized as
 *   incomplete and imposes cognitive/identity costs on communities whose
 *   lived jurisprudence is reframed as epistemically unjustified. Women gain
 *   legal equality under this reading, making it deeply asymmetric:
 *   substantial benefit for women, substantial cost for traditionalist
 *   institutions and identity-bound communities.
 *
 * KEY AGENTS:
 *   - Progressive Muslim scholars: institutional power, mobile exit, set the agenda for this reading in academic and publishing contexts
 *   - Women under egalitarian framework: moderate power, constrained exit, primary beneficiaries of equal inheritance and guardianship elimination
 *   - Traditionalist scholars: institutional power, identity-locked exit, see their interpretive framework delegitimized and authority eroded
 *   - Communities bound to literal reading: powerless, identity-locked exit, face epistemic violence and identity destabilization if this reading prevails
 *   - Secular Muslim-majority states: institutional power, arbitrage exit, can use this reading to justify civil gender equality within Islamic legitimacy
 *   - Diaspora Muslim women advocates: organized power, mobile exit, mobilize this reading for family law reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.88).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.76).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.88).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Qur'anic Gender Verses under Progressive Abrogation Reading").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "religious/jurisprudential/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, 'aab2fd75-d1c6-4d28-bf72-67670c00bb94').
narrative_ontology:cs_kernel_codification('aab2fd75-d1c6-4d28-bf72-67670c00bb94', fixed_text).
narrative_ontology:cs_authority_grounding('aab2fd75-d1c6-4d28-bf72-67670c00bb94', lineage).
narrative_ontology:cs_interpretation_layer_present('aab2fd75-d1c6-4d28-bf72-67670c00bb94').
narrative_ontology:cs_reading_relation('aab2fd75-d1c6-4d28-bf72-67670c00bb94', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('aab2fd75-d1c6-4d28-bf72-67670c00bb94', quranic_gender_verses__contextual_egalitarian, influences).
narrative_ontology:cs_axiom('aab2fd75-d1c6-4d28-bf72-67670c00bb94', foundational, quranic_trajectory_toward_equality).
narrative_ontology:cs_axiom_status(quranic_trajectory_toward_equality, holdable).
narrative_ontology:cs_axiom_grounding('aab2fd75-d1c6-4d28-bf72-67670c00bb94', quranic_trajectory_toward_equality, empirically_contingent).
narrative_ontology:cs_axiom('aab2fd75-d1c6-4d28-bf72-67670c00bb94', foundational, naskh_validity_as_hermeneutic).
narrative_ontology:cs_axiom_status(naskh_validity_as_hermeneutic, holdable).
narrative_ontology:cs_axiom_grounding('aab2fd75-d1c6-4d28-bf72-67670c00bb94', naskh_validity_as_hermeneutic, conventional).
narrative_ontology:cs_reference_frame('aab2fd75-d1c6-4d28-bf72-67670c00bb94', quranic_completion_via_later_revelation).
narrative_ontology:cs_drift_state('aab2fd75-d1c6-4d28-bf72-67670c00bb94', contemporary_islamic_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aab2fd75-d1c6-4d28-bf72-67670c00bb94', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, women_under_egalitarian_framework).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, traditionalist_scholars).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, communities_bound_to_literal_reading).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88 at interval end) because this reading completely reverses the normative framework: what traditionalist jurisprudence treated as divine ordinance, this reading treats as historically superseded. Suppression is substantial (0.76) because the reading's persistence depends on actively excluding alternative interpretations from authority structures—it requires institutional control (academic credentials, publishing platforms, fatwa authority) to establish naskh-based interpretation as canonical. Theater is moderate (0.42) because the abrogation framework genuinely reorganizes how the text is read and produced real doctrinal changes (women's inheritance cases, family law reform in some jurisdictions), but a growing share of the reading's social work is defending itself against traditionalist jurisprudence rather than explaining its positive content. Accessibility collapse is moderate (0.68) because alternative readings remain intellectually and institutionally available—traditionalist scholars maintain alternative interpretations, and the reading does not make competing readings logically impossible, only delegitimized within certain institutional contexts. Resistance is high (0.79) because traditionalist institutions actively resist this reading, and communities whose identity depends on literal interpretation mount sustained counter-argument. The measurement series shows rising extractiveness and suppression from interval start to midpoint (as the reading's institutional footprint expanded from niche scholarly position to broader influence in diaspora contexts), then plateauing (mature institutional position but unable to fully suppress traditionalist alternatives). Theater ratio rises early (as the reading generates defensive engagement with traditionalist jurisprudence) then stabilizes (the debate reaches a steady institutional state). This pattern reflects the reading's trajectory from insurgent reinterpretation to established-but-contested framework.
 *
 * PERSPECTIVAL GAP:
 *   The progressive scholar seat and the traditionalist scholar seat should compute vastly different classifications from the same constraint. From the progressive seat, this is a rope: it solves a real coordination problem (reconciling diverse Qur'anic principles) and the beneficiaries (women, egalitarian-minded Muslims) outnumber the payers in most contexts where this reading has institutional foothold. From the traditionalist seat, this is a snare: it extracts authority through a reading method (naskh) they consider methodologically illegitimate, delegitimizes their scholarship as incomplete, and enforces exclusion of their interpretations from prestige institutions. The engine computes this per-seat divergence from directionality: progressive scholars get low d (they benefit from the framework they set), traditionalists get high d (they bear reputational and institutional costs from a framework they did not choose and view as hermeneutically flawed). Diaspora women advocates and secular Muslim-majority state officials get low-moderate d (they benefit from gender equality without being directly targeted by the suppression machinery). Communities bound to literal reading and traditionalist institutions get high d (they are the direct targets of delegitimization and institutional marginalization).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: women gaining legal equality, progressive scholars gaining interpretive authority, secular Muslim-majority states gaining Islamic legitimacy for gender-equal law. d near 0.1–0.3 for these seats. Victims: traditionalist scholars (institutional marginalization, reputational delegitimization), communities bound to literal reading (identity destabilization, epistemic violence). d near 0.75–0.9 for these seats. The asymmetry is profound and permanent: the reading's core mechanism is the claim that earlier gender rulings are overridden, which logically requires treating traditionalist scholarship as incomplete or epistemically trapped. There is no symmetric reading where both literal and progressive interpretations are equally valid authorities—the abrogation reading's internal logic forecloses that possibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling gender-differentiated and egalitarian verses) is live and unsolved by traditionalist readings. The progressive abrogation reading proposes a specific solution: treat the later egalitarian verses as superseding earlier gender-specific rulings via naskh. This avoids the trap of falsely calling it a snare because the reading genuinely solves an interpretive coordination problem for Muslims who want to honor both types of verses. However, the reading is tangled rope, not pure rope, because the solution requires active enforcement (institutional authority structures that privilege naskh-based hermeneutics) and produces asymmetric extraction: the mechanism of solution (chronological supersession) logically entails that one party's interpretive framework is incomplete or wrong. The reading does not offer symmetric coordination where literalist and progressive jurisprudence coexist as equally valid; it offers a framework where one approach is superior because it captures the Qur'an's trajectory. This explains the high extractiveness: the coordination benefit is real, but it is purchased by redefining what counts as valid Islamic jurisprudence in ways that systematically delegitimize alternative readings. Without the active enforcement (control of publication, academic credentialing, fatwa authority), this reading reverts to minority opinion. With it, it becomes the framework through which gender law is reformed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_validity_contested,
    'Is naskh (abrogation) a valid and correct hermeneutical principle for reconciling Qur''anic verses, or does it rest on methodological assumptions that alternative readings reject?',
    'Academic consensus among Islamic jurisprudence scholars from multiple traditional schools on whether naskh meets the technical criteria established in classical usul al-fiqh (principles of jurisprudence). This would require scholarly debate producing a meta-consensus on methodology, not merely on outcomes.',
    'If naskh is accepted as valid by a supermajority of scholarly authorities, this reading''s institutional footprint expands and its claimed legitimacy rises. If naskh is rejected or treated as one among equally valid methods, this reading reverts to minority opinion and the extracted authority flows back to traditionalist scholarship.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naskh_validity_contested, conceptual, 'Whether naskh as hermeneutical method is epistemically justified or merely one contestable approach.').

omega_variable(
    epistemic_violence_asymmetry,
    'Does the progressive abrogation reading''s reframing of traditionalist readings as incomplete (rather than legitimate alternatives) constitute epistemic violence against communities whose identity is bound to literal interpretation?',
    'Post-adoption trajectory: if communities adopting this reading show evidence of dismissing traditionalist scholars as ignorant or backward (rather than simply disagreeing), and if traditionalist scholars report experiencing epistemic marginalization (their arguments not being heard as intellectually serious), the violence is instantiated in practice. If engagement remains charitable and alternative readings are contested but recognized as serious, the violence is minimized.',
    'If epistemic violence is substantiated, the reading''s suppression component must be understood as operating partly through dismissal of traditionalist reasoning itself, not merely institutional exclusion. This would strengthen the snare classification risk and create pressure for a mandatrophy reclassification. If engagement remains charitable despite institutional hierarchy, the reading can be defended as tangled rope (real coordination benefit, real institutional asymmetry, but no epistemic delegitimization of alternative reasoning).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_violence_asymmetry, empirical, 'Whether this reading dismisses traditionalist jurisprudence as epistemically inferior or merely institutionally subordinate.').

omega_variable(
    identity_locked_exit_ambiguity,
    'For scholars trained in traditionalist jurisprudence and communities structured around literal interpretation, is the barrier to adopting the progressive abrogation reading a structural lock (institutional gatekeeping, reputational cost) or an identity lock (the reading is epistemically incoherent from within traditionalist frameworks)?',
    'Biographical study of scholars who switched from traditionalist to progressive frameworks: did they experience a cognitive/identity rupture (the old framework became unintelligible, required fundamental identity reorganization), or did they face institutional barriers that made the switch professionally risky but intellectually continuous?',
    'If the exit is structural-institutional, the high suppression (0.76) is the correct measure of active enforcement needed. If the exit is deeply identity-locked, the suppression underestimates the constraint''s hold because it does not account for internalized commitment. Communities would be unable to exit even if institutional barriers were removed, because the reading is epistemically foreign to their self-understanding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_ambiguity, empirical, 'Whether the traditionalist exit barrier is institutional gatekeeping or identity-level incoherence.').

omega_variable(
    kernel_reading_distinction,
    'Is the progressive abrogation reading a genuinely distinct constraint from the contextual-egalitarian reading, or are they variations on a shared egalitarian framework that both seek to move past literal hierarchy?',
    'Detailed comparison of ε values if both readings were instantiated as separate constraints: Do they produce significantly different directionality profiles for the same stakeholders? Do they differ in which benefits accrue to whom and what mechanisms of extraction operate? If ε values differ by >0.15 points, they are distinct constraints; if ε values cluster within 0.10 points, they may be variants of a single constraint viewed through different hermeneutical lenses.',
    'If they are distinct constraints, this story stands alone. If they are variants, a constraint family linking both should be created, with this reading instantiating one member. The distinction matters because the contextual reading may avoid some epistemic violence (it does not claim earlier verses are overridden, only reinterpreted) and may generate different institutional trajectories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether progressive abrogation is structurally distinct from contextual egalitarian reading or a variant of shared egalitarian framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qura_tr_t8, quranic_gender_verses__progressive_abrogation, theater_ratio, 8, 0.28).
narrative_ontology:measurement(qura_tr_t16, quranic_gender_verses__progressive_abrogation, theater_ratio, 16, 0.32).
narrative_ontology:measurement(qura_tr_t24, quranic_gender_verses__progressive_abrogation, theater_ratio, 24, 0.37).
narrative_ontology:measurement(qura_tr_t32, quranic_gender_verses__progressive_abrogation, theater_ratio, 32, 0.4).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__progressive_abrogation, theater_ratio, 40, 0.42).
narrative_ontology:measurement(qura_tr_t50, quranic_gender_verses__progressive_abrogation, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(qura_be_t8, quranic_gender_verses__progressive_abrogation, base_extractiveness, 8, 0.71).
narrative_ontology:measurement(qura_be_t16, quranic_gender_verses__progressive_abrogation, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(qura_be_t24, quranic_gender_verses__progressive_abrogation, base_extractiveness, 24, 0.83).
narrative_ontology:measurement(qura_be_t32, quranic_gender_verses__progressive_abrogation, base_extractiveness, 32, 0.86).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__progressive_abrogation, base_extractiveness, 40, 0.88).
narrative_ontology:measurement(qura_be_t50, quranic_gender_verses__progressive_abrogation, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(qura_su_t8, quranic_gender_verses__progressive_abrogation, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(qura_su_t16, quranic_gender_verses__progressive_abrogation, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(qura_su_t24, quranic_gender_verses__progressive_abrogation, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(qura_su_t32, quranic_gender_verses__progressive_abrogation, suppression_requirement, 32, 0.74).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__progressive_abrogation, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(qura_su_t50, quranic_gender_verses__progressive_abrogation, suppression_requirement, 50, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__progressive_abrogation, 0.12).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).

% DUAL FORMULATION NOTE:
% Part of the quranic_gender_verses constraint family. The three members (literal_hierarchical, contextual_egalitarian, progressive_abrogation) are readings of a single contested kernel—the same Qur'anic gender verses. They differ structurally in their ε values, their hermeneutical methods, and their stakeholder directionality profiles. The progressive abrogation reading is distinct from the contextual egalitarian reading in mechanism (chronological supersession via naskh vs. semantic reinterpretation via maqasid) and in implication (earlier rulings are overridden vs. reinterpreted). Both egalitarian readings (contextual and progressive abrogation) conflict with the literal hierarchical reading but through different logical structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__progressive_abrogation, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
