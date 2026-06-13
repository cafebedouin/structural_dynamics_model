% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__state_enforced_creation_reading, []).

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
 *   constraint_id: quran_ontological_status__state_enforced_creation_reading
 *   human_readable: State-Enforced Mu'tazilite Creationism (Mihna Inquisition)
 *   domain: theological/political
 *
 * SUMMARY:
 *   During the Abbasid caliphate (9th century CE), al-Ma'mun and al-Mu'tasim
 *   institutionalized the Mu'tazilite doctrine that the Qur'an is created
 *   (makhlūq) as state orthodoxy. The constraint is the enforcement machinery
 *   (mihna inquisition) that transformed a theological claim into a
 *   suppression mechanism. Scholars like Ahmad ibn Hanbal were imprisoned and
 *   tortured until they publicly affirmed creationism or recanted their
 *   uncreated-Qur'an doctrine. This story captures ONE READING of a contested
 *   kernel: the theological dispute about Qur'an's ontological status. This
 *   reading emphasizes the state-enforcement apparatus and the snare
 *   structure it produced. Sibling readings (created_reading as pure
 *   theology, uncreated_reading as competing doctrine) are separate
 *   constraints with different ε values, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Caliphal authority (al-Ma'mun, al-Mu'tasim): sets doctrine, enforces via mihna tribunals, benefits from doctrinal control as political consolidation tool
 *   - Traditionalist scholars (Ahmad ibn Hanbal, al-Shafi'i's circle): bear the cost through imprisonment, torture, forced recantation; identity-locked exit because the doctrine contradicts core theological identity
 *   - Rationalist theologians (Mu'tazilites, al-Jahiz): temporarily benefit from state patronage and intellectual authority, but become contingently dependent on caliphal preference
 *   - Literalist communities (the majority of the population): excluded from public discourse, stigmatized, constrained to silent conformity
 *   - Scholarly pluralism itself: the analytical fact of theological diversity ceases to be an option; institutional ecosystem collapses to single state-mandated orthodoxy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.82).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.91).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "State-Enforced Mu'tazilite Creationism (Mihna Inquisition)").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "theological/political").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, '7dc7c67b-0ecf-4128-a383-92a82deaa918').
narrative_ontology:cs_kernel_codification('7dc7c67b-0ecf-4128-a383-92a82deaa918', distributed).
narrative_ontology:cs_authority_grounding('7dc7c67b-0ecf-4128-a383-92a82deaa918', extraction).
narrative_ontology:cs_reading_relation('7dc7c67b-0ecf-4128-a383-92a82deaa918', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_reading_relation('7dc7c67b-0ecf-4128-a383-92a82deaa918', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_axiom('7dc7c67b-0ecf-4128-a383-92a82deaa918', foundational, rational_theology_requires_created_qur_an).
narrative_ontology:cs_axiom_status(rational_theology_requires_created_qur_an, overridden).
narrative_ontology:cs_axiom_grounding('7dc7c67b-0ecf-4128-a383-92a82deaa918', rational_theology_requires_created_qur_an, empirically_contingent).
narrative_ontology:cs_axiom('7dc7c67b-0ecf-4128-a383-92a82deaa918', foundational, caliphal_authority_grounds_theological_truth).
narrative_ontology:cs_axiom_status(caliphal_authority_grounds_theological_truth, holdable).
narrative_ontology:cs_axiom_grounding('7dc7c67b-0ecf-4128-a383-92a82deaa918', caliphal_authority_grounds_theological_truth, conventional).
narrative_ontology:cs_reference_frame('7dc7c67b-0ecf-4128-a383-92a82deaa918', rationalist_unified_metaphysics).
narrative_ontology:cs_drift_state('7dc7c67b-0ecf-4128-a383-92a82deaa918', post_al_mutawakkil_collapse, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('7dc7c67b-0ecf-4128-a383-92a82deaa918', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, rationalist_theologians_temporarily).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__state_enforced_creation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__state_enforced_creation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint's persistence depends on the caliphate's coercive capacity, not on intellectual conviction or genuine coordination benefits. The trajectory rises (0.45→0.82) as enforcement intensifies under al-Mu'tasim (more imprisonment, more elaborate inquisition tribunals), then plateaus and slightly declines as the system becomes normalized (theater_ratio rises as performance of conformity replaces active suppression). Suppression is highest (0.91) because the constraint's enforcement is the constraint itself—the mihna is not a side effect of doctrine but the apparatus that manufactures compliance. Accessibility collapse is moderate-high (0.72) because exit from the theological position requires identity dissolution (for scholars) or institutional invisibility (for communities), but alternatives persist in underground networks. Resistance is high (0.79) because traditionalist scholars refuse recantation even under torture, and the populace largely continues literalist interpretation despite state stigma. Theater_ratio is low (0.28) because the inquisition tribunals are not theatrical—they are brutal enforcement—but over time the constraint does develop a performative layer (ritual affirmations of doctrine by scholars who privately maintained other views).
 *
 * PERSPECTIVAL GAP:
 *   From the caliphal seat: the constraint is a philosophical unification that consolidates authority (d near beneficiary). From the traditionalist scholar seat: the constraint is a torture apparatus that demands identity destruction (d at target end). From the rationalist seat: the constraint is state patronage and intellectual authority, but contingent on caliphal whim. The engine should compute these perspectives as strongly divergent classifications: agenda_setter and beneficiary seats should classify this as rope (coordination function, state-backed); victim seats should classify as snare (pure extraction, suppression, forced conformity). This divergence is exactly what the per-seat computation is designed to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Caliphal authority: d near 0.0 (beneficiary, powerful, arbitrage exit—can shift doctrine at will). Rationalist theologians: d around 0.3-0.4 (benefit from state backing, but constrained exit because career is tied to caliphal preference; contingent beneficiary). Traditionalist scholars: d near 1.0 (victims, identity-locked, trapped). Literalist communities: d near 0.9 (victims, powerless, constrained). Scholarly pluralism: analytical seat, d = 0.5 by definition. No directionality overrides needed; the base structural data (beneficiary/victim + exit + power) derives the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rational theology, unified metaphysics) is DEAD by the time the mihna collapses. When al-Mutawakkil ended state enforcement (after al-Mu'tasim), the scholarly consensus immediately returned to pluralism and uncreated-Qur'an doctrine, demonstrating that the state enforcement had manufactured an artificial orthodoxy, not solved a genuine intellectual problem. The constraint exhibits classic mandatrophy: the founding mandate (produce rational theological consensus) has been superseded or invalidated by its own operation (coercion revealed that consensus was false). The constraint persists as pure extraction because the caliphate benefits from doctrinal control, even though the founding problem no longer justifies the mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theology_vs_coercion_boundary,
    'Would the Mu''tazilite created-Qur''an doctrine have been adopted by the scholarly consensus without state enforcement, or did state coercion manufacture an artificial consensus?',
    'Counterfactual analysis from post-mihna theological development: after the collapse of mihna enforcement (under al-Mutawakkil), the scholarly consensus rapidly rejected state-enforced creationism and returned to pluralism. This suggests the doctrine did not reflect genuine intellectual conviction but rather coerced conformity.',
    'If the doctrine was purely enforced consensus (not genuine intellectual adoption), the entire extracted value appears as pure political rent-seeking with no coordination function. If some portion of rationalist scholars genuinely adopted it, a fraction of extraction might be attributed to intellectual competition rather than pure coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theology_vs_coercion_boundary, empirical, 'Whether the created-Qur''an doctrine commanded genuine intellectual authority or only coerced conformity.').

omega_variable(
    suppression_mechanism_internalization,
    'After the mihna collapsed, did traditionalist scholars retain internalized suppression (self-censorship, hesitation to speak on metaphysical issues openly), or did they immediately resume unconstrained scholarly activity?',
    'Post-mihna historical records of traditionalist scholarly output: if scholars like Ahmad ibn Hanbal''s immediate successors resumed vigorous public theological work without hesitation, suppression was structural only. If there was a lag period of self-imposed caution, suppression became partially internalized.',
    'Structural suppression ends when coercion ends; internalized suppression persists after exit. If suppression became internalized, the constraint''s true cost exceeded the measured suppression_requirement, and the regime''s extractive impact extended beyond formal enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression was purely structural (external coercion) or partially internalized (lasting psychological effect).').

omega_variable(
    rationalist_school_capture,
    'Did the rationalist theologians become intellectual agents of the caliphate, or did they retain independent scholarly authority while benefiting from state backing?',
    'Comparison of rationalist theological output before, during, and after the mihna: did their intellectual positions change to match caliphal preference, or did they maintain independent lines of argument despite state support?',
    'If rationalists were fully captured (intellectually subordinated to caliphal preference), they are not true beneficiaries but forced collaborators; the snare structure extends to them as well. If they retained some intellectual autonomy, the beneficiary categorization is accurate and the snare is localized to traditionalists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rationalist_school_capture, conceptual, 'Whether the rationalist school was genuinely beneficiary or was itself intellectually captured.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint (state-enforced creation doctrine via mihna) fundamentally different from a constraint describing the creation doctrine itself without enforcement, or are they variants of one constraint measured under different observable conditions?',
    'The ε-invariance test: the creation doctrine (as a pure theological claim) has near-zero extraction and moderate theater (scholarly debate). The state-enforced version has high extraction and high suppression. These are different ε values, indicating two distinct constraints, not one constraint viewed two ways. This confirms the kernel decomposition: the theological claim is one constraint; the state-enforcement mechanism is another, linked via network.',
    'If confirmed as two constraints, the network relationship (state_enforced_creation_reading affects theological_creation_doctrine) is structurally justified. If the constraints are the same, the decomposition was false and the reading should be re-authored as a single perspective-dependent story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether state enforcement creates a distinct constraint or is merely a perspective on the theology constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(qura_tr_t5, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(qura_tr_t10, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(qura_tr_t15, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(qura_tr_t20, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(qura_tr_t25, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement(qura_tr_t30, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(qura_tr_t35, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 35, 0.28).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(qura_be_t5, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(qura_be_t10, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(qura_be_t15, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement(qura_be_t20, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 20, 0.84).
narrative_ontology:measurement(qura_be_t25, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement(qura_be_t30, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(qura_be_t35, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 35, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(qura_su_t5, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement(qura_su_t10, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 10, 0.83).
narrative_ontology:measurement(qura_su_t15, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 15, 0.89).
narrative_ontology:measurement(qura_su_t20, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 20, 0.91).
narrative_ontology:measurement(qura_su_t25, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 25, 0.88).
narrative_ontology:measurement(qura_su_t30, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(qura_su_t35, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 35, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__state_enforced_creation_reading, 0.12).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__uncreated_reading).

% DUAL FORMULATION NOTE:
% The quran_ontological_status kernel decomposes into three separate constraints: (1) created_reading—pure theological claim that Qur'an is created, as intellectual doctrine; (2) state_enforced_creation_reading (THIS constraint)—the mihna inquisition apparatus enforcing the doctrine via coercion; (3) uncreated_reading—competing theological claim that Qur'an is uncreated eternal speech. Each reading has a distinct ε: created_reading has low extraction (intellectual claim only); state_enforced_creation_reading (THIS) has high extraction (coercion, suppression, torture); uncreated_reading has moderate extraction (theological competition, state suppression). The three constraints are linked via network.affects_constraints because each reading's institutional fate depends on the others' fate. The theological claim (created_reading) is logically independent; the state-enforcement reading forecloses the uncreated reading within the same caliphate framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__state_enforced_creation_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
