% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Reading of Usul al-Fiqh Method
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the Hanbali school's methodology within Usul
 *   al-Fiqh (principles of Islamic jurisprudence), characterized by maximal
 *   restrictiveness towards textual sources (Quran and authenticated Hadith),
 *   minimal use of analogical reasoning (qiyas), preference for weak Hadith
 *   over qiyas, and a strong emphasis on blocking innovations (sadd
 *   al-dhara'i). It is a reading of the broader 'usul_al_fiqh_method' kernel,
 *   which encompasses the diverse methodologies of the major Sunni legal
 *   schools. This reading prioritizes textual fidelity and tradition over
 *   rationalist or customary legal development.
 *
 * KEY AGENTS:
 *   - hanbali_scholars: Primary agenda-setter (institutional/identity_locked) — benefits from authority.
 *   - textualist_movements: Primary beneficiary (organized/constrained) — gains legitimacy.
 *   - rationalist_legal_theorists: Primary payer (moderate/constrained) — faces intellectual limits.
 *   - customary_law_practitioners: Primary payer (powerless/trapped) — marginalized by textualism.
 *   - innovative_jurists: Payer (moderate/constrained) — efforts labeled as bid'a.
 *   - hanafi_scholars: Observer (institutional/analytical) — represents a different methodology.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.75).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Reading of Usul al-Fiqh Method").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, '1a92f420-9303-4db3-9000-7cd44e874a68').
narrative_ontology:cs_kernel_codification('1a92f420-9303-4db3-9000-7cd44e874a68', fixed_text).
narrative_ontology:cs_authority_grounding('1a92f420-9303-4db3-9000-7cd44e874a68', lineage).
narrative_ontology:cs_interpretation_layer_present('1a92f420-9303-4db3-9000-7cd44e874a68').
narrative_ontology:cs_reading_relation('1a92f420-9303-4db3-9000-7cd44e874a68', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a92f420-9303-4db3-9000-7cd44e874a68', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a92f420-9303-4db3-9000-7cd44e874a68', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('1a92f420-9303-4db3-9000-7cd44e874a68', foundational, textual_primacy_over_reason).
narrative_ontology:cs_axiom_status(textual_primacy_over_reason, holdable).
narrative_ontology:cs_axiom_grounding('1a92f420-9303-4db3-9000-7cd44e874a68', textual_primacy_over_reason, deontological).
narrative_ontology:cs_axiom('1a92f420-9303-4db3-9000-7cd44e874a68', foundational, blocking_means_to_evil_as_legal_principle).
narrative_ontology:cs_axiom_status(blocking_means_to_evil_as_legal_principle, holdable).
narrative_ontology:cs_axiom_grounding('1a92f420-9303-4db3-9000-7cd44e874a68', blocking_means_to_evil_as_legal_principle, deontological).
narrative_ontology:cs_reference_frame('1a92f420-9303-4db3-9000-7cd44e874a68', early_hanbali_textualism).
narrative_ontology:cs_drift_state('1a92f420-9303-4db3-9000-7cd44e874a68', contemporary_global_islamic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1a92f420-9303-4db3-9000-7cd44e874a68', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hanbali_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, textualist_movements).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_legal_theorists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_law_practitioners).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, innovative_jurists).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, textual_fidelity_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, blocking_means_to_evil_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate legal matters primarily through strict adherence to Quran and authenticated Hadith, minimizing the role of human reason and analogy. They benefit from the authority derived from this textualist approach, which positions them as guardians of tradition.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanbali_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Advocate for a literalist interpretation of Islamic law, finding legitimacy and power in the Hanbali methodology's emphasis on textual sources and its rejection of innovation. They gain influence by presenting their views as the most authentic.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, textualist_movements, beneficiary,
    organized, biographical, constrained, regional).

% Seek to integrate reason, analogy (qiyas), and public interest (maslaha) more broadly into Islamic legal derivation. They face intellectual and institutional resistance from the Hanbali method, which limits their scope for legal development and interpretation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_legal_theorists, payer,
    moderate, biographical, constrained, global).

% Operate in contexts where local customs ('urf) and established practices have historically informed legal rulings. The Hanbali method's strict textualism often invalidates or marginalizes these customary sources, imposing a different legal framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_law_practitioners, payer,
    powerless, immediate, trapped, local).

% Propose new legal interpretations or methodologies to address contemporary challenges, often drawing on broader principles or analogical reasoning. They are constrained by the Hanbali method's emphasis on blocking innovations (sadd al-dhara'i), which can label their efforts as bid'a (unlawful innovation).
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, innovative_jurists, payer,
    moderate, biographical, constrained, global).

% Represent a different school of thought that allows for more expansive use of qiyas and istihsan. They observe the Hanbali method's operation and its impact on legal discourse, often engaging in scholarly debate but not directly subject to its enforcement.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanafi_scholars, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, hierarchical methodology for deriving Islamic law, ensuring consistency and fidelity to foundational texts, thereby coordinating legal interpretation across a community of scholars and practitioners.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual juristic reasoning and customary practice to a strict textualist framework, concentrating power in those who master and apply the foundational texts rigorously.
% ABSENT_VOICES: Early Islamic rationalist schools (e.g., Mu'tazila) and contemporary liberal Islamic thinkers, who would argue for a greater role for reason and contextual interpretation, are largely excluded from the Hanbali discourse, their methodologies deemed outside the accepted framework.
% DISAPPEARANCE_RATIONALE: If the Hanbali method's strictures vanished, the landscape of Islamic jurisprudence would immediately diversify, with a surge in analogical reasoning, contextual interpretations, and the integration of customary law. Legal rulings would become more varied, and the authority of textualist scholars would diminish, leading to a significant reorganization of legal thought and practice.
% FOUNDING_PROBLEM: The proliferation of diverse opinions and potential innovations (bid'a) in early Islamic legal thought, threatening the perceived purity and consistency of the nascent Islamic legal system.
% FOUNDING_PROBLEM_CORROBORATION: Hanbali scholars and textualist movements assert that the threat of innovation and deviation from textual sources remains live. Critics, including rationalist legal theorists and some Maliki scholars, acknowledge the historical problem but argue that the Hanbali solution has become overly rigid, stifling necessary legal evolution rather than merely preserving purity.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Hanbali method, while providing a clear framework, exhibits high extractiveness (0.68) by limiting the interpretive freedom of jurists and marginalizing alternative legal sources, thereby extracting intellectual and practical autonomy from those who seek broader legal development. Suppression (0.75) is high due to the strong institutional and theological pressure to conform to textual literalism and the active 'blocking of innovations' (sadd al-dhara'i), which functions as an enforcement mechanism against divergent thought. Theater ratio is low (0.20) because the textualist enforcement is largely genuine, aimed at maintaining doctrinal purity rather than mere performance. The historical measurements show a gradual increase in extractiveness and suppression as the methodology became more entrenched and its enforcement mechanisms more refined over centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hanbali scholars and textualist movements, this method is a 'rope' or even a 'mountain'—a necessary and natural framework for preserving the integrity of Islamic law. However, from the perspective of rationalist legal theorists, customary law practitioners, and innovative jurists, it operates as a 'tangled_rope' or 'snare,' coordinating legal interpretation at the cost of intellectual freedom and the suppression of alternative legal development.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanbali scholars and textualist movements are beneficiaries, as the method grants them significant authority and legitimacy by positioning them as guardians of tradition. Rationalist legal theorists, customary law practitioners, and innovative jurists are targets, as their methodologies are constrained or suppressed by the Hanbali approach. The 'identity_locked' exit option for Hanbali scholars reflects the deep professional and theological commitment to this methodology, making departure from it an existential challenge to their scholarly identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanbali method's mandate to preserve textual fidelity and prevent innovation remains 'live' according to its proponents. However, critics argue that while the founding problem of unchecked innovation may have been real, the method's rigidity has led to a 'tangled_rope' dynamic where genuine coordination (textual consistency) is intertwined with substantial extraction (suppression of diverse legal reasoning). The classification as 'tangled_rope' acknowledges both the coordination function and the asymmetric extraction, preventing mislabeling it as a pure 'rope' (as its proponents might claim) or a pure 'snare' (as its most ardent critics might assert).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hanbali_vs_other_schools_scope_of_qiyas,
    'To what extent does the Hanbali minimization of qiyas (analogical reasoning) genuinely preserve textual fidelity versus merely limiting the scope of rational legal development compared to other schools?',
    'Comparative legal analysis of rulings across different schools on novel cases where texts are silent, assessing the practical outcomes and their alignment with broader Islamic ethical principles.',
    'If qiyas minimization is found to unduly restrict beneficial legal development, the ''suppression'' and ''extractiveness'' metrics for the Hanbali reading would be re-evaluated upwards, potentially shifting its classification closer to a ''snare'' for those seeking legal innovation. If it demonstrably prevents harmful innovations, its coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hanbali_vs_other_schools_scope_of_qiyas, conceptual, 'Ambiguity in the functional impact of minimizing qiyas.').

omega_variable(
    sadd_al_dhara_i_legitimacy,
    'Is the principle of sadd al-dhara''i (blocking the means to evil) applied consistently to prevent genuine harm, or is it sometimes used to suppress legitimate intellectual and social innovation?',
    'Case studies of specific rulings based on sadd al-dhara''i, analyzing the actual harms prevented versus the opportunities for development foreclosed, and comparing with rulings from schools with different approaches to innovation.',
    'If sadd al-dhara''i is found to be over-applied or used to suppress legitimate innovation, the ''suppression'' metric would increase, and the ''theater_ratio'' might rise if the ''harm prevention'' justification becomes performative. This would reinforce the ''tangled_rope'' or ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sadd_al_dhara_i_legitimacy, empirical, 'The true function and impact of blocking innovations.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of the ''usul_al_fiqh_method'' kernel, or does its strictness constitute a distinct, non-comparable legal philosophy?',
    'Scholarly consensus on the shared foundational principles across the Sunni legal schools, and whether the Hanbali method''s divergences are within the bounds of interpretive difference or represent a fundamental break.',
    'If deemed a distinct philosophy, it would be reclassified as an independent constraint, not a reading of a shared kernel, altering its network relationships and comparative analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''usul_al_fiqh_method'' kernel, specifically the Hanbali interpretation. Sibling readings (Hanafi, Maliki, Shafii) would differ on the scope of qiyas, the role of custom, and the criteria for innovation. This reading''s emphasis on textual restrictiveness and blocking innovation would be less pronounced in other schools, leading to different beneficiary/victim structures and potentially lower extraction for rationalist jurists.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t800, usul_al_fiqh_method__hanbali_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement(usul_tr_t1200, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1200, 0.15).
narrative_ontology:measurement(usul_tr_t1600, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1600, 0.18).
narrative_ontology:measurement(usul_tr_t2024, usul_al_fiqh_method__hanbali_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(usul_be_t800, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 800, 0.55).
narrative_ontology:measurement(usul_be_t1200, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1200, 0.6).
narrative_ontology:measurement(usul_be_t1600, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1600, 0.65).
narrative_ontology:measurement(usul_be_t2024, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t800, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 800, 0.6).
narrative_ontology:measurement(usul_su_t1200, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1200, 0.68).
narrative_ontology:measurement(usul_su_t1600, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1600, 0.72).
narrative_ontology:measurement(usul_su_t2024, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, islamic_finance_regulation).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, family_law_interpretation).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, criminal_justice_application).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'usul_al_fiqh_method' kernel, each representing a major Sunni legal school. The Hanbali reading emphasizes textual restrictiveness, while others (Hanafi, Maliki, Shafii) allow for more expansive use of reason, custom, or public interest. Each reading constitutes a separate constraint due to differing ε values and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
