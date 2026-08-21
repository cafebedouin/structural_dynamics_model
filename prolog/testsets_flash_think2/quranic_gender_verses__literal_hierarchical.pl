% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Quranic Gender Verses: Literal-Hierarchical Reading
 *   domain: islamic_jurisprudence/legal_hermeneutics/gender_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'literal_hierarchical' reading of
 *   specific Quranic verses (4:11, 2:282, 4:34) concerning gender roles and
 *   rights. This reading asserts these verses as direct, timeless legal
 *   constraints establishing male guardianship and differentiated rights as
 *   divine ordinance. It is a contested interpretation within Islamic
 *   jurisprudence, with alternative readings offering contextual or
 *   abrogating perspectives. The high extractiveness and suppression reflect
 *   the structural disadvantages imposed on women by this interpretation,
 *   enforced through religious law and social norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.85).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.9).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.85).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, snare).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Quranic Gender Verses: Literal-Hierarchical Reading").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "islamic_jurisprudence/legal_hermeneutics/gender_studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, '9492df12-87bd-4c98-b1f1-aa576566b5e6').
narrative_ontology:cs_kernel_codification('9492df12-87bd-4c98-b1f1-aa576566b5e6', fixed_text).
narrative_ontology:cs_authority_grounding('9492df12-87bd-4c98-b1f1-aa576566b5e6', lineage).
narrative_ontology:cs_interpretation_layer_present('9492df12-87bd-4c98-b1f1-aa576566b5e6').
narrative_ontology:cs_reading_relation('9492df12-87bd-4c98-b1f1-aa576566b5e6', quranic_gender_verses__contextual_egalitarian, forecloses).
narrative_ontology:cs_reading_relation('9492df12-87bd-4c98-b1f1-aa576566b5e6', quranic_gender_verses__progressive_abrogation, forecloses).
narrative_ontology:cs_axiom('9492df12-87bd-4c98-b1f1-aa576566b5e6', foundational, divine_ordinance_timeless_applicability).
narrative_ontology:cs_axiom_status(divine_ordinance_timeless_applicability, holdable).
narrative_ontology:cs_axiom_grounding('9492df12-87bd-4c98-b1f1-aa576566b5e6', divine_ordinance_timeless_applicability, theological).
narrative_ontology:cs_axiom('9492df12-87bd-4c98-b1f1-aa576566b5e6', foundational, male_guardianship_divinely_mandated).
narrative_ontology:cs_axiom_status(male_guardianship_divinely_mandated, holdable).
narrative_ontology:cs_axiom_grounding('9492df12-87bd-4c98-b1f1-aa576566b5e6', male_guardianship_divinely_mandated, theological).
narrative_ontology:cs_reference_frame('9492df12-87bd-4c98-b1f1-aa576566b5e6', classical_islamic_jurisprudence).
narrative_ontology:cs_drift_state('9492df12-87bd-4c98-b1f1-aa576566b5e6', contemporary_global_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('9492df12-87bd-4c98-b1f1-aa576566b5e6', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_courts).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_in_literalist_jurisdictions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, conservative_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As per this reading, they are divinely ordained guardians (qawwamun) over women, granting them authority in household and family matters, and often control over resources. They benefit from a clear, divinely sanctioned hierarchy.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, agenda_setter,
    powerful, generational, arbitrage, global).

% These institutions interpret and enforce the verses as direct, timeless legal constraints, codifying differentiated rights in areas like inheritance, testimony, and divorce. Their authority is grounded in upholding this literal interpretation.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_courts, agenda_setter,
    institutional, generational, constrained, national).

% They bear the direct costs of this interpretation through constrained inheritance shares, reduced weight of testimony, limited legal autonomy, and often requiring male guardianship for significant life decisions. Exit from this framework often means social ostracization or legal penalties.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_in_literalist_jurisdictions, payer,
    powerless, biographical, identity_locked, global).

% Advocates for gender equality within Islamic frameworks, they challenge the literal-hierarchical reading, arguing for contextual or abrogated interpretations. They are often marginalized or actively suppressed by institutions upholding the literalist view.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, egalitarian_reformers, excluded,
    organized, generational, constrained, global).

% They uphold and propagate the literal-hierarchical reading, deriving significant social and intellectual authority from their role as interpreters and defenders of what they consider divine law. Their careers and influence are tied to the persistence of this interpretation.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, conservative_scholars, beneficiary,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:fixing_cost_class(quranic_gender_verses__literal_hierarchical, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, divinely sanctioned social and familial hierarchy, defining roles and responsibilities to maintain order and stability within the community.
% TRANSFER_FUNCTION: Transfers legal authority, social status, and control over resources from women to men, particularly male household heads and religious institutions, as a consequence of divinely ordained differentiated rights.
% ABSENT_VOICES: Egalitarian reformers and women's rights advocates are often excluded from interpretive authority and legal discourse, their alternative readings dismissed as heterodox or un-Islamic. They would argue for interpretations that align with universal human dignity and gender equity.
% DISAPPEARANCE_RATIONALE: If the literal-hierarchical interpretation of these verses vanished overnight, the legal systems, social norms, and family structures in many Muslim-majority societies would undergo profound reorganization, leading to significant shifts in gender roles, legal rights, and power dynamics.
% FOUNDING_PROBLEM: To establish a comprehensive legal and social framework for the nascent Muslim community in 7th-century Arabia, addressing issues of family structure, inheritance, and social order in a manner believed to be divinely guided.
% FOUNDING_PROBLEM_CORROBORATION: The literal-hierarchical reading is corroborated by centuries of traditional Islamic jurisprudence and the ongoing practice of religious courts in many Muslim-majority countries. However, this corroboration primarily comes from within the benefiting parties and their interpretive traditions; external corroboration from independent legal or social analyses is often absent or critical.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.85) because this interpretation directly translates into legal and social structures that grant men significant advantages in inheritance, testimony, and authority, while imposing constraints on women. Suppression is very high (0.90) due to the claim of divine ordinance, which makes challenging the interpretation akin to challenging religious truth, backed by the enforcement power of religious courts and strong social pressure. Accessibility collapse is high (0.80) as alternatives are actively suppressed by religious authority and social norms. Resistance is moderate (0.60) due to ongoing efforts by egalitarian reformers, but these efforts face significant institutional and social barriers. Theater ratio is low (0.10) because the constraint is presented as a direct, functional application of divine law, with little performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of male household heads and religious courts, this constraint is a divinely mandated framework for social order and justice, a 'rope' or even a 'mountain'. From the perspective of women in literalist jurisdictions, it is a 'snare' that extracts their rights and autonomy under the guise of divine will. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads and religious courts are clear beneficiaries and agenda-setters, gaining structural authority and resource control. Women in literalist jurisdictions are the primary targets, experiencing direct extraction of rights and autonomy. Conservative scholars also benefit from the authority derived from upholding this interpretation. Egalitarian reformers are excluded, as their perspectives challenge the very foundation of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a 'rope' (pure coordination) by highlighting the substantial extraction and suppression inherent in the literal-hierarchical reading. While proponents claim it coordinates social order, the asymmetric distribution of benefits and costs, coupled with active enforcement and suppression of alternatives, points to a snare. The 'live' status of the founding problem from the perspective of its proponents, contrasted with the high extraction, suggests a persistent, actively maintained extractive structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_intent_vs_human_interpretation,
    'Is the literal-hierarchical reading a direct, unambiguous reflection of divine intent, or is it a human interpretation shaped by historical and cultural contexts?',
    'Comparative theological and hermeneutical analysis across diverse Islamic traditions, examining the historical evolution of interpretive methodologies and their socio-political influences.',
    'If primarily a human interpretation, the constraint''s ''naturalness'' claim (emerges_naturally) would be undermined, potentially reclassifying it from a perceived ''mountain'' (by adherents) to a ''snare'' (by observers) with higher effective extraction. If truly unambiguous divine intent, its resistance to change would be structurally higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_intent_vs_human_interpretation, conceptual, 'Ambiguity regarding the source and immutability of the interpretation.').

omega_variable(
    enforcement_source_ambiguity,
    'To what extent is the suppression of women''s autonomy driven by explicit legal enforcement by religious courts versus internalized social norms and family pressure?',
    'Empirical studies comparing women''s legal and social status in jurisdictions with varying degrees of formal legal enforcement of these verses, alongside ethnographic research on community-level norm adherence.',
    'If primarily internalized, the effective suppression is higher and more resilient to legal reform, as the constraint persists even if formal legal barriers are removed. If primarily legal, targeted legal reforms could more effectively reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_source_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for women''s autonomy.').

omega_variable(
    contested_founding_problem_status,
    'Is the founding problem (establishing social order in early Islamic society) still ''live'' in a way that justifies the persistence of this hierarchical interpretation, or has the problem evolved such that the interpretation is now an anachronism?',
    'Sociological and historical analysis of contemporary Muslim societies, assessing whether the original social conditions persist and whether alternative, more equitable frameworks can achieve social order and justice.',
    'If the founding problem is ''dead'' or significantly altered, the constraint''s persistence becomes a clearer case of mandatrophy, strengthening its classification as a snare or piton, as its original justification no longer holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_founding_problem_status, empirical, 'Whether the constraint''s original justification remains valid in contemporary contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t10, quranic_gender_verses__literal_hierarchical, theater_ratio, 10, 0.11).
narrative_ontology:measurement(qura_tr_t20, quranic_gender_verses__literal_hierarchical, theater_ratio, 20, 0.1).
narrative_ontology:measurement(qura_tr_t30, quranic_gender_verses__literal_hierarchical, theater_ratio, 30, 0.1).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__literal_hierarchical, theater_ratio, 40, 0.1).
narrative_ontology:measurement(qura_tr_t50, quranic_gender_verses__literal_hierarchical, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(qura_be_t10, quranic_gender_verses__literal_hierarchical, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(qura_be_t20, quranic_gender_verses__literal_hierarchical, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(qura_be_t30, quranic_gender_verses__literal_hierarchical, base_extractiveness, 30, 0.84).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__literal_hierarchical, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(qura_be_t50, quranic_gender_verses__literal_hierarchical, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(qura_su_t10, quranic_gender_verses__literal_hierarchical, suppression_requirement, 10, 0.87).
narrative_ontology:measurement(qura_su_t20, quranic_gender_verses__literal_hierarchical, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(qura_su_t30, quranic_gender_verses__literal_hierarchical, suppression_requirement, 30, 0.89).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__literal_hierarchical, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(qura_su_t50, quranic_gender_verses__literal_hierarchical, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, islamic_family_law_codes).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, islamic_inheritance_laws).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'quranic_gender_verses' kernel. Its high extractiveness and suppression contrast sharply with the 'contextual_egalitarian' and 'progressive_abrogation' readings, which would yield lower extraction and suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
