% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Geneva Protective Scope by Conflict-Type Classification (Hybrid/Proportionality Reading)
 *   domain: international_humanitarian_law/legal_theory/armed_conflict
 *
 * SUMMARY:
 *   This story instantiates the hybrid/proportionality reading of the Geneva
 *   protective-scope kernel: the position that protections legitimately scale
 *   according to whether a conflict is classified international or
 *   non-international, with proportionality analysis operating as the
 *   adjudicative mechanism determining specific application. Unlike the
 *   state-centric reading (which grounds exclusion in formal Article 4
 *   combatant criteria) or the universal-rights reading (which denies that
 *   classification should determine protective floor at all), this reading
 *   treats the two-tier structure plus case-by-case proportionality balancing
 *   as the legitimate, negotiated architecture of IHL. The extraction this
 *   story measures is not a claim that hybrid classification is illegitimate
 *   in principle — it is a measurement of what happens when the
 *   classification decision and the proportionality calculus are both
 *   performed, in the first instance, by the party whose own conduct they
 *   constrain. The rise in extraction and theater ratio from 1977 through the
 *   mid-2000s tracks the proliferation of internationalized internal
 *   conflicts, transnational counterterrorism operations, and
 *   non-international conflicts with heavy external state involvement —
 *   precisely the conflict types where classification is most contested and
 *   where a party's incentive to select the lower-protection classification
 *   is strongest. The modest decline after 2017 reflects growing
 *   customary-law convergence and tribunal jurisprudence narrowing the
 *   practical gap between the two regimes, without eliminating the underlying
 *   structural discretion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.58).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.52).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Geneva Protective Scope by Conflict-Type Classification (Hybrid/Proportionality Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "international_humanitarian_law/legal_theory/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'd90e26a6-8324-4659-8570-443f59018a3b').
narrative_ontology:cs_kernel_codification('d90e26a6-8324-4659-8570-443f59018a3b', fixed_text).
narrative_ontology:cs_authority_grounding('d90e26a6-8324-4659-8570-443f59018a3b', practice).
narrative_ontology:cs_interpretation_layer_present('d90e26a6-8324-4659-8570-443f59018a3b').
narrative_ontology:cs_reading_relation('d90e26a6-8324-4659-8570-443f59018a3b', geneva_conventions_protective_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('d90e26a6-8324-4659-8570-443f59018a3b', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('d90e26a6-8324-4659-8570-443f59018a3b', foundational, protection_legitimately_scales_with_conflict_classification).
narrative_ontology:cs_axiom_status(protection_legitimately_scales_with_conflict_classification, holdable).
narrative_ontology:cs_axiom_grounding('d90e26a6-8324-4659-8570-443f59018a3b', protection_legitimately_scales_with_conflict_classification, conventional).
narrative_ontology:cs_axiom('d90e26a6-8324-4659-8570-443f59018a3b', foundational, proportionality_balancing_is_the_operative_adjudicative_standard).
narrative_ontology:cs_axiom_status(proportionality_balancing_is_the_operative_adjudicative_standard, holdable).
narrative_ontology:cs_axiom_grounding('d90e26a6-8324-4659-8570-443f59018a3b', proportionality_balancing_is_the_operative_adjudicative_standard, instrumental).
narrative_ontology:cs_reference_frame('d90e26a6-8324-4659-8570-443f59018a3b', ap_i_ap_ii_common_article_3_negotiated_compromise).
narrative_ontology:cs_drift_state('d90e26a6-8324-4659-8570-443f59018a3b', post_9_11_asymmetric_conflict_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d90e26a6-8324-4659-8570-443f59018a3b', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, classifying_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, legal_advisors_of_dominant_parties).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_group_fighters).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_contested_classification_zones).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, detainees_of_ambiguous_status).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_state_parties).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_state_parties).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__hybrid_proportionality_reading, conflict_classification_determines_applicable_law_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__hybrid_proportionality_reading, proportionality_as_legal_adjudicative_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines, through its own legal and operational chain, whether a given conflict is classified as international or non-international, and applies the correspondingly lower or higher protective standard. Because the classification decision is made internally before it is ever externally reviewed, the classifying party effectively selects which body of law constrains its own conduct, and the proportionality calculus it applies to targeting decisions is likewise self-administered in the first instance.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, classifying_state_militaries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, classifying_state_militaries, beneficiary).

% Provide the legal opinions that justify a given conflict classification and proportionality assessment after military operations are planned or completed. Their professional position is strengthened by the doctrinal flexibility of the hybrid framework — ambiguity generates demand for their interpretive expertise, and their institutional employer benefits when their opinions favor a lower protective standard.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, legal_advisors_of_dominant_parties, beneficiary,
    institutional, biographical, arbitrage, global).

% Fight in conflicts frequently classified as non-international, which triggers only Common Article 3 and AP II protections — a materially thinner protective regime than AP I combatant status affords. They have no standing to contest the classification decision, no forum in which to challenge it before harm occurs, and no capacity to exit the conflict zone or change which body of law applies to them.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_group_fighters, payer,
    powerless, immediate, trapped, regional).

% Live in areas where the conflict's classification is disputed or shifts over time (internationalized internal conflicts, cross-border interventions, proxy warfare). The protective floor available to them depends entirely on a classification made by parties to the conflict, not by them or on their behalf, and the proportionality analysis weighing their safety against military necessity is conducted by the same party whose operations put them at risk.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_contested_classification_zones, payer,
    powerless, immediate, trapped, local).

% Captured in operations where their combatant or civilian status, and the conflict's classification, are contested by the detaining power. Whether they receive POW treatment, civilian internee protections, or a lesser standard depends on a determination made by their captor, with limited independent review, leaving them without clarity on what protections apply to them at the moment they most need to know.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, detainees_of_ambiguous_status, payer,
    powerless, immediate, trapped, local).

% States without the military or diplomatic weight to contest a stronger adversary's or intervener's conflict classification. They benefit in principle from having a legal framework at all, but in practice absorb the cost when classification and proportionality determinations are made unilaterally by more powerful belligerents, with little recourse beyond diplomatic protest or international opinion.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_state_parties, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_state_parties, beneficiary).

% Monitors compliance, advocates for expansive interpretation of protective scope, and documents violations, but has no binding authority to override a belligerent's own classification decision or proportionality assessment. Its interpretive commentary is influential but non-binding, leaving it able to observe and protest without being able to correct in real time.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_committee_of_the_red_cross, observer,
    organized, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_committee_of_the_red_cross, excluded).

% Review conflict classification and proportionality determinations retrospectively, often years after the harm occurred, in the course of adjudicating individual criminal responsibility. Their rulings can reshape doctrine going forward but cannot restore protections that were absent at the time of the conduct in question.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_criminal_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__hybrid_proportionality_reading, classifying_state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__hybrid_proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable legal framework that lets belligerents, courts, and humanitarian actors agree on what law applies to a given armed conflict, avoiding a situation where every party claims a wholly different and incommensurable set of obligations governs the same fighting.
% TRANSFER_FUNCTION: Moves protective certainty away from populations in conflicts susceptible to contested classification (non-state fighters, civilians in internationalized internal conflicts, ambiguous-status detainees) and toward the classifying party, whose self-administered determination of conflict type and proportionality sets the operative protective floor before any external body reviews it.
% ABSENT_VOICES: Non-state armed group fighters and civilians in contested zones have no forum to contest a classification decision before it is applied to them; the ICRC and international tribunals can comment and adjudicate but only after the fact, and neither can bind the classifying party's initial determination in real time.
% DISAPPEARANCE_RATIONALE: If conflict-type classification and proportionality-based scaling of protections disappeared and a single uniform protective standard applied regardless of conflict type, non-state fighters and civilians in internationalized conflicts would gain the higher AP I-equivalent protective floor, states would lose the discretion to select a lower standard through classification, and the interpretive-advisory function that currently generates demand for classification expertise would substantially contract.
% FOUNDING_PROBLEM: The 1949 Conventions and 1977 Protocols were built to extend meaningful legal protection to persons affected by armed conflict, while accommodating the reality that states were unwilling to grant full combatant privileges to non-state actors in internal conflicts — the classification split was a negotiated compromise to secure state ratification at all.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's own commentaries and customary IHL study attest that the classification-based gap in protection was a ratification compromise rather than a principled distinction in humanitarian need, and note the trend toward narrowing that gap through customary law; independent legal scholars outside any belligerent's employ document that classification determinations are frequently outcome-driven rather than fact-driven. Classifying states and their legal advisors, by contrast, maintain the distinction reflects a live and necessary sovereignty-based limit on treaty scope.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is substantial but not extreme: the hybrid framework does perform genuine coordination work (a single classification-plus-proportionality architecture is far more workable than case-by-case ad hoc negotiation of applicable law), but the self-administration of both the classification decision and the initial proportionality assessment by the party being constrained introduces a persistent asymmetry. Suppression (0.52) reflects that alternatives to classification-dependent protection are not fully suppressed — the ICRC, tribunals, and customary law all pull toward convergence — but the classifying party's determination is operative and binding in the moment of harm, before any of those correcting mechanisms can act. Theater ratio (0.44) captures that a meaningful share of proportionality-analysis activity functions as ex post legal justification for targeting decisions already made, rather than as a genuine ex ante constraint on them. Accessibility collapse is moderate (0.4): the two-tier structure and its case law are extensively documented and theoretically navigable by anyone with legal resources, but functionally opaque to the powerless parties who bear its consequences. Resistance is substantial (0.62): non-state fighters, civilian populations, and humanitarian organizations actively contest classification decisions and proportionality assessments in the court of international opinion and before tribunals, even though that resistance rarely alters the determination in time to matter operationally.
 *
 * DIRECTIONALITY LOGIC:
 *   Classifying state militaries sit closest to the beneficiary end: they administer the classification, apply the proportionality standard to their own operations, and bear the lowest exit cost from any adverse determination given their institutional and diplomatic resources. Legal advisors of dominant parties benefit indirectly — doctrinal ambiguity is the raw material of their professional value. Non-state armed group fighters, civilians in contested zones, and ambiguous-status detainees sit at the target end: they are powerless, trapped in the conflict zone or in detention, and have no capacity to contest the classification determination before it is applied to them. Weaker state parties occupy an intermediate position — nominal beneficiaries of the framework's existence, but functional payers when a stronger adversary or intervening power unilaterally selects the classification and proportionality standard.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing any workable, ratifiable protective standard for persons in both international and internal armed conflicts — was live in 1977 and remains partially live today (states still will not accept full AP I-equivalent obligations in every internal conflict). This is why founding_problem_status is coded contested rather than dead: the compromise structure retains a genuine, currently-operative coordination function, distinguishing this reading from a pure snare reading in which classification exists solely to launder extraction. What has drifted is not the founding compromise itself but who performs the classification and proportionality determination in the first instance — increasingly the party whose own conduct is being assessed, with review occurring only retrospectively through tribunals whose rulings cannot restore protection that was absent when the harm occurred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_self_determination_ambiguity,
    'Is the persistent practice of the classifying party determining, in the first instance, both the conflict''s legal character and the proportionality of its own operations a necessary feature of a workable IHL framework, or a structural defect that could be corrected by mandatory contemporaneous independent review?',
    'Comparative analysis of conflicts where independent or third-party classification review occurred contemporaneously (e.g., through UN fact-finding missions or hybrid tribunals with real-time mandate) versus those relying solely on the belligerent''s self-classification, assessing whether independent review measurably altered protective outcomes.',
    'If self-determination is shown to be structurally necessary (no workable alternative institutional design exists), the extraction measured here is better understood as an inherent coordination cost of any classification-dependent regime. If independent contemporaneous review is shown to be feasible and outcome-altering, the current self-administered structure looks more like an avoidable asymmetry maintained for the benefit of classifying parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_self_determination_ambiguity, empirical, 'Whether self-administered classification and proportionality determination is structurally necessary or a correctable design defect.').

omega_variable(
    reading_selection_grounds,
    'What justifies treating the hybrid/proportionality framework as the legitimate baseline for this story rather than adopting the state-centric or universal-rights reading as the reference arrangement?',
    'This is a framing choice inherent to authoring one reading of a contested kernel (per DP-001/ε-invariance): the hybrid reading is selected because it reflects the doctrinally dominant, treaty-text-consistent position actually applied by states, tribunals, and the ICRC''s own operational commentary, distinguishing it from the state-centric reading''s narrower Article 4 test and the universal-rights reading''s rejection of classification-dependent scaling. Resolution would require tracking whether state practice and opinio juris continue to converge toward the universal-rights reading''s single floor, which would shift which reading is descriptively dominant.',
    'If state practice and tribunal jurisprudence converge substantially toward a universal floor, the hybrid reading''s ε and structural claims would need re-authoring to reflect a narrower and more contested domain of application, and the universal_rights_reading constraint would become the descriptively dominant sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_grounds, conceptual, 'Which kernel reading best reflects current doctrinally dominant practice, and how that could shift.').

omega_variable(
    proportionality_calculus_manipulability,
    'To what extent is the proportionality analysis itself (weighing anticipated military advantage against expected civilian harm) an objectively constraining legal test versus a discretionary judgment that systematically resolves in favor of the party performing it?',
    'Empirical review of publicly available proportionality assessments and post-hoc tribunal rulings on the same incidents, comparing the operative party''s contemporaneous assessment against independent reconstruction of the same facts.',
    'If independent reconstructions systematically diverge from the operative party''s contemporaneous proportionality conclusions in the direction of finding greater expected harm, this would support classifying the proportionality step itself as a significant driver of the measured extraction rather than a genuine constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_calculus_manipulability, empirical, 'Whether proportionality analysis functions as genuine constraint or discretionary self-justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 1977, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1977, 0.25).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2001, 0.4).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2010, 0.46).
narrative_ontology:measurement(gene_tr_t2017, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2017, 0.48).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1990, 0.46).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(gene_be_t2017, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2017, 0.6).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1977, 0.35).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2010, 0.54).
narrative_ontology:measurement(gene_su_t2017, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2017, 0.55).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__universal_rights_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'Geneva protective scope' per the ε-invariance principle: this file (hybrid_proportionality_reading, tangled_rope, ε=0.58) models classification-plus-proportionality scaling as the operative legal architecture; geneva_conventions_protective_scope__state_centric_reading models the narrower Article 4 combatant-status test as exhaustive of treaty scope; geneva_conventions_protective_scope__universal_rights_reading models Common Article 3 plus IHRL as establishing a single universal floor independent of classification. Each carries its own stable ε, beneficiary/victim set, and classification, reflecting the sibling reading's own structural premises rather than a shared observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
