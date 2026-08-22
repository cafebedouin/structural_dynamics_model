% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis in Common Law Precedent Corpus
 *   domain: legal/theoretical/constitutional
 *
 * SUMMARY:
 *   Strict stare decisis operates as a backward-binding constraint on the
 *   common law system: precedent, once established, binds future courts
 *   unless extraordinary justification for departure is shown. The constraint
 *   is claimed as a coordination mechanism (rope) by the judiciary and legal
 *   establishment, but the authored metrics reveal substantial extraction
 *   (transfer of normative authority from present communities to past courts)
 *   and active enforcement (the extraordinary justification standard,
 *   structural exclusion of constitutional textualist challenges,
 *   professional identity lock). The constraint has intensified over two
 *   centuries as the precedent corpus expanded and the overruling threshold
 *   hardened. The claim/metric gap is deliberate: the engine will compute
 *   per-seat types from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.68).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.72).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.68).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis in Common Law Precedent Corpus").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/theoretical/constitutional").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, 'c84bf037-8f8d-4b78-8bd2-1b1307cd21c4').
narrative_ontology:cs_kernel_codification('c84bf037-8f8d-4b78-8bd2-1b1307cd21c4', distributed).
narrative_ontology:cs_authority_grounding('c84bf037-8f8d-4b78-8bd2-1b1307cd21c4', lineage).
narrative_ontology:cs_interpretation_layer_present('c84bf037-8f8d-4b78-8bd2-1b1307cd21c4').
narrative_ontology:cs_reading_relation('c84bf037-8f8d-4b78-8bd2-1b1307cd21c4', common_law_precedent_corpus__evolutionary_framework, influences).
narrative_ontology:cs_reading_relation('c84bf037-8f8d-4b78-8bd2-1b1307cd21c4', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('c84bf037-8f8d-4b78-8bd2-1b1307cd21c4', foundational, precedent_binds_as_backward_constraint).
narrative_ontology:cs_axiom_status(precedent_binds_as_backward_constraint, holdable).
narrative_ontology:cs_axiom_grounding('c84bf037-8f8d-4b78-8bd2-1b1307cd21c4', precedent_binds_as_backward_constraint, conventional).
narrative_ontology:cs_axiom('c84bf037-8f8d-4b78-8bd2-1b1307cd21c4', foundational, overruling_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(overruling_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('c84bf037-8f8d-4b78-8bd2-1b1307cd21c4', overruling_requires_extraordinary_justification, conventional).
narrative_ontology:cs_reference_frame('c84bf037-8f8d-4b78-8bd2-1b1307cd21c4', classical_common_law_stare_decisis).
narrative_ontology:cs_drift_state('c84bf037-8f8d-4b78-8bd2-1b1307cd21c4', contemporary_judicial_supremacy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c84bf037-8f8d-4b78-8bd2-1b1307cd21c4', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, institutional_legal_establishment).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, precedent_dependent_practitioners).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, reform_seeking_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, marginalized_communities_under_entrenchment).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, novel_claim_anticipators).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, rule_of_law_as_stability).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, judicial_restraint_doctrine).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, precedent_as_constitutive_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies binding precedent; career and institutional identity fused to the authority of the precedent corpus; overruling own precedent is professionally exceptional and politically risky; cannot exit the role without leaving the judiciary.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, appellate_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Large firms, bar associations, law schools whose expertise, curricula, and business models are built on stable precedent; benefit from predictability and high switching costs for challengers; exit requires rebuilding professional capital.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, institutional_legal_establishment, beneficiary,
    organized, biographical, constrained, national).

% Practicing attorneys whose advice and litigation strategies depend on settled precedent; stability reduces client risk and training costs; departures increase malpractice exposure and retraining burden.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, precedent_dependent_practitioners, beneficiary,
    moderate, biographical, constrained, regional).

% Parties seeking to overturn or distinguish adverse precedent; face extraordinary justification standard; must convince the very institution that created the precedent; no alternative forum; loss entrenches the precedent further.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, reform_seeking_litigants, payer,
    powerless, immediate, trapped, local).

% Communities whose rights were denied or restricted by past precedent; bound by rulings they had no voice in creating; extraordinary justification standard requires them to overcome the weight of their own historical exclusion; exit from the legal system's authority is not meaningfully available.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, marginalized_communities_under_entrenchment, payer,
    powerless, generational, identity_locked, national).

% Activists, scholars, public interest lawyers developing new legal theories; must fit innovations into existing precedent architecture or face near-certain rejection; structural pressure to frame claims as extensions rather than breaks.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, novel_claim_anticipators, payer,
    moderate, biographical, constrained, national).

% Analyze the precedent system as a social institution; document the gap between formal bindingness and actual judicial behavior; provide the empirical basis for evaluating whether the constraint operates as claimed.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_realist_scholars, observer,
    analytical, civilizational, analytical, universal).

% Argue that precedent authority lacks constitutional warrant and displaces the written Constitution; structurally excluded from the precedent system's internal logic because their critique targets the system's foundation; must operate from outside to gain traction.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, constitutional_textualists, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for legal decision-making across time and jurisdictions; enables reliance interests, coordinates expectations, and reduces the cognitive load of re-litigating settled questions.
% TRANSFER_FUNCTION: Transfers normative authority from contemporary democratic majorities and affected communities to the accumulated holdings of past courts; moves the cost of legal change from the institution (which would have to justify novelty) to the challenger (who must justify departure).
% ABSENT_VOICES: Future generations who will inherit precedent they cannot contest; communities excluded from the bar and bench when the precedent was made; constitutional textualists who argue the system lacks democratic authorization; lay citizens who experience law as constraint without ever participating in its creation.
% DISAPPEARANCE_RATIONALE: If strict stare decisis vanished overnight, the entire architecture of legal advice, commercial ordering, constitutional litigation, and judicial decision-making would reorganize around a fundamentally different authority structure — courts would decide each case on its merits without backward constraint, legislative supremacy would likely be reasserted, and the legal profession's expertise would shift from precedent navigation to statutory interpretation.
% FOUNDING_PROBLEM: Early common law courts needed to create law in the absence of comprehensive statutes; binding precedent solved the coordination problem of making judicial decisions predictable and authoritative across a fragmented jurisdiction without legislative infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Lobban, Baker) document the historical emergence of stare decisis as a judicial innovation, not a constitutional mandate; the founding problem of statutory vacuum is largely resolved in modern systems with comprehensive codification, yet the constraint persists and has intensified — corroborated by scholars outside the benefiting judiciary (Friedman, Sunstein, critical legal studies).
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the transfer of lawmaking authority from living communities to dead judges — the constraint extracts the capacity for normative self-determination. Suppression (0.72) captures the extraordinary justification standard, the professional identity lock of judges, and the structural exclusion of foundational critiques. Theater ratio (0.28) acknowledges genuine coordination value (predictability, reliance interests) while marking the growing performative维护 of precedent authority beyond functional necessity. Accessibility collapse (0.78) is high because alternatives (legislative override, constitutional amendment, court-packing) are structurally difficult once precedent is understood as binding. Resistance (0.35) is moderate — the constraint meets resistance from reform movements but the institution absorbs and channels it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (appellate judiciary) experiences the constraint as genuine coordination they maintain — from their seat, the extraordinary justification standard is quality control. The payer seats (reform litigants, marginalized communities) experience the same structure as enforced extraction — the standard is a barrier to justice. The beneficiary seats (legal establishment) experience it as valuable stability worth defending. The engine computes these divergences from the declared power/exit/role structure; the claim of 'tangled_rope' reflects the author's assessment that both coordination and extraction are structurally real and inseparable in this constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Appellate judiciary (agenda_setter, identity_locked): d near 0.1 — they administer and benefit from the constraint; their professional identity is fused to it. Institutional legal establishment (beneficiary, constrained): d ~ 0.2 — collect rents from stability; exit requires career restructuring. Precedent-dependent practitioners (beneficiary, constrained): d ~ 0.25 — similar but less concentrated benefit. Reform-seeking litigants (payer, trapped): d ~ 0.9 — bear full cost of the extraordinary justification standard with no exit. Marginalized communities (payer, identity_locked): d ~ 0.85 — bound by precedent that entrenched their exclusion; identity lock from citizenship/subjection relationship. Novel claim anticipators (payer, constrained): d ~ 0.7 — must work within the system; constrained exit to other forums. Legal realist scholars (observer, analytical): d = 0.5 — analytical seat. Constitutional textualists (excluded, constrained): d ~ 0.6 — excluded from the system's internal logic but constrained by its output.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (statutory vacuum in early common law) is substantially resolved in modern systems with comprehensive legislation, yet the constraint has not sunset — it has intensified. This is classic mandatrophy: the coordination function that justified the arrangement has atrophied relative to the extraction function, but the constraint persists because the beneficiaries (judiciary, legal establishment) control the reform levers. The constraint is NOT a pure snare because the coordination function (predictability, reliance) remains real and valued by many parties — hence tangled_rope. The coordination function is the cover that makes the extraction politically sustainable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precedent_natural_law_vs_constructed,
    'Is the binding force of precedent a genuine feature of legal reasoning (like logic) or a constructed institutional choice that benefits identifiable agents?',
    'Comparative analysis of legal systems without formal stare decisis (civil law) — if they achieve comparable coordination with less extraction, the common law''s rigidity is constructed. Historical analysis of when/why the overruling threshold hardened.',
    'If constructed, the constraint is a false summit candidate (mountain claim with beneficiaries) — the FSM signature would reclassify as tangled_rope. If genuine natural law of legal systems, the high extraction/suppression metrics reflect a real coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_natural_law_vs_constructed, conceptual, 'Whether stare decisis rigidity is a natural law of legal order or a judicial construction').

omega_variable(
    extraction_coordination_separability,
    'Can the coordination function (predictability, reliance) be preserved while reducing the extraction function (extraordinary justification, identity lock, exclusion)?',
    'Natural experiment: jurisdictions that have relaxed stare decisis (e.g., UK Practice Statement 1966, Canadian ''living tree'' doctrine) — measure whether legal stability collapsed or adapted. Empirical study of overruling frequency vs. commercial/constitutional stability.',
    'If separable, the current extraction level is not necessary for coordination — the constraint is extractive beyond functional requirement. If inseparable, the high extraction is the price of the coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_coordination_separability, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the strict_stare_decisis reading logically foreclose the evolutionary_framework reading within a single legal framework, or do they coexist as competing interpretive positions?',
    'Analyze whether a court can simultaneously hold that precedent binds as backward constraint AND that contemporary normative evolution permits reinterpretation — or whether accepting one premise commits the court to rejecting the other''s core premise.',
    'If forecloses, the kernel has a genuine logical fracture — the readings cannot coexist in one framework. If coexists_with, the kernel hosts a persistent interpretive dispute. If influences, strict stare decisis creates structural pressure on evolutionary readings without eliminating them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Structural relationship between this reading and its sibling readings in the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 1800, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clpc_ssd_tr_t1800, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1800, 0.12).
narrative_ontology:measurement(clpc_ssd_tr_t1850, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(clpc_ssd_tr_t1900, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(clpc_ssd_tr_t1950, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1950, 0.22).
narrative_ontology:measurement(clpc_ssd_tr_t1975, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(clpc_ssd_tr_t2000, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(clpc_ssd_tr_t2025, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(clpc_ssd_be_t1800, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1800, 0.25).
narrative_ontology:measurement(clpc_ssd_be_t1850, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1850, 0.35).
narrative_ontology:measurement(clpc_ssd_be_t1900, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1900, 0.45).
narrative_ontology:measurement(clpc_ssd_be_t1950, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1950, 0.58).
narrative_ontology:measurement(clpc_ssd_be_t1975, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1975, 0.62).
narrative_ontology:measurement(clpc_ssd_be_t2000, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(clpc_ssd_be_t2025, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clpc_ssd_su_t1800, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement(clpc_ssd_su_t1850, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1850, 0.45).
narrative_ontology:measurement(clpc_ssd_su_t1900, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1900, 0.52).
narrative_ontology:measurement(clpc_ssd_su_t1950, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(clpc_ssd_su_t1975, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(clpc_ssd_su_t2000, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(clpc_ssd_su_t2025, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, identity_coordination).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__strict_stare_decisis, 0.08).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the common_law_precedent_corpus kernel. The evolutionary_framework reading (lower ε, different beneficiary/victim structure) and pluralist_balancing reading (intermediate ε) are separate constraint stories. The ε values differ substantially: strict_stare_decisis ε=0.68 (substantial extraction), evolutionary_framework ε≈0.35 (coordination-dominant), pluralist_balancing ε≈0.5 (mixed). They have different failure modes, different stakeholder coalitions, and different empirical status. The kernel label 'common law precedent' conflates them; the framework disambiguates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__strict_stare_decisis, institutional, 0.1).
constraint_indexing:directionality_override(common_law_precedent_corpus__strict_stare_decisis, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
