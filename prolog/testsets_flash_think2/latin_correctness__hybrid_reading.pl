% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__hybrid_reading, []).

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
 *   constraint_id: latin_correctness__hybrid_reading
 *   human_readable: Bifurcated Latin Normativity (Hybrid Reading)
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid reading' of Latin correctness,
 *   prevalent from the Renaissance through the 19th century. It posits that
 *   classical Latin norms apply strictly to literary and rhetorical domains,
 *   while medieval forms retain legitimacy for technical and practical
 *   writing. This framework attempts to reconcile the humanist ideal of
 *   classical purity with the historical reality of Latin's evolution, but in
 *   doing so, it creates a status hierarchy that extracts conformity from
 *   technical writers and devalues medieval scholarship.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.6).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.5).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Bifurcated Latin Normativity (Hybrid Reading)").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "historical_linguistics/intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, 'b92a872f-e309-4782-9fa0-f5a4280e1a4e').
narrative_ontology:cs_kernel_codification('b92a872f-e309-4782-9fa0-f5a4280e1a4e', fixed_text).
narrative_ontology:cs_authority_grounding('b92a872f-e309-4782-9fa0-f5a4280e1a4e', lineage).
narrative_ontology:cs_interpretation_layer_present('b92a872f-e309-4782-9fa0-f5a4280e1a4e').
narrative_ontology:cs_reading_relation('b92a872f-e309-4782-9fa0-f5a4280e1a4e', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b92a872f-e309-4782-9fa0-f5a4280e1a4e', latin_correctness__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('b92a872f-e309-4782-9fa0-f5a4280e1a4e', foundational, domain_specific_normativity).
narrative_ontology:cs_axiom_status(domain_specific_normativity, holdable).
narrative_ontology:cs_axiom_grounding('b92a872f-e309-4782-9fa0-f5a4280e1a4e', domain_specific_normativity, conventional).
narrative_ontology:cs_axiom('b92a872f-e309-4782-9fa0-f5a4280e1a4e', secondary, classical_literary_supremacy).
narrative_ontology:cs_axiom_status(classical_literary_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b92a872f-e309-4782-9fa0-f5a4280e1a4e', classical_literary_supremacy, conventional).
narrative_ontology:cs_reference_frame('b92a872f-e309-4782-9fa0-f5a4280e1a4e', renaissance_humanist_ideal).
narrative_ontology:cs_drift_state('b92a872f-e309-4782-9fa0-f5a4280e1a4e', contemporary_linguistic_science, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('b92a872f-e309-4782-9fa0-f5a4280e1a4e', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, literary_latin_scholars).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, technical_latin_writers).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, medieval_latin_scholars).
narrative_ontology:constraint_vindicates(latin_correctness__hybrid_reading, classical_latin_purity_doctrine).
narrative_ontology:constraint_vindicates(latin_correctness__hybrid_reading, domain_specific_linguistic_registers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, interpret, and enforce the classical norms for Latin, particularly in literary and rhetorical contexts. They benefit from the elevated status and perceived purity of classical Latin, which reinforces their academic authority.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, classical_philologists, agenda_setter,
    institutional, generational, analytical, universal).

% Benefit from the prestige and established standards of classical Latin in their literary analyses and compositions. They apply classical norms, finding clarity and a shared framework, but are constrained by these very norms in their creative or interpretive freedom.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, literary_latin_scholars, beneficiary,
    organized, biographical, constrained, global).

% Write in Latin for scientific, legal, or philosophical domains. While medieval forms might be more practical or historically accurate for their subject, they face pressure to conform to classical standards for academic legitimacy, incurring extra effort and potential status penalties if they deviate.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, technical_latin_writers, payer,
    moderate, biographical, constrained, global).

% Advocate for the historical continuity and legitimacy of medieval Latin as a living language. They bear the cost of a hierarchical system that often devalues medieval usage, especially when it deviates from classical prescriptive norms, facing an uphill battle for recognition.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, medieval_latin_scholars, payer,
    organized, biographical, constrained, global).

% Analyze the historical evolution of Latin, including its medieval forms, and the social construction of its norms. They observe the effects of this bifurcated normativity without directly participating in its enforcement or extraction.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, universal).

% Act as gatekeepers, enforcing stylistic and grammatical norms in academic publications. They uphold the bifurcated standards, influencing what is considered acceptable Latin in different scholarly domains.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, academic_publishers, agenda_setter,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for evaluating Latin usage by domain, allowing for both classical purity in literary contexts and functional flexibility in technical ones, thereby coordinating academic expectations and maintaining a perceived standard of correctness.
% TRANSFER_FUNCTION: Transfers prestige, academic legitimacy, and institutional resources towards classical philology and literary Latin studies, while imposing additional effort, conformity pressure, and potential status penalties on technical writers and medieval Latin scholars.
% ABSENT_VOICES: Scholars advocating for a unified, historically continuous view of Latin, or those who reject prescriptive grammar entirely in favor of descriptive linguistics, are often marginalized or excluded from the core debates on Latin correctness within this framework.
% DISAPPEARANCE_RATIONALE: If this bifurcated normativity vanished overnight, academic publishing standards for Latin would collapse, leading to a chaotic period of re-evaluation of what constitutes 'correct' Latin. This would likely result in a more descriptive approach to historical usage, a re-evaluation of medieval forms, and a significant shift in philological priorities.
% FOUNDING_PROBLEM: The tension between the desire to preserve the perceived purity and authority of classical Latin as a fixed standard, and the practical need for a living, evolving language to serve new technical and philosophical domains during the medieval and early modern periods.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguistic analyses, philological debates, and the ongoing challenges faced by scholars of post-classical Latin attest to this persistent tension. Scholars outside the classical philology establishment often highlight the practical evolution of the language, corroborating the problem's continued relevance.
narrative_ontology:disappearance_verdict(latin_correctness__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(latin_correctness__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__hybrid_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it performs a genuine coordination function (providing a framework for Latin usage) but simultaneously involves asymmetric extraction. The 'classical norms for literary' aspect coordinates by setting a high standard, while the 'medieval forms for technical' aspect acknowledges practical usage. However, the 'literary > technical' hierarchy imposes costs on those in the technical domain, who are pressured to adhere to often impractical classical standards for prestige. Active enforcement comes from academic gatekeeping, editorial boards, and philological criticism. Extractiveness is moderate, reflecting the burden on technical writers and medieval scholars. Suppression is moderate due to academic and publishing pressures. Theater ratio is low, as the distinction is generally genuinely applied.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of classical philologists, this constraint is a reasonable and necessary coordination mechanism to preserve the purity of Latin. From the perspective of technical writers or medieval scholars, it is an extractive system that imposes arbitrary standards and devalues their work. The engine's classification will highlight this divergence by computing different effective extraction values for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists and literary Latin scholars are beneficiaries, as their domain is elevated and their expertise validated. Technical Latin writers and medieval Latin scholars are payers, as they face pressure to conform to classical norms or have their work devalued. Linguistic historians and academic publishers act as observers and agenda-setters, respectively, shaping the discourse and enforcing the norms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_ambiguity,
    'Is the distinction between ''literary/rhetorical'' and ''technical/practical'' domains for Latin usage clear-cut, or is it a contested and fluid boundary?',
    'Analysis of historical texts and contemporary academic practice to identify instances where the domain classification is ambiguous or disputed, and how such disputes are resolved.',
    'If the boundary is fluid, the constraint''s application becomes more arbitrary, increasing extractiveness for those caught in the ambiguity. If clear, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_ambiguity, conceptual, 'Ambiguity in the application of domain-specific Latin norms.').

omega_variable(
    classical_revival_influence,
    'To what extent is the insistence on classical norms for literary Latin a genuine coordination function, versus a status-driven imposition from the classical revival movement?',
    'Historical sociological analysis of philological institutions and academic power structures during the Renaissance and subsequent periods, examining the motivations behind prescriptive grammars.',
    'If primarily status-driven, the extractiveness of the constraint is higher than its stated coordination function suggests, indicating a stronger Snare component. If genuinely functional, the Rope aspect is more prominent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classical_revival_influence, empirical, 'Distinguishing coordination from status-driven imposition in classical Latin norms.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''hybrid_reading'' of the ''latin_correctness'' kernel. How would adopting a sibling reading alter its structural properties?',
    'Comparative analysis with the ''continuity_reading'' (which would likely lower extractiveness and suppression by validating medieval forms more broadly) and the ''rupture_reading'' (which would likely increase extractiveness and suppression by rejecting all non-classical forms as corrupt).',
    'Each sibling reading would instantiate a structurally distinct constraint with different extractiveness, suppression, and beneficiary/victim sets, leading to different classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of alternative readings of the Latin correctness kernel on constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 1500, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t1500, latin_correctness__hybrid_reading, theater_ratio, 1500, 0.15).
narrative_ontology:measurement(lati_tr_t1575, latin_correctness__hybrid_reading, theater_ratio, 1575, 0.18).
narrative_ontology:measurement(lati_tr_t1650, latin_correctness__hybrid_reading, theater_ratio, 1650, 0.2).
narrative_ontology:measurement(lati_tr_t1725, latin_correctness__hybrid_reading, theater_ratio, 1725, 0.22).
narrative_ontology:measurement(lati_tr_t1800, latin_correctness__hybrid_reading, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(lati_tr_t1900, latin_correctness__hybrid_reading, theater_ratio, 1900, 0.18).

% Extraction over time
narrative_ontology:measurement(lati_be_t1500, latin_correctness__hybrid_reading, base_extractiveness, 1500, 0.45).
narrative_ontology:measurement(lati_be_t1575, latin_correctness__hybrid_reading, base_extractiveness, 1575, 0.5).
narrative_ontology:measurement(lati_be_t1650, latin_correctness__hybrid_reading, base_extractiveness, 1650, 0.55).
narrative_ontology:measurement(lati_be_t1725, latin_correctness__hybrid_reading, base_extractiveness, 1725, 0.6).
narrative_ontology:measurement(lati_be_t1800, latin_correctness__hybrid_reading, base_extractiveness, 1800, 0.65).
narrative_ontology:measurement(lati_be_t1900, latin_correctness__hybrid_reading, base_extractiveness, 1900, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t1500, latin_correctness__hybrid_reading, suppression_requirement, 1500, 0.35).
narrative_ontology:measurement(lati_su_t1575, latin_correctness__hybrid_reading, suppression_requirement, 1575, 0.4).
narrative_ontology:measurement(lati_su_t1650, latin_correctness__hybrid_reading, suppression_requirement, 1650, 0.45).
narrative_ontology:measurement(lati_su_t1725, latin_correctness__hybrid_reading, suppression_requirement, 1725, 0.5).
narrative_ontology:measurement(lati_su_t1800, latin_correctness__hybrid_reading, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement(lati_su_t1900, latin_correctness__hybrid_reading, suppression_requirement, 1900, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, academic_publishing_standards).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, humanist_education_curriculum).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'latin_correctness' kernel, alongside 'continuity_reading' and 'rupture_reading'. Each represents a distinct structural claim about Latin normativity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
