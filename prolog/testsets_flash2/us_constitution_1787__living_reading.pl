% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: US Constitution (Living Reading)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint story describes the 'living Constitution' reading of the
 *   US Constitution, where its meaning evolves with societal values and
 *   needs, serving as an aspirational framework rather than a fixed text.
 *   This reading is one of several competing interpretations of the same
 *   foundational document (the 'us_constitution_1787' kernel). The metrics
 *   reflect the moderate extractiveness and suppression inherent in a system
 *   where judicial interpretation can shift established norms, benefiting
 *   some groups while imposing costs on others who prefer a more static or
 *   text-bound understanding. The claimed type is 'rope' because it aims for
 *   broad coordination and adaptation, despite the contestation.
 *
 * KEY AGENTS:
 *   - judicial_activists: Primary agenda-setter (institutional/constrained) — interprets and expands constitutional meaning.
 *   - marginalized_groups: Primary beneficiary (powerless/constrained) — gains rights and protections from evolving interpretations.
 *   - conservative_legal_scholars: Primary payer (organized/constrained) — bears the cost of interpretations that deviate from original intent.
 *   - states_rights_advocates: Secondary payer (organized/constrained) — bears the cost of federal judicial mandates.
 *   - general_public: Beneficiary/Payer (moderate/constrained) — benefits from adaptability, bears costs of uncertainty/unpopular rulings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.45).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.3).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "US Constitution (Living Reading)").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, '9c60b6e3-dbcc-4e2a-b84d-2c1f0e4ea2e8').
narrative_ontology:cs_kernel_codification('9c60b6e3-dbcc-4e2a-b84d-2c1f0e4ea2e8', fixed_text).
narrative_ontology:cs_authority_grounding('9c60b6e3-dbcc-4e2a-b84d-2c1f0e4ea2e8', lineage).
narrative_ontology:cs_interpretation_layer_present('9c60b6e3-dbcc-4e2a-b84d-2c1f0e4ea2e8').
narrative_ontology:cs_reading_relation('9c60b6e3-dbcc-4e2a-b84d-2c1f0e4ea2e8', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c60b6e3-dbcc-4e2a-b84d-2c1f0e4ea2e8', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('9c60b6e3-dbcc-4e2a-b84d-2c1f0e4ea2e8', foundational, constitutional_meaning_evolves).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves, holdable).
narrative_ontology:cs_axiom_grounding('9c60b6e3-dbcc-4e2a-b84d-2c1f0e4ea2e8', constitutional_meaning_evolves, deontological).
narrative_ontology:cs_axiom('9c60b6e3-dbcc-4e2a-b84d-2c1f0e4ea2e8', foundational, constitution_as_aspirational_framework).
narrative_ontology:cs_axiom_status(constitution_as_aspirational_framework, holdable).
narrative_ontology:cs_axiom_grounding('9c60b6e3-dbcc-4e2a-b84d-2c1f0e4ea2e8', constitution_as_aspirational_framework, conventional).
narrative_ontology:cs_reference_frame('9c60b6e3-dbcc-4e2a-b84d-2c1f0e4ea2e8', aspirational_framework_adaptability).
narrative_ontology:cs_drift_state('9c60b6e3-dbcc-4e2a-b84d-2c1f0e4ea2e8', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9c60b6e3-dbcc-4e2a-b84d-2c1f0e4ea2e8', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, judicial_activists).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, marginalized_groups).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, conservative_legal_scholars).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, states_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, general_public).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, general_public).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, evolving_standards_of_decency).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, substantive_due_process).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges who interpret the Constitution as a living document, adapting its meaning to contemporary societal values and needs. They expand rights and protections beyond the explicit text, often leading social change.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, judicial_activists, agenda_setter,
    institutional, generational, constrained, national).

% Groups whose rights and protections have been expanded by living constitutional interpretations (e.g., LGBTQ+ rights, privacy rights). They benefit from judicial interpretations that address contemporary injustices not explicitly covered by the original text.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, marginalized_groups, beneficiary,
    powerless, biographical, constrained, national).

% Academics and legal practitioners who argue against the living Constitution, viewing it as an illegitimate exercise of judicial power that undermines democratic processes and the fixed meaning of the text. They bear the cost of seeing their preferred interpretations overridden.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, conservative_legal_scholars, payer,
    organized, generational, constrained, national).

% Advocates who see living constitutionalism as eroding state sovereignty by imposing national standards through judicial fiat. They bear the cost of federal judicial mandates that limit state legislative power.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, states_rights_advocates, payer,
    organized, generational, constrained, national).

% Benefits from a Constitution that can adapt to modern challenges without formal amendment, but also bears the cost of judicial decisions that may not align with popular will or create legal uncertainty.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, general_public, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows the Constitution to remain relevant and effective in a changing society by adapting its principles to new social, technological, and moral realities, thereby avoiding obsolescence or the need for frequent, difficult amendments.
% TRANSFER_FUNCTION: Transfers interpretive authority from the original framers' intent or strict textualism to contemporary judicial and societal understandings, leading to shifts in rights, powers, and legal obligations.
% ABSENT_VOICES: Future generations who might prefer a more fixed or different interpretive framework are not present to object to the ongoing evolution of meaning. The 'dead hand of the past' argument is often invoked by originalists, but the living reading prioritizes the present.
% DISAPPEARANCE_RATIONALE: If the living reading vanished, the US Constitution would revert to a more rigid, potentially anachronistic document. Many established rights (e.g., privacy, certain aspects of equality) would lose their judicial grounding, leading to significant legal and social upheaval as society grappled with a static foundational text.
% FOUNDING_PROBLEM: The problem of governing a dynamic society with a static foundational document, anticipating that future challenges and moral understandings would outpace the specific intentions of the framers.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, social scientists, and civil rights organizations outside the immediate judicial activist community corroborate that the problem of constitutional adaptability remains live, citing ongoing societal changes and the difficulty of formal amendment. They argue that a purely static interpretation would lead to an unworkable or unjust system.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__living_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).
:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while this reading allows for the expansion of rights and adaptation, it also involves judicial discretion that can be perceived as overreach by those who prefer a more constrained interpretation. Suppression (0.30) is present in that alternative interpretive frameworks (originalism, positivism) are actively challenged and often overridden in judicial practice, though they remain live in academic and political discourse. The theater ratio is low (0.10) as the interpretive function is genuine, not merely performative, though it is often framed in terms of 'discovering' rather than 'creating' meaning. The temporal measurements show a relatively stable, though slightly fluctuating, level of extractiveness and suppression, reflecting ongoing contestation rather than a clear trend of increasing or decreasing judicial power.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of judicial activists and marginalized groups, the living reading is a necessary and beneficial mechanism for justice and societal progress, computing as a Rope or even a Scaffold (for transitional justice). From the perspective of conservative legal scholars and states' rights advocates, it is a Snare or Tangled Rope, extracting power from democratic processes and fixed principles. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Judicial activists are beneficiaries (d near 0.0) as they gain the power to shape law and society. Marginalized groups are also beneficiaries (d near 0.0) as the constraint expands their rights. Conservative legal scholars and states' rights advocates are targets (d near 1.0) as their preferred interpretations and power bases are challenged. The general public is more symmetric (d near 0.5), experiencing both benefits of adaptability and costs of judicial activism.
 *
 * MANDATROPHY ANALYSIS:
 *   The living reading prevents mandatrophy by ensuring the Constitution's mandate remains relevant to contemporary society, avoiding the obsolescence that a purely static interpretation might entail. However, critics argue it risks a different form of mandatrophy, where the original mandate of limited government and fixed principles is eroded by judicial overreach. The classification as a Rope acknowledges its coordination function while the moderate extractiveness and suppression reflect the ongoing contestation over its legitimate scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_capture_of_evolving_norms,
    'Does the ''living'' aspect of the Constitution primarily reflect genuine societal evolution, or is it susceptible to elite capture by judicial or academic factions, imposing their preferred norms?',
    'Empirical analysis of judicial decisions'' correlation with public opinion vs. elite legal consensus over time, particularly in controversial areas. Longitudinal studies of the social background and ideological leanings of judges and their impact on constitutional interpretation.',
    'If elite capture is dominant, the constraint''s effective extractiveness and suppression would be higher for the general public and those outside the elite consensus, potentially reclassifying it closer to a Tangled Rope or Snare for those seats. If genuine societal evolution is primary, its Rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_of_evolving_norms, empirical, 'Assesses whether ''evolving norms'' are genuinely societal or driven by specific elite groups.').

omega_variable(
    legitimacy_of_judicial_supremacy,
    'Is judicial interpretation of the Constitution, particularly in its ''living'' form, a legitimate exercise of authority in a democratic system, or does it usurp legislative power?',
    'Conceptual analysis of democratic theory and the role of unelected judiciary, alongside comparative legal studies of constitutional review mechanisms in other democracies. This is a foundational question of political philosophy.',
    'If deemed illegitimate, the constraint''s suppression and extractiveness would be re-evaluated as higher for the democratic process and the legislative branch, potentially shifting its classification towards a Snare from those perspectives. If legitimate, its coordination function is affirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_judicial_supremacy, conceptual, 'Examines the philosophical grounding of judicial review in a living constitution.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the ''us_constitution_1787'' kernel. What would a sibling reading (e.g., originalist_reading) change structurally?',
    'Compare the declared axioms and structural relationships of this ''living_reading'' with those of the ''originalist_reading'' and ''positivist_reading'' constraints. The differences in declared beneficiaries, victims, and core axioms define the structural delta.',
    'The ''originalist_reading'' would likely have lower extractiveness for states'' rights advocates and higher for marginalized groups, with a stronger emphasis on fixed textual meaning. The ''positivist_reading'' would emphasize democratic amendment over judicial interpretation. Each reading instantiates a distinct constraint with different classifications for various seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Documents the structural differences between this ''living_reading'' and its sibling interpretations of the US Constitution kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__living_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__living_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__living_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_1787__living_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__living_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_1787__living_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__living_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__living_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__living_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(us_c_be_t30, us_constitution_1787__living_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__living_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(us_c_be_t50, us_constitution_1787__living_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__living_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(us_c_su_t10, us_constitution_1787__living_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(us_c_su_t20, us_constitution_1787__living_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(us_c_su_t30, us_constitution_1787__living_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(us_c_su_t40, us_constitution_1787__living_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement(us_c_su_t50, us_constitution_1787__living_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, federal_judicial_appointments).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the US Constitution (us_constitution_1787 kernel). Its sibling constraints are 'us_constitution_1787__originalist_reading' and 'us_constitution_1787__positivist_reading'. Each reading has a distinct ε and stakeholder structure, reflecting different interpretations of the same foundational text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
