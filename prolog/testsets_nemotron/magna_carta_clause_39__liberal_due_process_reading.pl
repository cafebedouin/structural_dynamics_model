% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__liberal_due_process_reading, []).

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
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Magna Carta Clause 39 — Liberal Due Process Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   Magna Carta Clause 39 ('No free man shall be seized or imprisoned...
 *   except by the lawful judgment of his peers or by the law of the land') is
 *   the kernel. This story instantiates the liberal due process reading: an
 *   expansive constraint that universalizes the clause's protection to all
 *   citizens and treats it as a structural limit on arbitrary executive
 *   power. The reading extracts heavily from unchecked authority (ε=0.65)
 *   while coordinating a polity-wide rule-of-law order. The constraint is
 *   claimed as tangled_rope — genuine coordination of collective
 *   accountability fused with asymmetric extraction from executive discretion
 *   — and requires active enforcement through courts and parliamentary
 *   oversight. Suppression is moderate (0.35) because the constraint operates
 *   through law, not force; alternatives (arbitrary power) are suppressed by
 *   institutionalizing legal process.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.65).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.35).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Magna Carta Clause 39 — Liberal Due Process Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, '89805306-0c85-497e-b9b8-70f32fdaa145').
narrative_ontology:cs_kernel_codification('89805306-0c85-497e-b9b8-70f32fdaa145', fixed_text).
narrative_ontology:cs_authority_grounding('89805306-0c85-497e-b9b8-70f32fdaa145', lineage).
narrative_ontology:cs_interpretation_layer_present('89805306-0c85-497e-b9b8-70f32fdaa145').
narrative_ontology:cs_reading_relation('89805306-0c85-497e-b9b8-70f32fdaa145', magna_carta_clause_39__feudal_prerogative_reading, forecloses).
narrative_ontology:cs_reading_relation('89805306-0c85-497e-b9b8-70f32fdaa145', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('89805306-0c85-497e-b9b8-70f32fdaa145', foundational, universal_individual_rights_against_state).
narrative_ontology:cs_axiom_status(universal_individual_rights_against_state, holdable).
narrative_ontology:cs_axiom_grounding('89805306-0c85-497e-b9b8-70f32fdaa145', universal_individual_rights_against_state, deontological).
narrative_ontology:cs_axiom('89805306-0c85-497e-b9b8-70f32fdaa145', foundational, due_process_as_structural_limit_on_executive).
narrative_ontology:cs_axiom_status(due_process_as_structural_limit_on_executive, holdable).
narrative_ontology:cs_axiom_grounding('89805306-0c85-497e-b9b8-70f32fdaa145', due_process_as_structural_limit_on_executive, deontological).
narrative_ontology:cs_reference_frame('89805306-0c85-497e-b9b8-70f32fdaa145', feudal_law_of_the_land_1215).
narrative_ontology:cs_drift_state('89805306-0c85-497e-b9b8-70f32fdaa145', contemporary_constitutional_order, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('89805306-0c85-497e-b9b8-70f32fdaa145', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, citizens_subject_to_state_power).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, parliamentary_institutions).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, common_law_courts).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, unchecked_executive_authority).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, arbitrary_prerogative_power).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, due_process_of_law).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, rule_of_law_supremacy).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, individual_rights_against_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain protection against arbitrary imprisonment, dispossession, and punishment without lawful judgment. Their exit from the constraint's protection is constrained by the fact that they cannot individually opt out of the state's jurisdiction; the right is structural, not contractual. They benefit from the constraint's limitation on executive power but bear no cost of maintaining it.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, citizens_subject_to_state_power, beneficiary,
    organized, generational, constrained, national).

% Derive legislative supremacy and oversight authority from the constraint's establishment that law binds the executive. They can leverage the constraint to expand parliamentary power against the crown. Exit is arbitrage-grade: they can invoke or ignore the constraint strategically across constitutional debates.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, parliamentary_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Administer and interpret the constraint through judicial review, habeas corpus, and procedural due process doctrines. They set the agenda for how the constraint operates in practice. Exit is mobile: courts can narrow or expand the reading through precedent, but institutional role binds them to engage with the constraint.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, common_law_courts, agenda_setter,
    institutional, generational, mobile, national).

% The constraint directly extracts from the executive's discretionary power to imprison, seize property, or punish without legal process. The executive is structurally trapped — it cannot exit the constraint without relinquishing the very powers the constraint limits. Every exercise of arbitrary power is checked; the extraction is the loss of unilateral authority.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, unchecked_executive_authority, payer,
    powerful, biographical, trapped, national).

% Represents the institutionalized capacity for rule-by-decree that the constraint targets. This power pays the full cost of the constraint: it loses its operational freedom. It is trapped because the constraint's logic — that no power is above law — directly negates its existence. There is no exit that preserves the prerogative.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, arbitrary_prerogative_power, payer,
    organized, generational, trapped, national).

% Analyze the constraint's historical development, doctrinal evolution, and comparative influence. They neither collect from nor pay into the constraint; their seat is the analytical observer that sees the full structural field across readings and centuries.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of limiting sovereign power: no single subject can constrain the executive alone; the constraint creates a shared legal framework that coordinates resistance to arbitrariness across the whole polity, making executive accountability a structural feature rather than a contingent negotiation.
% TRANSFER_FUNCTION: Transfers discretionary authority from the executive to legal process: the power to imprison, dispossess, or punish moves from unilateral executive will to lawful judgment by peers and established law. The extraction is the executive's loss of arbitrary power; the benefit is the subject's gain of procedural protection.
% ABSENT_VOICES: The feudal barons who originally extracted the clause as a class privilege — their narrow interest was the seed but not the mature reading. Colonized peoples and enslaved persons whose subjection the liberal reading historically excluded despite its universal language. Women and propertyless men denied standing in the very courts the constraint empowered. These voices are absent from the constraint's operational history even as the reading claims universality.
% DISAPPEARANCE_RATIONALE: If the liberal due process reading vanished overnight, the structural constraint on executive arbitrariness would collapse. Parliamentary oversight would lose its constitutional anchor; courts would lose the doctrinal foundation for judicial review of executive action; citizens would revert to petitioning for grace rather than claiming rights. The modern rule-of-law order would rearrange toward executive dominance.
% FOUNDING_PROBLEM: The arbitrary imprisonment, dispossession, and punishment of subjects by King John without judgment of peers or law of the land — a pattern of executive caprice that threatened the stability of the realm and the security of all propertied classes.
% FOUNDING_PROBLEM_CORROBORATION: The liberal reading's claim that the founding problem was universal arbitrary power (not just baronial privilege) is corroborated by the clause's textual breadth — 'no free man' — and by its immediate reissue and expansion in subsequent charters. Originalist scholars contest this, arguing the text protected only a narrow feudal class. Parliamentary history and the clause's invocation in 17th-century struggles against the Stuart kings corroborate the expansive reading; the founding problem's status remains contested between these traditions.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__liberal_due_process_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the constraint systematically strips the executive of discretionary power over life, liberty, and property — a substantial transfer of authority. Suppression is moderate (0.35) because the mechanism is legal/institutional, not coercive in the physical sense; the constraint suppresses arbitrary power by making it unlawful, not by making it impossible. Theater ratio is low-moderate (0.25) because the legal process function is genuine and operational, though symbolic invocations exist. Accessibility collapse (0.60) reflects that once the principle 'no punishment without law' is understood, alternatives (arbitrary rule) become cognitively and institutionally difficult to sustain. Resistance (0.40) reflects ongoing executive pushback (national security exceptions, emergency powers, signing statements) that tests but does not overturn the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the citizen/parliamentary seats, the constraint appears as a rope — genuine coordination securing liberty. From the executive/prerogative seats, it appears as a snare — pure extraction of sovereign discretion. The court seat sits in the tangled_rope zone: it both coordinates (administers justice) and extracts (limits executive power). The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the analytical observer's synthesis.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizens are structural beneficiaries (d ~ 0.15): they gain protection without bearing enforcement costs. Parliamentary institutions are beneficiaries with arbitrage exit (d ~ 0.10): they strategically deploy the constraint. Courts are agenda_setters with mobile exit (d ~ 0.45): they administer the constraint and shape its scope through precedent — near-symmetric because they both constrain and are constrained by it. Executive authority and prerogative power are payers with trapped exit (d ~ 0.90 and 0.95): the constraint's entire function is extracting their arbitrary power, and they cannot exit without ceasing to be what they are. Constitutional scholars are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary royal power) was live in 1215. By 1689 it was substantially resolved in England — but the constraint persisted and expanded because the coordination function (rule of law) proved valuable beyond the founding problem. The constraint avoided mandatrophy by migrating from a specific remedy to a general constitutional principle. The liberal reading claims the founding problem is 'contested' because executive arbitrariness recurs in new forms (emergency powers, administrative detention, surveillance); the arrangement persists not as a zombie but as a living coordination mechanism against a recurring threat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the liberal due process reading foreclose the feudal prerogative reading, or do they coexist as live positions in different frameworks?',
    'Doctrinal analysis: if a single legal framework can simultaneously treat Clause 39 as both a universal individual right and a narrow feudal privilege without contradiction, they coexist. If the universal reading logically entails the falsity of the feudal reading''s scope claim within any coherent framework, it forecloses.',
    'If forecloses, the kernel has a logical fracture — only one reading can be structurally true in any given constitutional order. If coexists_with, the kernel sustains productive ambiguity that different constitutional moments resolve differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between the liberal universalist reading and the feudal narrow reading of the same clause.').

omega_variable(
    universal_language_historical_exclusion,
    'How does the reading''s claim of universal rights (''no free man'') structurally relate to its historical exclusion of women, enslaved persons, colonized peoples, and the propertyless?',
    'Genealogical analysis: trace whether the exclusion is internal to the reading''s logic (the reading''s own categories produce the exclusion) or external (the reading''s logic is universal but historical power blocked its application). The former makes the reading structurally extractive toward excluded groups; the latter makes the exclusion a contingent failure of enforcement.',
    'If internal, the liberal reading itself operates as a snare toward excluded groups — its universality is the extraction mechanism. If external, the reading is a genuine rope whose universality was suppressed by power; the constraint''s current operation may still carry the structural trace.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_language_historical_exclusion, conceptual, 'Whether the liberal reading''s universal language structurally generates or merely historically failed to include marginalized groups.').

omega_variable(
    extraction_vs_coordination_boundary,
    'At what point does the constraint''s extraction from executive discretion exceed its coordination function, tipping it from tangled_rope toward snare?',
    'Comparative constitutional analysis: identify jurisdictions where due process protections so constrain executive action that governance capacity degrades (extraction > coordination) versus jurisdictions where the balance holds. Track the theater_ratio trajectory as executive power asserts national security exceptions.',
    'If extraction consistently exceeds coordination across contexts, the liberal reading''s claimed_type should be snare. If the balance holds, tangled_rope is structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'The empirical boundary between hybrid coordination/extraction and pure extraction for due process constraints on executive power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_tr_t1215, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1215, 0.4).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_tr_t1297, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1297, 0.35).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_tr_t1628, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1628, 0.3).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_tr_t1679, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1679, 0.25).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_tr_t1765, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1765, 0.28).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_tr_t1870, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1870, 0.26).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_tr_t1966, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1966, 0.25).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_tr_t2025, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_be_t1215, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1215, 0.3).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_be_t1297, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1297, 0.35).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_be_t1628, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1628, 0.45).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_be_t1679, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1679, 0.55).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_be_t1765, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1765, 0.58).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_be_t1870, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1870, 0.62).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_be_t1966, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1966, 0.64).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_be_t2025, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_su_t1215, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1215, 0.8).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_su_t1297, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1297, 0.5).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_su_t1628, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1628, 0.4).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_su_t1679, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1679, 0.35).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_su_t1765, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1765, 0.3).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_su_t1870, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1870, 0.3).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_su_t1966, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1966, 0.35).
narrative_ontology:measurement(magna_carta_clause_39__liberal_due_process_reading_su_t2025, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__liberal_due_process_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__liberal_due_process_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__originalist_limitation_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, habeas_corpus_act_1679).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, petition_of_right_1628).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, bill_of_rights_1689).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, fifth_amendment_due_process).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, fourteenth_amendment_due_process).

% DUAL FORMULATION NOTE:
% This reading, the feudal_prerogative_reading, and the originalist_limitation_reading form the magna_carta_clause_39 constraint family. All three share the kernel_id 'magna_carta_clause_39' but instantiate distinct constraints with different ε values, beneficiary/victim structures, and claimed_types. The liberal reading has the highest ε (0.65) because it universalizes the constraint's extraction from executive power; the feudal reading has the lowest ε (~0.15) because it limits extraction to baronial privileges; the originalist reading sits between (~0.30). The liberal reading influences both siblings by establishing the interpretive precedent that the clause's language bears universalist weight.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_clause_39__liberal_due_process_reading, institutional, 0.1).
constraint_indexing:directionality_override(magna_carta_clause_39__liberal_due_process_reading, powerful, 0.9).
constraint_indexing:directionality_override(magna_carta_clause_39__liberal_due_process_reading, organized, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
