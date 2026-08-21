% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Text: Popular Sovereignty Reading
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint represents the 'popular sovereignty' reading of
 *   constitutional text, where the ultimate authority for constitutional
 *   interpretation resides with the people (the demos), not with courts or
 *   legislatures. It emphasizes the constituent power of the people to amend,
 *   convene, or even revolutionize the constitutional order. This reading is
 *   distinct from those asserting judicial or legislative supremacy, which
 *   are modeled as sibling constraints. The metrics reflect a relatively low
 *   extractiveness and suppression, as this reading primarily legitimizes
 *   popular action rather than coercing it, though it extracts from
 *   institutional stability.
 *
 * KEY AGENTS:
 *   - the_demos: Primary beneficiary (organized/mobile) — retains ultimate interpretive authority.
 *   - democratic_participation: Beneficiary (moderate/mobile) — legitimizes popular action.
 *   - institutional_stability: Primary payer (institutional/constrained) — bears costs from potential disruption.
 *   - judicial_expertise: Payer (institutional/constrained) — interpretive supremacy is denied.
 *   - legislative_efficiency: Payer (institutional/constrained) — authority is subordinated.
 *   - courts: Excluded (institutional/identity_locked) — claims to supremacy are rejected.
 *   - legislature: Excluded (institutional/identity_locked) — claims to finality are rejected.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.3).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.2).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Text: Popular Sovereignty Reading").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, '339aaa07-510f-451b-a6ae-f984dc39b21b').
narrative_ontology:cs_kernel_codification('339aaa07-510f-451b-a6ae-f984dc39b21b', fixed_text).
narrative_ontology:cs_authority_grounding('339aaa07-510f-451b-a6ae-f984dc39b21b', lineage).
narrative_ontology:cs_interpretation_layer_present('339aaa07-510f-451b-a6ae-f984dc39b21b').
narrative_ontology:cs_reading_relation('339aaa07-510f-451b-a6ae-f984dc39b21b', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('339aaa07-510f-451b-a6ae-f984dc39b21b', constitutional_text__legislative_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('339aaa07-510f-451b-a6ae-f984dc39b21b', foundational, constituent_power_supremacy).
narrative_ontology:cs_axiom_status(constituent_power_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('339aaa07-510f-451b-a6ae-f984dc39b21b', constituent_power_supremacy, deontological).
narrative_ontology:cs_axiom('339aaa07-510f-451b-a6ae-f984dc39b21b', secondary, institutional_subordination_to_demos).
narrative_ontology:cs_axiom_status(institutional_subordination_to_demos, holdable).
narrative_ontology:cs_axiom_grounding('339aaa07-510f-451b-a6ae-f984dc39b21b', institutional_subordination_to_demos, conventional).
narrative_ontology:cs_reference_frame('339aaa07-510f-451b-a6ae-f984dc39b21b', founding_constitutional_moment).
narrative_ontology:cs_drift_state('339aaa07-510f-451b-a6ae-f984dc39b21b', contemporary_constitutional_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('339aaa07-510f-451b-a6ae-f984dc39b21b', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, the_demos).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, democratic_participation).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_stability).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, judicial_expertise).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislative_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate source of constitutional authority, retaining interpretive power through direct action (amendment, convention, revolution). Benefits from the ability to shape fundamental law.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, the_demos, beneficiary,
    organized, generational, mobile, national).

% The active engagement of citizens in shaping constitutional meaning. Benefits from the popular sovereignty reading by legitimizing extra-institutional political action.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, democratic_participation, beneficiary,
    moderate, biographical, mobile, local).

% The predictability and continuity of governmental structures. Bears costs from the popular sovereignty reading due to the potential for disruption by extra-institutional action.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, institutional_stability, payer,
    institutional, generational, constrained, national).

% The specialized knowledge and interpretive authority of the judiciary. Bears costs as its interpretive supremacy is denied, and its decisions are subject to popular override.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, judicial_expertise, payer,
    institutional, generational, constrained, national).

% The ability of the legislature to enact and maintain laws without constant threat of popular constitutional revision. Bears costs as its authority is subordinated to the demos.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislative_efficiency, payer,
    institutional, biographical, constrained, national).

% Under this reading, courts are subordinate interpreters, not final arbiters. They would argue for their role in protecting minority rights and ensuring legal consistency, but their claims to supremacy are denied.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, courts, excluded,
    institutional, generational, identity_locked, national).

% Under this reading, the legislature is also subordinate to the people's ultimate authority. They would argue for their democratic mandate and representative function, but their claims to finality are rejected.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislature, excluded,
    institutional, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ultimate source of constitutional legitimacy, ensuring that fundamental law remains responsive to the will of the people and preventing institutional capture of interpretive authority.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority from institutional actors (courts, legislature) to the constituent power of the people, legitimizing popular mobilization as a means of constitutional change.
% ABSENT_VOICES: Those who advocate for judicial or legislative supremacy would object, arguing that popular constitutionalism leads to instability, majoritarian tyranny, or undermines the rule of law. They are 'absent' in the sense that their claims to final authority are explicitly rejected by this reading.
% DISAPPEARANCE_RATIONALE: If the popular sovereignty reading vanished, the constitutional landscape would fundamentally shift. Either judicial or legislative supremacy would likely fill the void, altering the balance of power, the avenues for constitutional change, and the perceived legitimacy of popular political action.
% FOUNDING_PROBLEM: The problem of ensuring that constitutional text remains a living document responsive to the people, rather than becoming an ossified instrument of elite control or judicial fiat.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists, historians of constitutional conventions, and social movements attest to the ongoing struggle for popular control over fundamental law, citing historical examples of constitutional moments and contemporary debates over judicial review and legislative power. This corroboration comes from outside the direct beneficiaries of this reading.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).
:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.3) because this reading primarily empowers rather than extracts, though it does 'extract' from the stability and finality of institutional interpretations. Suppression is low (0.2) as it does not rely on active coercion to maintain itself, but rather on the inherent legitimacy of popular will. Theater ratio is low (0.1) because the claims are direct and functional, not performative. Resistance is high (0.7) because this reading is often in active contestation with institutional claims to supremacy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the demos, this is a pure rope, enabling collective action and self-governance. From the perspective of institutional actors (courts, legislature), it appears as a force that undermines their authority and introduces instability, thus extracting from their established roles. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The demos and democratic participation are clear beneficiaries (d near 0.0) as this reading legitimizes their ultimate authority. Institutional stability, judicial expertise, and legislative efficiency are targets (d near 1.0) because their claims to finality and their operational predictability are challenged by this reading. Courts and legislatures are 'excluded' in the sense that their claims to supremacy are denied, making them targets of the constraint's re-allocation of authority.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is inherently resistant to mandatrophy because its mandate is the ongoing, living will of the people. Its function is to prevent the ossification of constitutional meaning and the capture of interpretive authority by institutions. If it were to become a piton, it would imply that the idea of popular sovereignty itself had become a mere performance, with no real power to shape constitutional outcomes, which would fundamentally contradict its core premise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    popular_sovereignty_vs_institutional_stability,
    'Is the exercise of popular constitutional authority inherently destabilizing to institutional governance, or can it be integrated into a stable constitutional order?',
    'Comparative analysis of constitutional systems that incorporate elements of popular sovereignty (e.g., referenda, constitutional conventions) and their long-term stability metrics.',
    'If inherently destabilizing, the ''extraction'' from institutional stability is a necessary cost of this reading. If integrable, the extraction is a remediable design flaw, suggesting a more ''rope-like'' potential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_sovereignty_vs_institutional_stability, empirical, 'The trade-off between popular constitutionalism and institutional stability.').

omega_variable(
    popular_sovereignty_vs_minority_rights,
    'Does the popular sovereignty reading adequately protect minority rights against potential majoritarian overreach, or does it inherently risk ''tyranny of the majority''?',
    'Conceptual analysis of the mechanisms within popular sovereignty theory (e.g., supermajority requirements for amendments, deliberative processes) intended to safeguard minorities, and empirical study of their effectiveness in practice.',
    'If minority rights are systematically vulnerable, the ''extraction'' from vulnerable groups is higher than currently measured, potentially shifting the classification towards a ''tangled_rope'' or ''snare'' for those groups. If robustly protected, the ''rope'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_sovereignty_vs_minority_rights, conceptual, 'The tension between popular sovereignty and minority protections.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint truly a distinct reading of the ''constitutional_text'' kernel, or merely a political preference within a broader ''judicial_supremacy'' or ''legislative_sovereignty'' framework?',
    'Analysis of the foundational axioms and their logical coherence. If the axioms of popular sovereignty are fundamentally incompatible with the core premises of judicial/legislative supremacy, it is a distinct reading. If they are merely subordinate claims, it is a preference.',
    'If a distinct reading, the current classification stands. If a mere preference, it would be reclassified as a ''snare'' or ''tangled_rope'' within the dominant framework, as its claims would be suppressed by the prevailing institutional authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinguishing a genuine kernel reading from a political preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__popular_sovereignty_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__popular_sovereignty_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__popular_sovereignty_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__popular_sovereignty_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__popular_sovereignty_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cons_be_t10, constitutional_text__popular_sovereignty_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(cons_be_t20, constitutional_text__popular_sovereignty_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(cons_be_t30, constitutional_text__popular_sovereignty_reading, base_extractiveness, 30, 0.29).
narrative_ontology:measurement(cons_be_t40, constitutional_text__popular_sovereignty_reading, base_extractiveness, 40, 0.27).
narrative_ontology:measurement(cons_be_t50, constitutional_text__popular_sovereignty_reading, base_extractiveness, 50, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(cons_su_t10, constitutional_text__popular_sovereignty_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(cons_su_t20, constitutional_text__popular_sovereignty_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(cons_su_t30, constitutional_text__popular_sovereignty_reading, suppression_requirement, 30, 0.19).
narrative_ontology:measurement(cons_su_t40, constitutional_text__popular_sovereignty_reading, suppression_requirement, 40, 0.17).
narrative_ontology:measurement(cons_su_t50, constitutional_text__popular_sovereignty_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'constitutional_text' kernel. This 'popular sovereignty' reading emphasizes the constituent power of the people, contrasting with judicial and legislative supremacy readings. Each reading has distinct beneficiaries, victims, and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
