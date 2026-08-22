% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: Refugee Convention — Procedural Integrity Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the procedural_integrity_reading of
 *   the refugee_convention_text kernel. The reading frames the 1951
 *   Convention and 1967 Protocol as a procedural safeguard requiring fair
 *   individualized assessment of protection claims — the protection threshold
 *   (who qualifies) is flexible and subject to state interpretation, but the
 *   integrity of the assessment process (non-refoulement, access to
 *   adjudication, reasoned decisions) is non-negotiable. Outcome is secondary
 *   to procedure: a negative decision after fair process is structurally
 *   different from denial of process. This reading coexists with two sibling
 *   readings: the expansive_humanitarian_reading (Convention as unbendable
 *   humanitarian mandate with broad protection) and the
 *   restrictive_sovereignty_reading (Convention as minimum floor with maximum
 *   sovereign discretion).
 *
 * KEY AGENTS:
 *   - asylum_seekers_with_procedural_access: Primary beneficiary (organized/constrained) — receives fair individualized assessment
 *   - asylum_seekers_denied_procedural_access: Primary victim (powerless/trapped) — excluded from process entirely
 *   - states_parties: Agenda setter/payer (institutional/biographical) — administers adjudication, bears costs, controls definitions
 *   - unhcr_monitoring_bodies: Beneficiary/observer (institutional/generational) — monitors compliance, provides guidance
 *   - domestic_asylum_adjudicators: Beneficiary (organized/biographical) — professional role constituted by the procedural framework
 *   - legal_aid_providers: Beneficiary (organized/biographical) — professional ecosystem dependent on procedural rights
 *   - excluded_migrant_categories: Victim (powerless/trapped) — categories narrowed by state definitions fall outside procedural reach
 *   - restrictive_sovereignty_advocates: Observer (institutional/analytical) — reads Convention as sovereign discretion floor
 *   - expansive_humanitarian_advocates: Observer (institutional/analytical) — reads Convention as humanitarian mandate ceiling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.38).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.28).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "Refugee Convention — Procedural Integrity Reading").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, '3fd9b7e6-248a-4f87-a1c2-99112ba213d9').
narrative_ontology:cs_kernel_codification('3fd9b7e6-248a-4f87-a1c2-99112ba213d9', formalized).
narrative_ontology:cs_authority_grounding('3fd9b7e6-248a-4f87-a1c2-99112ba213d9', lineage).
narrative_ontology:cs_interpretation_layer_present('3fd9b7e6-248a-4f87-a1c2-99112ba213d9').
narrative_ontology:cs_reading_relation('3fd9b7e6-248a-4f87-a1c2-99112ba213d9', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('3fd9b7e6-248a-4f87-a1c2-99112ba213d9', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('3fd9b7e6-248a-4f87-a1c2-99112ba213d9', foundational, procedural_integrity_as_nonnegotiable_floor).
narrative_ontology:cs_axiom_status(procedural_integrity_as_nonnegotiable_floor, holdable).
narrative_ontology:cs_axiom_grounding('3fd9b7e6-248a-4f87-a1c2-99112ba213d9', procedural_integrity_as_nonnegotiable_floor, conventional).
narrative_ontology:cs_axiom('3fd9b7e6-248a-4f87-a1c2-99112ba213d9', foundational, outcome_secondary_to_fair_process).
narrative_ontology:cs_axiom_status(outcome_secondary_to_fair_process, holdable).
narrative_ontology:cs_axiom_grounding('3fd9b7e6-248a-4f87-a1c2-99112ba213d9', outcome_secondary_to_fair_process, conventional).
narrative_ontology:cs_axiom('3fd9b7e6-248a-4f87-a1c2-99112ba213d9', secondary, state_definitional_discretion_within_procedural_bounds).
narrative_ontology:cs_axiom_status(state_definitional_discretion_within_procedural_bounds, holdable).
narrative_ontology:cs_axiom_grounding('3fd9b7e6-248a-4f87-a1c2-99112ba213d9', state_definitional_discretion_within_procedural_bounds, conventional).
narrative_ontology:cs_reference_frame('3fd9b7e6-248a-4f87-a1c2-99112ba213d9', postwar_procedural_minimum_consensus).
narrative_ontology:cs_drift_state('3fd9b7e6-248a-4f87-a1c2-99112ba213d9', contemporary_migration_governance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3fd9b7e6-248a-4f87-a1c2-99112ba213d9', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, asylum_seekers_with_procedural_access).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, unhcr_monitoring_bodies).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, domestic_asylum_adjudicators).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, legal_aid_providers).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_seekers_denied_procedural_access).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, states_bearing_adjudication_costs).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, excluded_migrant_categories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, states_parties).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, due_process_as_protection_floor).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, non_refoulement_as_procedural_guarantee).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, individualized_assessment_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who reach a state party's territory or effective jurisdiction and trigger the Convention's procedural guarantees: access to individualized status determination, non-refoulement protection during adjudication, reasoned decisions, and appeal. They gain substantive protection through fair process. Their exit is constrained — they need state recognition to regularize status, but can sometimes move to other jurisdictions or access complementary protection.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers_with_procedural_access, beneficiary,
    organized, biographical, constrained, global).

% Individuals intercepted, diverted, or detained before triggering procedural guarantees: pushbacks at borders, offshore processing without substantive review, safe third country transfers without individual assessment, carrier sanctions preventing embarkation. They bear the full cost of exclusion — return to persecution, indefinite detention, or onward dangerous movement — with no procedural remedy. Exit is trapped: they cannot access the process that would protect them, and return is dangerous.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers_denied_procedural_access, payer,
    powerless, immediate, trapped, global).

% Administer the asylum system: establish adjudication bodies, fund processing, define 'particular social group' and 'well-founded fear' within Convention margins, negotiate burden-sharing, implement offshore processing. They bear significant financial and administrative costs but gain orderly migration management, border control legitimacy, and international cooperation. Exit is constrained: withdrawal from Convention is legally possible but carries severe reputational and diplomatic costs; most states remain parties while testing definitional boundaries.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, states_parties, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, states_parties, payer).

% Monitor state compliance, issue guidance on interpretation, provide technical assistance, conduct protection monitoring. They gain institutional mandate and operational relevance from the Convention's procedural framework. Their exit is analytical: they observe and evaluate the constraint from outside the extraction/coordination dynamic, though their legitimacy depends on the framework's perceived effectiveness.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, unhcr_monitoring_bodies, beneficiary,
    institutional, generational, analytical, global).

% Judges, officers, and tribunal members who conduct individualized status determinations. Their professional role and expertise are constituted by the procedural framework — without fair process requirements, their function collapses to administrative rubber-stamping. They benefit from institutionalized procedures that legitimize their decisions. Exit is mobile: they can transfer to other adjudicative roles, but their specialized expertise is Convention-dependent.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, domestic_asylum_adjudicators, beneficiary,
    organized, biographical, mobile, national).

% NGOs, lawyers, and advocates who represent asylum seekers in proceedings. Their funding, professional specialization, and organizational missions depend on the existence of procedural rights to vindicate. They benefit from a functioning adversarial process. Exit is mobile: they can pivot to other human rights or immigration work, but the asylum docket is a major practice area.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, legal_aid_providers, beneficiary,
    organized, biographical, mobile, national).

% Groups narrowed out of 'refugee' definition by state interpretation: climate-displaced persons, generalized violence fleeers, economic migrants with protection needs, stateless persons without persecution nexus. They fall outside the Convention's procedural reach not because process was denied them individually, but because the definition was narrowed to exclude their category. They bear protection gaps with no procedural remedy. Exit is trapped: no access to Convention process, no alternative framework with equivalent guarantees.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, excluded_migrant_categories, payer,
    powerless, biographical, trapped, global).

% States, scholars, and policy actors who read the Convention as a minimum floor preserving maximum sovereign discretion. They argue for narrow definitions, broad state margin of appreciation, and procedural minimalism. They observe the constraint from a seat that would narrow its coordination function toward sovereign discretion. Their analytical exit means they evaluate the constraint as a policy choice, not a binding coordination mechanism.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, restrictive_sovereignty_advocates, observer,
    institutional, generational, analytical, global).

% UNHCR (in its advocacy capacity), human rights NGOs, and scholars who read the Convention as an unbendable humanitarian mandate. They argue for broad definitions, expansive 'particular social group', and substantive protection floors beyond process. They observe the constraint from a seat that would expand its coordination function toward humanitarian guarantee. Their analytical exit means they evaluate the constraint as a moral imperative, not a negotiated coordination mechanism.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, expansive_humanitarian_advocates, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of fair individualized protection assessment across sovereign states: establishes a common procedural floor (non-refoulement, access to determination, reasoned decisions) so that protection seekers know what process they are owed and states know what process they must provide, preventing a race to the bottom on procedural standards.
% TRANSFER_FUNCTION: Moves adjudication costs and procedural obligations from asylum seekers (who would otherwise bear the burden of proving entitlement without process) to states (which must fund and administer fair determination procedures). Moves protection outcomes from arbitrary state discretion to rule-governed process. Does not transfer substantive protection entitlements — those remain contested.
% ABSENT_VOICES: Climate-displaced persons, stateless persons without persecution nexus, generalized violence fleeers, and economic migrants with protection needs are structurally excluded by the Convention's definitional architecture. They would object to a procedural-only guarantee that does not reach their protection needs, but they are not in the Convention conversation — they are governed by complementary protection frameworks that are weaker and non-binding. Also absent: future generations of displacement drivers not contemplated in 1951.
% DISAPPEARANCE_RATIONALE: If the procedural integrity constraint vanished overnight, states would immediately eliminate individualized assessment for disfavored categories, expand pushbacks and offshore processing without review, and the global asylum system would fragment into bilateral arrangements with no common procedural floor. Asylum seekers would lose even the procedural leverage they currently possess. The world would rearrange toward raw sovereign discretion.
% FOUNDING_PROBLEM: Post-WWII refugee protection chaos: no common definition of who qualifies, no shared procedural standards, states unilaterally granting or denying protection, refoulement routine, protection seekers with no predictable access to any process. The Convention was built to create a minimum procedural floor that all states would honor.
% FOUNDING_PROBLEM_CORROBORATION: The procedural chaos of the 1930s-1940s is documented in UN archival records and the Convention's travaux préparatoires — corroborated by historians outside the beneficiary set. However, whether the founding problem is 'solved' is contested: states argue the procedural floor exists and functions (problem solved); asylum seekers denied access argue the floor has gaps that replicate the original chaos (problem persists); UNHCR argues the floor exists but new displacement drivers require expansion (problem mutated). No single corroboration settles it.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).
:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects that the constraint primarily coordinates a fair process rather than extracting resources — states bear adjudication costs but gain orderly migration management; asylum seekers with access gain procedural protection. Suppression (0.28) is modest: the constraint requires states to provide process but does not coercively dictate outcomes. Theater ratio (0.22) is low-moderate: procedural compliance is largely genuine, though some states perform compliance while substantively denying access. Accessibility collapse (0.42) is moderate: alternatives to Convention process exist (complementary protection, humanitarian parole) but are fragile. Resistance (0.48) is moderate: states resist procedural expansion but rarely reject the framework entirely.
 *
 * PERSPECTIVAL GAP:
 *   From the state seat, the constraint is a coordination mechanism that allocates protection costs fairly and manages migration orderly — a rope. From the denied-access asylum seeker seat, the same constraint is a snare that legitimizes exclusion through procedural performance — the process exists but they cannot reach it. From the procedural-access asylum seeker seat, it is a rope that delivers genuine protection. The engine computes this divergence from the structural data: identical constraint, different directionalities, different effective extractions.
 *
 * DIRECTIONALITY LOGIC:
 *   Asylum seekers with procedural access are beneficiaries (d ~ 0.15): they gain protection through fair process, exit constrained by need for state recognition. Asylum seekers denied procedural access are victims (d ~ 0.85): they bear the cost of exclusion with no procedural remedy, exit trapped. States are agenda setters and payers (d ~ 0.45 symmetric): they administer and fund the system but control definitions; their exit is constrained by treaty obligation and reputational cost. UNHCR and adjudicators are beneficiaries (d ~ 0.2): they gain institutional role and legitimacy from the procedural framework. Excluded migrant categories are victims (d ~ 0.7): state definitional narrowing places them outside procedural reach without their consent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — post-WWII refugee protection chaos with no common standards — is substantially solved (the Convention created the framework). But the problem has mutated: new displacement drivers (climate, generalized violence, non-state persecution) fall outside the Convention's definitional core. The procedural integrity reading manages this by holding process constant while allowing definitional evolution — this prevents mandatrophy (the arrangement persists because its coordination function adapts). However, if states narrow definitions to exclude emerging displacement while maintaining procedural theater, the constraint becomes a piton: procedurally intact but substantively hollow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine procedural coordination mechanism or a humanitarian extraction cover?',
    'Compare procedural_integrity_reading classification across power seats: if payer seats (states) compute as rope/tangled_rope while beneficiary seats (asylum seekers with access) compute as rope, the coordination function holds; if both compute as extractive, the procedural frame is cover for humanitarian extraction.',
    'If the procedural frame is extractive cover, the constraint reclassifies toward tangled_rope or snare depending on enforcement asymmetry; if genuine coordination, the rope claim holds and procedural guarantees are the non-extractive core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether procedural integrity reading instantiates genuine coordination or humanitarian extraction disguised as process').

omega_variable(
    procedural_access_as_victim_boundary,
    'Does the victim set properly include those denied procedural access, or does it improperly exclude those who receive process but lose on merits?',
    'Track outcomes for asylum seekers who receive full individualized assessment but are denied protection: if denial rates remain high despite procedural compliance, the constraint''s victim boundary is procedural access only; if denial rates drop with procedural compliance, the constraint''s protection function extends beyond process.',
    'If victims are only those denied process, the constraint is rope (coordination of fair process); if victims include those who lose on merits despite fair process, the constraint has humanitarian extraction dimension and may be tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(procedural_access_as_victim_boundary, empirical, 'Whether the constraint''s victim boundary is procedural access or substantive protection').

omega_variable(
    offshore_processing_structural_permissibility,
    'Does the reading''s structural permission for offshore processing with procedural guarantees create a covert extraction channel for states?',
    'Monitor offshore processing regimes that claim procedural compliance: if procedural guarantees are nominal while substantive access is eliminated, the reading enables extraction; if procedural guarantees are substantive and enforced, the reading maintains coordination.',
    'If offshore processing becomes a procedural veneer for denial of access, the constraint shifts toward snare/tangled_rope; if offshore processing maintains genuine procedural integrity, the reading remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshore_processing_structural_permissibility, empirical, 'Whether offshore processing permission under procedural guarantees functions as extraction channel or genuine coordination').

omega_variable(
    state_discretion_vs_procedural_floor,
    'How much definition-narrowing can states perform before the procedural floor collapses?',
    'Track jurisprudential evolution of ''particular social group'' and ''well-founded fear'' definitions alongside procedural compliance metrics: if definition-narrowing correlates with procedural erosion, the floor is unstable; if procedural integrity holds despite definitional shifts, the floor is robust.',
    'If definition-narrowing erodes procedural guarantees, the constraint is a degrading rope toward piton/snare; if procedural guarantees survive definitional restriction, the reading''s core coordination claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_discretion_vs_procedural_floor, empirical, 'Whether state definitional discretion has a structural limit at the procedural floor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1951, 0.12).
narrative_ontology:measurement(refu_tr_t1967, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1967, 0.14).
narrative_ontology:measurement(refu_tr_t1980, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1980, 0.16).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(refu_tr_t2001, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(refu_tr_t2015, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2015, 0.21).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1951, 0.22).
narrative_ontology:measurement(refu_be_t1967, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1967, 0.25).
narrative_ontology:measurement(refu_be_t1980, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1990, 0.31).
narrative_ontology:measurement(refu_be_t2001, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2001, 0.34).
narrative_ontology:measurement(refu_be_t2015, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2015, 0.37).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1951, 0.18).
narrative_ontology:measurement(refu_su_t1967, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1967, 0.2).
narrative_ontology:measurement(refu_su_t1980, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1980, 0.22).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(refu_su_t2001, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2001, 0.26).
narrative_ontology:measurement(refu_su_t2015, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2015, 0.27).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2024, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__procedural_integrity_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, eu_asylum_acquis).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, regional_refugee_law_africa).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, regional_refugee_law_americas).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, complementary_protection_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is one member of the refugee_convention_text constraint family (kernel). The three readings instantiate distinct constraints with different ε, different beneficiary/victim structures, and different classifications. They are linked via network.affects_constraints. The procedural_integrity_reading occupies the middle ground: it coordinates fair process (rope) while the expansive reading claims humanitarian mandate (mountain/tangled_rope from state seats) and the restrictive reading claims sovereign floor (piton from asylum seeker seats).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_text__procedural_integrity_reading, institutional, 0.2).
constraint_indexing:directionality_override(refugee_convention_text__procedural_integrity_reading, organized, 0.25).
constraint_indexing:directionality_override(refugee_convention_text__procedural_integrity_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
