% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Knesset as Sovereign Interpreter of Basic Laws (Parliamentary Sovereignty Reading)
 *   domain: constitutional_law/political_science
 *
 * SUMMARY:
 *   This constraint represents the 'parliamentary sovereignty' reading of the
 *   Basic Laws' interpretive boundary in Israel. Under this reading, the
 *   Knesset, as the directly elected legislative body, holds ultimate and
 *   unchallengeable authority to interpret and amend Basic Laws, including
 *   the power to override any judicial review. This perspective views the
 *   Knesset's will as the direct expression of the sovereign electorate,
 *   making its legislative decisions, even those touching on constitutional
 *   matters, final. The constraint is claimed as a Mountain because, from
 *   this reading's internal logic, the Knesset's sovereignty is an
 *   irreducible feature of the constitutional order, not a contingent
 *   arrangement. However, the presence of beneficiaries (Knesset majority,
 *   electorate) triggers False Summit Mountain detection, prompting an omega
 *   variable to address the contestability of this 'natural' claim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.15).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.25).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, mountain).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Knesset as Sovereign Interpreter of Basic Laws (Parliamentary Sovereignty Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional_law/political_science").

domain_priors:emerges_naturally(basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '102e8ae8-2443-41e5-ad08-e327fe699cfa').
narrative_ontology:cs_kernel_codification('102e8ae8-2443-41e5-ad08-e327fe699cfa', formalized).
narrative_ontology:cs_authority_grounding('102e8ae8-2443-41e5-ad08-e327fe699cfa', lineage).
narrative_ontology:cs_interpretation_layer_present('102e8ae8-2443-41e5-ad08-e327fe699cfa').
narrative_ontology:cs_reading_relation('102e8ae8-2443-41e5-ad08-e327fe699cfa', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('102e8ae8-2443-41e5-ad08-e327fe699cfa', basic_law_interpretive_boundary__balanced_contestation_reading, forecloses).
narrative_ontology:cs_axiom('102e8ae8-2443-41e5-ad08-e327fe699cfa', foundational, knesset_ultimate_sovereign).
narrative_ontology:cs_axiom_status(knesset_ultimate_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('102e8ae8-2443-41e5-ad08-e327fe699cfa', knesset_ultimate_sovereign, conventional).
narrative_ontology:cs_axiom('102e8ae8-2443-41e5-ad08-e327fe699cfa', foundational, judicial_review_advisory_only).
narrative_ontology:cs_axiom_status(judicial_review_advisory_only, holdable).
narrative_ontology:cs_axiom_grounding('102e8ae8-2443-41e5-ad08-e327fe699cfa', judicial_review_advisory_only, conventional).
narrative_ontology:cs_reference_frame('102e8ae8-2443-41e5-ad08-e327fe699cfa', unfettered_parliamentary_supremacy).
narrative_ontology:cs_drift_state('102e8ae8-2443-41e5-ad08-e327fe699cfa', contemporary_judicial_activism_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('102e8ae8-2443-41e5-ad08-e327fe699cfa', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, israeli_electorate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_of_israel).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, legal_scholars_and_civil_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The political bloc that forms the government, which benefits from the ability to enact its legislative agenda without judicial impediment, interpreting Basic Laws as it sees fit to achieve policy goals.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition, agenda_setter,
    institutional, biographical, mobile, national).

% The body of citizens whose votes empower the Knesset. This reading asserts their direct democratic will is paramount, unmediated by an unelected judiciary, ensuring policy reflects current electoral mandates.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, israeli_electorate, beneficiary,
    organized, generational, constrained, national).

% The judiciary, whose power to review and invalidate legislation is curtailed or rendered advisory under this reading. Its institutional identity is tied to upholding the rule of law, which this reading redefines.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_of_israel, payer,
    institutional, generational, identity_locked, national).

% Advocates for robust judicial review and constitutional checks on parliamentary power. They bear the cost of reduced judicial oversight and potential erosion of minority rights, having limited direct means to alter the constitutional framework.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, legal_scholars_and_civil_society, payer,
    moderate, generational, constrained, national).

% Observes and comments on the state of judicial independence and constitutionalism in Israel, potentially influencing international perceptions and diplomatic relations, but without direct enforcement power over the Knesset.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_legal_community, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a clear, singular locus of ultimate authority in the Israeli political system, preventing institutional deadlock between the legislative and judicial branches by prioritizing the elected parliament's will.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over Basic Laws from a potentially shared or contested domain to the Knesset, effectively reducing the Supreme Court's power to invalidate legislation.
% ABSENT_VOICES: Future generations and minority groups, whose long-term rights and interests might be less protected without robust judicial review, are not directly represented in the current majoritarian legislative process.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the Supreme Court would likely reassert a stronger role in judicial review, leading to potential constitutional crises, legislative gridlock, and a fundamental reordering of power dynamics between the branches of government.
% FOUNDING_PROBLEM: The absence of a formal, entrenched constitution in Israel led to ambiguity regarding the hierarchy of Basic Laws and the scope of judicial review, creating potential for institutional conflict.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and political scientists outside the Knesset majority coalition corroborate that the ambiguity regarding constitutional authority and judicial review remains a live and contested issue in Israeli constitutional law.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(basic_law_interpretive_boundary__parliamentary_sovereignty_reading),
    narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because this reading primarily defines a power distribution rather than extracting resources, though it enables majoritarian policy that might be extractive. Suppression is low (0.25) as it's a structural feature of the system, not actively enforced against a resisting populace, but rather against a competing institutional claim (the judiciary). Theater ratio is very low (0.05) as the claim of parliamentary sovereignty is a direct assertion of power, not a performance masking a different function. Accessibility collapse is high (0.88) because, within this reading, alternatives to Knesset supremacy are structurally foreclosed. Resistance is low (0.1) because, while there is institutional contestation, this reading asserts a fundamental, 'natural' order.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Knesset majority, this is a natural and proper ordering of democratic power (Mountain). From the perspective of the Supreme Court and civil society, it is a constructed constraint that extracts power from the judiciary and concentrates it in the legislature (potentially a Snare or Tangled Rope, depending on the degree of coordination vs. extraction). The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Knesset majority coalition and the Israeli electorate are beneficiaries, as this reading empowers them directly. The Supreme Court and legal scholars/civil society are payers, as their institutional roles or advocacy for judicial checks are diminished. The international legal community is an observer, assessing the implications without direct participation in the domestic power struggle.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_political_choice,
    'Is the Knesset''s ultimate authority to interpret Basic Laws a ''natural'' and irreducible feature of Israel''s constitutional order, or a political choice that could be altered?',
    'A formal constitutional entrenchment of Basic Laws with a supermajority amendment clause, or a sustained period of judicial deference to Basic Laws without legislative override.',
    'If a political choice, the ''Mountain'' claim is a false summit, and the constraint would reclassify as a ''Tangled Rope'' or ''Snare'' from the perspective of the judiciary and civil society, reflecting the active enforcement required to maintain this power distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_political_choice, conceptual, 'Contestability of the ''naturalness'' of parliamentary sovereignty.').

omega_variable(
    democratic_will_vs_minority_rights,
    'Does this reading genuinely uphold the democratic will of the electorate, or does it enable a ''tyranny of the majority'' that undermines minority rights and long-term constitutional stability?',
    'Empirical analysis of legislative outcomes under this reading, specifically examining the protection of minority rights and the stability of fundamental legal principles over time, compared to systems with stronger judicial review.',
    'If it consistently leads to erosion of minority rights, the ''beneficiary'' status of the electorate becomes contested, and the constraint''s extractiveness from vulnerable groups would be re-evaluated upward, potentially shifting classification towards ''Snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_will_vs_minority_rights, empirical, 'Impact of parliamentary sovereignty on minority protections.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1948, 0.02).
narrative_ontology:measurement(basi_tr_t1970, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1970, 0.03).
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1992, 0.04).
narrative_ontology:measurement(basi_tr_t2005, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(basi_tr_t2015, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2015, 0.04).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(basi_be_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1948, 0.1).
narrative_ontology:measurement(basi_be_t1970, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1970, 0.12).
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1992, 0.15).
narrative_ontology:measurement(basi_be_t2005, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2005, 0.14).
narrative_ontology:measurement(basi_be_t2015, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2015, 0.13).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1948, 0.2).
narrative_ontology:measurement(basi_su_t1970, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1970, 0.22).
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1992, 0.25).
narrative_ontology:measurement(basi_su_t2005, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2005, 0.24).
narrative_ontology:measurement(basi_su_t2015, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2015, 0.23).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'basic_law_interpretive_boundary' kernel. This 'parliamentary sovereignty' reading asserts the Knesset's ultimate authority, contrasting with 'judicial supremacy' and 'balanced contestation' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
