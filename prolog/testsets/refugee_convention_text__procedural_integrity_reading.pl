% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: 1951 Refugee Convention as Procedural Integrity Safeguard
 *   domain: international_law/migration/human_rights
 *
 * SUMMARY:
 *   The 1951 Refugee Convention is contested across three readings. This
 *   story instantiates the PROCEDURAL INTEGRITY READING: the Convention is a
 *   safeguard requiring fair individualized assessment of asylum claims
 *   (notice, hearing, reasoned decision, appeal). The protection threshold
 *   (who qualifies as a refugee) can be narrowly defined by receiving states,
 *   but the PROCESS of assessment is non-negotiable. Offshore processing is
 *   permitted only with full procedural guarantees. The reading prioritizes
 *   process integrity over substantive outcome — a state that grants asylum
 *   to 5% of applicants but via transparent, fair procedure is
 *   Convention-compliant under this reading, while a state that grants 80%
 *   via opaque, arbitrary process is in breach.
 *
 * KEY AGENTS:
 *   - asylum_seekers_with_procedural_access: receive fair assessment; their protection depends on state willingness to provide process
 *   - asylum_seekers_denied_procedural_access: intercepted offshore or returned without individualized review; visible as breach only under the procedural reading
 *   - receiving_states: agenda-setters; retain wide discretion on substantive definitions (who counts as refugee) but cannot eliminate procedural review
 *   - offshore_processing_authorities: constrained to maintain full procedural guarantees or create Convention violations
 *   - adjudicatory_bodies (UNHCR, regional courts, national tribunals): enforce procedural compliance; can overturn decisions lacking individualized assessment
 *   - restrictive_sovereignty_advocates (excluded): argue for maximum discretion and minimal procedure; outside the legitimacy framework of this reading
 *   - humanitarian_expansion_advocates (excluded): argue for broader substantive definitions and outcome priority; reject the procedural formalism of this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.38).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.42).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "1951 Refugee Convention as Procedural Integrity Safeguard").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, 'e53e48dd-97c7-4b62-bf54-ef05ccb78713').
narrative_ontology:cs_kernel_codification('e53e48dd-97c7-4b62-bf54-ef05ccb78713', formalized).
narrative_ontology:cs_authority_grounding('e53e48dd-97c7-4b62-bf54-ef05ccb78713', lineage).
narrative_ontology:cs_interpretation_layer_present('e53e48dd-97c7-4b62-bf54-ef05ccb78713').
narrative_ontology:cs_reading_relation('e53e48dd-97c7-4b62-bf54-ef05ccb78713', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('e53e48dd-97c7-4b62-bf54-ef05ccb78713', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('e53e48dd-97c7-4b62-bf54-ef05ccb78713', foundational, individualized_assessment_non_delegable).
narrative_ontology:cs_axiom_status(individualized_assessment_non_delegable, holdable).
narrative_ontology:cs_axiom_grounding('e53e48dd-97c7-4b62-bf54-ef05ccb78713', individualized_assessment_non_delegable, deontological).
narrative_ontology:cs_axiom('e53e48dd-97c7-4b62-bf54-ef05ccb78713', foundational, procedural_integrity_prior_to_outcome).
narrative_ontology:cs_axiom_status(procedural_integrity_prior_to_outcome, holdable).
narrative_ontology:cs_axiom_grounding('e53e48dd-97c7-4b62-bf54-ef05ccb78713', procedural_integrity_prior_to_outcome, conventional).
narrative_ontology:cs_reference_frame('e53e48dd-97c7-4b62-bf54-ef05ccb78713', fair_individual_assessment_framework).
narrative_ontology:cs_drift_state('e53e48dd-97c7-4b62-bf54-ef05ccb78713', contemporary_offshore_processing_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e53e48dd-97c7-4b62-bf54-ef05ccb78713', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, asylum_seekers_with_procedural_access).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, international_rule_of_law_advocates).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_seekers_denied_procedural_access).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, offshore_processing_subjects).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.38 at interval end) because the constraint genuinely solves a coordination problem (standardizing fair assessment across jurisdictions) while also imposing real costs on receiving states (procedural infrastructure, slower decisions). Suppression is moderate (0.42) because the constraint requires active enforcement — states have incentives to skip procedure for efficiency, and offshore processing creates structural temptation to bypass individualized assessment. Theater ratio is low-moderate (0.28) because procedural compliance can be performed (written decisions that are purely formulaic, hearings that are theatrical) without substantive scrutiny, and the interval shows rising theater as offshore processing proliferates. Accessibility_collapse is moderate-high (0.65) because once an asylum seeker is intercepted offshore or in non-procedure-compliant detention, alternatives to accepting their exclusion nearly vanish. Stakes_inflation is high (individual level 0.78 at 2026) because the cost of violating procedure is deportation to persecution. Suppression at individual level (0.48) is highest when offshore processing denies procedure access entirely. The measurement series show rising theater and suppression over 75 years, indicating the constraint has drifted from coordination toward enforced formalism. Individual-level resistance is persistently high (0.72 at 2026) because asylum seekers and advocacy organizations continuously contest procedure-denial.
 *
 * PERSPECTIVAL GAP:
 *   Receiving_states and offshore_processing_authorities experience this constraint as coordination with embedded cost (they set substantive definitions and execute process, both costly). Asylum_seekers_with_procedural_access experience it as genuine protection (the process is their only safeguard). Asylum_seekers_denied_procedural_access experience it as snare — the reading makes them visible as breach victims by making procedure mandatory. The procedural_integrity reading creates this gap structurally: process is the protection, so denial of process is the harm. The restrictive_sovereignty reading would not create this gap because it prioritizes state discretion over procedure. The engine should compute different directionalities for the offshore_processing_authorities seat under this reading versus the restrictive reading: here they are constrained payers (high d); under restrictive reading they are mobile beneficiaries (low d).
 *
 * DIRECTIONALITY LOGIC:
 *   Asylum_seekers_with_procedural_access are beneficiaries (d low, toward 0.2): they receive fair process they could not demand unilaterally. International_rule_of_law_advocates are beneficiaries (d low, toward 0.15): the constraint vindicates procedural integrity doctrine. Asylum_seekers_denied_procedural_access are victims (d high, toward 0.85): they bear the cost of exclusion precisely because procedure is denied. Receiving_states are agenda-setters with moderate extraction (d toward 0.55): they benefit from procedural legitimacy (international acceptance of their decisions) but pay the cost of process infrastructure and procedural constraints on speed. Offshore_processing_authorities are payers (d high, toward 0.7): constrained to maintain equivalence to onshore procedure, they cannot simply intercept and return. Restrictive_sovereignty_advocates and humanitarian_expansion_advocates are excluded (they are out of the covenant, not seats within it). Adjudicatory_bodies are observers (d analytical): they police compliance but don't collect or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was post-WWII displacement: millions of refugees with no protection mechanism, need for coordinated assessment standard. The procedural reading claims the founding problem is LIVE (displacement and assessment complexity persist) and the procedure is the solution (standardize what fair assessment means across states). Alternative readings dispute both: restrictive reading says the founding problem is now border control (not displacement), and procedure is an obstacle; humanitarian reading says the founding problem is now generalized violence (not individual persecution), and procedure is too narrow. The measurement series show rising theater_ratio (procedural formalism without substantive scrutiny), which is the classic mandatrophy signal: the procedure performs but the function it was built for (fair assessment of borderline cases) atrophies as states narrow definitions and offshore processing limits meaningful review. The constraint drifts from coordination (shared standard for assessment) toward enforcement (procedure as rationing device). The mandatrophy question is whether this reading can survive the drift — if procedure becomes purely theatrical, does the constraint collapse into snare (procedure as legitimation of exclusion)? The omegas carry this: if offshore processing cannot maintain procedural equivalence, the constraint is already breached by major states. If procedure is a rationing mechanism (similar outcomes regardless of rigor), the constraint was always snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_procedural_vs_substantive,
    'Is the Refugee Convention''s primary mandate procedural (fair assessment of claims) or substantive (broad protection from specified harms)? Can a state discharge its Convention obligation by providing perfect procedure but narrow substantive eligibility?',
    'International Court of Justice or treaty body interpretation; examination of travaux préparatoires and contemporary state practice to determine legislative intent. Empirical test: if states consistently narrow definitions while preserving procedure, and international bodies accept the narrow definitions as Convention-compliant, the procedural reading holds; if international bodies override procedures to reach substantive protection outcomes, the substantive reading holds.',
    'This reading claims procedure is primary and outcome secondary. If the substantive reading is correct, this constraint is a disguised snare — apparent protection that permits substantive exclusion through procedural formality. If this reading is correct, the constraint genuinely coordinates on process and states retain legitimate scope for definition-narrowing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_procedural_vs_substantive, conceptual, 'The kernel contest: is the Convention primarily a procedural safeguard or a substantive protection mandate?').

omega_variable(
    offshore_processing_procedural_feasibility,
    'Can offshore processing centers maintain full procedural guarantees (reasoned written decisions, appeal pathways, legal representation) equivalent to onshore review, or does offshore processing structurally degrade procedural integrity?',
    'Empirical audit of offshore processing centers: coding of decision rationales, appeal rates, reversal rates, representation access, and comparison to onshore tribunals. If offshore centers achieve comparable procedural rigor and transparency, this reading permits them; if they show systematic procedural deficits, this reading prohibits them.',
    'This reading declares procedure non-negotiable but permits location flexibility. If offshore processing can deliver equivalent procedure, the constraint allows cost-shifting; if procedural integrity is inherently compromised offshore, this reading mandates onshore processing for Convention compliance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(offshore_processing_procedural_feasibility, empirical, 'Whether offshore processing can maintain equivalent procedural guarantees.').

omega_variable(
    reading_viability_with_restrictive_definitions,
    'Can this procedural reading coexist with the restrictive_sovereignty_reading''s narrow substantive definitions of ''well-founded fear'' and ''particular social group''? Or does this reading implicitly require broader substantive scope to avoid becoming an empty procedural shell?',
    'Test case: apply this reading''s procedural requirements to a state using the restrictive reading''s definitions; assess whether procedurally fair application of narrow definitions satisfies Convention obligations. If yes, the readings genuinely coexist; if no, this reading implicitly forecloses the restrictive reading''s core premise.',
    'If the readings coexist, a state can be procedurally compliant while being substantively restrictive. If this reading forecloses restrictive substantive definitions, the kernel is not genuinely contested — one reading would be unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_viability_with_restrictive_definitions, conceptual, 'Whether this procedural reading permits coexistence with restrictive substantive definitions.').

omega_variable(
    individualized_assessment_cost_asymmetry,
    'Is individualized assessment a benefit for asylum seekers (procedural protection against mass rejection) or a cost imposed on receiving states (resource burden, slow processing)? Is the constraint fundamentally a procedural safeguard or a rationing device?',
    'Comparative institutional analysis: examine states with high procedural rigor and high approval rates versus high procedural rigor and low approval rates. If high-rigor states show systematic differences in approval rates, procedure may be operating as a rationing device (producing outcome convergence regardless of rigor). If high-rigor states show wide approval-rate variance, procedure may be genuine safeguard.',
    'If procedure is genuinely a safeguard, this reading''s claim is vindicated. If procedure is a rationing device that produces similar outcomes regardless of rigor, this reading disguises a snare: the procedural appearance of protection without substantive protection (false summit of procedure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individualized_assessment_cost_asymmetry, empirical, 'Whether individualized assessment functions as procedural protection or as a rationing mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 1951, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1951, 0.08).
narrative_ontology:measurement(refu_tr_t1980, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(refu_tr_t2000, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(refu_tr_t2015, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(refu_tr_t2026, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1951, 0.15).
narrative_ontology:measurement(refu_be_t1980, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(refu_be_t2000, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement(refu_be_t2015, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(refu_be_t2026, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1951, 0.18).
narrative_ontology:measurement(refu_su_t1980, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1980, 0.28).
narrative_ontology:measurement(refu_su_t2000, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(refu_su_t2015, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(refu_su_t2026, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__procedural_integrity_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__expansive_humanitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested refugee_convention_text kernel. The three readings decompose the Convention into three structurally distinct claims: (1) procedural_integrity_reading (this file) — the Convention requires fair individualized assessment; procedure is non-negotiable, outcome secondary. (2) restrictive_sovereignty_reading — the Convention establishes a minimum floor permitting maximum state discretion; 'well-founded fear' requires proof, 'particular social group' is narrow. (3) expansive_humanitarian_reading — the Convention mandates broad protection; 'well-founded fear' includes generalized violence, 'particular social group' is capacious. Each reading has different ε, different victim/beneficiary structure, different enforcement dynamics. They are NOT one constraint viewed from different angles; they are three constraints instantiated by the same founding text. The kernel contest is which reading is the authoritative interpretation. The omegas carry the irreducible ambiguities: is the Convention primarily about procedure or substance, narrowness or breadth, state discretion or humanitarian mandate? Each reading claims to resolve these ambiguities; the corpus data is how we measure which reading's claims are structurally sound.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
