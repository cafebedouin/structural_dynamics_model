% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Text
 *   domain: constitutional law / legal philosophy / interpretive theory
 *
 * SUMMARY:
 *   This constraint instantiates the originalist_reading of the contested
 *   kernel us_constitution_text. It treats the Constitution as a fixed text
 *   whose public meaning at ratification is binding on contemporary
 *   interpreters. The constraint is actively enforced by a federal judiciary
 *   captured by the conservative legal movement, which uses historical
 *   methodology to suppress adaptive interpretation. Sibling readings
 *   (living_constitutionalist_reading, positivist_reading) are modeled as
 *   separate constraints. This file authors only the originalist reading as a
 *   clean, Îµ-invariant constraint.
 *
 * KEY AGENTS:
 *   - conservative_legal_movement: Primary beneficiary (institutional/arbitrage) â captures judicial appointments and derives institutional dominance from the methodology.
 *   - contemporary_rights_claimants: Primary target (moderate/constrained) â bear the extraction through doctrinal exclusion of rights lacking 18th/19th-century pedigree.
 *   - federal_judiciary_originalists: Agenda-setter (institutional/mobile) â administers and enforces the interpretive constraint through federal courts.
 *   - progressive_legal_scholars: Excluded voice (organized/analytical) â argues against the methodology but is structurally marginalized in controlling institutions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.72).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.78).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Reading of Constitutional Text").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "constitutional law / legal philosophy / interpretive theory").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, '49e03eb9-df2d-422c-98a7-72f1922efcba').
narrative_ontology:cs_kernel_codification('49e03eb9-df2d-422c-98a7-72f1922efcba', fixed_text).
narrative_ontology:cs_authority_grounding('49e03eb9-df2d-422c-98a7-72f1922efcba', lineage).
narrative_ontology:cs_interpretation_layer_present('49e03eb9-df2d-422c-98a7-72f1922efcba').
narrative_ontology:cs_reading_relation('49e03eb9-df2d-422c-98a7-72f1922efcba', us_constitution_text__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('49e03eb9-df2d-422c-98a7-72f1922efcba', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('49e03eb9-df2d-422c-98a7-72f1922efcba', foundational, original_public_meaning_fixation).
narrative_ontology:cs_axiom_status(original_public_meaning_fixation, holdable).
narrative_ontology:cs_axiom_grounding('49e03eb9-df2d-422c-98a7-72f1922efcba', original_public_meaning_fixation, conventional).
narrative_ontology:cs_reference_frame('49e03eb9-df2d-422c-98a7-72f1922efcba', original_public_meaning_era).
narrative_ontology:cs_drift_state('49e03eb9-df2d-422c-98a7-72f1922efcba', contemporary_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('49e03eb9-df2d-422c-98a7-72f1922efcba', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, contemporary_rights_claimants).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, original_public_meaning_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, fixed_constitutional_meaning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls federal judicial appointments, clerkship pipelines, and originalist legal infrastructure. Benefits from a methodology that anchors constitutional interpretation to historical materials from the founding and Reconstruction eras, preventing adaptive evolution of rights and locking in institutional dominance.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, beneficiary,
    institutional, generational, arbitrage, national).

% Seek constitutional protection for rights not clearly grounded in 18th or 19th-century historical practice, such as privacy, reproductive autonomy, and LGBTQ equality. Their claims are systematically disadvantaged by interpretive rules that treat post-ratification social evolution as irrelevant to constitutional meaning.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, contemporary_rights_claimants, payer,
    moderate, biographical, constrained, national).

% Federal judges and justices who apply original public meaning methodology to invalidate or narrow statutes and rights claims lacking historical pedigree. They control the interpretive rules of the federal court system and actively enforce the constraint against adaptive methodologies.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, federal_judiciary_originalists, agenda_setter,
    institutional, generational, mobile, national).

% Produce scholarship arguing for evolving constitutional meaning and precedent-based adaptation, but their methodological framework is systematically excluded from majority opinions in federal courts controlled by originalist majorities.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, progressive_legal_scholars, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_text__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates intergenerational constitutional governance by fixing meaning at a historical moment, claiming to enable legal predictability and constrain contemporary judicial discretion based on a settled founding bargain.
% TRANSFER_FUNCTION: Moves interpretive authority from contemporary democratic majorities and evolving social movements to historical materials and professional historians; shifts doctrinal outcomes toward positions supported by the political coalition that controls founding-era historical narratives.
% ABSENT_VOICES: Living constitutionalist jurists and progressive legal scholars are structurally marginalized in originalist-dominated courts; ordinary citizens seeking rights protections unanticipated by 18th-century practice have no seat in the interpretive framework.
% DISAPPEARANCE_RATIONALE: If original meaning fixation vanished overnight, federal courts would revert to precedent-based, pragmatic, or moral-reading methodologies; landmark originalist decisions would become unstable; the conservative legal movement's institutional dominance would lose its methodological anchor, and constitutional doctrine would reorganize around contemporary values rather than historical pedigree.
% FOUNDING_PROBLEM: Judicial overreach and democratic illegitimacy: unelected judges imposing personal preferences under the guise of constitutional interpretation, destabilizing law and undermining democratic self-governance.
% FOUNDING_PROBLEM_CORROBORATION: Conservative legal scholars and originalist jurists attest the problem from within the beneficiary movement. Progressive constitutional scholars, historians, and political scientists outside the beneficiary set argue the originalist cure has produced a new form of judicial power; empirical judicial-behavior studies from neutral political scientists suggest methodological rhetoric does not tightly constrain outcomes.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the methodology systematically transfers interpretive authority to historical materials controlled by a specific political coalition, producing asymmetric doctrinal outcomes. Suppression (0.78) is higher still because the constraint's persistence depends on actively excluding adaptive interpretation and non-originalist precedent from federal judicial reasoning. Theater ratio (0.48) is moderate-high: a genuine scholarly apparatus exists, but an increasing share of originalist argument functions as law-office history deployed to ratify politically preferred outcomes. Accessibility collapse (0.82) reflects that once inside the originalist framework, alternatives (moral reasoning, pragmatic adaptation, precedent-based evolution) are methodologically excluded. Resistance (0.62) captures sustained academic and political opposition from excluded interpretive communities. The temporal series trace originalism's trajectory from academic theory to dominant institutional methodology.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist judicial seat, the constraint appears as genuine coordination (binding law, democratic legitimacy, intergenerational stability). From the contemporary rights-claimant seat, the same structure operates as enforced extraction that freezes 18th-century limitations onto 21st-century problems. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The conservative legal movement sits near the beneficiary end (low d): the constraint subsidizes their institutional control by locking in interpretive rules that favor their historical narrative and policy preferences. Contemporary rights claimants sit near the full-target end (high d): the constraint extracts their ability to secure constitutional protections for practices unanticipated by the founding generation. Federal judiciary originalists occupy an ambiguous middle position (moderate d): they are constrained by the methodology they enforce, but their institutional power and career mobility differentiate them from trapped claimants. Progressive legal scholars are analytically outside the extraction circuit (analytical exit, high scope).
 *
 * MANDATROPHY ANALYSIS:
 *   Originalism was founded to solve judicial overreach and democratic illegitimacy. The R5 genealogy records this founding problem as contested: the beneficiary movement asserts it is still live, while observers outside the benefiting coalition argue the problem has mutated into a new form of counter-majoritarian judicial power. The measurement trajectory shows extraction rising over time as originalism moved from academic theory to controlling institutional methodology, suggesting a tangled rope rather than a pure coordination mechanism. If the coordination function were primary and extraction incidental, we would expect flat or falling extractiveness as the methodology stabilized; the observed accumulation signals rent-seeking layered onto coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalism_naturalness_ambiguity,
    'Is originalism a genuine discovery of fixed linguistic meaning inherent in the constitutional text, or a constructed methodological constraint developed in the 1970s-80s to serve identifiable political beneficiaries?',
    'Archival and intellectual-history analysis of the methodological formation of originalism; comparison of asserted original meanings with professional historical consensus; tracking whether judicial outcomes under originalism correlate with the policy preferences of the benefiting coalition.',
    'If constructed, the constraint''s mountain-like claim of natural fixity is a false summit, reclassifying it as a snare or high-extraction tangled rope with elevated theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_naturalness_ambiguity, conceptual, 'Whether originalism is a natural feature of law or a constructed political methodology.').

omega_variable(
    historical_pedigree_indeterminacy,
    'Is original public meaning sufficiently determinate to actually constrain judicial discretion, or does the historical record underdetermine outcomes, leaving selection effects to be filled by present-day political preference?',
    'Empirical studies comparing originalist judges'' historical claims with professional historians'' assessments; outcome-analysis of originalist methodology in high-salience cases.',
    'If indeterminate, the coordination function (fixing meaning) fails and the constraint becomes a piton of performative historical argument, with extraction depending on who controls the historical narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_pedigree_indeterminacy, empirical, 'Whether historical materials actually fix constitutional meaning or provide a rhetorical canvas.').

omega_variable(
    kernel_reading_exclusivity,
    'Does adopting the originalist reading logically foreclose the living constitutionalist reading within a single jurist''s framework, or can the two readings be blended or alternated?',
    'Analysis of judicial opinions and scholarly frameworks claiming to synthesize originalism with living constitutionalism; examination of whether such syntheses collapse into one reading on close inspection.',
    'If foreclosing, the kernel generates zero-sum institutional conflict and the constraint''s suppression metric reflects genuine mutual exclusion; if coexisting, suppression is overstated because methodological pluralism is structurally available.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_exclusivity, conceptual, 'Whether originalism and living constitutionalism are logically incompatible or pragmatically combinable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__originalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(us_c_tr_t6, us_constitution_text__originalist_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(us_c_tr_t12, us_constitution_text__originalist_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(us_c_tr_t18, us_constitution_text__originalist_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement(us_c_tr_t24, us_constitution_text__originalist_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(us_c_tr_t36, us_constitution_text__originalist_reading, theater_ratio, 36, 0.45).
narrative_ontology:measurement(us_c_tr_t44, us_constitution_text__originalist_reading, theater_ratio, 44, 0.48).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__originalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(us_c_be_t6, us_constitution_text__originalist_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(us_c_be_t12, us_constitution_text__originalist_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(us_c_be_t18, us_constitution_text__originalist_reading, base_extractiveness, 18, 0.54).
narrative_ontology:measurement(us_c_be_t24, us_constitution_text__originalist_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(us_c_be_t36, us_constitution_text__originalist_reading, base_extractiveness, 36, 0.68).
narrative_ontology:measurement(us_c_be_t44, us_constitution_text__originalist_reading, base_extractiveness, 44, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__originalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(us_c_su_t6, us_constitution_text__originalist_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(us_c_su_t12, us_constitution_text__originalist_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(us_c_su_t18, us_constitution_text__originalist_reading, suppression_requirement, 18, 0.64).
narrative_ontology:measurement(us_c_su_t24, us_constitution_text__originalist_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(us_c_su_t36, us_constitution_text__originalist_reading, suppression_requirement, 36, 0.76).
narrative_ontology:measurement(us_c_su_t44, us_constitution_text__originalist_reading, suppression_requirement, 44, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the us_constitution_text kernel, decomposed per the Îµ-invariance principle. The colloquial label 'the Constitution' conflates multiple structurally distinct constraints: originalist_reading (fixed original public meaning), living_constitutionalist_reading (evolving principles), and positivist_reading (procedural validity). Each has distinct Îµ, stakeholders, and classification. This file models only the originalist reading. Sibling files model the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
