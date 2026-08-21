% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: US Constitutional Meaning: Originalist Reading
 *   domain: legal/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of the US
 *   Constitution, which posits that the meaning of the Constitution is fixed
 *   at the time of its ratification or amendment, and judges are bound to
 *   interpret it according to its historical public meaning. This reading is
 *   presented as a 'rope' by its proponents, offering stability and judicial
 *   restraint. However, its operation involves significant extraction from
 *   rights claimants whose arguments lack historical grounding, and
 *   suppression of alternative interpretive methods.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.75).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "US Constitutional Meaning: Originalist Reading").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "legal/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, '3da674c2-c849-42a1-9570-1de2a21203dd').
narrative_ontology:cs_kernel_codification('3da674c2-c849-42a1-9570-1de2a21203dd', fixed_text).
narrative_ontology:cs_authority_grounding('3da674c2-c849-42a1-9570-1de2a21203dd', lineage).
narrative_ontology:cs_interpretation_layer_present('3da674c2-c849-42a1-9570-1de2a21203dd').
narrative_ontology:cs_reading_relation('3da674c2-c849-42a1-9570-1de2a21203dd', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('3da674c2-c849-42a1-9570-1de2a21203dd', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('3da674c2-c849-42a1-9570-1de2a21203dd', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('3da674c2-c849-42a1-9570-1de2a21203dd', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('3da674c2-c849-42a1-9570-1de2a21203dd', foundational, judicial_role_is_to_apply_not_make_law).
narrative_ontology:cs_axiom_status(judicial_role_is_to_apply_not_make_law, holdable).
narrative_ontology:cs_axiom_grounding('3da674c2-c849-42a1-9570-1de2a21203dd', judicial_role_is_to_apply_not_make_law, deontological).
narrative_ontology:cs_reference_frame('3da674c2-c849-42a1-9570-1de2a21203dd', founding_era_public_meaning).
narrative_ontology:cs_drift_state('3da674c2-c849-42a1-9570-1de2a21203dd', contemporary_legal_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3da674c2-c849-42a1-9570-1de2a21203dd', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_judges).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_lacking_historical_support).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, living_constitutionalist_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, general_public).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, general_public).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, judicial_restraint_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges who adhere to originalism, believing their role is to apply the Constitution's meaning as fixed at the time of its ratification or amendment. They enforce this interpretive method, shaping legal outcomes and judicial appointments.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, identity_locked, national).

% Legal and political groups who advocate for originalism as a means to limit judicial discretion and ensure that constitutional interpretation is not swayed by contemporary political or social trends. They benefit from the stability and perceived legitimacy originalism provides.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    organized, generational, mobile, national).

% Individuals or groups whose claims for constitutional rights or protections are denied because they lack clear historical support in the 18th-century public meaning of the Constitution. They bear the direct cost of this interpretive method.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, rights_claimants_lacking_historical_support, payer,
    powerless, immediate, trapped, national).

% Legal academics and practitioners who advocate for a 'living Constitution' where principles evolve with society. Their interpretive method is often rejected or marginalized by originalist courts, limiting their influence on legal outcomes.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_scholars, excluded,
    powerful, biographical, constrained, national).

% Academics who study the Constitution from a positivist perspective, focusing on its formal enactment and institutional authority rather than historical intent or evolving meaning. They observe the operation of originalism without necessarily endorsing its normative claims.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, legal_positivist_scholars, observer,
    analytical, biographical, analytical, national).

% Benefits from the perceived stability and predictability of constitutional law under originalism, but also bears the cost of its rigidity, particularly when it leads to outcomes that seem out of step with contemporary values or needs.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, general_public, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__originalist_reading, originalist_judges).
narrative_ontology:fixing_cost_class(us_constitution_meaning__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for constitutional interpretation by anchoring meaning to a fixed historical point, thereby limiting judicial discretion and ensuring fidelity to the founding generation's intent.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary judges and society to the historical public meaning of the Constitution at the time of its ratification or amendment, effectively transferring power from present majorities to past ones.
% ABSENT_VOICES: Future generations, whose evolving values, circumstances, and understanding of justice are deemed irrelevant to constitutional meaning, are structurally absent from the interpretive process. Their perspectives are suppressed in favor of historical ones.
% DISAPPEARANCE_RATIONALE: If originalism vanished overnight, constitutional interpretation would immediately shift, likely towards living constitutionalism or a more pragmatic approach. This would lead to different legal outcomes, a reordering of judicial power, and a significant re-evaluation of established precedents, fundamentally reorganizing the legal landscape.
% FOUNDING_PROBLEM: To prevent judicial activism, ensure democratic self-governance by limiting judges to applying law rather than making it, and to preserve the original intent of the framers against evolving societal preferences.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and conservative legal organizations attest the problem of judicial overreach is still live. Living constitutionalist scholars and many legal historians argue the founding problem is either misframed, or that originalism itself creates new problems by entrenching outdated norms; legislative-hearing testimony and independent legal analysis from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high because originalism often denies contemporary rights claims that lack 18th-century historical precedent, imposing costs on those seeking legal redress. Suppression (0.75) is also high, as it actively marginalizes or rejects other interpretive methodologies (like living constitutionalism) within the judicial system. Theater ratio (0.40) reflects that while genuine historical research is involved, there's also a performative aspect in asserting the discoverability and unambiguous nature of historical meaning, sometimes to justify predetermined outcomes. The rising trend in metrics reflects the increasing institutionalization and enforcement of originalism over the past decades.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist judges, this constraint is a necessary 'rope' for judicial fidelity and democratic self-governance. From the perspective of rights claimants or living constitutionalist scholars, it operates as a 'snare' or 'tangled rope,' extracting from certain groups and suppressing legitimate interpretive alternatives. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges and counter-majoritarian advocates are beneficiaries, gaining interpretive authority, legitimacy, and the ability to shape legal outcomes. Rights claimants lacking historical support and living constitutionalist scholars are victims, bearing the costs of denied claims and the marginalization of their interpretive frameworks. The general public experiences both benefits (stability) and costs (rigidity, denial of evolving rights).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by acknowledging the proponents' 'rope' claim while simultaneously measuring the high extractiveness and suppression inherent in its operation. This allows for detection of whether the constraint's stated coordination function (judicial restraint, stability) is genuinely served, or if it primarily functions as an extractive mechanism for certain ideological outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_meaning_determinacy,
    'Is the historical public meaning of the Constitution truly discoverable, unambiguous, and consistently applicable across all contemporary issues?',
    'Extensive historical and linguistic scholarship, combined with empirical analysis of judicial application. If historical meaning proves consistently indeterminate or contradictory, the foundational premise of originalism is weakened.',
    'If historical meaning is largely indeterminate, the constraint''s claimed coordination function (providing clear guidance) is undermined, potentially reclassifying it as more extractive or theatrical, as judicial discretion would still be at play, but under a historical guise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_meaning_determinacy, empirical, 'Ambiguity in discovering and applying historical constitutional meaning.').

omega_variable(
    counter_majoritarian_justification_validity,
    'Is the counter-majoritarian justification for originalism valid, or does it merely entrench past majorities and their values over present ones?',
    'Political philosophy and democratic theory analysis, examining whether originalism genuinely protects minority rights or primarily serves to block contemporary majoritarian preferences that conflict with historical norms.',
    'If originalism primarily entrenches past majorities, its claimed ''rope'' function (protecting fundamental law from transient majorities) would be re-evaluated as a ''snare'' (extracting from present majorities for the benefit of past ones or their contemporary proxies).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counter_majoritarian_justification_validity, conceptual, 'Validity of originalism''s counter-majoritarian justification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative interpretations structural (e.g., stare decisis, judicial appointments) or internalized (e.g., judges self-censoring non-originalist arguments)?',
    'Analysis of judicial opinions and legal scholarship over time, tracking the explicit rejection of non-originalist arguments versus the absence of such arguments. If suppression persists even with changes in judicial composition, it suggests internalized mechanisms.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, as judges carry the interpretive framework with them, making exit from originalism more difficult even if formal barriers are lowered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of interpretive alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_meaning__originalist_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_meaning__originalist_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_meaning__originalist_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_meaning__originalist_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_meaning__originalist_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_meaning__originalist_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_meaning__originalist_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_meaning__originalist_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_meaning__originalist_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_meaning__originalist_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_meaning__originalist_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_meaning__originalist_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_meaning__originalist_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_meaning__originalist_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_meaning__originalist_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_meaning__originalist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_meaning__originalist_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_meaning__originalist_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'us_constitution_meaning' kernel. Each reading represents a different structural claim about constitutional interpretation, with different ε values and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
