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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: US Constitution: Originalist Reading of Meaning
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of the US
 *   Constitution, where its meaning is fixed at the moment of ratification
 *   and judges are bound by historical public meaning. This is one reading of
 *   the 'us_constitution_meaning' kernel. The constraint operates as a
 *   Tangled Rope: it provides a coordination function (predictable
 *   interpretation) but also extracts from those whose claims lack historical
 *   support, requiring active enforcement to suppress alternative
 *   interpretive methodologies. The metrics reflect a growing assertiveness
 *   and enforcement of this reading over time.
 *
 * KEY AGENTS:
 *   - originalist_judges: Agenda setter (institutional/identity_locked) — enforces the reading
 *   - counter_majoritarian_advocates: Beneficiary (organized/mobile) — benefits from constrained contemporary majorities
 *   - rights_claimants_lacking_historical_support: Payer (powerless/trapped) — bears the cost of historical limitations
 *   - living_constitutionalist_judges: Payer (institutional/constrained) — suppressed by originalist dominance
 *   - originalist_legal_scholars: Beneficiary (powerful/mobile) — intellectual and career advancement
 *   - general_public: Observer (organized/constrained) — affected by rulings, may align or diverge from outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.65).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.78).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "US Constitution: Originalist Reading of Meaning").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, '26c5860e-5a88-497a-92e4-c29163895ba1').
narrative_ontology:cs_kernel_codification('26c5860e-5a88-497a-92e4-c29163895ba1', fixed_text).
narrative_ontology:cs_authority_grounding('26c5860e-5a88-497a-92e4-c29163895ba1', lineage).
narrative_ontology:cs_interpretation_layer_present('26c5860e-5a88-497a-92e4-c29163895ba1').
narrative_ontology:cs_reading_relation('26c5860e-5a88-497a-92e4-c29163895ba1', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('26c5860e-5a88-497a-92e4-c29163895ba1', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('26c5860e-5a88-497a-92e4-c29163895ba1', foundational, constitutional_meaning_is_fixed).
narrative_ontology:cs_axiom_status(constitutional_meaning_is_fixed, holdable).
narrative_ontology:cs_axiom_grounding('26c5860e-5a88-497a-92e4-c29163895ba1', constitutional_meaning_is_fixed, deontological).
narrative_ontology:cs_axiom('26c5860e-5a88-497a-92e4-c29163895ba1', foundational, judicial_role_is_to_discover_not_create_law).
narrative_ontology:cs_axiom_status(judicial_role_is_to_discover_not_create_law, holdable).
narrative_ontology:cs_axiom_grounding('26c5860e-5a88-497a-92e4-c29163895ba1', judicial_role_is_to_discover_not_create_law, deontological).
narrative_ontology:cs_reference_frame('26c5860e-5a88-497a-92e4-c29163895ba1', original_public_meaning_at_ratification).
narrative_ontology:cs_drift_state('26c5860e-5a88-497a-92e4-c29163895ba1', contemporary_legal_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('26c5860e-5a88-497a-92e4-c29163895ba1', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_legal_scholars).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_lacking_historical_support).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, living_constitutionalist_judges).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, judicial_restraint_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges who adhere to the originalist methodology, interpreting the Constitution's meaning as fixed at the time of its ratification or amendment. They actively enforce this interpretive constraint, often suppressing alternative readings in their rulings.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, identity_locked, national).

% Advocates who benefit from the originalist reading's ability to constrain contemporary majoritarian preferences, particularly when those preferences conflict with historical understandings of rights or governmental power. They use originalism to justify outcomes that might be unpopular today.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_advocates, beneficiary,
    organized, generational, mobile, national).

% Individuals or groups seeking to assert rights or legal protections that are not explicitly or implicitly supported by the historical public meaning of the Constitution at the time of its adoption. Their claims are often denied or weakened by originalist interpretations.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, rights_claimants_lacking_historical_support, payer,
    powerless, immediate, trapped, national).

% Judges who believe the Constitution's meaning can evolve to address contemporary societal needs and values. They find their interpretive methodology and desired outcomes suppressed by the dominance of originalist approaches in certain courts or legal discourse.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_judges, payer,
    institutional, generational, constrained, national).

% Academics and legal theorists who develop and promote originalist methodologies. Their intellectual work and careers are advanced by the adoption and enforcement of originalist interpretations in the judiciary.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_legal_scholars, beneficiary,
    powerful, generational, mobile, national).

% The citizenry whose lives are affected by constitutional rulings. While some may align with originalist outcomes, many experience the constraint as an abstract legal principle that can produce outcomes detached from contemporary societal norms or needs.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, general_public, observer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for constitutional interpretation, aiming to limit judicial discretion and ensure fidelity to the original intent or public meaning of the framers, thereby coordinating legal expectations across generations.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary judges and societal values to historical evidence and the framers' intent, effectively transferring power from present-day majorities to past ones. It also transfers the burden of proof for new rights claims to historical textual support.
% ABSENT_VOICES: Future generations and marginalized groups whose experiences and rights were not contemplated by the framers are structurally absent from the originalist interpretive process. They would argue for an evolving understanding of constitutional principles to ensure justice in contemporary society.
% DISAPPEARANCE_RATIONALE: If the originalist reading vanished overnight, constitutional interpretation would immediately shift towards more flexible methodologies. Judicial decisions would likely reflect contemporary values more directly, potentially leading to different outcomes on issues like privacy, equality, and governmental power. The legal landscape would be significantly reordered.
% FOUNDING_PROBLEM: The problem of judicial overreach and the desire to prevent judges from imposing their personal policy preferences under the guise of constitutional interpretation, ensuring that the Constitution remains a fixed, fundamental law rather than a malleable document.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and conservative legal organizations consistently attest that judicial overreach remains a live problem. While critics (e.g., living constitutionalists, some legal historians) dispute whether originalism truly solves this problem or merely substitutes one form of judicial activism for another, the concern about judicial discretion is widely acknowledged across the legal community, even by those who disagree with originalist solutions.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).

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
 *   The extractiveness (0.65) is substantial because the fixed historical meaning often conflicts with contemporary needs, forcing claimants to bear the cost of anachronistic interpretations. Suppression (0.78) is high due to the active judicial and scholarly efforts to delegitimize and exclude non-originalist arguments. The theater ratio (0.20) is low, indicating that while there's some performative aspect to historical inquiry, the core function of constraining interpretation is genuinely pursued. The rising trend in extractiveness and suppression reflects the increasing dominance and enforcement of originalist methodologies in the judiciary over the past decades.
 *
 * PERSPECTIVAL GAP:
 *   Originalist judges and scholars perceive this as a legitimate, even necessary, constraint that upholds the rule of law and prevents judicial activism. Rights claimants and living constitutionalist judges, however, experience it as an extractive and suppressive force that denies justice based on an outdated understanding of societal needs and rights. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges and counter-majoritarian advocates are beneficiaries (low d) as the constraint serves their interpretive and political goals. Rights claimants and living constitutionalist judges are targets (high d) as they bear the costs of this interpretive framework. The general public is more symmetric, experiencing both the stability and the limitations.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing judicial overreach) is still 'live' according to its proponents, preventing a clear mandatrophy resolution. However, the rising extractiveness and suppression suggest that while the founding problem may persist, the solution has accumulated significant costs for certain groups, indicating a potential drift towards a more extractive function than originally intended. The 'contested' status of the founding problem corroborates this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_determinacy,
    'Is the ''original public meaning'' of the Constitution genuinely discoverable and determinate, or is it inherently ambiguous and subject to contemporary interpretive choices?',
    'Extensive historical and linguistic analysis, coupled with meta-analysis of originalist scholarly disagreements. If originalist scholars consistently arrive at different ''original meanings'' for key provisions, it suggests indeterminacy.',
    'If indeterminate, the constraint''s claimed objectivity collapses, revealing a greater degree of judicial discretion and thus higher effective extraction from those whose claims are denied under the guise of ''original meaning''. This would shift the classification closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_determinacy, empirical, 'Ambiguity of original public meaning.').

omega_variable(
    originalism_vs_living_constitutionalism_framing,
    'Is the originalist reading a genuinely distinct interpretive methodology, or is it a rhetorical framing for a particular set of policy outcomes?',
    'Analysis of originalist judicial outcomes: if outcomes consistently align with a specific political agenda, even when historical evidence is weak or contested, it suggests a rhetorical framing. Compare with outcomes from living constitutionalist judges.',
    'If primarily a rhetorical framing, the constraint''s coordination function (predictable interpretation) is largely theatrical, and its true function is pure extraction, pushing it closer to a Snare. If a genuine methodology, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_vs_living_constitutionalism_framing, conceptual, 'Originalism as methodology vs. rhetorical framing.').

omega_variable(
    mandate_drift_or_capture,
    'Has the originalist reading''s mandate drifted from preventing judicial overreach to actively shaping policy outcomes, or has it been captured by specific political factions?',
    'Longitudinal study of judicial decisions and legal scholarship, tracking the evolution of arguments and outcomes. Look for shifts in emphasis from process (restraint) to substance (specific policy results).',
    'If the mandate has drifted or been captured, the constraint''s justification as a ''Rope'' (coordination) is undermined, and its classification as a ''Tangled Rope'' or even ''Snare'' becomes more robust, as the extraction becomes the primary, rather than secondary, function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_drift_or_capture, empirical, 'Drift or capture of originalist mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_meaning__originalist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(us_c_tr_t1985, us_constitution_meaning__originalist_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_meaning__originalist_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_meaning__originalist_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_meaning__originalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_meaning__originalist_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(us_c_be_t1985, us_constitution_meaning__originalist_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_meaning__originalist_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_meaning__originalist_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_meaning__originalist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_meaning__originalist_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(us_c_su_t1985, us_constitution_meaning__originalist_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_meaning__originalist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_meaning__originalist_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_meaning__originalist_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'us_constitution_meaning' kernel. Its fixed-meaning approach directly contrasts with the evolving meaning of living constitutionalism and the procedural focus of positivism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
