% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism Interpretive Regime
 *   domain: legal/political
 *
 * SUMMARY:
 *   This constraint instantiates the popular constitutionalism reading of the
 *   U.S. Constitution interpretive kernel. It contests the premise that
 *   judicial pronouncement is the sole or final source of constitutional
 *   meaning, locating interpretive authority instead in popular political
 *   movements, legislative action, and democratic contestation. The
 *   constraint is actively enforced through political mobilization,
 *   institutional resistance to judicial supremacy, and the cultural
 *   legitimation of lay constitutional interpretation. Key agents include
 *   popular movements and legislative majorities who gain interpretive
 *   authority, alongside minorities and stability-dependent actors who bear
 *   the costs of majoritarian constitutional determination, and judicial
 *   elites who lose finality.
 *
 * KEY AGENTS:
 *   - popular_movements: Primary beneficiary (organized/constrained) â gains interpretive authority through mobilization
 *   - legislative_majorities: Primary beneficiary and gain capturer (institutional/constrained) â accrues formal constitutional authority and reduced judicial override
 *   - anti_elitist_claimants: Secondary beneficiary (moderate/mobile) â validates lay constitutional interpretation against expert supremacy
 *   - judicial_finality_advocates: Primary payer (institutional/constrained) â loses exclusive interpretive authority and institutional supremacy
 *   - minorities_requiring_counter_majoritarian_protection: Primary target (powerless/trapped) â bears risk of majoritarian override without judicial shield
 *   - stability_dependent_actors: Secondary payer (organized/constrained) â bears costs of constitutional uncertainty
 *   - constitutional_scholars: Analytical observer (analytical/analytical) â maps the interpretive contest without stakes in the outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.62).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.58).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism Interpretive Regime").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, '4cbf33a8-bc45-4b3b-bf17-5237304a7b3b').
narrative_ontology:cs_kernel_codification('4cbf33a8-bc45-4b3b-bf17-5237304a7b3b', fixed_text).
narrative_ontology:cs_authority_grounding('4cbf33a8-bc45-4b3b-bf17-5237304a7b3b', distributed).
narrative_ontology:cs_reading_relation('4cbf33a8-bc45-4b3b-bf17-5237304a7b3b', us_constitution_interpretive__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('4cbf33a8-bc45-4b3b-bf17-5237304a7b3b', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('4cbf33a8-bc45-4b3b-bf17-5237304a7b3b', foundational, constitutional_meaning_emerges_from_political_struggle).
narrative_ontology:cs_axiom_status(constitutional_meaning_emerges_from_political_struggle, holdable).
narrative_ontology:cs_axiom_grounding('4cbf33a8-bc45-4b3b-bf17-5237304a7b3b', constitutional_meaning_emerges_from_political_struggle, deontological).
narrative_ontology:cs_axiom('4cbf33a8-bc45-4b3b-bf17-5237304a7b3b', secondary, judicial_review_requires_popular_ratification).
narrative_ontology:cs_axiom_status(judicial_review_requires_popular_ratification, holdable).
narrative_ontology:cs_axiom_grounding('4cbf33a8-bc45-4b3b-bf17-5237304a7b3b', judicial_review_requires_popular_ratification, conventional).
narrative_ontology:cs_reference_frame('4cbf33a8-bc45-4b3b-bf17-5237304a7b3b', popular_sovereignty_framework).
narrative_ontology:cs_drift_state('4cbf33a8-bc45-4b3b-bf17-5237304a7b3b', contemporary_judicial_supremacy_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4cbf33a8-bc45-4b3b-bf17-5237304a7b3b', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, minorities_requiring_counter_majoritarian_protection).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, stability_dependent_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim authority to shape constitutional meaning through mobilization, protest, and political contestation rather than litigation. Their constitutional claims gain traction when they can marshal majoritarian support or influence legislative and executive action.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements, beneficiary,
    organized, generational, constrained, national).

% Gain interpretive authority that would otherwise reside with courts; can advance constitutional visions through legislation and political action with reduced expectation of judicial override. Capture the formal institutional gains from distributed interpretive authority.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, generational, constrained, national).

% Individuals and groups who challenge elite legal expertise, asserting that ordinary citizens and political actors possess legitimate constitutional interpretive authority. Benefit from an interpretive regime that validates lay constitutionalism.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants, beneficiary,
    moderate, biographical, mobile, national).

% Judges, courts, and legal elites who assert that judicial interpretation supplies final, authoritative constitutional meaning. Lose exclusive interpretive authority when constitutional meaning is determined through political struggle; their professional role and institutional supremacy are contested.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    institutional, generational, constrained, national).

% Discrete and insular minorities who rely on judicial review to protect rights against majoritarian override. Bear the cost of an interpretive regime that empowers popular majorities to determine constitutional meaning, potentially diluting counter-majoritarian safeguards.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, minorities_requiring_counter_majoritarian_protection, payer,
    powerless, generational, trapped, national).

% Businesses, administrative agencies, and institutions requiring predictable legal settlement. Bear the costs of constitutional uncertainty when meaning is determined by shifting political struggle rather than judicial precedent and reasoned elaboration.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, stability_dependent_actors, payer,
    organized, biographical, constrained, national).

% Academic observers who analyze competing constitutional theories and map the distribution of interpretive authority. Neither collect gains nor bear costs from the regime's operation; they document and evaluate the structural contest.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Democratic self-government in constitutional interpretation: coordinates how a diverse polity generates shared constitutional meaning through participatory political struggle rather than insulating fundamental law from popular contestation.
% TRANSFER_FUNCTION: Moves constitutional interpretive authority from the judiciary to popular movements, legislative majorities, and democratic contestation; transfers the power to determine constitutional meaning from insulated legal reasoning to political mobilization.
% ABSENT_VOICES: Minorities lacking majoritarian support and legal elites committed to stare decisis are partially excluded from interpretive authority; their claims must win popular political traction rather than judicial protection to gain constitutional standing.
% DISAPPEARANCE_RATIONALE: If this interpretive regime vanished and judicial supremacy became absolute, constitutional meaning would be determined exclusively by courts; popular movements would lose their claimed authority to shape constitutional understanding, legislative majorities would face stronger judicial override, and constitutional politics would shift from popular contestation to litigation-centered interpretation.
% FOUNDING_PROBLEM: Judicial supremacy and elite constitutional interpretation became disconnected from popular democratic will; ordinary citizens and their elected representatives lacked effective voice in shaping constitutional meaning.
% FOUNDING_PROBLEM_CORROBORATION: Popular constitutionalist scholars attest the problem from within the tradition. Federal judges and originalist scholars dispute the diagnosis, arguing judicial review maintains democratic legitimacy through representation reinforcement. Independent legal historians and political scientists corroborate that popular constitutional moments have occurred, but disagree on whether judicial supremacy was ever genuinely disconnected from popular will.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-to-high because the constraint systematically transfers interpretive authority to majoritarian actors at the expense of minorities and settled expectations, even while it enables genuine democratic participation. Suppression (0.58) reflects the active marginalization of judicial-finality alternatives through political contestation and institutional resistance, though courts persist as an alternative forum. Theater ratio (0.42) captures the performative dimension of constitutional politics â rallies, popular sovereignty rhetoric, and claims about 'the people' â which exceeds the functional institutional change achieved. Accessibility collapse (0.48) is moderate because judicial supremacy remains an intellectually and institutionally live alternative despite popular constitutionalist pressure. Resistance (0.72) is high because judicial elites, legal academia, and minority-rights advocates actively contest the erosion of judicial finality.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (popular movements, legislative majorities) experience this constraint as enabling genuine democratic self-government and corrective majoritarianism. The payer seats experience it as the erosion of rights-protective settlement and predictable legal order. The engine computes this divergence from structural data: beneficiaries with constrained or mobile exit face low effective extraction (subsidized by the constraint), while trapped minorities and constrained judicial actors face amplified extraction. The perspectival gap is the central datum the corpus exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (popular_movements, legislative_majorities, anti_elitist_claimants) sit near the full-beneficiary end: the constraint subsidizes their interpretive authority and political agency. Legislative majorities in particular capture the formal gains. Victims (judicial_finality_advocates, minorities_requiring_counter_majoritarian_protection, stability_dependent_actors) sit near the full-target end: minorities are trapped and powerless, experiencing maximal directionality, while judicial advocates and stability-dependent actors are institutional or organized but constrained, experiencing moderate-to-high directionality. No overrides are required because beneficiary/victim declarations plus exit options produce accurate structural d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function â democratic self-government and popular sovereignty â prevents snare classification despite real extraction from minorities. The presence of a genuine collective-action problem (how does a polity generate shared constitutional meaning without elite capture) supplies the coordination strand. Conversely, the asymmetric extraction from trapped minorities and the suppression of judicial-finality alternatives prevent rope classification despite the democratic benefits. The tangled_rope classification captures both strands: it is neither pure coordination nor pure extraction, but a hybrid sustained by active enforcement of the interpretive regime.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does this constraint instantiate a genuine distribution of interpretive authority to popular movements, or does it function as majoritarian legitimation for outcomes that ordinary politics would produce regardless?',
    'Comparative analysis of constitutional regimes with strong judicial supremacy versus popular constitutionalist arrangements, measuring whether popular movements gain substantively different constitutional outcomes under each regime.',
    'If outcomes are indistinguishable, the coordination function is cover and the constraint reads as snare; if popular movements gain distinct authority, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether popular constitutionalism is genuine authority distribution or majoritarian cover').

omega_variable(
    majoritarian_minority_protection,
    'Does popular constitutionalism structurally advantage organized majorities at the expense of discrete and insular minorities, or can popular movements themselves protect minority interests without judicial mediation?',
    'Historical case studies of popular constitutional moments examining whether minority protections advanced or retreated during periods of high popular constitutionalist activity.',
    'If minorities consistently lose protections under popular constitutionalism, the victim set expands and the extraction component dominates; if popular movements sometimes advance minority claims, the coordination-extraction balance is more even.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_minority_protection, empirical, 'Whether popular constitutionalism protects or endangers minorities').

omega_variable(
    cs_framing_underdetermination,
    'Should the commitment system be framed as the interpretive authority structure (who decides) or the constitutional text itself (what is decided)?',
    'Evaluate whether changing the kernel from popular interpretive authority to the constitutional text as popularly ratified changes the beneficiary-victim structure.',
    'An authority-structure framing yields tangled_rope; a text-framing might yield rope or mountain depending on whether the text is treated as fixed or evolving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framing of the commitment system kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(popcon_tr_t0, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(popcon_tr_t10, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(popcon_tr_t20, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(popcon_tr_t30, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(popcon_tr_t40, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(popcon_tr_t50, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(popcon_be_t0, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(popcon_be_t10, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(popcon_be_t20, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(popcon_be_t30, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(popcon_be_t40, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(popcon_be_t50, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(popcon_su_t0, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(popcon_su_t10, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(popcon_su_t20, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(popcon_su_t30, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(popcon_su_t40, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(popcon_su_t50, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, living_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the us_constitution_interpretive kernel. The kernel decomposes into three structurally distinct constraints because the epsilon values and beneficiary/victim structures differ across readings. Originalist reading treats meaning as fixed with negligible extraction; living constitution reading treats meaning as evolving through judicial reason; popular constitutionalism reading treats meaning as emerging from political struggle with asymmetric majoritarian costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
