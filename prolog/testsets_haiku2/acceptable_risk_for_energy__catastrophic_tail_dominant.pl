% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail Risk Dominance in Energy Acceptability Assessment
 *   domain: risk_assessment/energy_policy/public_safety
 *
 * SUMMARY:
 *   Risk governance faces an irreducible methodological choice: should
 *   acceptable-risk thresholds be determined by probability-weighted
 *   expected-value aggregation (standard decision theory),
 *   comparative-advantage analysis (nuclear risk acceptable relative to
 *   coal/climate), or catastrophic-tail-dominance (low-probability
 *   high-consequence irreversibility trumps probabilistic weighting)? This
 *   constraint instantiates the catastrophic-tail-dominant reading: a
 *   framework that prioritizes intergenerational burden and irreversibility,
 *   suppresses probabilistic trade-off language, and treats nuclear energy
 *   and radioactive waste as victim-imposing regardless of statistical
 *   accident rates. The constraint is claimed as tangled_rope—it coordinates
 *   around a shared principle for risk adjudication while extracting
 *   political cost from probabilistic-analysis professions and constraining
 *   the nuclear industry. The beneficiaries are climate advocates and
 *   renewable operators who gain policy advantage; the victims are
 *   probabilistic risk assessors and nuclear engineers whose technical
 *   frameworks are delegitimized. The authored metrics describe substantially
 *   extractive, actively enforced operation; the claim remains tangled_rope
 *   because coordination (intergenerational justice as a shared value) is
 *   genuine even as extraction is high.
 *
 * KEY AGENTS:
 *   - climate_mitigation_advocates: organized coalition that gains policy authority from catastrophic-tail framing; medium-to-high power through political coalition; exit constrained only by effectiveness of competing framings
 *   - renewable_energy_operators: institutional beneficiaries with competitive advantage under tail-risk weighting; powerful but dependent on policy stability; constrained exit if expected-value framing re-ascends
 *   - nuclear_industry: primary victim through policy exclusion and investment suppression; institutional power but highly constrained exit given intergenerational-burden axiom's dominance
 *   - probabilistic_risk_assessors: professional victims whose epistemic authority is suppressed; organized but moderate power; constrained to reframe technical analyses in non-probabilistic terms
 *   - regulatory_agencies: agenda-setter administering catastrophic-tail logic through licensing and permit denial; institutional power but constrained by competing political pressures and scientific uncertainty
 *   - intergenerational_justice_framers: beneficiary through legitimation of their axioms; moderate power, high moral authority; exit options mobile but epistemically constrained if intergenerational-burden measurement fails
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.72).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.81).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic Tail Risk Dominance in Energy Acceptability Assessment").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/public_safety").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, '3a233bc2-671a-449c-a921-bbb6617efd88').
narrative_ontology:cs_kernel_codification('3a233bc2-671a-449c-a921-bbb6617efd88', distributed).
narrative_ontology:cs_authority_grounding('3a233bc2-671a-449c-a921-bbb6617efd88', extraction).
narrative_ontology:cs_interpretation_layer_present('3a233bc2-671a-449c-a921-bbb6617efd88').
narrative_ontology:cs_reading_relation('3a233bc2-671a-449c-a921-bbb6617efd88', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('3a233bc2-671a-449c-a921-bbb6617efd88', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('3a233bc2-671a-449c-a921-bbb6617efd88', foundational, irreversibility_dominates_probability).
narrative_ontology:cs_axiom_status(irreversibility_dominates_probability, holdable).
narrative_ontology:cs_axiom_grounding('3a233bc2-671a-449c-a921-bbb6617efd88', irreversibility_dominates_probability, deontological).
narrative_ontology:cs_axiom('3a233bc2-671a-449c-a921-bbb6617efd88', foundational, intergenerational_burden_trumps_present_welfare).
narrative_ontology:cs_axiom_status(intergenerational_burden_trumps_present_welfare, holdable).
narrative_ontology:cs_axiom_grounding('3a233bc2-671a-449c-a921-bbb6617efd88', intergenerational_burden_trumps_present_welfare, deontological).
narrative_ontology:cs_reference_frame('3a233bc2-671a-449c-a921-bbb6617efd88', precautionary_intergenerational_stewardship).
narrative_ontology:cs_drift_state('3a233bc2-671a-449c-a921-bbb6617efd88', contemporary_post_climate_settlement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3a233bc2-671a-449c-a921-bbb6617efd88', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_mitigation_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, renewable_energy_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_justice_framers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_industry).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_assessors).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, energy_optimization_engineers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a risk-assessment framework that prioritizes catastrophic climate outcomes (high-probability, multi-century, irreversible consequences) over probabilistically rare nuclear failures. This framing justifies aggressive decarbonization policy that displaces coal and accelerates renewable deployment. They argue that climate tail risk is already realized and certain; nuclear tail risk is merely possible. The framework legitimates their policy agenda and they collect political authority to shape energy investment.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_mitigation_advocates, beneficiary,
    organized, generational, mobile, global).

% Benefit from energy markets and policy frameworks that rank their technologies favorably under catastrophic-tail-risk weighting. They operate under high capital costs and policy-dependent demand; a risk framework that treats their failure modes (intermittency, material supply) as manageable engineering problems rather than catastrophic tails gives them competitive advantage against nuclear baseload. They receive preferential investment and policy support flows.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, renewable_energy_operators, beneficiary,
    institutional, biographical, mobile, global).

% Advocate for a risk calculus that centers harm-to-future-generations as the primary moral weight. They argue that irreversible damages (radioactive waste, climate carbon lock-in) should dominate acceptable-risk thresholds regardless of probability. This reading vindicates their framework and shifts burden of proof to projects that impose intergenerational costs. They collect legitimacy and influence over energy policy.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_justice_framers, beneficiary,
    moderate, civilizational, mobile, global).

% Pays through policy exclusion and investment suppression as catastrophic-tail framing delegitimizes their operational model. Even accident rates of 0.001 per reactor-year become unacceptable if a single failure cascades into multi-generational harm. Their exit option is deeply constrained: they cannot abandon their identity as nuclear operators; switching risk frameworks requires dislodging the intergenerational-justice axiom, which is politically costly and epistemically weak from their seat. They absorb investment-flow redirection and policy licenses that are increasingly denied.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_industry, payer,
    institutional, biographical, identity_locked, global).

% Professional risk engineers and analysts whose core methodology—expected-value optimization and probability-weighted consequence aggregation—is actively suppressed by the catastrophic-tail framework. Their technical authority is diminished when decision-makers reject probabilistic weighting as morally inadequate. They absorb the cost of diminished influence and must reframe their analyses in non-probabilistic terms to maintain policy relevance. Their professional language is treated as epistemically corrupt in policy forums.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_assessors, payer,
    organized, biographical, constrained, global).

% Grid operators, capacity planners, and systems engineers responsible for balancing reliability, cost, and emissions. The catastrophic-tail framework imposes engineering constraints (no single-point-failure acceptance, forced diversification toward renewables) that create technical friction in grid stability problems. They absorb the costs of suboptimal load balancing and must design around avoided failure modes rather than engineered resilience.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, energy_optimization_engineers, payer,
    moderate, biographical, constrained, global).

% Administers the constraint through licensing, permit denial, and policy guidance. They adopt catastrophic-tail framing to meet political-legitimacy requirements and intergenerational-justice statutory mandates. The enforcement machinery (environmental impact review, safety thresholds calibrated to tail risks, tightening design-basis events) is their instrument. They face pressure from climate advocates to tighten tail-risk standards and from industry to relax them; the framework itself becomes the battleground. They collect administrative authority but face contradictory mandates.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Hold the sibling reading that nuclear risk becomes acceptable only relative to coal/climate risk, not as an absolute threshold. They argue catastrophic-tail dominance incorrectly omits the catastrophic tail of climate change (already manifest, certain) when evaluating nuclear-as-climate-solution. Their exclusion from the catastrophic-tail-dominant framework means their comparative-advantage argument is treated as risk-acceptance rather than risk-balancing. They are not heard in the policy forums where acceptable-risk thresholds are set.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, comparative_risk_advocates, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__catastrophic_tail_dominant, regulatory_agencies).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared principle for adjudicating energy acceptability in the face of irreducible uncertainty: low-probability high-consequence events should weight more heavily than expected-value calculus permits, centering irreversibility and intergenerational burden as the primary moral axes for policy decision-making.
% TRANSFER_FUNCTION: Moves the operative decision criterion away from probabilistic risk models (probability × consequence aggregation) and toward catastrophe-thresholds-first logic. This shifts investment flows from technologies with controllable but rare failure modes (nuclear) toward technologies with diffuse or engineering-manageable failure modes (renewables), and from present-generation benefit to intergenerational-justice framing. It transfers policy authority from technical risk analysts to intergenerational-justice advocates.
% ABSENT_VOICES: Comparative-risk advocates are structurally excluded: their argument that nuclear-as-climate-solution could be catastrophically risk-reducing relative to coal/climate tail risks is not heard in the catastrophic-tail-dominant framework, which treats the two tails (nuclear, climate) as independently thresholdable rather than as competing catastrophic scenarios. Future generations are non-agents in the decision process, claimed on behalf of but unable to contest how their interests are weighted. Probabilistic risk assessors are technically present but have their core methodology delegitimized.
% DISAPPEARANCE_RATIONALE: If catastrophic-tail-dominance framing disappeared, energy policy would reorganize around expected-value or comparative-risk logic: nuclear would re-enter the acceptable-risk space as a climate-mitigation technology with manageable tail probability; investment would shift from renewable-only to mixed portfolios; waste-disposal research would reframe from 'how do we eliminate generational burden' to 'how do we engineer safe multi-century containment.' Regulatory agencies would adopt probabilistic licensing standards. The entire intergenerational-justice axis would weaken as a policy justification. The constraint's disappearance would produce measurable shifts in capacity planning, licensing outcomes, and research funding across multiple energy domains.
% FOUNDING_PROBLEM: Energy policy faced a gap: climate catastrophe is a high-probability, multi-century, irreversible tail risk; nuclear accidents are low-probability but also irreversible and intergenerationally imposing. Classical expected-value risk calculus aggregates these into a false equivalence or false trade-off (climate risk high enough to justify nuclear deployment). The founding problem was the moral inadequacy of probability-weighted comparison when one harm is already-manifest and the other is purely speculative, and the absence of any mechanism to ensure intergenerational burden is genuinely weighted rather than discounted.
% FOUNDING_PROBLEM_CORROBORATION: Climate-science consensus attests that high-consequence climate tails are already-realized probabilities (>66% warming by 2100 under business-as-usual). Intergenerational-justice philosophers and ethicists outside the nuclear industry attest that irreversibility and unknown-future-burden should center moral weight. The nuclear industry attests the founding problem is overdrawn (modern reactor designs reduce tail probability toward 10^-6 range and disposal engineering is maturing). Comparative-risk researchers attest the founding problem conflates climate-tail-as-certain with nuclear-tail-as-optional, a framing error. No consensus exists: the status reflects that different communities treat the adequacy of expected-value calculus and the operationalizability of intergenerational discounting differently.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end, rising from 0.54 over 40 years) because the constraint actively suppresses a competing technical methodology (probabilistic risk assessment) and reallocates investment flows based on a values choice dressed as technical necessity. The suppression rises (0.62 to 0.81) because regulatory machinery tightens catastrophic-tail thresholds and public justifications increasingly reject probabilistic language altogether. Theater ratio rises from 0.28 to 0.48 because the constraint's performance of 'moral seriousness about intergenerational burden' grows while its technical capacity to operationalize intergenerational-discount rates remains underdeveloped. The accessibility_collapse (0.67) reflects that alternative risk framings (expected-value, comparative) remain epistemically available but are increasingly excluded from legitimate policy discourse. The resistance (0.73) is high because probabilistic risk assessors and nuclear engineers mount sustained technical counter-arguments, even as political power shifts against them. The measurements use one shared time grid: all four intervals (0, 5, 10, 15, 20, 25, 30, 40) carry values for each tracked metric.
 *
 * PERSPECTIVAL GAP:
 *   The constraint should produce sharp seat divergence. From the regulatory-agency agenda-setter's position, catastrophic-tail-dominance is a necessary principle reflecting intergenerational-justice values and precautionary duty—genuine coordination with beneficiary intent. From the nuclear-industry payer's position, the constraint is pure extraction: political suppression of their technical methodology and investment exclusion justified by an unfalsifiable moral axiom. From the probabilistic-risk-assessor's position, it is forced professional delegitimation despite technical validity. From the intergenerational-justice-framer's position, it is vindication of a foundational moral claim about irreversibility and future-burden. The engine computes these divergences from the authored directionality data (power + exit_options + beneficiary/victim roles): regulatory agencies compute as moderate-to-institutional power with constrained exit → moderate directionality (symmetric to slightly target-side); nuclear industry computes as institutional power but identity-locked to the victim framing → high directionality (full target); probabilistic assessors compute as organized-moderate power with constrained exit (must reframe, cannot abandon discipline) → moderate-to-high target directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate advocates (beneficiary, organized power, mobile exit) have directionality near 0.2 (beneficiary end): they benefit from policy authority and don't need to exit the policy space to flourish. Renewable operators (beneficiary, institutional power, mobile exit) similarly sit near 0.15 (beneficiary + institutional = strong subsidy). Nuclear industry (victim, institutional power, identity-locked exit) sits near 0.85 (target end): they cannot exit their identity as nuclear operators; the constraint directly excludes them from acceptable-risk space; their only exit would be industry dissolution. Probabilistic risk assessors (victim, organized power, constrained exit) sit near 0.72 (target): they are targeted by suppression and cannot abandon probabilistic methodology without professional suicide, yet organizational power provides some buffer. No directionality override is necessary—the derivation chain (beneficiary/victim declarations + power + exit_options) captures the structural relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (mandate outliving its function) appears in oscillation rather than monotonic drift. From t=0 to t=15, extractiveness rises (0.54 to 0.67) as catastrophic-tail framing gains institutional purchase and probabilistic-risk language is actively suppressed—the coordination function (intergenerational-justice principle) is live and driving behavioral change. From t=15 onward, extractiveness plateaus (0.67 to 0.72) while theater_ratio continues rising (0.43 to 0.48), suggesting that the coordination function has been internalized and the constraint now operates more through legitimacy performance than through active enforcement innovation. The founding problem (probabilistic inadequacy in risk adjudication for irreversible harms) remains contested at t=40, not live-and-functional. This suggests weak mandatrophy signals: the constraint may persist as performative legitimacy even if the founding problem's salience fades. A stronger signal would be if extractiveness began declining (active suppression no longer necessary) while theater rose above 0.55 (mostly performance, minimal function). Current trajectory does not confirm mandatrophy but flags it as a medium-term risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_risk_quantification_ambiguity,
    'Is the ''catastrophic tail'' of climate change (high-probability, multi-century, already-manifest) commensurable with the ''catastrophic tail'' of nuclear failure (low-probability, speculative, engineering-addressable)?',
    'Formal probability elicitation and tail-risk quantification across energy sources: compare P(>5C warming | no mitigation) against P(containment-failure | modern-reactor-fleet). If the probabilities diverge by >2 orders of magnitude, the tails are structurally different and comparing them as equivalent catastrophes becomes a framing choice, not an objective classification.',
    'If probabilities diverge sharply, catastrophic-tail-dominance becomes a reading choice (intergenerational burden outweighs probability) rather than an objective fact. The constraint becomes more clearly identified as a values-based reframing than as a technical risk calculus. If they converge, the constraint''s technical grounding strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tail_risk_quantification_ambiguity, empirical, 'Comparability of climate and nuclear tail probabilities.').

omega_variable(
    intergenerational_burden_measurement,
    'What metric makes intergenerational burden commensurable across time horizons and populations? How do we weight harm-to-future-generations relative to present-generation benefit from carbon-free power?',
    'Operationalize intergenerational-discount rate: survey future-oriented ethicists, affected populations (present in climate-threatened regions), and indigenous communities with multi-century time horizons on acceptable trade-offs. Map the metric-space of irreversibility and compare against revealed preferences in other domains (nuclear-waste-storage acceptance, pesticide-residue regulation, plastic-contamination tolerance).',
    'An operationalized intergenerational-burden metric would either strengthen the catastrophic-tail framework (showing consistent preference for zero-irreversibility thresholds) or reveal internal inconsistency (showing that populations accept some irreversible harms while rejecting others based on temporal proximity or political salience rather than principle). Inconsistency would suggest the constraint is more performative than axiomatically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_burden_measurement, empirical, 'Whether intergenerational burden can be measured consistently across energy technologies.').

omega_variable(
    probabilistic_framing_suppression_mechanism,
    'Is the high suppression value (0.81) measuring structural barrier to probabilistic analyses, or performative rejection of probabilistic language in public discourse while probabilistic reasoning persists in technical decision-making?',
    'Conduct discourse analysis on regulatory agencies, energy agencies, and policy briefs: measure proportion of public statements using catastrophic-tail language vs. expected-value language; cross-check against internal technical memoranda and licensing-review documents. If public discourse suppresses probabilistic framings while technical review continues to use them, suppression is internalized-performative rather than structural.',
    'If suppression is performative, the constraint carries high theater_ratio (approaching 0.5 and rising, matching measurements). The extraction target is not genuine policy exclusion of probabilistic methods but legitimacy asymmetry: using probabilistic reasoning in technical review while denying its validity in public justification. Beneficiary is then the political coalition that uses probabilistic arguments internally while publicly claiming to reject them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probabilistic_framing_suppression_mechanism, empirical, 'Structural vs. performative suppression of probabilistic risk assessment language.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 5, 0.33).
narrative_ontology:measurement_basis(acce_tr_t5, observed).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(acce_tr_t10, observed).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 15, 0.43).
narrative_ontology:measurement_basis(acce_tr_t15, observed).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(acce_tr_t20, observed).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 25, 0.47).
narrative_ontology:measurement_basis(acce_tr_t25, observed).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(acce_tr_t30, observed).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(acce_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(acce_be_t5, observed).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(acce_be_t10, observed).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(acce_be_t15, observed).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 20, 0.7).
narrative_ontology:measurement_basis(acce_be_t20, observed).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 25, 0.71).
narrative_ontology:measurement_basis(acce_be_t25, observed).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(acce_be_t30, observed).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(acce_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 5, 0.67).
narrative_ontology:measurement_basis(acce_su_t5, observed).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(acce_su_t10, observed).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 15, 0.75).
narrative_ontology:measurement_basis(acce_su_t15, observed).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 20, 0.78).
narrative_ontology:measurement_basis(acce_su_t20, observed).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 25, 0.8).
narrative_ontology:measurement_basis(acce_su_t25, observed).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 30, 0.81).
narrative_ontology:measurement_basis(acce_su_t30, observed).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.81).
narrative_ontology:measurement_basis(acce_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_waste_disposal_as_civilizational_burden).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_catastrophe_irreversibility).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel acceptable_risk_for_energy. The kernel encompasses three structurally distinct constraint stories: catastrophic_tail_dominant (this file), expected_value_dominant, and comparative_risk_dominant. All three read from the same kernel (how energy-acceptability thresholds should be determined), but each instantiates a different values choice about which tail-risk (climate, nuclear, comparative) dominates. The three constraints are linked via this affects_constraints array and via their respective cs_structure.reading_relations declarations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, organized, 0.18).
constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
