% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__expected_value_dominant, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: acceptable_risk_energy__expected_value_dominant
 *   human_readable: Expected-Value-Dominant Acceptable-Risk Criterion for Energy Pathways
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   Energy regulators govern pathway acceptability through a single
 *   criterion: aggregate expected harm, computed as mortality per
 *   terawatt-hour and summed across every energy pathway. Under this
 *   arrangement fossil combustion and mining deaths enter the ledger at full
 *   weight — they are the largest numbers in the table — while
 *   reactor-accident consequences are multiplied by their estimated
 *   probabilities before entry, and the fossil pathway is accordingly
 *   suppressed hardest. This file instantiates ONE reading —
 *   expected_value_dominant — of the contested kernel acceptable_risk_energy;
 *   the catastrophic_tail_dominant and option_value_preserving readings are
 *   separate constraints with their own victim sets and are not averaged into
 *   this one. The epsilon referent is the standing EV-governed arrangement
 *   itself, assessed by this reading's own lights: where the arrangement
 *   implements the reading faithfully the reading sees coordination, and
 *   where it deviates — uncompensated transition losses, unconsented tail
 *   concentration, menu control — the reading's own impartiality standard
 *   registers extraction. Claimed type and metrics are authored
 *   independently: the claim is tangled_rope because the structure carries
 *   both a genuine commensuration function and asymmetric extraction through
 *   the same table; the metrics describe how the arrangement actually
 *   operates.
 *
 * KEY AGENTS:
 *   - nuclear_industry_operators: Principal beneficiary (institutional/arbitrage) — the probability-discounting feature is the load-bearing wall of nuclear's social license
 *   - downwind_urban_populations: Secondary beneficiary with residual payer exposure (organized/constrained) — their deaths are the metric's central currency, counted at full weight
 *   - renewable_energy_developers: Beneficiary (powerful/mobile) — ranked benign by the same comparative table
 *   - fossil_fuel_workers: Primary payer (organized/trapped) — livelihoods closed by the same arithmetic that counts their occupational deaths precisely
 *   - fossil_dependent_regions: Payer (moderate/trapped) — regional economic dissolution is booked nowhere in the table
 *   - nuclear_host_communities: Concentrated tail-risk bearer (moderate/constrained) — catastrophic exposure divided by probability and spread over everyone
 *   - energy_regulator: Agenda setter (institutional/constrained) — administers the metric; the metric's authority and the office's authority are the same asset
 *   - catastrophic_tail_advocates: Excluded voice (organized/constrained) — their premise, that aggregation itself is the error, is inadmissible inside the framework
 *   - public_health_epidemiologists: Analytical observer (analytical/analytical) — supplies the mortality coefficients and sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.52).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.7).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.52).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected-Value-Dominant Acceptable-Risk Criterion for Energy Pathways").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, '30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c').
narrative_ontology:cs_kernel_codification('30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c', formalized).
narrative_ontology:cs_authority_grounding('30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c', expertise).
narrative_ontology:cs_interpretation_layer_present('30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c').
narrative_ontology:cs_reading_relation('30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c', acceptable_risk_energy__catastrophic_tail_dominant, forecloses).
narrative_ontology:cs_reading_relation('30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c', foundational, expected_harm_commensurable_across_pathways).
narrative_ontology:cs_axiom_status(expected_harm_commensurable_across_pathways, holdable).
narrative_ontology:cs_axiom_grounding('30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c', expected_harm_commensurable_across_pathways, empirically_contingent).
narrative_ontology:cs_axiom('30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c', foundational, equal_moral_weight_per_expected_death).
narrative_ontology:cs_axiom_status(equal_moral_weight_per_expected_death, holdable).
narrative_ontology:cs_axiom_grounding('30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c', equal_moral_weight_per_expected_death, deontological).
narrative_ontology:cs_axiom('30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c', secondary, pathway_suppression_proportional_to_mortality_rate).
narrative_ontology:cs_axiom_status(pathway_suppression_proportional_to_mortality_rate, holdable).
narrative_ontology:cs_axiom_grounding('30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c', pathway_suppression_proportional_to_mortality_rate, instrumental).
narrative_ontology:cs_reference_frame('30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c', probability_weighted_harm_aggregation).
narrative_ontology:cs_drift_state('30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c', post_fukushima_deep_uncertainty_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('30c4c9c5-c6ff-49fd-ae5c-e7b10acb029c', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, nuclear_industry_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, downwind_urban_populations).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, renewable_energy_developers).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_workers).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_dependent_regions).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, nuclear_host_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, downwind_urban_populations).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, expected_utility_aggregation_doctrine).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, statistical_life_comparability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Builds, licenses, and operates reactors and fuel-cycle facilities. Every periodic safety review runs pathway comparisons through the mortality-per-TWh table, and the table returns numbers favorable to relicensing and life extension because accident consequences are multiplied by their estimated probabilities. The industry funds much of the underlying probabilistic research and staffs the advisory committees that maintain the coefficients. Capital is internationally mobile; if one jurisdiction's framework turns unfavorable, fleets and vendors redeploy.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_industry_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Live in the airsheds of large fossil-fired metropolitan regions. When fossil generation retires on the table's schedule, their premature-mortality burden falls — they are the population whose counted deaths justify the retirements. During the transition they continue absorbing residual combustion emissions, and those deaths are entered at full weight. Moving to a cleaner airshed is possible for the affluent and slow for everyone else.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, downwind_urban_populations, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__expected_value_dominant, downwind_urban_populations, payer).

% Develop wind, solar, and hydro assets. The same comparative table ranks their pathways among the least harmful per unit delivered, which channels permitting priority, subsidy eligibility, and investor confidence toward them. Hydro operators carry dam-failure exposure that the same probability arithmetic smooths in their favor.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, renewable_energy_developers, beneficiary,
    powerful, biographical, mobile, global).

% Mine, drill, haul, and fire the plants the table prices out. Their occupational death rates were among the first coefficients entered into the mortality columns — the ledger knows exactly what their work costs. Closure schedules arrived with retraining commitments that lag the shutdowns; skills, mortgages, and family networks are anchored to towns built around the pithead or the plant gate.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_workers, payer,
    organized, immediate, trapped, regional).

% Counties and municipalities whose tax base, school funding, and hospital budgets ride on fossil payroll and severance revenue. The comparative table has no column for regional economic dissolution; transition funds exist but are partial, competitive, and discretionary. Relocation of an entire municipal economy is not an available move.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_dependent_regions, payer,
    moderate, generational, trapped, regional).

% Live and farm inside emergency-planning zones. They bear the concentrated version of a risk the national tables publish as a small average: the same expected-deaths figure that reads as negligible per capita nationally lands on them as a single site's worst day. Consent was solicited through hearings held after the governing arithmetic was already fixed; property values and insurance terms move with every distant accident. Long-hosting towns have grown up around the station — festivals, sponsorships, second-generation plant families — and leaving means forfeiting that accumulated life.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_host_communities, payer,
    moderate, generational, constrained, local).

% Sets and administers the acceptable-risk framework: maintains the mortality coefficients, runs the pathway comparisons, issues or denies licenses on the results. Staff careers are built on methodological neutrality; the framework's authority and the office's authority are the same asset, and revising the criterion would mean declaring decades of past licensing decisions biased. Rotating political principals inherit the machinery rather than rebuild it.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, energy_regulator, agenda_setter,
    institutional, generational, constrained, national).

% Post-accident citizen movements, precautionary-law jurists, and a minority of risk analysts who hold that some low-probability outcomes deserve categorical weight regardless of their expected value. They attend consultations and submit comments on coefficient values, but their actual objection — that multiplying catastrophe by improbability is itself the wrong move — has no slot in the framework's comment process. After major accidents their membership swells; between accidents it subsides.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, catastrophic_tail_advocates, excluded,
    organized, generational, constrained, global).

% Produce the burden-of-disease attributions and dose-response curves from which the mortality coefficients are built. They watch the framework use their numbers and can see which seats the arithmetic loads onto; they hold no licensing authority and publish critiques that the framework absorbs as coefficient updates.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, public_health_epidemiologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__expected_value_dominant, nuclear_industry_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the cross-pathway comparability problem: energy harms arrive in incommensurable forms — chronic airshed mortality, occupational death, rare reactor accidents, dam failure — and no allocation of the acceptable-risk budget is defensible without a common currency. The mortality-per-TWh table provides it, letting a regulator compare a coal fleet against a reactor fleet on one axis and defend the comparison publicly.
% TRANSFER_FUNCTION: Moves tolerable-risk allocation away from fossil pathways, whose mortality burden is entered at full weight and priced out of the acceptable set, toward nuclear and renewable pathways whose harms survive the probability arithmetic. Moves concentrated catastrophic exposure onto nuclear host communities as a discounted externality, and moves transition costs onto fossil workers and regions, while the mortality reductions accrue diffusely to urban air-breathing populations.
% ABSENT_VOICES: Catastrophic-tail advocates are present in the room but their premise is not: consultations admit comment on coefficient values, never on the choice to multiply catastrophe by probability. Future accident victims have no seat at all — the dead of an accident that has not happened cannot testify, and the framework speaks for them as decimals. Fossil-region residents without formal representation learn of closure schedules through employer announcements. All three stand outside the decision rule rather than inside it.
% DISAPPEARANCE_RATIONALE: If the expected-harm criterion vanished overnight, no common arbiter would remain for pathway acceptability: jurisdictions would sort by whichever risk language each holds — tail-weighting polities would suspend nuclear licensing outright, flexibility-first polities would refuse to concentrate on any single mix, and licensing pipelines everywhere would freeze mid-review pending a replacement criterion. The generation mix, the siting map, and the fossil retirement schedule would all rearrange around whatever filled the vacuum.
% FOUNDING_PROBLEM: Mid-century regulators faced successive visible catastrophes — lethal smogs, mining disasters, and then reactor-accident anxiety — with no defensible, capture-resistant way to answer 'how safe is safe enough?' across energy sources. Ad hoc, headline-driven judgment handed the question to whichever interest shouted loudest; the founding problem was to replace it with a common, quantitative, publicly auditable standard.
% FOUNDING_PROBLEM_CORROBORATION: Independent burden-of-disease epidemiology — global attributable-death studies published outside any energy beneficiary's orbit — attests that cross-pathway mortality comparison remains a real and unsolved governance need. Host-community litigation records and precautionary-jurist commentary attest, from outside the benefiting parties, that the metric's answer is contested even where the problem's reality is granted. No corroborating source claims the founding problem is dead.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__expected_value_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) because most of what the table does is transparent trade — counting fossil deaths fully is the opposite of hiding them — but two seats pay through the structure without comparable receipt: fossil workers and regions absorb closure costs the table books nowhere, and host communities absorb concentrated tail exposure the table divides by probability. Suppression is high (0.70) because persistence requires active coercive machinery — closure mandates, permitting denial, carbon-price ratchets — aimed squarely at the fossil pathway, plus procedural override of host-community objections; alternative pathways exist, so suppression is substantial rather than total. Theater is low-to-moderate (0.22): the comparative assessments do real allocative work, but a growing fraction of quantification legitimates positions already fixed by menu selection and coefficient provenance. Accessibility collapse is 0.62: inside the framework, granting the metric collapses the alternative risk languages almost mechanically — once the arithmetic is conceded the verdicts follow — while outside it the sibling readings remain live and reachable. Resistance is 0.58: fossil labor politics, host-community litigation, and post-accident movements mount sustained opposition that the framework absorbs rather than answers. All three tracked series share one time grid (points 0 to 30, step 6). Suppression_requirement is authored because the story specifically traces enforcement-capacity change — the closure-schedule and permitting machinery hardened monotonically over the interval — not merely shifting extraction. The trajectory is monotonic rather than cyclical: accident shocks spike resistance and briefly swell the excluded seat, but each shock is absorbed back into the frame as a probability update rather than a criterion change.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the regulator's chair the table is neutral method — the office's legitimacy and the criterion's authority are one asset, and staff professional identity fuses with methodological neutrality, so revising the criterion feels like confessing bias. From the host-community seat the same table is the machine that converted a single site's worst day into a national decimal; long-hosting towns carry relational identity fusion with the station, which softens local opposition but sharpens the sense of betrayal when accident news lands. From the fossil-worker seat the table is a ledger that knows their deaths to the third decimal and their town's death not at all. The two payer seats are structurally opposed — fossil workers need the pathway the table suppresses; host communities need scrutiny the table discounts — which blocks the coalition either would need, and that mutual blocking is itself a stability mechanism of the arrangement. Suppression here is predominantly structural (mandates, denials, price ratchets); a smaller internalized component lives in regulator-cadre belief that the metric simply is neutrality.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation without overrides. Nuclear operators and renewable developers are declared beneficiaries with mobile or arbitrage-grade exit, placing them near the beneficiary end; the operator sits closest to zero because the probability-discounting feature is the specific load-bearing element of its social license. Downwind urban populations are declared beneficiaries but carry residual payer exposure during the transition, so their derived d sits low but off zero. Fossil workers and fossil-dependent regions are declared victims with trapped exit — skills, mortgages, and municipal economies anchored in place — putting them near the full-target end despite organized power. Nuclear host communities are declared victims with constrained exit and local scope; their d is high, and the probability-discounting feature concentrates effective extraction on exactly this seat. The regulator holds the agenda_setter seat, which the engine classifies from its administrative position rather than from a beneficiary declaration. Catastrophic-tail advocates are authored as excluded — commentary-grade absence, not a correction-grade input.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the six-type apparatus this arrangement invites two symmetric mislabels: critics read the table as a snare (a neutral-sounding metric that is really cover for nuclear rent), and defenders read it as a rope (pure method, no politics). The structural data defeats both: the commensuration function is genuine and load-bearing — remove the common currency and no defensible allocation exists — while the same table's discounting and its missing columns move real costs onto seats that did not agree to them. That combination, with enforcement required and victims identifiable, is the tangled-rope signature. On genealogy: the founding problem (capture-resistant cross-pathway comparability) is still live, corroborated by independent epidemiology, so no mandatrophy resolution is declared; the status-live x world-rearranges pairing leaves the mismatch consumer correctly silent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_weighting_dispute,
    'This constraint is the expected_value_dominant reading of the acceptable_risk_energy kernel; how would the catastrophic_tail_dominant and option_value_preserving sibling readings restructure the victim set, the beneficiary set, and the resulting classification?',
    'Observe polities that switch governing readings after major accidents (post-accident phase-outs, moratoria, reversals) and re-run the structural inventory under each sibling''s weighting; the counterfactual victim and beneficiary sets are the resolution data.',
    'Under catastrophic_tail_dominant, nuclear host exposure enters at full categorical weight, nuclear operators flip from principal beneficiary to principal target, and the arrangement likely computes as extraction-dominated at the nuclear seats; under option_value_preserving, suppression diffuses across all pathways and the coordination share rises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_weighting_dispute, conceptual, 'Committer structure: one of three readings of the acceptable-risk kernel; sibling readings relocate the victim set.').

omega_variable(
    tail_discount_consent,
    'Do nuclear host communities actually accept having their catastrophic exposure divided by probability, or is the discounting imposed without consent — is probability-weighting neutral accounting or the extraction mechanism itself?',
    'Structured elicitation of host-community risk preferences against metric-implied weights; revealed preference through siting referenda and emergency-planning-zone property and insurance markets.',
    'Systematic rejection of the implied weights raises effective extraction at the host seat sharply and pushes that seat''s classification toward pure extraction; acceptance supports treating the discount as coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_discount_consent, empirical, 'Whether probability-discounting of catastrophic tail risk tracks host-community consent.').

omega_variable(
    harm_numerator_boundary,
    'The metric''s numerator is immediate human mortality per terawatt-hour; morbidity, ecosystem destruction, and slow cascade harms fall outside it — is that boundary a principled scope choice or an exclusion that flatters the pathways the metric favors?',
    'Re-run pathway rankings with expanded harm numerators (disability-adjusted life-year accounting, ecosystem-service accounting, full climate-cascade attribution) and compare rank stability.',
    'If rankings invert under expanded numerators, the narrow numerator is doing distributive work and base extractiveness is understated; stable rankings support the reading''s scope as principled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_numerator_boundary, conceptual, 'Whether the mortality-only numerator is principled scope or self-serving exclusion.').

omega_variable(
    option_set_authorship,
    'Expected-harm minimization is only defined against a menu of pathways — who authors the menu, and does menu control predetermine the metric''s verdicts?',
    'Audit the provenance of pathway option sets in regulatory impact assessments: which alternatives were modeled, which were excluded, and on whose submission.',
    'If option sets systematically exclude threatening alternatives such as demand-reduction portfolios, the metric''s neutrality is menu-deep and extraction rises across the payer seats; representative menus support the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_set_authorship, empirical, 'Whether control of the pathway menu predetermines the metric''s verdicts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acceptable_risk_evd_tr_t0, acceptable_risk_energy__expected_value_dominant, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(acceptable_risk_evd_tr_t0, observed).
narrative_ontology:measurement(acceptable_risk_evd_tr_t6, acceptable_risk_energy__expected_value_dominant, theater_ratio, 6, 0.13).
narrative_ontology:measurement_basis(acceptable_risk_evd_tr_t6, observed).
narrative_ontology:measurement(acceptable_risk_evd_tr_t12, acceptable_risk_energy__expected_value_dominant, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(acceptable_risk_evd_tr_t12, observed).
narrative_ontology:measurement(acceptable_risk_evd_tr_t18, acceptable_risk_energy__expected_value_dominant, theater_ratio, 18, 0.18).
narrative_ontology:measurement_basis(acceptable_risk_evd_tr_t18, observed).
narrative_ontology:measurement(acceptable_risk_evd_tr_t24, acceptable_risk_energy__expected_value_dominant, theater_ratio, 24, 0.2).
narrative_ontology:measurement_basis(acceptable_risk_evd_tr_t24, observed).
narrative_ontology:measurement(acceptable_risk_evd_tr_t30, acceptable_risk_energy__expected_value_dominant, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(acceptable_risk_evd_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(acceptable_risk_evd_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(acceptable_risk_evd_be_t0, observed).
narrative_ontology:measurement(acceptable_risk_evd_be_t6, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(acceptable_risk_evd_be_t6, observed).
narrative_ontology:measurement(acceptable_risk_evd_be_t12, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 12, 0.45).
narrative_ontology:measurement_basis(acceptable_risk_evd_be_t12, observed).
narrative_ontology:measurement(acceptable_risk_evd_be_t18, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 18, 0.47).
narrative_ontology:measurement_basis(acceptable_risk_evd_be_t18, observed).
narrative_ontology:measurement(acceptable_risk_evd_be_t24, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 24, 0.5).
narrative_ontology:measurement_basis(acceptable_risk_evd_be_t24, observed).
narrative_ontology:measurement(acceptable_risk_evd_be_t30, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(acceptable_risk_evd_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(acceptable_risk_evd_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(acceptable_risk_evd_su_t0, observed).
narrative_ontology:measurement(acceptable_risk_evd_su_t6, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 6, 0.56).
narrative_ontology:measurement_basis(acceptable_risk_evd_su_t6, observed).
narrative_ontology:measurement(acceptable_risk_evd_su_t12, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 12, 0.61).
narrative_ontology:measurement_basis(acceptable_risk_evd_su_t12, observed).
narrative_ontology:measurement(acceptable_risk_evd_su_t18, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 18, 0.65).
narrative_ontology:measurement_basis(acceptable_risk_evd_su_t18, observed).
narrative_ontology:measurement(acceptable_risk_evd_su_t24, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(acceptable_risk_evd_su_t24, observed).
narrative_ontology:measurement(acceptable_risk_evd_su_t30, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(acceptable_risk_evd_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, information_standard).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% The colloquial label 'acceptable risk in energy policy' covers three structurally distinct claims that share one kernel and diverge on the weighting of low-probability catastrophic outcomes. Decomposed per the epsilon-invariance principle: this file (expected_value_dominant) counts fossil deaths at full weight and discounts nuclear tails, making nuclear operators the principal beneficiaries and host communities the discounted payers; catastrophic_tail_dominant reverses that weighting; option_value_preserving refuses aggregation altogether. Each member links the others here; epsilon differs across members because the victim sets differ, not because one constraint is viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
