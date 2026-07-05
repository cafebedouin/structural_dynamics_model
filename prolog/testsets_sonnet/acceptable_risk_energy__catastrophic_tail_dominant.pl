% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__catastrophic_tail_dominant, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic-Tail-Dominant Acceptable Risk Standard (Energy Policy)
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint is the catastrophic-tail-dominant reading of the
 *   acceptable-risk-in-energy kernel: it weights low-probability,
 *   high-magnitude, geographically concentrated failure modes (principally
 *   nuclear accidents) as effectively infinite-cost outcomes to be avoided
 *   regardless of their contribution to aggregate expected harm, while
 *   treating high-probability, low-magnitude, geographically distributed
 *   harms (principally fossil fuel combustion mortality and climate damage)
 *   as an acceptable, discountable background cost because they are
 *   statistically diffuse and appear 'reversible' in a way a meltdown does
 *   not. The regime that instantiates this reading — nuclear licensing
 *   regulation, siting law, insurance liability caps, environmental review
 *   processes — produces a standard under which vastly more people die from
 *   the accepted pathway (fossil generation) than from the suppressed pathway
 *   (nuclear), yet the suppressed pathway is treated as the greater wrong.
 *   This is a coherent, structurally distinct constraint from a sibling
 *   reading (expected_value_dominant) that would weight all pathways by
 *   mortality-per-TWh, and from a further sibling (option_value_preserving)
 *   that would justify diversification on uncertainty-preservation grounds
 *   rather than tail-avoidance grounds. Per the ε-invariance principle, these
 *   are not the same constraint measured three ways — they are three
 *   constraints, linked here via network.affects_constraints, each with its
 *   own beneficiary/victim structure and its own ε.
 *
 * KEY AGENTS:
 *   - incumbent_fossil_fuel_operators: primary beneficiary (powerful/arbitrage) — benefit from asymmetric scrutiny that spares their chronic emissions
 *   - anti_nuclear_advocacy_organizations: beneficiary and co-agenda-setter (organized/mobile) — embed catastrophic framing into regulatory review
 *   - risk_averse_regulators: agenda_setter (institutional/constrained) — administer asymmetric licensing standards, personally exposed to catastrophic-failure blame
 *   - populations_near_fossil_fuel_plants: primary payer (powerless/trapped) — bear statistically certain but individually undramatic harm
 *   - future_generations_facing_climate_harm: payer (powerless/trapped, civilizational horizon) — inherit delayed decarbonization
 *   - decision_theorists_and_risk_analysts: analytical observer — document the expected-value/tail-weighting divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.79).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Acceptable Risk Standard (Energy Policy)").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, 'a6c000ee-3fc3-4665-8577-340dc6d3881c').
narrative_ontology:cs_kernel_codification('a6c000ee-3fc3-4665-8577-340dc6d3881c', distributed).
narrative_ontology:cs_authority_grounding('a6c000ee-3fc3-4665-8577-340dc6d3881c', distributed).
narrative_ontology:cs_reading_relation('a6c000ee-3fc3-4665-8577-340dc6d3881c', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('a6c000ee-3fc3-4665-8577-340dc6d3881c', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('a6c000ee-3fc3-4665-8577-340dc6d3881c', foundational, irreversible_involuntary_harm_categorically_dominant).
narrative_ontology:cs_axiom_status(irreversible_involuntary_harm_categorically_dominant, holdable).
narrative_ontology:cs_axiom_grounding('a6c000ee-3fc3-4665-8577-340dc6d3881c', irreversible_involuntary_harm_categorically_dominant, deontological).
narrative_ontology:cs_axiom('a6c000ee-3fc3-4665-8577-340dc6d3881c', secondary, catastrophic_magnitude_outweighs_aggregate_frequency).
narrative_ontology:cs_axiom_status(catastrophic_magnitude_outweighs_aggregate_frequency, holdable).
narrative_ontology:cs_axiom_grounding('a6c000ee-3fc3-4665-8577-340dc6d3881c', catastrophic_magnitude_outweighs_aggregate_frequency, empirically_contingent).
narrative_ontology:cs_reference_frame('a6c000ee-3fc3-4665-8577-340dc6d3881c', post_disaster_precautionary_consensus).
narrative_ontology:cs_drift_state('a6c000ee-3fc3-4665-8577-340dc6d3881c', contemporary_decarbonization_pressure_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a6c000ee-3fc3-4665-8577-340dc6d3881c', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, incumbent_fossil_fuel_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, risk_averse_regulators).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, populations_near_fossil_fuel_plants).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, future_generations_facing_climate_harm).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry_workers_and_engineers).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, energy_poor_households_paying_higher_prices).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, precautionary_principle_for_irreversible_harm).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, catastrophic_risk_aversion_rationality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Continue operating existing fossil generation capacity and building new gas plants while nuclear licensing timelines stretch to decades and costs balloon under tail-risk-driven regulatory requirements. Benefit from a regulatory regime that treats their distributed, chronic emissions and mortality as an acceptable background cost while treating a competitor technology's rare catastrophic failure mode as intolerable. Face no comparably strict per-fatality standard despite killing far more people annually through air pollution.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, incumbent_fossil_fuel_operators, beneficiary,
    powerful, generational, arbitrage, national).

% Campaign to keep nuclear licensing, siting, and safety requirements maximally stringent by invoking catastrophic-outcome imagery (meltdown, exclusion zones, multi-generational contamination). Successfully embed the tail-risk framing into regulatory review processes, environmental impact statements, and public referenda. Their organizational identity and fundraising are structurally tied to nuclear remaining the salient danger; a normalized nuclear buildout would dissolve their core mandate.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations, agenda_setter).

% Administer licensing and safety review regimes that apply asymmetric scrutiny: a probabilistic risk assessment showing a one-in-a-million-per-reactor-year severe accident triggers years of additional review, while an existing coal plant's demonstrated, statistically certain excess mortality from particulate exposure requires no comparable process. Personally and institutionally exposed to blame for approving a facility that later fails catastrophically, but face no equivalent personal liability for the diffuse deaths their inaction perpetuates by keeping fossil plants running longer.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, risk_averse_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% Bear elevated rates of respiratory and cardiovascular disease and shortened life expectancy from continuous exposure to combustion byproducts, a harm that is well-quantified, certain, and ongoing, but is not weighted as 'catastrophic' because it is statistically distributed across many individuals rather than concentrated in a single dramatic event. Cannot relocate easily due to housing costs and employment ties, and have no equivalent advocacy infrastructure to nuclear's opponents.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, populations_near_fossil_fuel_plants, payer,
    powerless, biographical, trapped, regional).

% Inherit the accumulated atmospheric carbon burden produced by a slower-than-necessary transition off fossil generation, itself partly attributable to nuclear buildout being throttled by tail-risk-driven licensing delays and cost overruns. Have no representation in current regulatory proceedings and cannot bargain for a different risk-weighting today.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, future_generations_facing_climate_harm, payer,
    powerless, civilizational, trapped, global).

% Work within a licensing and public-approval environment where safety margins are set not purely by engineering risk analysis but by the political cost of a low-probability, high-salience failure. Careers are subject to project cancellation, cost-overrun blame, and public distrust disproportionate to the technology's actual comparative mortality record. Cannot easily redirect skills to a fossil-fuel-favoring market without retraining.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry_workers_and_engineers, payer,
    moderate, biographical, constrained, national).

% Pay higher electricity prices where nuclear cost overruns driven by tail-risk-oriented regulatory requirements get passed through, or where slower decarbonization keeps volatile fossil fuel prices as the marginal price-setter. Have no seat in the regulatory risk-weighting process and experience the standard only as a bill.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, energy_poor_households_paying_higher_prices, payer,
    powerless, biographical, trapped, national).

% Study the divergence between expected-value risk aggregation and catastrophic-tail-weighted risk aggregation, publishing comparative mortality-per-TWh analyses that show nuclear as statistically among the safest generation sources per unit energy produced, while noting the psychological and institutional salience of rare catastrophic events distorts policy weighting away from the expected-value calculation.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, decision_theorists_and_risk_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__catastrophic_tail_dominant, incumbent_fossil_fuel_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, legible standard for what counts as an unacceptable risk when siting and licensing energy infrastructure, allowing regulators, courts, insurers, and the public to coordinate around a common threshold for catastrophic, irreversible, unbounded harms rather than litigating each proposed facility's risk profile from first principles every time.
% TRANSFER_FUNCTION: Moves accepted mortality and morbidity burden away from concentrated, dramatic, attributable failure events (nuclear accidents) and onto diffuse, chronic, statistically-certain-but-individually-unattributable harm (fossil fuel combustion exposure, delayed decarbonization, climate damage) — a transfer from the politically salient minority who could be harmed by a rare catastrophic event to the politically diffuse majority who are certainly harmed by the status quo continuing.
% ABSENT_VOICES: Future generations bearing accumulated climate damage have no seat in present licensing and regulatory proceedings. Populations near existing fossil plants are individually powerless and rarely organized at the scale anti-nuclear advocacy groups achieve. Statisticians pointing out that per-TWh nuclear mortality is far lower than fossil generation are present in academic literature but structurally absent from the regulatory weighting process itself, which privileges catastrophic-scenario testimony over comparative mortality tables.
% DISAPPEARANCE_RATIONALE: If the catastrophic-tail-dominant standard were replaced overnight by an expected-value standard, nuclear licensing timelines and cost structures would shift substantially, fossil fuel operators would face the first regime treating their statistically certain mortality burden as comparably weighted to rare catastrophic events, and capital allocation across generation technologies would reorganize around comparative mortality-per-TWh rather than catastrophe-avoidance framing.
% FOUNDING_PROBLEM: Following Three Mile Island, Chernobyl, and later Fukushima, policymakers and publics confronted genuinely novel harm properties in nuclear accidents — potential unbounded geographic contamination, multi-generational exposure, and involuntary imposition on people who received no benefit from the facility — that classical expected-value mortality accounting did not obviously capture, motivating a precautionary standard specifically for catastrophic, irreversible, involuntarily-imposed risks.
% FOUNDING_PROBLEM_CORROBORATION: Decision theorists and comparative-mortality researchers, working from outside both the fossil incumbency and anti-nuclear advocacy communities, corroborate that the underlying concern about irreversible, involuntary, unbounded harm is a genuine and still-live problem in risk theory — but many of the same researchers attest that its current instantiation has drifted into an asymmetric standard that no longer tracks aggregate harm reduction, citing IPCC and WHO mortality data showing fossil generation's per-TWh death toll substantially exceeds nuclear's even including Chernobyl and Fukushima.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) and suppression (0.79) are both substantial and rising because the standard requires active, intensifying regulatory and political machinery to keep the asymmetry in place — probabilistic risk assessment frameworks, licensing review boards, insurance liability regimes, and public communication campaigns all must continually reproduce the framing that concentrates dread on nuclear failure modes while normalizing distributed fossil harm. Theater ratio (0.42) reflects that a real coordination function exists (some standard for catastrophic, irreversible, involuntary risk is genuinely needed) but a growing share of enforcement activity — additional review rounds, precautionary relicensing delays, worst-case scenario modeling exercises — now serves to perform diligence against a politically salient failure mode rather than to reduce aggregate harm. Accessibility collapse (0.61) is moderate-high: once a jurisdiction adopts the catastrophic-tail framing in its licensing law and public discourse, alternative expected-value framings become very difficult to reintroduce because any policymaker who relaxes nuclear scrutiny bears personal and political liability for a future accident, however statistically rare, while no symmetric liability attaches to perpetuating fossil-linked mortality. Resistance (0.57) is real but structurally weaker than the beneficiary coalition: comparative-mortality researchers and some public-health advocates push back, but lack the visceral narrative power of catastrophe imagery.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator's and anti-nuclear advocate's seats, this constraint reads as prudent, values-protective coordination against irreversible catastrophe — a rope. From the seat of populations near fossil plants and future generations, the identical structure reads as an enforced transfer that sacrifices their certain, quantifiable safety for the avoidance of a rarer, more narratively vivid harm to a different population — a tangled rope shading toward snare. The engine's per-seat computation is expected to diverge sharply here precisely because the standard's coordination function (a genuine need to weight irreversible, involuntary catastrophic risk specially) is real, but is yoked to an asymmetric application that the beneficiary coalition has strong incentive to maintain and weak incentive to correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent fossil operators and anti-nuclear advocacy organizations sit near the beneficiary end: the standard's asymmetry directly protects the former's market position and directly serves the latter's institutional mandate. Risk-averse regulators are agenda-setters who administer the asymmetry but are also partially trapped by it — their personal liability structure makes them structurally unable to unwind the standard even if they privately doubt its aggregate-harm consequences, which is why they are NOT coded as pure beneficiaries. Populations near fossil plants, future generations, and energy-poor households sit near the full-target end: they bear concentrated, certain costs from a standard ostensibly designed to protect people from risk, and their exit options are trapped precisely because the harm is diffuse and slow enough to defy organized resistance. Nuclear industry workers are targets of a different kind — their livelihoods absorb the cost of licensing delay and public distrust that the standard generates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to weigh genuinely novel harm properties of nuclear accidents (unbounded contamination, multi-generational exposure, involuntary imposition) — was live and serious at founding. Its status today is contested: the underlying decision-theoretic problem (how should irreversible, involuntary catastrophic risk be weighted against reversible, chronic, statistically-certain risk) remains a genuine open question in risk theory, so this is not simple mandatrophy where the founding problem has vanished. What has drifted is the APPLICATION: the standard was meant to solve a weighting problem for catastrophic-versus-chronic risk in general, but in practice it has hardened into an asymmetric standard applied almost exclusively against one energy technology while leaving a comparably or more lethal alternative pathway essentially unweighted by the same logic. Classifying this as tangled_rope rather than snare or mountain preserves the genuine coordination function (some catastrophic-risk standard is defensible) while flagging the asymmetric extraction (the standard's actual operation transfers harm rather than minimizing it in aggregate) — collapsing it to a pure snare would deny the founding problem's continued legitimacy, while accepting the claimed rope framing at face value would launder the standard's asymmetric operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophic_weighting_rational_or_captured,
    'Is the infinite/near-infinite weighting of catastrophic, low-probability nuclear harm a rationally defensible response to genuinely distinct harm properties (irreversibility, involuntary imposition, unbounded geographic scope), or is it a captured standard maintained by incumbent fossil interests and anti-nuclear advocacy organizations whose institutional survival depends on nuclear remaining maximally salient as a danger?',
    'Comparative institutional analysis of which actors fund, testify in, and structurally benefit from maintaining asymmetric licensing review, cross-referenced against whether the same actors apply comparable precautionary logic to other catastrophic-tail risks (e.g., large hydro dam failure, LNG facility explosion) that do not implicate a competing generation technology.',
    'If the weighting tracks genuine harm-property distinctions applied consistently across technologies, this reading is closer to a legitimate scaffold or rope solving a real decision-theory problem. If the weighting is applied asymmetrically and tracks incumbent benefit rather than harm-property consistency, this confirms the tangled_rope/snare-leaning classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophic_weighting_rational_or_captured, conceptual, 'Whether catastrophic-tail weighting is a principled decision-theoretic stance or a captured asymmetric standard.').

omega_variable(
    kernel_framing_underdetermination,
    'Given that the acceptable_risk_energy kernel supports at least three coherent readings (catastrophic_tail_dominant, expected_value_dominant, option_value_preserving), is the choice among them itself a value judgment (how much should society discount for irreversibility and involuntariness) or is it resolvable by better empirical risk data?',
    'Trace whether disagreement among readings persists even when all parties are shown identical mortality-per-TWh and irreversibility data — if disagreement persists on identical facts, the kernel contest is a values dispute (preference type_class); if disagreement dissolves with better data, it is empirical.',
    'If the reading choice is a genuine values dispute, no amount of additional risk-quantification research will resolve which reading ''should'' govern policy, and the contest among readings is a permanent feature of energy governance rather than a temporary data gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the choice among catastrophic-tail, expected-value, and option-value readings is empirically resolvable or an irreducible values contest.').

omega_variable(
    future_generations_representation_gap,
    'Does the absence of any mechanism for future generations to participate in current risk-weighting decisions systematically bias the standard toward discounting slow, cumulative harms (climate damage from delayed decarbonization) relative to dramatic, immediate harms visible to currently-living voters?',
    'Compare policy outcomes in jurisdictions with formal future-generations representation mechanisms (e.g., ombudsman offices, intergenerational equity statutes) against jurisdictions without them, controlling for other factors affecting nuclear/fossil licensing balance.',
    'If representation mechanisms measurably shift the catastrophic/chronic risk balance, this substantiates the absent_voices claim as a structural driver of the constraint''s asymmetry, not merely a rhetorical point.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_representation_gap, empirical, 'Whether lack of future-generations representation structurally biases risk weighting toward present-salient over cumulative harms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(acce_tr_t8, observed).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 16, 0.33).
narrative_ontology:measurement_basis(acce_tr_t16, observed).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 24, 0.37).
narrative_ontology:measurement_basis(acce_tr_t24, observed).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(acce_tr_t32, observed).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(acce_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(acce_be_t8, observed).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(acce_be_t16, observed).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 24, 0.63).
narrative_ontology:measurement_basis(acce_be_t24, observed).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 32, 0.66).
narrative_ontology:measurement_basis(acce_be_t32, observed).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(acce_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(acce_su_t8, observed).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 16, 0.69).
narrative_ontology:measurement_basis(acce_su_t16, observed).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 24, 0.73).
narrative_ontology:measurement_basis(acce_su_t24, observed).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 32, 0.77).
narrative_ontology:measurement_basis(acce_su_t32, observed).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.79).
narrative_ontology:measurement_basis(acce_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__catastrophic_tail_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% This constraint is one of three members of the acceptable_risk_energy kernel family. acceptable_risk_energy__expected_value_dominant reads the same kernel as requiring aggregate mortality-per-TWh minimization across all pathways, with a correspondingly different beneficiary/victim structure (fossil operators would become victims of stricter scrutiny; nuclear pathway would gain beneficiary status). acceptable_risk_energy__option_value_preserving reads the kernel as justifying pathway diversification under deep uncertainty rather than tail-avoidance per se, and would show low suppression against any single pathway. Each story carries its own ε, its own claimed_type, and its own stakeholder set per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
