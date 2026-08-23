% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__degrowth_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Degrowth Requirement on Legitimate Climate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the degrowth reading of the
 *   climate_harm_prevention kernel: the claim that a legitimate climate
 *   response requires planned economic contraction in the Global North
 *   because mitigation attempted within a growth framework is physically and
 *   politically impossible. The standing arrangement under contest is the
 *   growth-compatible climate regime (pledge-and-review targets, decoupling
 *   expectations, market-led transition); this reading's distinguishing move
 *   is to reject growth as the constraint boundary and to relocate the costs
 *   of prevention onto present Northern consumption. Per the anti-laundering
 *   purpose of the epsilon-referent rule, the reading's endorsed program is
 *   not authored as costless: the authored epsilon measures the contraction
 *   requirement's own imposed-cost structure as this reading honestly
 *   assesses it — substantial, deliberate, and asymmetric, held warranted as
 *   correction of harm previously externalized onto the Global South and
 *   future generations. Sibling readings (mitigation_priority,
 *   adaptation_priority) instantiate different constraints from the same
 *   kernel and are linked via network.affects_constraints; the disagreement
 *   between readings is located in one load-bearing feasibility premise and
 *   in the consequent locus of costs. Claim and metrics are authored
 *   independently: the claimed type states what the structural record
 *   supports; the metrics describe the arrangement's actual operation;
 *   divergence between them is measurement, not error.
 *
 * KEY AGENTS:
 *   - degrowth_advocacy_coalition: agenda-setting advocate (moderate/identity_locked) — formulates the impossibility premise and the legitimacy standard; professionally and morally fused with the claim
 *   - global_south_populations: primary beneficiary (organized/constrained) — gains reduced harm and negotiated development space; bloc leverage, no exit from the climate system
 *   - future_generations: silent beneficiary (powerless/trapped) — represented only by present advocates; cannot consent or object
 *   - nonhuman_ecosystems: non-agent beneficiary (powerless/trapped) — damaged by warming, represented by no one, collects nothing
 *   - northern_mass_consumers: primary payer (moderate/constrained) — bears consumption limits; ballot-box retaliation is their lever
 *   - northern_energy_poor: regressive-cost payer (powerless/trapped) — hit first and hardest, thinnest representation in package design
 *   - carbon_intensive_sector_workers: organized payer (organized/trapped) — concentrated job losses; union leverage over transition terms
 *   - fossil_fuel_asset_holders: escappable payer (powerful/arbitrage) — stranded assets; partially mobile capital funds resistance
 *   - northern_states: dual administrator/bearer (institutional/constrained) — would run the quota machinery while bearing GDP and electoral costs
 *   - southern_growth_aspirants: excluded voice (powerless/constrained) — majority-world development aspirations capped by a regime drafted without them
 *   - integrated_assessment_community: analytical observer (institutional/analytical) — publishes the feasibility and emissions-gap accounting every seat argues from
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.66).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.72).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Degrowth Requirement on Legitimate Climate Response").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, '3dfe71cf-a982-478f-b493-510a4ec37b16').
narrative_ontology:cs_kernel_codification('3dfe71cf-a982-478f-b493-510a4ec37b16', distributed).
narrative_ontology:cs_authority_grounding('3dfe71cf-a982-478f-b493-510a4ec37b16', expertise).
narrative_ontology:cs_interpretation_layer_present('3dfe71cf-a982-478f-b493-510a4ec37b16').
narrative_ontology:cs_reading_relation('3dfe71cf-a982-478f-b493-510a4ec37b16', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('3dfe71cf-a982-478f-b493-510a4ec37b16', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('3dfe71cf-a982-478f-b493-510a4ec37b16', foundational, growth_frame_mitigation_physically_politically_impossible).
narrative_ontology:cs_axiom_status(growth_frame_mitigation_physically_politically_impossible, holdable).
narrative_ontology:cs_axiom_grounding('3dfe71cf-a982-478f-b493-510a4ec37b16', growth_frame_mitigation_physically_politically_impossible, empirically_contingent).
narrative_ontology:cs_axiom('3dfe71cf-a982-478f-b493-510a4ec37b16', foundational, legitimate_response_requires_planned_northern_contraction).
narrative_ontology:cs_axiom_status(legitimate_response_requires_planned_northern_contraction, holdable).
narrative_ontology:cs_axiom_grounding('3dfe71cf-a982-478f-b493-510a4ec37b16', legitimate_response_requires_planned_northern_contraction, instrumental).
narrative_ontology:cs_axiom('3dfe71cf-a982-478f-b493-510a4ec37b16', secondary, differentiated_contraction_north_first).
narrative_ontology:cs_axiom_status(differentiated_contraction_north_first, holdable).
narrative_ontology:cs_axiom_grounding('3dfe71cf-a982-478f-b493-510a4ec37b16', differentiated_contraction_north_first, conventional).
narrative_ontology:cs_reference_frame('3dfe71cf-a982-478f-b493-510a4ec37b16', contraction_to_biophysical_steady_state).
narrative_ontology:cs_drift_state('3dfe71cf-a982-478f-b493-510a4ec37b16', contemporary_post_sr15, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('3dfe71cf-a982-478f-b493-510a4ec37b16', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, nonhuman_ecosystems).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, northern_mass_consumers).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, northern_energy_poor).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, carbon_intensive_sector_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, fossil_fuel_asset_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, northern_states).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, degrowth_impossibility_thesis).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, steady_state_economics_doctrine).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, planetary_boundaries_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ecological economists, climate-justice organizers, and post-growth policy groups who formulate the feasibility case that emissions targets cannot be met while economies grow, and who supply the legitimacy standard that climate plans are judged against. They run journals, movements, and advisory networks; their careers, coalitions, and moral commitments are built on the impossibility claim, so revising it would unravel their professional and ethical identity rather than just one policy position.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, degrowth_advocacy_coalition, agenda_setter,
    moderate, generational, identity_locked, global).

% Populations of Africa, South Asia, small island states, and Latin America who contributed least to cumulative emissions and stand to lose most from warming. Under a contraction regime they are positioned to gain reduced harm and negotiated development space, and they hold collective leverage through negotiating blocs and debt and loss-and-damage claims, but they cannot exit the climate system and their gains depend on Northern compliance they cannot compel.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_populations, beneficiary,
    organized, generational, constrained, global).

% People not yet born whose exposure to warming damage is decided by present emission paths. They hold no vote, market power, or seat in any negotiation; everything said on their behalf comes from present advocates, and they can neither consent to the burdens placed on present populations nor object if the protection promised them is deferred.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Coral reefs, forests, ice systems, and species ranges that are damaged by warming and would benefit incidentally from any successful reduction path. They have no representatives, collect nothing, and appear in the debate only as objects of valuation by the other seats.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, nonhuman_ecosystems, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(climate_harm_prevention__degrowth_reading, nonhuman_ecosystems).

% Households across high-income economies whose energy use, travel, diets, and housing are the direct object of consumption limits. They hold votes in democracies and have repeatedly punished perceived austerity at the ballot box; individual exit means emigration or off-grid self-provision, both costly, so their realistic options are organizing to reverse or soften the limits, or absorbing them.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, northern_mass_consumers, payer,
    moderate, immediate, constrained, continental).

% Low-income households in high-income countries who spend a disproportionate share of income on energy and transport. Uniform consumption limits raise their costs first and hardest, they lack savings to invest in efficiency or relocation, and they are thinly represented in the professional networks where contraction packages are designed.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, northern_energy_poor, payer,
    powerless, immediate, trapped, regional).

% Workers in mining, oil and gas, heavy industry, and combustion-engine manufacturing whose jobs depend on production the regime shrinks. Unions give them organized leverage to negotiate transition terms, but their skills, mortgages, and towns are tied to the declining sectors, so leaving is not realistically available and delay is their main defense.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, carbon_intensive_sector_workers, payer,
    organized, biographical, trapped, regional).

% Shareholders, creditors, and companies holding reserves and infrastructure that contraction strands. Capital mobility lets them shift portfolios and production geography ahead of regulation, which softens what they ultimately lose while financing sustained opposition to the regime; their losses are real but partially escapable.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, fossil_fuel_asset_holders, payer,
    powerful, biographical, arbitrage, global).

% Governments of high-income economies that would have to run the quota, rationing, and planning machinery contraction requires while absorbing GDP and revenue losses and answering electorates who bear the costs. They are simultaneously the prospective administrators of the regime and among its largest cost-bearers, and no government can exit the climate system itself.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, northern_states, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, northern_states, agenda_setter).

% Majority-world populations whose development aspirations a contraction regime would cap. The reading's formulation centers Northern sacrifice and leaves their entitlements, ceilings, and timelines undefined; they would insist on differentiated schedules and guaranteed floors but hold no seat in the networks where contraction packages are drafted.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, southern_growth_aspirants, excluded,
    powerless, generational, constrained, global).

% Climate scientists and integrated-assessment modelers who compute carbon budgets, test scenario feasibility, and publish the emissions-gap accounting that keeps the distance between promised and delivered policy visible. They take no side in the legitimacy contest, but their feasibility findings are the common evidentiary ground every seat argues from.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, integrated_assessment_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_harm_prevention__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of decarbonizing faster than growth-framed incentives allow: synchronizes high-income economies on absolute reduction paths, allocates remaining atmospheric and development space between North and South, and removes the growth-competition dynamic that otherwise consumes the carbon budget.
% TRANSFER_FUNCTION: Moves material consumption capacity and emissions entitlement from present Global North populations toward stabilization benefits accruing to Global South populations, future generations, and ecosystems; secondarily moves planning authority over economic scale from markets and consumers to state administrative institutions.
% ABSENT_VOICES: Southern growth aspirants — the majority of humanity whose development pathways a contraction regime would cap — are structurally outside the formulation: the reading specifies Northern sacrifice but leaves Southern ceilings and timelines undefined, and they would demand both. Northern energy-poor households are thinly present in the professional networks designing contraction packages. Future generations appear only through present advocates. Ecosystems have no seat at all.
% DISAPPEARANCE_RATIONALE: If the requirement vanished overnight, no installed machinery stops working — it commands little enforcement today — so material arrangements would barely move at first. But the discursive architecture would rearrange: climate politics would lose its most radical legitimacy benchmark, Southern negotiation framings (debt, loss-and-damage, atmospheric space) would lose their anchor, and the emissions-gap critique would lose its sharpest voice. Advocates judge that dependence real; opponents judge the requirement marginal noise; the parties dispute which world we are in.
% FOUNDING_PROBLEM: Closing the emissions gap fast enough to prevent dangerous warming under conditions where growth-framed mitigation appeared unable to deliver reductions at the required rate or scale.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated from outside the beneficiary set: Earth-system science and UNEP emissions-gap accounting attest the persistent gap between pledged and delivered reductions, and Global South negotiating blocs attest the harm-transfer structure that motivates the reading. The specific remedy — contraction as a legitimacy requirement — is attested by no one outside the degrowth coalition and allied movements; mitigation economists and most incumbent governments actively dispute its necessity. Corroboration of the problem: strong. Corroboration of the constraint's solution: contested and coalition-internal.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, contested).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66: where the requirement binds it deliberately transfers consumption capacity from Northern present populations toward stabilization benefits accruing elsewhere — real, concentrated, intended costs, but corrective in direction rather than rent-collection, which holds it below snare-grade. Suppression 0.72: contraction against revealed Northern preference requires quota, rationing, and planning machinery that overrides market and electoral signals; the suppression is structural (state planning powers), not internalized. Theater 0.42: the movement's output is disproportionately declarative — conferences, manifestos, pledge-adjacent symbolism — relative to material contraction delivered, a rising Goodhart signal still below the substitution threshold. Accessibility_collapse 0.62: inside the reading's frame alternatives collapse sharply (if growth-frame mitigation is impossible, only contraction variants remain legitimate), but the frame itself is contestable and the sibling readings stay live, so cross-frame alternatives persist. Resistance 0.78: the broadest possible opposing coalition — asset holders, affected workers, consumers, most incumbent governments. All three temporal series share one grid (T0..T50, approximately 1972 limits-to-growth origins to present); rising base_extractiveness tracks the requirement's growing concreteness from academic thesis to policy-demanding standard, rising theater tracks discursive surplus outpacing delivery, and the suppression_requirement series is authored because this story specifically traces enforcement intensification — from voluntary-simplicity voluntarism to explicit state-planning proposals. End-state values equal the base_properties scalars. Receipt surface: gains are diffuse (no seat captures the transferred capacity — it is retired as avoided warming), and fixing is prohibitive because making the requirement moot requires closing the emissions gap itself; deleting the demand leaves the driver intact and the demand regenerates.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the northern_mass_consumers and carbon_intensive_sector_workers seats the requirement is experienced as enforced extraction — concentrated, compulsory, resistant to exit (trapped workers cannot leave skills and towns behind). From the global_south_populations and future_generations seats the same structure operates as protection and compensation — subsidy-side, near-zero directionality. The fossil_fuel_asset_holders seat is the instructive middle case: nominally a payer, but arbitrage-grade exit (portfolio and production mobility) damps what the structure can actually take from them while their retained resources fund resistance — the derivation's exit modulation is doing real work there. The degrowth_advocacy_coalition seat is identity-locked rather than merely interested: the impossibility premise constitutes the coalition's professional and moral identity, so evidence that would revise the premise threatens the seat itself, not just a policy position. The northern_energy_poor seat shows intra-class divergence at nominally equal national standing: trapped by income, hit regressively, thinly represented — and a coalition of energy-poor households with transition-threatened workers is the plausible internal challenger to regressive package designs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: global_south_populations (organized, constrained) and future_generations (powerless, trapped — trappedness amplifies the subsidy side since nothing can be taken back from them) derive near the beneficiary pole; nonhuman_ecosystems is declared but agent-flagged false, so it feeds no directional arithmetic. Victim declarations drive high directionality: northern_mass_consumers (moderate, constrained), northern_energy_poor (powerless, trapped — the highest effective target in the story), carbon_intensive_sector_workers (organized, trapped), and fossil_fuel_asset_holders (powerful, arbitrage — high base target, damped effective extraction). The agenda_setter seat (advocacy coalition) derives low-mid: it administers the standard without bearing its material costs. northern_states carries a genuine dual position — prospective administrator and major bearer — recorded via secondary_role so the per-seat computation reads both positions from the same structural row. Scopes run continental-to-global across seats, so the engine's scope amplification applies broadly; verification difficulty at global scope is exactly why the regime would need heavy enforcement machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline prevents two symmetrical mislabels. Read through its own advocacy, the contraction requirement presents as pure coordination (everyone must reduce or budgets blow) and its asymmetric costs disappear; read through its Northern opponents, it presents as pure imposition and its genuine collective-action function disappears. The tangled_rope structure keeps both faces: a coordination function (synchronized absolute reductions and atmospheric-space allocation) and asymmetric extraction (costs concentrated on identifiable Northern seats) operating through the same enforced structure. Mandatrophy is unresolved: the founding problem — closing the emissions gap before budgets exhaust — is live and corroborated outside the beneficiary set, so no dead-mandate drift is declared. The forward risk is piton drift by another route: if the impossibility premise were empirically falsified while the advocacy identity held, the requirement would persist theatrically — the rising theater_ratio series is the early indicator, and the advocacy_identity_fusion omega names the mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the climate_harm_prevention kernel (reading: degrowth_reading). What structurally changes if a sibling reading is adopted instead?',
    'Comparative classification across the three sibling stories sharing the kernel referent: shifts in victim sets, enforcement machinery, and cost locus trace which reading is ascendant in discourse and policy.',
    'Under mitigation_priority this constraint''s victim set dissolves into diffuse transition costs and its enforcement machinery is never built; under adaptation_priority the contraction demand converts into resilience-finance claims and the Northern cost locus evaporates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three readings of the climate-harm-prevention kernel; siblings would relocate victims, costs, and enforcement.').

omega_variable(
    impossibility_thesis_empirical_status,
    'Is the load-bearing premise true — that mitigation within a growth framework is physically and politically incapable of closing the emissions gap at the required rate and scale?',
    'Two further decades of absolute-decoupling evidence at required rates and scales, or its continued absence; political-economy study of whether growth-dependent polities can legislate contraction-scale reductions.',
    'Falsification collapses the coordination warrant and the requirement degrades toward pure imposed austerity (snare-direction); confirmation locks the tangled_rope structure and raises the legitimacy stakes of enforcement design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impossibility_thesis_empirical_status, empirical, 'Empirical status of the impossibility premise on which the entire reading rests.').

omega_variable(
    democratic_legitimacy_of_contraction,
    'Can planned contraction be legitimated through Northern democratic processes, or does implementation require technocratic or coercive imposition that outruns consent?',
    'Comparative study of rationing and austerity episodes; deliberative assemblies on contraction packages; electoral response to explicit contraction platforms.',
    'If consent-capable, suppression is a transient enforcement cost; if not, suppression ratchets permanently and the arrangement drifts toward coercion-sustained extraction regardless of its warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_of_contraction, empirical, 'Whether the required suppression is consent-transient or structurally permanent.').

omega_variable(
    southern_development_ceiling_ambiguity,
    'Does the Northern-focused contraction imply indefinite Southern development space, or eventual global contraction with Southern ceilings left undefined?',
    'Convergence-pathway modeling and Southern negotiating positions on entitlements, timelines, and burden-sharing formulas.',
    'If ceilings are eventual, today''s primary beneficiary seats become successive payers and the beneficiary structure is phase-dependent; if indefinite, aggregate carbon budgets likely fail and the protection promise to future generations breaks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(southern_development_ceiling_ambiguity, conceptual, 'Where the North/South asymmetry ends — the reading''s beneficiary structure may be phase-dependent.').

omega_variable(
    advocacy_identity_fusion,
    'Is the advocacy coalition''s commitment to the impossibility premise evidence-tracking or identity-fused?',
    'Observe the coalition''s response to strong falsifying evidence (sustained absolute decoupling at required rates): revision versus doubling-down; examination of career and funding structures built on the premise.',
    'Identity fusion predicts piton-direction drift — theatrical maintenance of the requirement after its warrant fails — and would explain a continuing theater_ratio rise independent of empirical developments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advocacy_identity_fusion, empirical, 'Cognitive-capture check on the agenda-setting seat: evidence-tracking versus identity-fused commitment.').

omega_variable(
    contraction_phase_permanence,
    'Is planned contraction a transitional phase toward a lower steady state (after which the legitimacy requirement relaxes) or a permanent civilizational condition?',
    'Specification of the end-state in the degrowth literature against biophysical ceiling analysis: does any published pathway reach a state where the requirement''s enforcement retires?',
    'If transitional, the arrangement carries an undeclared sunset and scaffold-elements deserve recognition in classification; if permanent, the enforcement machinery is a standing feature and suppression assessments should assume permanence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contraction_phase_permanence, conceptual, 'Undeclared sunset question: whether the contraction requirement is phase or fixture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chp_degrow_tr_t0, climate_harm_prevention__degrowth_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(chp_degrow_tr_t0, observed).
narrative_ontology:measurement(chp_degrow_tr_t10, climate_harm_prevention__degrowth_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(chp_degrow_tr_t10, observed).
narrative_ontology:measurement(chp_degrow_tr_t20, climate_harm_prevention__degrowth_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(chp_degrow_tr_t20, observed).
narrative_ontology:measurement(chp_degrow_tr_t30, climate_harm_prevention__degrowth_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(chp_degrow_tr_t30, observed).
narrative_ontology:measurement(chp_degrow_tr_t40, climate_harm_prevention__degrowth_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(chp_degrow_tr_t40, observed).
narrative_ontology:measurement(chp_degrow_tr_t50, climate_harm_prevention__degrowth_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(chp_degrow_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(chp_degrow_be_t0, climate_harm_prevention__degrowth_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(chp_degrow_be_t0, observed).
narrative_ontology:measurement(chp_degrow_be_t10, climate_harm_prevention__degrowth_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(chp_degrow_be_t10, observed).
narrative_ontology:measurement(chp_degrow_be_t20, climate_harm_prevention__degrowth_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement_basis(chp_degrow_be_t20, observed).
narrative_ontology:measurement(chp_degrow_be_t30, climate_harm_prevention__degrowth_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement_basis(chp_degrow_be_t30, observed).
narrative_ontology:measurement(chp_degrow_be_t40, climate_harm_prevention__degrowth_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement_basis(chp_degrow_be_t40, observed).
narrative_ontology:measurement(chp_degrow_be_t50, climate_harm_prevention__degrowth_reading, base_extractiveness, 50, 0.66).
narrative_ontology:measurement_basis(chp_degrow_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(chp_degrow_su_t0, climate_harm_prevention__degrowth_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(chp_degrow_su_t0, observed).
narrative_ontology:measurement(chp_degrow_su_t10, climate_harm_prevention__degrowth_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(chp_degrow_su_t10, observed).
narrative_ontology:measurement(chp_degrow_su_t20, climate_harm_prevention__degrowth_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(chp_degrow_su_t20, observed).
narrative_ontology:measurement(chp_degrow_su_t30, climate_harm_prevention__degrowth_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(chp_degrow_su_t30, observed).
narrative_ontology:measurement(chp_degrow_su_t40, climate_harm_prevention__degrowth_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement_basis(chp_degrow_su_t40, observed).
narrative_ontology:measurement(chp_degrow_su_t50, climate_harm_prevention__degrowth_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(chp_degrow_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).

% DUAL FORMULATION NOTE:
% Constraint family: three readings of the climate_harm_prevention kernel share one referent (the standing growth-compatible climate regime) and author reading-indexed epsilon over it. This member instantiates the degrowth_reading; mitigation_priority and adaptation_priority are separate files. The upstream/downstream structure runs through the shared feasibility premise: Earth-system carbon-budget accounting (upstream, high empirical confidence) feeds all three readings; this reading's impossibility premise is downstream of that accounting and upstream of its enforcement implications. Decomposition follows the epsilon-invariance principle: the colloquial label 'climate response' covers structurally distinct claims with distinct beneficiary/victim structures, so each reading is a separate story linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
