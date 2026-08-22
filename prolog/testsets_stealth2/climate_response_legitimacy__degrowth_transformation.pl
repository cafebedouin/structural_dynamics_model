% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__degrowth_transformation, []).

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
 *   constraint_id: climate_response_legitimacy__degrowth_transformation
 *   human_readable: Degrowth Transformation Requirement for Climate Response Legitimacy
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the degrowth_transformation reading of the
 *   climate_response_legitimacy kernel: the claim that a climate response
 *   counts as legitimate only if it dismantles the growth imperative in
 *   wealthy nations through structural economic transformation — universal
 *   basic services, working time reduction, democratic firm ownership. The
 *   constraint operates today as a normative standard enforced discursively
 *   (by the scholarly movement and green-left parties) rather than by state
 *   machinery; its cost-bearers are the current developed-economy generation,
 *   and its beneficiaries are future generations and presently-exposed
 *   populations. Per the epsilon-referent rule for kernel-reading stories,
 *   epsilon is authored for the standing arrangement under contest — the
 *   dismantling requirement itself, as a live candidate standard — assessed
 *   by the reading's own lights: the reading concedes substantial real costs
 *   to the current generation while denying they are unjust, which places
 *   epsilon at substantial-but-not-predatory levels. The claim and the
 *   metrics are independent authored facts: claimed_type is what I believe
 *   structurally true (a genuine coordination function fused with an inherent
 *   intertemporal extraction asymmetry), and the metrics describe the
 *   arrangement's actual operation. The three readings of this kernel are
 *   separate constraint files linked through network.affects_constraints;
 *   this file does not average over them.
 *
 * KEY AGENTS:
 *   - - developed_economy_households: Primary cost-bearer (moderate/constrained) — bears income reduction and structural change; partly endorsing, partly resisting
 *   - - carbon_intensive_sector_workers: Concentrated cost-bearer (organized/constrained) — occupations scheduled for terminal contraction
 *   - - future_generations: Primary beneficiary (powerless/trapped) — receives avoided warming, holds no seat
 *   - - climate_vulnerable_populations: Secondary beneficiary (powerless/trapped) — gains from earlier stabilization
 *   - - degrowth_scholarly_movement: Agenda-setter (organized/identity_locked) — articulates and referees the standard
 *   - - green_left_policy_parties: Agenda-setter with beneficiary position (institutional/constrained) — legislates the agenda, collects coalition identity
 *   - - export_dependent_developing_economies: Excluded party (organized/constrained) — bears demand spillovers, absent from the conversation
 *   - - integrated_assessment_community: Analytical observer (institutional/analytical) — quantifies pathways, embeds contested assumptions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, 0.6).
domain_priors:suppression_score(climate_response_legitimacy__degrowth_transformation, 0.48).
domain_priors:theater_ratio(climate_response_legitimacy__degrowth_transformation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, extractiveness, 0.6).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Degrowth Transformation Requirement for Climate Response Legitimacy").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, '50a7425a-0bd9-49ef-ae32-daa100603fb9').
narrative_ontology:cs_kernel_codification('50a7425a-0bd9-49ef-ae32-daa100603fb9', distributed).
narrative_ontology:cs_authority_grounding('50a7425a-0bd9-49ef-ae32-daa100603fb9', expertise).
narrative_ontology:cs_interpretation_layer_present('50a7425a-0bd9-49ef-ae32-daa100603fb9').
narrative_ontology:cs_reading_relation('50a7425a-0bd9-49ef-ae32-daa100603fb9', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('50a7425a-0bd9-49ef-ae32-daa100603fb9', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('50a7425a-0bd9-49ef-ae32-daa100603fb9', foundational, absolute_decoupling_insufficient_for_budgets).
narrative_ontology:cs_axiom_status(absolute_decoupling_insufficient_for_budgets, holdable).
narrative_ontology:cs_axiom_grounding('50a7425a-0bd9-49ef-ae32-daa100603fb9', absolute_decoupling_insufficient_for_budgets, empirically_contingent).
narrative_ontology:cs_axiom('50a7425a-0bd9-49ef-ae32-daa100603fb9', foundational, legitimacy_requires_wealthy_nation_contraction).
narrative_ontology:cs_axiom_status(legitimacy_requires_wealthy_nation_contraction, holdable).
narrative_ontology:cs_axiom_grounding('50a7425a-0bd9-49ef-ae32-daa100603fb9', legitimacy_requires_wealthy_nation_contraction, deontological).
narrative_ontology:cs_reference_frame('50a7425a-0bd9-49ef-ae32-daa100603fb9', structural_sufficiency_baseline).
narrative_ontology:cs_drift_state('50a7425a-0bd9-49ef-ae32-daa100603fb9', post_paris_backlash_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('50a7425a-0bd9-49ef-ae32-daa100603fb9', '2026-06-12T09:30:00Z').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, developed_economy_households).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, carbon_intensive_sector_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, developed_economy_households).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, green_left_policy_parties).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, absolute_decoupling_insufficiency_thesis).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, fair_share_burden_sharing_principle).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, sufficiency_sovereignty_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People not yet born who will inherit whatever atmospheric concentration this century's choices produce. They receive the benefit of earlier stabilization and bear the cost of every year of delay, with no channel to consent, negotiate, or refuse. Every other seat in this arrangement speaks on their behalf; none is held by them.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Communities in low-emitting regions already exposed to flooding, heat extremes, and crop failure. They gain from any pathway that cuts emissions sooner and lose from every year of postponement. They hold no lever over wealthy-nation domestic policy beyond moral appeal and diplomatic blocs.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, climate_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, global).

% Middle-income households in wealthy democracies. Under this standard their climate duty takes the form of reduced disposable income, smaller homes and cars, and adjusted working lives, delivered through guaranteed services and shorter hours rather than cash compensation. Exit means emigration or political reversal, both costly; many of the same households sincerely endorse the standard, which complicates simple opposition.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, developed_economy_households, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__degrowth_transformation, developed_economy_households, beneficiary).

% Workers in fossil fuel extraction, refining, and carbon-intensive manufacturing whose occupations are scheduled for contraction. Guaranteed services and retraining promises cushion but do not erase regionally concentrated job loss, skill devaluation, and community decline. Their unions bargain for transition terms inside a framework that treats their industries as terminal.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, carbon_intensive_sector_workers, payer,
    organized, biographical, constrained, regional).

% Researchers, writers, and organizers who articulate the standard: journals, conference series, institutes, and activist networks. They define what counts as adequate climate policy, referee internal disputes over evidence, and supply the moral vocabulary. Their careers, networks, and public standing are built on the framework's core claims; leaving it would mean disavowing their life's work.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, degrowth_scholarly_movement, agenda_setter,
    organized, generational, identity_locked, global).

% Parties and factions that carry the standard into legislatures. They convert the transformation agenda into bills, budgets, and coalition demands, gaining members, votes, and programmatic identity from doing so. Pivoting away would cost them their distinct coalition niche; pivoting harder exposes them to electorate backlash they cannot yet absorb.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, green_left_policy_parties, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__degrowth_transformation, green_left_policy_parties, beneficiary).

% Countries whose development paths depend on exporting commodities and manufactures to wealthy markets. Contraction in rich-country demand narrows their route to prosperity, yet they sit outside the forums where the standard is debated, appearing mainly as an afterthought in solidarity rhetoric.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, export_dependent_developing_economies, excluded,
    organized, generational, constrained, continental).

% Economists and climate modelers in official advisory bodies who compare pathways. They quantify what each approach implies for welfare, feasibility, and carbon budgets, and their scenario choices quietly shape which policies appear rational. They take no side in the legitimacy dispute, but their instruments embed assumptions each camp contests.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, integrated_assessment_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__degrowth_transformation, diffuse).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate adequacy standard for collective climate action: it specifies what sufficient effort looks like for wealthy societies — contraction of material throughput, guaranteed universal services, shorter working hours, democratized firm ownership — so that effort comparisons and mutual accountability become possible across households, firms, and states.
% TRANSFER_FUNCTION: Moves present consumption capacity and investment control from current developed-economy populations toward atmospheric stabilization whose benefits accrue to future generations and exposed populations; concurrently moves firm governance from shareholder toward worker and community ownership.
% ABSENT_VOICES: Export-dependent developing economies would object that their development space is being narrowed without consultation; they are outside the movement's journals, summits, and coalition tables. Mainstream macroeconomists who dispute the decoupling premise engage the movement adversarially rather than inside its venues. Future generations are structurally voiceless — represented vicariously by every other seat, present in none.
% DISAPPEARANCE_RATIONALE: If the standard vanished overnight, climate discourse would reorganize around technology-and-pricing framings and impact-protection framings; the movement's institutions would dissolve or rebrand; green-left parties would lose their programmatic distinctness and part of their coalition; and the burden-sharing critique would disappear from legislative debate. Little implemented machinery would halt, because little has been implemented — but the organized seats that constitute the arrangement would rearrange around its absence.
% FOUNDING_PROBLEM: Three decades of growth-compatible climate policy produced persistent shortfalls: emissions kept rising through successive carbon-pricing experiments, decoupling in wealthy economies ran slower than carbon budgets require, and accumulating burdens fell hardest on populations that emitted least. The reading was formulated to explain that failure and to specify what an adequate response would instead require.
% FOUNDING_PROBLEM_CORROBORATION: The problem is corroborated from outside the benefiting set: UNEP Emissions Gap assessments and IPCC working-group reports document the continuing adequacy shortfall and the limited pace of demand-side decoupling, and energy-statistics agencies record rebound and offset-quality failures. These bodies attest the diagnosis while explicitly declining to endorse the dismantling prescription — corroboration of the founding problem, not of the remedy.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__degrowth_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__degrowth_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__degrowth_transformation, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.60: the requirement imposes real, specified sacrifice on the current developed-economy generation (income reduction, consumption ceilings, occupational destruction in whole sectors), and the reading's own accounting treats these as genuine costs rather than denying them — substantial, but directed at a public good rather than into any capturer's pocket. Suppression is 0.48 and is authored as a raw structural property, unscaled by power or scope: it consists of discursive marginalization of rival readings plus the foreclosing of growth-compatible pathways that enactment would entail; no state coercion currently backs it. Theater is 0.35: real functional activity exists (basic-services pilots, working-time trials, concrete program costing), alongside a visible layer of rhetorical adoption by actors with no transformation intent. Accessibility_collapse is 0.60: within the reading's logic, accepting its premises collapses growth-compatible alternatives almost completely, yet empirically those alternatives persist and the standard itself remains optional — hence mid-range. Resistance is 0.80: the feasibility barrier the reading itself flags is the dominant observable fact about it. The temporal series run on one shared grid (nine points, every tracked metric at every point). The suppression series traces enforcement-capacity change — intensification through the Paris-era adequacy debates and school-strike wave, peak around t10-t12, then decay under post-pandemic growth-restoration politics — which is why suppression_requirement is tracked here rather than left static. The theater series shows a mainstreaming hump (virtue-signaling adoption inflating the ratio) followed by partial correction as backlash clarified positions. These are single-wave dynamics, not sustained oscillation; the wave shape itself documents the enforcement lifecycle.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute differently from the same structure. From the movement's position the standard is a vocation: its holders are identity-locked (professional and ideological identity fused with the framework's core claims — exit means disavowing their life's work), so they experience the arrangement as discovery, not imposition. From the household seat the same standard arrives as demanded sacrifice, softened by the fact that many households internally endorse it — a dual consciousness the engine will register as divided directionality within one seat. Workers experience it as terminal scheduling of their industries with compensation promised but not yet credible. The beneficiary seats experience nothing at all: future generations are represented vicariously by every other seat, which is precisely why their interests require structural declaration rather than testimony. Green-left parties occupy a hinge position — administering the standard while collecting electoral identity from it — and should compute as partly invested rather than purely principled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: future_generations and climate_vulnerable_populations sit near the full-beneficiary end (d near 0), with trapped exit reinforcing it — they cannot arbitrage away either the benefit or the residual risk. Developed_economy_households and carbon_intensive_sector_workers sit near the full-target end (d near 1), with constrained exit keeping them from damping their own extraction; the households' secondary beneficiary position (their children inherit the stabilized climate) moderates but does not invert this. The agenda-setting seats derive mid-low directionalities: the movement is structurally invested but collects standing rather than the extracted value; the parties collect votes and coalition identity, warranting their secondary beneficiary role. The observer seat is analytical and feeds no extraction arithmetic. Receipt surface: I checked every named seat for capture of the extracted value — the sacrifice converts to avoided warming landing diffusely across future generations and exposed populations; the movement and parties collect standing and votes, which are positional goods adjacent to the arrangement, not its extracted product — hence gain_flow is affirmatively 'diffuse'. Fixing_cost is 'prohibitive' for the seats that could revise the standard: abandonment costs the movement its identity and the parties their programmatic niche, and removes the only adequacy path those seats recognize, so the perceived cost of fixing exceeds any benefit they perceive; outsider repeal would be mechanically cheap but leaves the founding problem unsolved, which is a negative-benefit case rather than a cheap-fix case.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and externally corroborated (UNEP gap assessments, IPCC adequacy analyses), and the disappearance verdict is world_rearranges — the status-by-verdict pair is consistent, so the mismatch consumer should register no zombie flag: this is not an arrangement outliving its function. The mandatrophy risks run in both directions and the classification guards against each. Mislabeling the standard as pure extraction would erase its genuine coordination function — providing the determinate adequacy standard without which climate effort fragments into symbolic gestures — which is why the tangled_rope structure requires both halves to be named. Conversely, romanticizing it as pure coordination would hide the inherent intertemporal asymmetry: the paying generation cannot be repaid by its beneficiaries, so the extraction half is structural, not incidental, and no institutional reform removes it — only the weighting question (omega: intergenerational_weighting) can change its valence. The piton risk is premature: the arrangement is young, its function largely unimplemented, and its theater ratio moderate; but if the feasibility barrier proves structural (omega: feasibility_barrier_attribution) and enforcement decays while the standard persists rhetorically, the theater series would climb and a drift toward theatrical maintenance becomes the live failure mode to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the degrowth_transformation reading of the climate_response_legitimacy kernel; what structurally changes under the sibling readings mitigation_priority and adaptation_priority?',
    'Comparative classification across the three sibling story files: locate where the victim and beneficiary sets move (mitigation_priority shifts costs to taxpayers and subsidized industries while preserving household income; adaptation_priority shifts the focal population to impact-exposed communities and accepts the warming trajectory), and where the disagreement is located (whether legitimacy requires dismantling growth, merely permits it, or is orthogonal to emissions-reduction strategy).',
    'If the sibling classifications show disjoint victim sets and incompatible enforcement profiles, the kernel label ''legitimate climate response'' is confirmed as covering three distinct constraints rather than one contestable claim; if the sets overlap heavily, the readings are variants of one structure and the family should be collapsed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings relocate the cost-bearer and beneficiary sets.').

omega_variable(
    absolute_decoupling_empirics,
    'Is the reading''s foundational empirical premise true — that GDP cannot be absolutely decoupled from emissions and material throughput fast enough to fit remaining carbon budgets in wealthy nations?',
    'Long-run panel data comparing territorial and consumption-based decoupling rates in OECD economies against sectoral carbon-budget trajectories, with attention to outsourcing effects, rebound, and offset quality.',
    'If sufficient decoupling is demonstrated, the dismantling requirement loses its necessity claim and the reading collapses toward mitigation_priority; if decoupling continues to undershoot, the coordination function of this constraint is confirmed as load-bearing and its extraction profile stands as the price of adequacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_empirics, empirical, 'Whether the empirical premise that forces the transformation requirement actually holds.').

omega_variable(
    feasibility_barrier_attribution,
    'Does the severe political resistance reflect the size of the burden the standard imposes, or contingent factors of framing, sequencing, and communication that a different presentation could overcome?',
    'Natural experiments from jurisdictions trialing components (four-day week trials, universal basic services pilots, windfall taxation): if identical burdens framed as wellbeing policy meet far less resistance, the barrier is communicative; if resistance tracks burden size across framings, it is structural.',
    'If the barrier is communicative, high resistance signals sequencing lag rather than extraction-driven rejection, and the persistence outlook improves without any change in the burden profile; if structural, the standard''s enforcement requirements will ratchet as implementation attempts proceed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feasibility_barrier_attribution, empirical, 'Attribution of the political feasibility barrier: burden-size versus framing.').

omega_variable(
    intergenerational_weighting,
    'What weight does the current generation''s sacrifice carry relative to future generations'' benefit — and does the ethically defensible weighting reverse which seat counts as bearing unjustified cost?',
    'Explicit normative argument over intergenerational discount rates and fair-share principles, tested against revealed preferences in deliberative assemblies and constitutional environmental-rights litigation.',
    'At a high discount on future welfare, developed_economy_households dominate the victim set and the arrangement reads as imposing uncompensated net loss; at a low discount, the current generation reads as free-riding on a commons and the extraction assessment inverts toward under-enforcement rather than over-extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_weighting, preference, 'Intergenerational weighting determines the valence of the extraction asymmetry.').

omega_variable(
    spillover_incidence_on_exporters,
    'Do contraction policies in wealthy nations impose significant costs on export-dependent developing economies through shrinking demand, and should those economies be added to the victim set?',
    'Trade-flow modeling of wealthy-nation demand contraction scenarios, plus historical evidence from prior demand shocks in commodity-exporting states.',
    'If spillovers are large, the victim set expands beyond the declared wealthy-nation seats and the extraction asymmetry deepens (costs spread to populations who never consented and receive little of the stabilizing benefit), pushing effective extraction upward for the arrangement as a whole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spillover_incidence_on_exporters, empirical, 'Whether degrowth spillovers create an undeclared victim class among exporter economies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__degrowth_transformation, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t2, climate_response_legitimacy__degrowth_transformation, theater_ratio, 2, 0.24).
narrative_ontology:measurement_basis(clim_tr_t2, observed).
narrative_ontology:measurement(clim_tr_t4, climate_response_legitimacy__degrowth_transformation, theater_ratio, 4, 0.27).
narrative_ontology:measurement_basis(clim_tr_t4, observed).
narrative_ontology:measurement(clim_tr_t6, climate_response_legitimacy__degrowth_transformation, theater_ratio, 6, 0.3).
narrative_ontology:measurement_basis(clim_tr_t6, observed).
narrative_ontology:measurement(clim_tr_t8, climate_response_legitimacy__degrowth_transformation, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(clim_tr_t8, observed).
narrative_ontology:measurement(clim_tr_t10, climate_response_legitimacy__degrowth_transformation, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t12, climate_response_legitimacy__degrowth_transformation, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(clim_tr_t12, observed).
narrative_ontology:measurement(clim_tr_t14, climate_response_legitimacy__degrowth_transformation, theater_ratio, 14, 0.37).
narrative_ontology:measurement_basis(clim_tr_t14, observed).
narrative_ontology:measurement(clim_tr_t16, climate_response_legitimacy__degrowth_transformation, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(clim_tr_t16, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t2, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 2, 0.44).
narrative_ontology:measurement_basis(clim_be_t2, observed).
narrative_ontology:measurement(clim_be_t4, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 4, 0.47).
narrative_ontology:measurement_basis(clim_be_t4, observed).
narrative_ontology:measurement(clim_be_t6, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 6, 0.5).
narrative_ontology:measurement_basis(clim_be_t6, observed).
narrative_ontology:measurement(clim_be_t8, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 8, 0.53).
narrative_ontology:measurement_basis(clim_be_t8, observed).
narrative_ontology:measurement(clim_be_t10, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t12, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(clim_be_t12, observed).
narrative_ontology:measurement(clim_be_t14, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 14, 0.59).
narrative_ontology:measurement_basis(clim_be_t14, observed).
narrative_ontology:measurement(clim_be_t16, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 16, 0.6).
narrative_ontology:measurement_basis(clim_be_t16, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t2, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 2, 0.32).
narrative_ontology:measurement_basis(clim_su_t2, observed).
narrative_ontology:measurement(clim_su_t4, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 4, 0.36).
narrative_ontology:measurement_basis(clim_su_t4, observed).
narrative_ontology:measurement(clim_su_t6, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 6, 0.42).
narrative_ontology:measurement_basis(clim_su_t6, observed).
narrative_ontology:measurement(clim_su_t8, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 8, 0.48).
narrative_ontology:measurement_basis(clim_su_t8, observed).
narrative_ontology:measurement(clim_su_t10, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 10, 0.54).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t12, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 12, 0.55).
narrative_ontology:measurement_basis(clim_su_t12, observed).
narrative_ontology:measurement(clim_su_t14, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 14, 0.51).
narrative_ontology:measurement_basis(clim_su_t14, observed).
narrative_ontology:measurement(clim_su_t16, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 16, 0.48).
narrative_ontology:measurement_basis(clim_su_t16, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__degrowth_transformation, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__adaptation_priority).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'legitimate climate response' covers three structurally distinct claims with different epsilon values, victim sets, and enforcement profiles. This file instantiates the degrowth_transformation reading (epsilon indexed to the dismantling requirement's burden on the current developed-economy generation, assessed by the reading's own lights). The mitigation_priority sibling carries a different epsilon (burdens of innovation subsidy and carbon pricing on industries and taxpayers under preserved growth) and the adaptation_priority sibling another (burdens shifted onto resilience finance with the warming trajectory accepted). The upstream sibling (mitigation_priority) is the incumbent with higher empirical establishment; this reading exerts critical pressure on it via the decoupling-insufficiency claim without logically eliminating it. Each story links the others through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
