% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__systems_transition_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__systems_transition_reading
 *   human_readable: Systems-Transition Reading: Legitimate Mitigation Requires Decentralized Democratic Energy Control
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   This story instantiates the systems-transition reading of the contested
 *   kernel 'climate mitigation imperative': the claim that legitimate
 *   mitigation requires transforming energy systems toward decentralized,
 *   democratically controlled ownership, and that nuclear power is
 *   disqualified because it perpetuates extractive centralization regardless
 *   of its carbon performance. The arrangement under examination is the
 *   institutionalized governance gate built on that claim — party platforms,
 *   funding criteria, procurement rules, and campaign machinery that
 *   condition mitigation legitimacy on ownership form. The claim/metric gap
 *   is deliberate and load-bearing: proponents present the imperative in the
 *   language of necessity ('mitigation requires'), while the authored metrics
 *   describe a hybrid structure — a real coordination function wrapped around
 *   asymmetric, actively enforced exclusion. Per the epsilon-referent rule,
 *   epsilon is authored for the standing arrangement under contest (the gate
 *   as it actually operates), assessed by this reading's own lights: the
 *   reading counts centralization harms as extraction, treats the fossil
 *   windfall as parasitic capture its own program never intended, and weighs
 *   nuclear's cost objections less heavily than the opportunity-cost sibling
 *   would. Constraint-family note: this is one of three linked stories
 *   decomposing the colloquial label 'climate mitigation imperative'; the
 *   siblings author different epsilon values over their own referents and are
 *   linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - distributed_renewables_industry: Primary beneficiary (organized/mobile) — designated carrier of legitimate mitigation; collects mandated market share
 *   - - energy_democracy_movement_orgs: Beneficiary and informal agenda-setter (organized/identity_locked) — defines the frame, collects legitimacy, funding, members
 *   - - energy_ministries_and_regulators: Formal agenda-setter (institutional/constrained) — writes the criteria; bound by the coalitions the frame sustains
 *   - - nuclear_operators_and_vendors: Primary target (institutional/trapped) — stranded capital, denied approvals, early closures
 *   - - nuclear_specialist_workforce: Secondary target (moderate/constrained) — depreciating skills tied to closing facilities
 *   - - electricity_ratepayers: Diffuse target with partial benefit (powerless/constrained) — absorbs system and stranded-asset costs; affluent slice self-supplies
 *   - - incumbent_fossil_generators: Incidental beneficiary (institutional/arbitrage) — monetizes every year excluded firm capacity is missing
 *   - - future_cohorts: Excluded voice (powerless/trapped) — inherits the carbon-budget consequence, holds no seat
 *   - - international_assessment_bodies: Analytical observer (institutional/analytical) — publishes cross-technology pathway evidence all sides cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.63).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.68).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.37).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.37).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Systems-Transition Reading: Legitimate Mitigation Requires Decentralized Democratic Energy Control").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, '45ac6486-bacf-4410-ab87-b8f881047e32').
narrative_ontology:cs_kernel_codification('45ac6486-bacf-4410-ab87-b8f881047e32', formalized).
narrative_ontology:cs_authority_grounding('45ac6486-bacf-4410-ab87-b8f881047e32', lineage).
narrative_ontology:cs_interpretation_layer_present('45ac6486-bacf-4410-ab87-b8f881047e32').
narrative_ontology:cs_reading_relation('45ac6486-bacf-4410-ab87-b8f881047e32', climate_mitigation_imperative__portfolio_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('45ac6486-bacf-4410-ab87-b8f881047e32', climate_mitigation_imperative__opportunity_cost_reading, forecloses).
narrative_ontology:cs_axiom('45ac6486-bacf-4410-ab87-b8f881047e32', foundational, democratic_decentralization_constitutive_of_legitimate_mitigation).
narrative_ontology:cs_axiom_status(democratic_decentralization_constitutive_of_legitimate_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('45ac6486-bacf-4410-ab87-b8f881047e32', democratic_decentralization_constitutive_of_legitimate_mitigation, deontological).
narrative_ontology:cs_axiom('45ac6486-bacf-4410-ab87-b8f881047e32', foundational, nuclear_centralization_categorical_incompatibility).
narrative_ontology:cs_axiom_status(nuclear_centralization_categorical_incompatibility, holdable).
narrative_ontology:cs_axiom_grounding('45ac6486-bacf-4410-ab87-b8f881047e32', nuclear_centralization_categorical_incompatibility, empirically_contingent).
narrative_ontology:cs_reference_frame('45ac6486-bacf-4410-ab87-b8f881047e32', decentralized_democratic_low_carbon_order).
narrative_ontology:cs_drift_state('45ac6486-bacf-4410-ab87-b8f881047e32', contemporary_post_taxonomy_contestation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('45ac6486-bacf-4410-ab87-b8f881047e32', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_renewables_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, energy_democracy_movement_orgs).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, incumbent_fossil_generators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_operators_and_vendors).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_specialist_workforce).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, electricity_ratepayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, electricity_ratepayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write the procurement rules, funding criteria, and planning frameworks that decide which generation technologies count as legitimate climate action. Several have adopted decentralized-community-ownership criteria and phased-out or declined new reactor programs under coalition pressure. Their flexibility is bounded by the political coalitions that sustain them in office.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_ministries_and_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Campaign organizations, community-energy networks, and think tanks that define what counts as real climate action for millions of supporters. They receive membership, donations, media standing, and agenda-setting access by maintaining the decentralized-democratic framing. Their founding identity predates climate politics in the anti-nuclear and appropriate-technology movements of the 1970s; abandoning the framing would dissolve the organizational reason for being.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_democracy_movement_orgs, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, energy_democracy_movement_orgs, agenda_setter).

% Solar installers, wind developers, storage firms, and cooperative energy suppliers whose products are the designated carriers of legitimate mitigation. Policy frameworks built on this reading channel subsidies, mandates, and finance toward their assets, and they can deploy capital across jurisdictions wherever the framing holds.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewables_industry, beneficiary,
    organized, biographical, mobile, global).

% Gas, lignite, and coal operators that kept running — and in some jurisdictions expanded — when firm low-carbon reactors closed ahead of schedule. They publicly endorse technology-neutral rhetoric, quietly benefit from every year the excluded capacity is missing, and can shuffle fuel portfolios and trading positions to monetize the gap.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, incumbent_fossil_generators, beneficiary,
    institutional, biographical, arbitrage, continental).

% Utilities, reactor vendors, and fuel-cycle firms holding licensed plants, long-lived capital stock, and specialized supply chains. Where the reading dominates policy, their assets are retired early or denied new-build approval regardless of carbon performance; the capital cannot be repurposed, licenses cannot migrate, and the workforce pipeline collapses with each closure wave.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_operators_and_vendors, payer,
    institutional, generational, trapped, global).

% Engineers, technicians, and regulators whose skills attach to a shrinking domestic fleet. Relocation to the few national programs still building is possible but costly, and the skill set depreciates outside operating plants. Career paths, pensions, and home regions are bound to facilities scheduled for closure.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_specialist_workforce, payer,
    moderate, biographical, constrained, global).

% Households and businesses that fund the system through bills and taxes. They receive cleaner air and, where distributed generation reaches them, bill savings; they also absorb the integration, backup, and network costs of a system built around variable generation, and the stranded-asset charges of early reactor closures. Affluent customers can partially self-supply with rooftop solar and storage; renters and apartment dwellers largely cannot.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, electricity_ratepayers, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, electricity_ratepayers, beneficiary).

% People not yet born who will live inside whatever carbon budget and energy system today's choices leave. They bear any emissions added by slower or costlier mitigation paths and inherit whichever infrastructure gets built, but hold no seat in any forum where the framing is contested.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, future_cohorts, excluded,
    powerless, civilizational, trapped, universal).

% Scenario-modeling and assessment institutions that publish pathway comparisons across all low-carbon technologies, including nuclear. They take input from all camps, publish cost and feasibility ranges, and their reports are cited as ammunition by every side without their endorsing any reading's governance criteria.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, international_assessment_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__systems_transition_reading, distributed_renewables_industry).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__systems_transition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem in rapid energy-system change: decentralized, community-held projects localize benefits, pre-empt siting conflict, and give a broad climate coalition a shared program its members can all mobilize behind; the common framing also aggregates dispersed capital into a deployable distributed-generation market.
% TRANSFER_FUNCTION: Moves legitimacy, subsidy flows, and investment toward distributed renewable assets and the organizations that promote them; moves closure risk and stranded capital onto nuclear operators, vendors, and their workforce; moves system-integration and stranded-asset costs onto ratepayers; and, where displaced demand is met by existing fossil plants, moves operating revenue to incumbent fossil generators.
% ABSENT_VOICES: Future cohorts who inherit the carbon budget outcome; low-income households for whom system-cost increases are largest relative to income; developing-country planners seeking firm low-carbon capacity; and communities hosting retiring reactors. None are seated in the ministerial processes, funding committees, or movement congresses where the framing is enforced.
% DISAPPEARANCE_RATIONALE: Overnight removal would reopen every suspended reactor debate, dissolve the unifying program of a large segment of the climate coalition, force rewrite of funding criteria and portfolio standards that name ownership form, strand the distributed-finance pipelines built on the framing, and renegotiate the fossil bridge-fuel arrangements that currently fill excluded capacity.
% FOUNDING_PROBLEM: The arrangement descends from a grievance older than climate politics: mid-twentieth-century energy systems concentrated generation, and with it economic and political power, in giant utilities and states — crystallized for its founders in reactor accidents, weapons linkage, and technocratic secrecy. The 1970s appropriate-technology and anti-nuclear movements built the deconcentration program first; climate deadlines were later fused onto it, turning a governance grievance into a mitigation requirement.
% FOUNDING_PROBLEM_CORROBORATION: Independent scholarly corroboration of the original concentration grievance exists outside the benefiting parties — the Mumford-to-Winner lineage in history of technology, energy-justice literature, and contemporaneous documentation of 1970s anti-nuclear mobilization all attest the pre-climate origins of the deconcentration demand. The counter-position is attested by scenario-modeling and energy-economics communities outside the movement, who argue the binding problem is now carbon arithmetic and that governance form is orthogonal to it. Attestation that the concentration grievance should govern mitigation specifically comes overwhelmingly from the benefiting parties themselves; no external body endorses that step.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__systems_transition_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__systems_transition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__systems_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.63: the gate's operation strands long-lived nuclear capital and redirects finance by ownership form, but the exclusion is jurisdictional rather than global and the coordination payload (community buy-in, aggregated distributed capital) is real. Suppression at 0.68 reflects enforcement machinery — legislated phaseouts, financing screens, procurement criteria — that must stay active because nuclear's carbon performance keeps refuting the aesthetic case; suppression is authored as a raw structural property, unscaled, while the engine scales only effective extractiveness by directionality and scope. Theater ratio 0.37: victory-era maintenance (anniversary mobilizations, purity contests, symbolic target-setting) is a growing but minority share next to substantive organizing. Accessibility collapse 0.40 — the sibling readings remain fully accessible counterpositions; understanding this gate does not close the conceptual space, it competes inside it. Resistance 0.60 — pronuclear advocacy, ecomodernist criticism, taxonomy litigation, and continued national build programs constitute sustained organized pushback. The measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point; the base_extractiveness and suppression_requirement trajectories trace the enforcement ratchet (discursive pressure to legislated force after the 2011 accident shock, maturing screening machinery), and the post-t20 flattening and slight decline encode the 2021-2023 counter-pressure episode (taxonomy inclusion fights, tripling pledges, crisis-driven reassessments) — the series is intentionally non-monotonic because that reversal is descriptively real and dates the contested present accurately.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the agenda-setter seat the gate is a governable narrative and coalition-stabilizer; from the trapped nuclear-operator seat it is confiscation by legitimacy; from the movement seat it is democratization under siege. The identity-lock mechanism binding the movement seat is ideological and relational: organizations founded in the anti-nuclear struggle fused their self-concept with the exclusion decades before climate politics supplied the deadline — for them, admitting nuclear as compatible is not a policy update but the dissolution of organizational identity, which is why exit_options is identity_locked and why the frame survived evidence shocks that should have softened it. If that identity frame broke — if the movement re-founded around carbon arithmetic — enforcement capacity would collapse quickly: the formal criteria would lose their street-level enforcers, suppression would fall toward the 0.3 range, and the arrangement would decay toward inertia rather than active defense. The powerless ratepayer seat carries a latent coalition possibility: consumer bodies uniting across the bill-payer class could convert diffuse individual exposure into organized countervailing power, which is why resistance potential exceeds current organized resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality_overrides are authored: the beneficiary/victim declarations plus exit atoms carry the signal, and the two institutional-power seats are separated by role declaration rather than needing override surgery. Distributed renewables derive near the beneficiary pole (d near 0.0): declared beneficiaries with mobile exit and direct collection. Movement organizations sit similarly low while their identity_locked exit explains persistence independent of material payoff. Incumbent fossil generators are declared beneficiaries because the displacement effect routes real revenue to them — the derivation correctly places them beneficiary-side even though the benefit is incidental and unloved by the gate's designers. Nuclear operators and the specialist workforce derive near the target pole (d near 1.0): declared victims, trapped and constrained respectively, bearing the transfer directly. Ratepayers land mid-range with a downward tilt: dual-declared payer-and-beneficiary, diffuse costs, partial self-supply escape for the affluent. Formal ministries derive mildly beneficiary-side without declaration — the gate hands them a workable narrative — but their enforcement costs and lost flexibility keep them from the deep-beneficiary region.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (concentration of energy power) is contested-live: proponents argue the democracy deficit persists and the mandate is current; opponents argue the operative problem is now carbon arithmetic, for which ownership form is noise. Because status is contested rather than dead, the mismatch consumer sees contested paired with world_rearranges — no zombie flag fires; the arrangement is not maintained against a corpse. The classification discipline prevents mislabeling in both directions: reading the gate as pure virtue (rope) erases the stranded capital, the workforce destruction, and the fossil windfall; reading it as pure cover (snare) erases the genuine siting-consent and coalition-aggregation functions that no alternative framing currently performs as well. The tangled_rope claim holds both truths in one structure: the same gate that coordinates consent also enforces exclusion, and neither function can be described away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexing,
    'This constraint is one reading of the kernel ''climate mitigation imperative'' — the systems-transition reading. Would a sibling reading (portfolio_optimization_reading or opportunity_cost_reading) restructure this story''s victim and beneficiary sets?',
    'Author and compile the sibling stories; compare computed classifications across the kernel family. Portfolio optimization would remove nuclear operators from the victim set and treat exclusion as waste rather than extraction; opportunity cost would make victimhood conditional on measured cost-per-tonne data rather than categorical.',
    'If a sibling governs, the victim set inverts or dissolves: nuclear moves from target to instrument, and the classification of this arrangement shifts from hybrid coordination/extraction toward contested-but-functional portfolio planning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexing, conceptual, 'Committer-frame indexing: which reading of the mitigation kernel this story instantiates.').

omega_variable(
    categorical_vs_conditional_exclusion,
    'Is nuclear''s disqualification categorical (any centrally operated fission fleet is incompatible with democratic energy control as such) or conditional (current corporate/state ownership forms are incompatible, while municipally owned or cooperatively governed reactors could pass the democratic-control standard)?',
    'Test whether credible community-ownership or municipal-governance reactor models satisfy the reading''s own democratic-control criteria; survey movement-source treatments of small modular reactors and public-power proposals.',
    'If the exclusion is conditional, the constraint decomposes into a general governance standard applicable to all generation (including investor-owned distributed renewables) plus a contingent empirical prediction; the nuclear victim set narrows sharply and the arrangement moves toward a rope-like governance screen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_conditional_exclusion, conceptual, 'Whether the nuclear victim set rests on a categorical or a contingent incompatibility claim.').

omega_variable(
    fossil_displacement_incidence,
    'Does excluding firm low-carbon nuclear capacity actually prolong fossil generation and transfer revenue to incumbent fossil operators, or does coalition focus on renewables accelerate their retirement?',
    'Jurisdiction-level displacement analysis comparing generation mixes before and after reactor closures under reading-dominated policy (e.g. German phaseout versus French retention), using marginal emissions and capacity-factor data.',
    'If fossil displacement is substantial, incumbent_fossil_generators is a genuine beneficiary seat and the arrangement drifts toward pure extraction with a coordination cover story; if negligible, that seat is spurious and the hybrid coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_displacement_incidence, empirical, 'Whether the fossil-incumbency benefit is a real structural effect or an artifact.').

omega_variable(
    gatekeeping_vs_coordination_share,
    'What share of the arrangement''s activity is genuine coalition-building and siting-conflict resolution, versus legitimacy gatekeeping that filters technologies by ownership form regardless of performance?',
    'Audit of movement and funding-institution resource flows: proportion expended on local consent, community ownership, and grid-access problems versus proportion expended on maintaining technology exclusions.',
    'Below roughly half genuine coordination, the arrangement approaches pure extraction with the transition story as cover; above it, the tangled coordination-plus-extraction structure holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gatekeeping_vs_coordination_share, empirical, 'Functional-to-performative split of the arrangement''s operating activity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 30, 0.37).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 5, 0.36).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 30, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__opportunity_cost_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'climate mitigation imperative' decomposes into three structurally distinct readings of one kernel, each with its own stable epsilon, beneficiary/victim structure, and type. This story (systems_transition_reading) authors the highest structural extraction: the governance gate excludes a technology categorically, stranding its capital base. portfolio_optimization_reading treats exclusion as waste rather than extraction (victims become the carbon budget and ratepayers, not the nuclear sector as such); opportunity_cost_reading makes victimhood contingent on measured cost-per-tonne rather than categorical. Upstream/downstream: the portfolio and opportunity-cost readings draw on shared scenario-modeling evidence, while this reading draws on the older deconcentration lineage and cites that evidence selectively — contamination propagates from the evidentiary siblings into this one when cost data improve, and from this one into them when governance criteria enter funding rules. The decomposition follows the epsilon-invariance principle: measuring 'the mitigation imperative' by governance form, by portfolio completeness, or by cost-effectiveness yields different epsilons because they are three different constraints wearing one label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
