% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Systems-Transition Reading of the Mitigation Imperative: Decentralization-and-Democratic-Control Gate
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'climate
 *   mitigation is imperative': the systems-transition reading, which holds
 *   that legitimate mitigation requires transforming energy systems toward
 *   decentralized, democratically controlled ownership, and that nuclear
 *   generation perpetuates extractive centralization and is therefore
 *   disqualified. The constraint modeled is that reading's rule as it
 *   operates in climate-mitigation governance: a legitimacy gate administered
 *   by advocacy networks and enforced through funding criteria, coalition
 *   boundaries, and platform discipline. EPSILON REFERENT (fixed rule): the
 *   standing arrangement under contest is the incumbent centralized,
 *   investor-owned energy regime together with the source-neutral legitimacy
 *   allocation that lets capital flow to any low-carbon asset regardless of
 *   ownership; epsilon 0.78 is THIS READING'S assessment of that arrangement
 *   — monopoly rents from captive ratepayers, burdens dumped on frontline
 *   communities, democratic exclusion from energy decisions. The reading's
 *   endorsed decentralized alternative is NOT the referent and no metric here
 *   describes it. The epsilon is reading-indexed: the portfolio-optimization
 *   sibling would author materially lower epsilon over the same referent, the
 *   opportunity-cost sibling intermediate — same referent, different
 *   readings, different values. The claim/metric gap is deliberate: the frame
 *   is CLAIMED by its holders as a corrective principle, while the authored
 *   metrics describe its full operation including its exclusionary costs; the
 *   engine measures that divergence.
 *
 * KEY AGENTS:
 *   - - energy_democracy_organizations: Agenda-setting administrator of the frame (organized/identity_locked) — writes platforms, certifies legitimacy, gates coalitions; its institutional identity is fused with the frame
 *   - - philanthropic_climate_funders: Secondary agenda-setter (institutional/mobile) — enforces the gate through grant criteria; collects grantee coherence
 *   - - distributed_renewables_industry: Primary beneficiary (organized/mobile) — receives preferential legitimation and mission-labeled capital
 *   - - community_energy_cooperatives: Beneficiary with payer residue (moderate/constrained) — gains sponsorship, bears participation burdens and tokenization risk
 *   - - nuclear_industry_and_workforce: Primary payer (powerful/trapped) — loses social license inside climate spaces regardless of plant performance; capital and skills are sunk
 *   - - nuclear_host_communities: Payer (powerless/trapped) — continuity path disqualified in forums they do not control
 *   - - investor_owned_utilities: Payer (institutional/constrained) — centralized model delegitimated; fights rearguard actions while acquiring distributed assets
 *   - - ratepaying_households: Dual-positioned (powerless/constrained) — promised democratic energy, bearing transition costs meanwhile
 *   - - ecomodernist_analysts: Excluded voice (organized/constrained) — contests the ownership-blindness premise from outside the rooms
 *   - - ipcc_style_assessment_bodies: Analytical observer (analytical/analytical) — catalogues options without ownership weighting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.78).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.62).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Systems-Transition Reading of the Mitigation Imperative: Decentralization-and-Democratic-Control Gate").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, '92d1bb81-aae1-4d34-9ea5-e70cdcf622fb').
narrative_ontology:cs_kernel_codification('92d1bb81-aae1-4d34-9ea5-e70cdcf622fb', formalized).
narrative_ontology:cs_authority_grounding('92d1bb81-aae1-4d34-9ea5-e70cdcf622fb', lineage).
narrative_ontology:cs_interpretation_layer_present('92d1bb81-aae1-4d34-9ea5-e70cdcf622fb').
narrative_ontology:cs_reading_relation('92d1bb81-aae1-4d34-9ea5-e70cdcf622fb', climate_mitigation_imperative__portfolio_optimization_reading, influences).
narrative_ontology:cs_reading_relation('92d1bb81-aae1-4d34-9ea5-e70cdcf622fb', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_axiom('92d1bb81-aae1-4d34-9ea5-e70cdcf622fb', foundational, legitimate_mitigation_requires_democratic_energy_control).
narrative_ontology:cs_axiom_status(legitimate_mitigation_requires_democratic_energy_control, holdable).
narrative_ontology:cs_axiom_grounding('92d1bb81-aae1-4d34-9ea5-e70cdcf622fb', legitimate_mitigation_requires_democratic_energy_control, deontological).
narrative_ontology:cs_axiom('92d1bb81-aae1-4d34-9ea5-e70cdcf622fb', foundational, nuclear_perpetuates_extractive_centralization).
narrative_ontology:cs_axiom_status(nuclear_perpetuates_extractive_centralization, holdable).
narrative_ontology:cs_axiom_grounding('92d1bb81-aae1-4d34-9ea5-e70cdcf622fb', nuclear_perpetuates_extractive_centralization, empirically_contingent).
narrative_ontology:cs_reference_frame('92d1bb81-aae1-4d34-9ea5-e70cdcf622fb', decentralized_energy_democracy_settlement).
narrative_ontology:cs_drift_state('92d1bb81-aae1-4d34-9ea5-e70cdcf622fb', contemporary_post_paris_implementation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('92d1bb81-aae1-4d34-9ea5-e70cdcf622fb', '2026-08-05T14:22:31Z').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_renewables_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, energy_democracy_organizations).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_industry_and_workforce).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_host_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, philanthropic_climate_funders).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, ratepaying_households).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, investor_owned_utilities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, ratepaying_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National and transnational advocacy networks that wrote the energy-democracy frame into movement platforms and administer its boundaries day to day: they decide which campaigns receive coalition endorsement, which technologies count as climate solutions, and which partners are welcome in climate spaces. Staff careers, member loyalty, and coalition standing all flow through maintaining the frame. Departing from it would dissolve the networks' shared purpose, so exit is not a realistic option for them.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_democracy_organizations, agenda_setter,
    organized, generational, identity_locked, global).

% Major climate foundations condition grant capital on alignment with the decentralization-and-democratic-control frame and decline applications built around excluded technologies. They gain grantee coherence and legible impact narratives from enforcing the gate, and can rebalance portfolios toward other theories of change at moderate cost if the frame loses credibility.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, philanthropic_climate_funders, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, philanthropic_climate_funders, beneficiary).

% Manufacturers, developers, and installers of distributed solar, storage, and wind whose products receive preferential treatment in climate coalitions, policy platforms, and green finance because of the frame. They would sell into energy markets regardless, but the frame adds political tailwind and access to mission-labeled capital; they can serve any jurisdiction and are not bound to the advocacy networks.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewables_industry, beneficiary,
    organized, biographical, mobile, global).

% Locally owned energy projects and cooperatives that gain legitimation, grants, technical assistance, and policy sponsorship under the frame. They also carry participation burdens, reporting requirements, and the risk of being showcased as symbols while larger actors absorb the actual capital flows; their options are bounded by local regulators and utility interconnection processes.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives, payer).

% Plant operators, vendors, and the specialized engineering and trades workforce whose social license inside climate policy spaces is stripped by the frame regardless of any individual plant's carbon or safety performance. Capital is sunk in single-purpose licensed assets, workforce skills are largely non-transferable, and plant lifetimes span decades, so the sector cannot reposition on the frame's timeline; its recourse is litigation, lobbying outside climate coalitions, and public-opinion campaigns.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_industry_and_workforce, payer,
    powerful, generational, trapped, continental).

% Towns whose tax base, employment, and civic institutions are tied to reactor sites and their eventual decommissioning or life extension. The frame disqualifies their preferred continuity path in the coalitions that shape federal and state energy policy; they appear as petitioners in proceedings but do not set agendas, and they cannot relocate the plant or their attachment to it.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_host_communities, payer,
    powerless, generational, trapped, local).

% Franchised monopolies whose centralized, corporate-owned generation and distribution model the frame explicitly delegitimates. They fight community-choice aggregation, public-power, and rooftop-solar expansion proceedings while simultaneously adapting by acquiring distributed assets themselves; regulatory obligations, ratepayer franchises, and sunk network infrastructure prevent them from simply exiting the fight.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, investor_owned_utilities, payer,
    institutional, generational, constrained, national).

% Households promised cheaper, locally governed energy under community ownership, who in the interim bear transition surcharges, interconnection cost allocations, and bill volatility during grid reconfiguration. Individual households cannot practically leave the grid, and their influence runs through occasional consumer-advocacy interventions in rate cases.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, ratepaying_households, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, ratepaying_households, payer).

% Analysts, climate scientists, and writers who argue that carbon arithmetic is indifferent to ownership structure and that excluding firm low-carbon technologies lengthens the fossil bridge. They publish in adjacent venues and advise governments outside the movement, but hold no standing inside the coalition rooms where the frame's boundaries are enforced, and pro-nuclear speech inside climate spaces carries professional and social cost for them.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, ecomodernist_analysts, excluded,
    organized, civilizational, constrained, global).

% Intergovernmental and academic assessment processes that catalogue mitigation options technology by technology, reporting cost, carbon, and feasibility without weighting ownership structure. They take input from every seat, publish scenario libraries that all factions cite, and impose no enforcement of their own.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, ipcc_style_assessment_bodies, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__systems_transition_reading, energy_democracy_organizations).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__systems_transition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns a geographically dispersed climate movement on a single theory of change — energy democracy — converting scattered local fights over plants, bills, and siting into one program with shared demands, and standardizes what counts as a just transition so that funders, campaigns, and local projects can pull in the same direction.
% TRANSFER_FUNCTION: Moves legitimation, grant capital, coalition standing, and policy attention toward distributed and community-owned renewable development, and moves social license and policy access away from nuclear and centralized corporate ownership — uncompensated in both directions.
% ABSENT_VOICES: Ecomodernist analysts and nuclear-skilled workforce representatives are outside the coalition rooms entirely; nuclear host communities are present only as petitioners in proceedings others agenda-set; ratepayer advocates from regions with weak renewable resources and high firm-power needs are thinly represented. Unanimity inside the frame arises partly because these seats were never admitted to it.
% DISAPPEARANCE_RATIONALE: If the frame vanished overnight, coalition platforms would drop the ownership criterion, funding gates would reopen to technology-neutral applications, nuclear would regain partial standing in climate policy within a few budget cycles, and distributed-energy advocacy would fragment into technology-specific and price-specific campaigns without a unifying program.
% FOUNDING_PROBLEM: Climate policy was pursuing mitigation as pure technology substitution — swapping carbon sources while leaving ownership concentration, energy poverty, utility shutoffs, and siting burdens untouched — and the frame was built to force decarbonization to carry a transfer of control over energy systems to the communities that depend on them.
% FOUNDING_PROBLEM_CORROBORATION: Energy-justice scholarship and frontline community organizations independent of the major advocacy networks attest that the underlying problems persist — energy-poverty incidence, shutoff rates, and siting-burden distributions are documented outside the beneficiary set. No corroborating source outside the beneficiary set attests that nuclear exclusion follows from that diagnosis: assessment-body scenario libraries and analysts who share the ownership critique but reject the technology exclusion explicitly contest that inference.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__systems_transition_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.78) because the referent arrangement — the incumbent centralized energy-governance settlement — concentrates ownership and decision power while externalizing burdens, which is precisely this reading's indictment; the rule's own operation then redistributes legitimation asymmetrically, imposing disqualification costs on nuclear actors independent of carbon performance. Suppression (0.62) is the raw structural cost of dissent inside climate spaces: funding-gate exclusion and coalition discipline (structural) layered over movement identity that renders pro-nuclear speech heretical (internalized) — the omega variable records that the scalar does not separate these mechanisms. Suppression is authored as an unscaled structural property; only extractiveness is context-scaled downstream. Theater ratio (0.38) reflects a real functional core — community projects, municipalization wins, interconnection reform — beneath a growing performative layer of justice language in grant applications and consultation rituals. Accessibility collapse is moderate (0.48): source-neutral and portfolio framings remain articulable at reputational and financial cost, unlike a natural law's alternatives. Resistance (0.58) is sustained: ecomodernist publications, nuclear advocacy resurgence, and host-community political mobilization. The measurement series run on ONE shared grid (points 0,5,10,15,20,25 for all three metrics) showing monotonic consolidation — the frame hardened rather than oscillated, so no cyclical mechanism is claimed; rising base_extractiveness over the interval documents exclusionary ratcheting as funding gates formalized. Coalition note: the two victim groups are structurally fragmented — a licensed-asset workforce and place-bound towns in different geographies with different timelines — so coalition power among nominally weaker seats remains unrealized, which sustains the asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical structural data. From the agenda-setter seat the frame is hard-won coordination it built and administers — a principled boundary without which the movement fragments. From the nuclear-industry and host-community seats the same gate is uncompensated dispossession of a low-carbon pathway on grounds unrelated to its carbon performance. From the distributed-renewables seat it is tailwind. From the observer seat it is one weighted scenario filter among several. Same-level lateral dynamics matter here: the distributed-renewables industry and the nuclear industry are both organized, capitalized energy sectors facing the same rule, differentiated almost entirely by exit structure — renewables sell fungible products into any market (mobile), while nuclear holds single-purpose licensed assets and non-transferable skills (trapped). Inter-institutionally, philanthropic funders enforce a gate they did not build and can leave; advocacy networks built it and cannot.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for distributed_renewables_industry, community_energy_cooperatives, and (via its secondary role) the funder seat — the frame subsidizes their standing, so effective extraction damps toward zero or inverts. Victim declarations drive high directionality for nuclear_industry_and_workforce and nuclear_host_communities; combined with trapped exit and multi-decade time horizons, their effective extraction amplifies toward the full-target end — from their seats the rule takes social license and policy access wholesale. Investor_owned_utilities derive high-mid directionality as payers under the rule even though the reading casts them as the incumbent arrangement's chief beneficiaries — the dual position is real and is left to the engine to compute rather than overridden. Ratepaying_households sit near symmetric: promised benefit, interim cost. No directionality overrides were authored because the beneficiary/victim-plus-exit derivation already captures every seat's relationship; the one candidate (utilities' dual position) is carried by role structure instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — energy poverty, corporate utility power, and siting burdens are independently documented — so this is not a mandate outliving its function, and no mandatrophy resolution is declared. The classification work cuts both ways. Calling the rule pure coordination (rope) erases the real, uncompensated exclusion costs borne by nuclear seats; calling it pure extraction (snare) erases the genuine collective-action function that binds dispersed campaigns into a program. Tangled rope preserves both truths and requires the enforcement data (funding gates, coalition boundaries) to be named, which it is. The forward risk is the opposite of atrophy: as decarbonization proceeds, the governance-transform clause could survive its diagnostic core if distributed ownership arrives without democracy — corporate aggregators branding as community solar — which would show up as theater_ratio crossing 0.5 with flat functional output; the temporal series is authored to make that drift detectable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_vs_carbon_legitimacy,
    'Does legitimate mitigation properly condition on governance and ownership structure, or only on carbon outcomes?',
    'Comparative decarbonization-outcome studies across ownership regimes holding technology mix constant, plus assessment-body scenario analysis weighting governance variables.',
    'Resolving toward carbon-only collapses this rule''s exclusionary edge onto opportunity-cost grounds and removes nuclear from the victim set; resolving toward governance-primary vindicates the gate as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_vs_carbon_legitimacy, conceptual, 'The located kernel disagreement: where the legitimacy condition attaches.').

omega_variable(
    nuclear_centralization_contingency,
    'Is centralization intrinsic to nuclear generation as a technology, or contingent on financing and regulatory choices that are separable from it?',
    'Cross-national comparison of nuclear ownership models (state, municipal, cooperative, small-modular licensing regimes) and their measured decision-power concentration.',
    'If centralization is contingent, the victim declaration for nuclear rests on conflating technology with ownership; the rule narrows to ownership requirements and the blanket technology exclusion fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_centralization_contingency, empirical, 'Whether the nuclear exclusion tracks the technology or the ownership arrangements around it.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of pro-nuclear dissent inside climate spaces structural (funding gates, coalition discipline) or internalized (movement identity making the position unthinkable)?',
    'Post-defection trajectory study: track organizations and individuals who broke ranks on nuclear and measure whether dissent costs persisted after funders relaxed gate conditions.',
    'If substantially internalized, effective suppression exceeds the structural measure and persists even where no enforcer acts; if structural, relaxing funding gates would rapidly widen the coalition''s technology range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of within-movement dissent costs.').

omega_variable(
    community_energy_capture_trajectory,
    'Are the frame''s benefits accruing to genuinely community-owned projects, or to corporate aggregators marketing as community solar?',
    'Longitudinal ownership-composition analysis of capacity built under community-energy programs and of grant flows labeled for energy democracy.',
    'If corporate capture dominates, theater_ratio understates hollowness and the frame drifts toward serving distributed-energy incumbents while its democratic substance atrophies — a piton-direction watch condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_energy_capture_trajectory, empirical, 'Whether the democratic core of the frame is being captured by distributed-energy incumbents.').

omega_variable(
    reading_indexed_epsilon_divergence,
    'This constraint is one reading of kernel climate_mitigation_imperative; the sibling readings author different epsilon over the same referent — which legitimacy condition should govern?',
    'No empirical resolution: the divergence is constitutive of the kernel contest. Cross-reading comparison of computed per-seat classifications against observed coalition behavior (who actually gets admitted, funded, and cited) is the available diagnostic.',
    'Adopting the portfolio reading moves nuclear from victim set to beneficiary set and deletes the democratic-control requirement; adopting the opportunity-cost reading keeps nuclear excluded but dissolves the ownership criterion, collapsing this rule''s distinct structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexed_epsilon_divergence, conceptual, 'Committer structure: reading-indexed epsilon over a shared referent, with the disagreement located in legitimacy-condition attachment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 25, 0.38).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, identity_coordination).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__opportunity_cost_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the climate mitigation imperative' covers three structurally distinct constraints that share one kernel but attach legitimacy conditions to different things. This story (systems_transition_reading) conditions legitimacy on governance form and places nuclear in the victim set and distributed renewables among beneficiaries; portfolio_optimization_reading conditions on technology breadth and places nuclear among beneficiaries; opportunity_cost_reading conditions on cost-adjusted speed and excludes nuclear on efficiency grounds. The readings are linked pairwise via affects_constraints; each carries its own epsilon, its own victim/benefit structure, and its own classification. Upstream/downstream: the systems-transition reading exerts structural pressure on the portfolio reading's operating environment (source-neutrality now requires governance justification) without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
