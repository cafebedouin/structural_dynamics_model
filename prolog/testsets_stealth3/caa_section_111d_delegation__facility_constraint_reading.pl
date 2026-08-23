% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__facility_constraint_reading, []).

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
 *   constraint_id: caa_section_111d_delegation__facility_constraint_reading
 *   human_readable: Clean Air Act Section 111(d) Facility-Level Best System Ceiling (Judicial Reading)
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   Section 111(d) of the Clean Air Act directs EPA to regulate existing
 *   stationary sources using the 'best system of emission reduction'
 *   adequately demonstrated. The statutory phrase never specified whether a
 *   'system' may span the electricity grid as a whole or only the equipment
 *   and operations of individual covered facilities. This story authors ONE
 *   reading of that contested kernel — the facility_constraint_reading —
 *   under which the best-system determination is confined to measures
 *   implementable at individual sources: heat-rate improvements, co-firing,
 *   retrofit carbon capture. Under this reading EPA cannot mandate
 *   generation-shifting, coal units are shielded from regulatorily scheduled
 *   retirement, states retain control over their resource mixes, and the
 *   seats that sought a federal transformation instrument bear the
 *   arrangement's costs instead. Constraint-family decomposition
 *   (epsilon-invariance): the colloquial question 'how far does Section
 *   111(d) authorize EPA to reach?' covers two structurally distinct
 *   arrangements with different epsilon values, and is authored as two linked
 *   stories. THIS story authors the facility-scoped reading: epsilon
 *   approximately 0.62 over the standing ceiling arrangement — concentrated
 *   gains to incumbent fossil generators against diffuse, partly prospective
 *   losses to climate-exposed seats. The sibling story (assumed constraint_id
 *   caa_section_111d_delegation__systemic_transformation_reading, mirroring
 *   this file's kernel__reading naming) authors the systemic reading, whose
 *   epsilon indexes the transformation-mandate arrangement with inverted seat
 *   polarity: heavy concentrated burdens on coal operators and mining
 *   regions, diffuse benefits to climate-exposed publics. The
 *   upstream/downstream citation traffic between the two (each side citing
 *   the same statutory text as evidence) is why they are linked rather than
 *   merged. Interval decoding: time points are years, t0 = 2010 through t16 =
 *   2026. The series traces the ceiling through one full
 *   displacement-and-restoration cycle: baseline facility-era consensus, the
 *   Clean Power Plan episode in which the ceiling lost operative force, and
 *   the appellate reversal that restored and progressively hardened it. KEY
 *   AGENTS (by structural relationship): - federal_judiciary: Agenda-setting
 *   enforcement seat (institutional/constrained) — adjudicates the statutory
 *   boundary EPA drafts within - coal_plant_operators: Primary beneficiary
 *   (powerful/trapped) — extended operating horizons preserve revenue
 *   servicing sunk capital - coal_miner_communities: Secondary beneficiary
 *   (organized/identity_locked) — employment and civic structure tied to
 *   continued production - existing_gas_generators: Incidental beneficiary
 *   (powerful/mobile) — dispatch share improves under slower renewable
 *   buildout - epa_rule_writers: Administering payer seat
 *   (institutional/mobile) — drafts standards inside the marked boundary,
 *   bears reversal risk - frontline_climate_communities: Primary payer
 *   (powerless/trapped, global scope) — absorb continued emissions with no
 *   procedural seat - renewable_energy_developers: Payer
 *   (organized/constrained) — largest federal demand lever closed to them -
 *   environmental_litigation_ngos: Payer (moderate/identity_locked) —
 *   mission-bound opposition absorbing litigation losses -
 *   coal_state_regulators / clean_energy_state_regulators: Same-level lateral
 *   pair (organized/constrained) — identical nominal office, opposite
 *   incidence - congress_legislative_branch: Excluded seat
 *   (institutional/constrained) — holds superseding authority but is absent
 *   from the adjudicated conversation - future_generations_climate_bearers:
 *   Non-agent payer entry (civilizational/trapped, global) — bear compounded
 *   damages, admitted nowhere - energy_policy_analysts: Analytical observer
 *   (analytical/analytical)
 *
 * KEY AGENTS:
 *   - - federal_judiciary: Agenda-setting enforcement seat (institutional/constrained) — marks and polices the statutory boundary
 *   - - coal_plant_operators: Primary beneficiary (powerful/trapped) — asset preservation is the arrangement's concentrated gain
 *   - - coal_miner_communities: Secondary beneficiary (organized/identity_locked) — regional livelihood continuity
 *   - - existing_gas_generators: Incidental beneficiary (powerful/mobile) — favorable dispatch conditions
 *   - - epa_rule_writers: Administering payer seat (institutional/mobile) — authority exercised inside a marked boundary
 *   - - frontline_climate_communities: Primary payer (powerless/trapped) — diffuse, compounding, procedurally unrepresented losses
 *   - - renewable_energy_developers: Payer (organized/constrained) — foreclosed federal demand driver
 *   - - environmental_litigation_ngos: Payer (moderate/identity_locked) — mission-bound opposition bearing litigation costs
 *   - - coal_state_regulators: Beneficiary seat of the same-level lateral pair (organized/constrained)
 *   - - clean_energy_state_regulators: Payer seat of the same-level lateral pair (organized/constrained)
 *   - - congress_legislative_branch: Excluded seat (institutional/constrained) — superseding authority withheld from the conversation
 *   - - future_generations_climate_bearers: Non-agent payer (civilizational/trapped) — procedural absence itself is the structural fact
 *   - - energy_policy_analysts: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.62).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.58).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "Clean Air Act Section 111(d) Facility-Level Best System Ceiling (Judicial Reading)").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, '829f5bab-cd5a-47d7-b46d-23b9f56f01ac').
narrative_ontology:cs_kernel_codification('829f5bab-cd5a-47d7-b46d-23b9f56f01ac', fixed_text).
narrative_ontology:cs_authority_grounding('829f5bab-cd5a-47d7-b46d-23b9f56f01ac', lineage).
narrative_ontology:cs_interpretation_layer_present('829f5bab-cd5a-47d7-b46d-23b9f56f01ac').
narrative_ontology:cs_reading_relation('829f5bab-cd5a-47d7-b46d-23b9f56f01ac', caa_section_111d_delegation__systemic_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('829f5bab-cd5a-47d7-b46d-23b9f56f01ac', foundational, bser_confined_to_individual_source_measures).
narrative_ontology:cs_axiom_status(bser_confined_to_individual_source_measures, holdable).
narrative_ontology:cs_axiom_grounding('829f5bab-cd5a-47d7-b46d-23b9f56f01ac', bser_confined_to_individual_source_measures, conventional).
narrative_ontology:cs_axiom('829f5bab-cd5a-47d7-b46d-23b9f56f01ac', secondary, transformative_energy_policy_requires_explicit_congressional_sanction).
narrative_ontology:cs_axiom_status(transformative_energy_policy_requires_explicit_congressional_sanction, holdable).
narrative_ontology:cs_axiom_grounding('829f5bab-cd5a-47d7-b46d-23b9f56f01ac', transformative_energy_policy_requires_explicit_congressional_sanction, deontological).
narrative_ontology:cs_reference_frame('829f5bab-cd5a-47d7-b46d-23b9f56f01ac', fence_line_facility_controls_frame).
narrative_ontology:cs_drift_state('829f5bab-cd5a-47d7-b46d-23b9f56f01ac', contemporary_post_cpp_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('829f5bab-cd5a-47d7-b46d-23b9f56f01ac', '2026-06-14T12:00:00Z').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_plant_operators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_miner_communities).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, existing_gas_generators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, frontline_climate_communities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, environmental_litigation_ngos).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_state_regulators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, epa_rule_writers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, clean_energy_state_regulators).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, major_questions_doctrine).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, state_energy_mix_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Appellate panels and the Supreme Court adjudicate what the statutory words authorize, and their rulings set the operating boundary EPA must draft within. Precedent, institutional role, and insulated tenure protect the decision point from electoral reversal; the judges who mark the boundary collect no programmatic gain and absorb no programmatic loss from how it distributes costs. Exit, for an institution constituted by adjudication, would mean abdicating the question — not a move available within the role.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Own and operate coal-fired units covered by Section 111(d). Under the facility-scoped reading, applicable standards key to equipment and operational measures available at the unit — efficiency upgrades, co-firing, retrofit capture — rather than fleet replacement schedules. Extended operating horizons preserve the revenue streams that service construction debt on sunk assets; converting fuels or retiring ahead of schedule strands capital and idles dedicated rail, water, and grid connections built around each site. Trade associations they fund sustain the amicus and litigation activity defending the reading.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_plant_operators, beneficiary,
    powerful, biographical, trapped, national).

% Towns and unionized workforces whose employment, local tax bases, and civic institutions are organized around extraction and plant operation. Standards keyed to unit-level measures keep demand for the product inside the community's planning horizon; mine and plant closures otherwise arrive on regulatory timetables set elsewhere. Leaving means abandoning kin networks, place-bound skills, and a self-understanding built around the work — relocation is experienced as becoming someone else rather than moving.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_miner_communities, beneficiary,
    organized, generational, identity_locked, regional).

% Operate dispatchable gas fleets whose dispatch share improves when coal units remain online and renewable buildout proceeds at market-plus-tax-credit pace rather than mandate pace. Flexible fuel procurement and multi-market positioning mean their position improves under the reading without depending on it — they could reposition portfolios quickly if the operative standard changed.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, existing_gas_generators, beneficiary,
    powerful, biographical, mobile, national).

% Career staff and rotating political appointees who draft the Section 111 standards and thereby administer the program day to day. Every proposal must survive appellate review against the marked boundary; drafts reaching past unit-level measures invite reversal, litigation exposure, and career damage, so proposals are narrowed in anticipation. Staff can and do rotate out to firms, consultancies, and other agencies — the exit is personal mobility, not relief from the drafting constraint while seated.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, epa_rule_writers, payer,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, epa_rule_writers, agenda_setter).

% Live in flood-, heat-, and storm-exposed regions that absorb the damages of whatever emissions continue under the operative standard. They hold no formal seat in rulemakings or appellate argument; their stake reaches proceedings only through generalized comment periods and NGO proxies. Household migration reduces some families' exposure at high personal cost and leaves the accumulating stock of warming untouched — there is no geography outside the arrangement's incidence.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, frontline_climate_communities, payer,
    powerless, generational, trapped, global).

% Build wind, solar, storage, and transmission projects. With federal standards confined to unit-level measures, federal rulemaking buys less of their product than a portfolio-scoped standard would, and sector growth runs on state mandates and tax credits alone. They operate profitably and can develop anywhere in the country — the binding limitation is not mobility but the closure of the single largest policy demand lever their business case references.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers, payer,
    organized, biographical, constrained, national).

% Public-interest law firms and advocacy organizations that mount administrative-law challenges and organize comment campaigns against the operative reading's scope. Their organizational missions, staffing, and donor relationships are constituted around this policy terrain; exiting would dissolve the organization rather than relocate it. Losses arrive as adverse rulings, spent litigation budgets, and doctrinal setbacks rather than direct levies — and each loss raises the perceived cost of the next challenge.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, environmental_litigation_ngos, payer,
    moderate, generational, identity_locked, national).

% State public utility commissions and environmental agencies in jurisdictions where severance royalties, plant payrolls, and comparatively low retail rates tie fiscal and political budgets to continued fossil dispatch. Implementation flexibility under unit-level standards lets them demonstrate compliance without restructuring state resource plans or confronting the revenue consequences of scheduled retirements.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_state_regulators, beneficiary,
    organized, generational, constrained, regional).

% Commissions in states pursuing aggressive renewable buildouts. Absent a federal standard pulling lagging states upward, their industries compete against neighboring grids running cheaper legacy generation, and interstate leakage erodes in-state program economics. They cannot compel other states' resource choices, and their own citizens finance the difference between their ambition and the national baseline.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, clean_energy_state_regulators, payer,
    organized, generational, constrained, regional).

% Holds the authority to amend Section 111 or to authorize or forbid generation-shifting outright, either of which would supersede any judicial reading of the current text. Senate supermajority thresholds, committee gatekeeping, and member-level electoral incentives have kept amendment off the docket. Members answer constituents on both sides of the question and would speak with decisive authority if convened — the conversation over the statute's meaning currently proceeds without them.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, congress_legislative_branch, excluded,
    institutional, generational, constrained, national).

% Bear the compounded consequences of emissions released under whichever standard operates during the decades the arrangement persists. No procedural mechanism admits them to rulemaking dockets, advisory committees, or appellate argument; their interests enter only as proxy claims voiced by present-day litigants. Listed for completeness of the incidence picture — as a non-acting entity it contributes no behavioral seat, but the damages ledger is theirs.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, future_generations_climate_bearers, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(caa_section_111d_delegation__facility_constraint_reading, future_generations_climate_bearers).

% Academic economists and legal scholars who model compliance pathways, publish on the statutory interpretation, and testify or consult when invited. They take no administrative side; journals, seminars, and agency dockets are their interfaces with the arrangement, and their analyses are cited by every faction without belonging to any.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, energy_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates regulatory authority between the federal government and the states, and disciplines agency discretion: EPA solves the unit-level engineering problem once, centrally (which control technologies count as adequately demonstrated, at what stringency, verified how), while decisions about the shape of the generation fleet remain with states and market actors. It also gives regulated entities a predictable compliance perimeter and gives the polity a rule for which branch decides transformative economic questions.
% TRANSFER_FUNCTION: Moves compliance burden and investment direction rather than money directly: avoided retrofit-and-retirement costs flow to incumbent fossil generators (extended asset lives, preserved debt service); the corresponding emission reductions are not delivered, so climate damages accumulate on exposed publics and future cohorts; rulemaking ambition and litigation risk are absorbed by agency staff; and the demand pull that a portfolio-scoped standard would have provided to renewable builders is withdrawn.
% ABSENT_VOICES: Future cohorts hold no seat in any forum and never will without present-day proxy representation; frontline climate communities participate only through comment-period channels and NGO intermediation; Congress — the body whose intent the reading purports to reconstruct — is absent from the adjudications that settle the meaning; renewable developers appear as commercial commenters but not as participants in the doctrinal argument that determines their largest demand lever.
% DISAPPEARANCE_RATIONALE: If the facility-scoped reading ceased to bind overnight, EPA could issue portfolio-level standards crediting generation substitution and accelerated coal retirement; coal-unit lifetimes would shorten on regulatory timetables, renewable and transmission buildout would accelerate against a federal demand floor, coal-dependent state budgets would confront revenue cliffs they currently defer, and litigation coalitions would redeploy from defending to attacking the new baseline — a visible redistribution of regulatory power and capital flows, not a landscape unchanged.
% FOUNDING_PROBLEM: The Clean Air Act Amendments of 1990 left 'best system of emission reduction' undefined on precisely the question that later mattered: whether a 'system' may comprise measures beyond an individual source's fence line, and how far Congress intended EPA to reach into decisions (fuel choice, plant scheduling, grid composition) that look like state and utility policy rather than pollution control. After the Clean Power Plan forced the question into the open, the further founding problem became who decides transformative policy questions of vast economic and political significance when the elected branches have not spoken clearly — the accountability boundary for delegated authority.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: administrative-law scholarship across the interpretive spectrum treats the boundary question as recurrent rather than settled; environmental NGO litigation records document that every subsequent Section 111 rulemaking is contested on exactly this axis; and EPA's own successive rule designs — each narrower in rhetorical claim than the last, each still challenged — attest that the agency does not regard the boundary as administratively settled. No attestation comes from within the coal-benefiting set alone; the recurring-litigation pattern is independently documented in appellate docket records.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the ceiling forecloses the portfolio measures that most cost-effectively reduce power-sector emissions, while its gains are concentrated (extended asset lives for incumbent generators) and its losses diffuse and compounding (continued emissions borne globally and prospectively). It is not maximal: states remain free to transform voluntarily, facility measures still deliver real reductions, and the arrangement preserves genuine planning predictability. Suppression 0.58: the foreclosed option is suppressed by external judicial invalidation threat plus an internalized component (agencies narrowing proposals in anticipation — see the suppression_mechanism_split omega); no direct coercive force touches individuals, and suppression here is a raw structural property, unscaled by power or scope — only extractiveness is scaled downstream. Theater ratio 0.31: a real interpretive craft operates beneath a performed neutrality ('we merely apply the statute') that obscures the arrangement's distributional incidence. Accessibility collapse 0.42: once the ceiling is understood, EPA's system-level option collapses entirely, but Congress, states, and markets remain open channels — alternatives narrow sharply without disappearing. Resistance 0.60: sustained NGO litigation, scholarly critique, and boundary-probing rulemaking meet the ceiling continuously without displacing it. The claimed type (tangled_rope) and the metrics were authored independently: the claim asserts a genuine coordination function (stable federal/state authority allocation, separation-of-powers discipline) coexisting with asymmetric extraction; the metric values assert what descriptively obtains. The measurement series run on one shared time grid ({0,3,6,8,10,12,14,16}) so every tracked metric is authored at every examined point; suppression_requirement is tracked deliberately because the story's narrative is one of enforcement intensification after t12, not a static enforcement picture. Provenance caveat: prompt_commit and schema_commit are recorded as unrecorded_at_generation_time because the generating process had no access to repository state; all other provenance fields reflect actual generation conditions (model ox-alpha, temperature=1.0, unseeded, draw 0).
 *
 * PERSPECTIVAL GAP:
 *   Per-seat classifications should diverge sharply. From the coal operator seat the arrangement presents as ordinary regulatory predictability protecting capital already committed — a stable rule of the road. From the climate-exposed and NGO seats the same structure operates as an enforced ceiling that converts their preferred instrument into an unusable one while its costs compound. From the judiciary's own seat it is boundary-keeping with no stake in incidence. The engine computes these divergences from the structural data; this commentary does not adjudicate them.
 *   
 *   Same-level lateral dynamics: coal_state_regulators and clean_energy_state_regulators hold identical offices, identical formal authority, and identical exit constraints — yet experience opposite arrangements. The differentiator is grid composition and revenue exposure: royalty and payroll-linked budgets versus leakage-exposed renewable buildouts. Equal global standing, opposite incidence.
 *   
 *   Coalition potential: frontline_climate_communities are individually powerless and procedurally seatless, but they are not structurally mute — environmental_litigation_ngos function as a funded proxy coalition carrying their interests into the exact forums (appellate argument, rulemaking dockets) the communities cannot enter. The measured resistance level substantially reflects that coalition rather than any single seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: coal_plant_operators (trapped exit deepens their dependence on the arrangement's continuation), coal_miner_communities (identity-locked), existing_gas_generators (mobile exit pulls them nearest the beneficiary pole — they gain incidentally and could reposition if the arrangement changed). Victim declarations drive high directionality: frontline_climate_communities (powerless, trapped, global scope — the scope amplifier applies because verification of diffuse harms grows harder at planetary scale), renewable_energy_developers (constrained), environmental_litigation_ngos (identity-locked opposition). epa_rule_writers carry a dual seat: administering the program (agenda-setter secondary role) while bearing curtailed authority and reversal risk (primary payer role). federal_judiciary sits administratively symmetric — it collects no programmatic gain and absorbs no programmatic loss. No directionality overrides are used: the derivation chain distinguishes even the two same-power state-regulator seats through their opposed beneficiary/victim declarations, so an override keyed to a shared power atom would only blur what the structural data already separates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — marking the outer edge of delegated authority over questions of transformative economic significance — remains live: every subsequent Section 111 rulemaking is drafted against it and litigated through it, so no mandatrophy_resolved flag is declared. The tangled_rope classification does double duty here: it blocks the mislabel of the ceiling as pure extraction (which would erase the genuine federalism and anti-commandeering coordination the arrangement performs and which survives distributionally neutral reformulations), and it blocks the mislabel of pure coordination (which would erase the measurable incidence: concentrated gains, diffuse compounding losses, a foreclosed instrument). For the mismatch consumer: founding_problem_status 'live' combined with disappearance_verdict 'world_rearranges' is the coherent cell — the boundary question is unresolved AND the arrangement's removal would visibly redistribute regulatory power and investment flows — so no zombie flag should fire from this story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of kernel caa_section_111d_delegation (facility_constraint_reading); what structurally changes if the sibling reading (caa_section_111d_delegation__systemic_transformation_reading) becomes the operative interpretation?',
    'Doctrinal settlement: an authoritative Supreme Court ruling resolving the extension of ''best system of emission reduction'', or an explicit congressional amendment authorizing or forbidding generation-shifting under Section 111(d).',
    'Under the sibling reading the beneficiary/victim structure inverts: coal operators and coal-mining regions become the paying seats, climate-vulnerable publics and renewable developers gain a regulatory instrument, and this file''s ceiling arrangement dissolves into the sibling''s mandate arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story instantiates the facility-scoped reading of the Section 111(d) delegation kernel; the sibling reading assigns opposite polarities to the same seats.').

omega_variable(
    bser_textual_underdetermination,
    'Where the kernel contest is located: does the statutory text and legislative history of the phrase ''best system of emission reduction'' support confining the system to measures implementable at individual covered sources, or does ''system'' extend to fleet- and grid-wide measures?',
    'Textualist exegesis, archival legislative-history research, and comparison with parallel statutory usages elsewhere in the Clean Air Act; ultimately adjudicated in appellate argument.',
    'If the text itself privileges the facility-scoped meaning, this reading''s coordination function rests on firmer footing and measured extraction reads as the price of textual fidelity; if the text is genuinely capacious, the ceiling is better read as a distributionally motivated narrowing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bser_textual_underdetermination, conceptual, 'The textual underdetermination that sustains both sibling readings of the kernel.').

omega_variable(
    foregone_abatement_cost_magnitude,
    'How much additional decarbonization cost does the facility-measures-only pathway impose relative to a pathway permitting generation substitution and accelerated coal retirement?',
    'Integrated assessment modeling and levelized-cost comparisons of facility measures (heat-rate improvement, retrofit capture) versus portfolio measures under equivalent emissions targets.',
    'A large cost differential would raise the measured burden the ceiling places on climate-exposed seats and support reclassification pressure toward the snare boundary; a small differential would support the reading''s own claim that little of substance is foregone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foregone_abatement_cost_magnitude, empirical, 'Magnitude of the economic burden shifted onto climate-bearing seats by the facility-only scope.').

omega_variable(
    judicial_durability_of_ceiling,
    'Will the ceiling persist under foreseeable changes in court composition and in the litigation pipeline (challenges to subsequent Section 111 rulemakings), or is it a contingent artifact of the current appellate configuration?',
    'Track confirmation patterns of relevant appellate vacancies, the outcome record of subsequent Section 111(d) cases, and the rate at which boundary-probing rule proposals survive review.',
    'Persistence hardens the ceiling (pushing the arrangement toward enforced-extraction territory); fragility would soften it toward a transitional boundary-marking device whose costs are temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_durability_of_ceiling, empirical, 'Durability of the judicial enforcement that holds the ceiling in place.').

omega_variable(
    coordination_or_cover_question,
    'Is the authority-allocation function served by the ceiling genuine coordination (a stable federal/state boundary that would be worth maintaining under any distributional outcome), or is it cover adopted because its distributional incidence happens to protect incumbents?',
    'Counterfactual coalition analysis: ask whether the same supporting coalition would defend an identical authority boundary under a distributionally neutral assignment of compliance burdens; examine whether supporters'' revealed preferences track the boundary or the incidence.',
    'If the boundary function is genuine, the arrangement sits squarely in hybrid coordination/extraction territory; if the boundary talk is cover, the coordination gate weakens and the arrangement trends toward pure extraction with a constitutional alibi.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_or_cover_question, conceptual, 'Whether the ceiling''s coordination function is real or a post hoc rationale.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression of ambitious rulemaking structural (external threat of judicial invalidation) or internalized (anticipatory self-limitation inside the agency, where staff narrow proposals before any external actor does)?',
    'Post-doctrinal-change trajectory: compare the ambition envelope of draft proposals before and after enforcement events; interview and documentary evidence of internal review thresholds.',
    'If largely internalized, the ceiling''s restrictive force would outlast the enforcement conditions that produced it — suppression carried in agency habit rather than imposed by courts — raising effective suppression above the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized suppression of the foreclosed regulatory option.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa111d_facility_tr_t0, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(caa111d_facility_tr_t0, observed).
narrative_ontology:measurement(caa111d_facility_tr_t3, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement_basis(caa111d_facility_tr_t3, observed).
narrative_ontology:measurement(caa111d_facility_tr_t6, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement_basis(caa111d_facility_tr_t6, observed).
narrative_ontology:measurement(caa111d_facility_tr_t8, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement_basis(caa111d_facility_tr_t8, observed).
narrative_ontology:measurement(caa111d_facility_tr_t10, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(caa111d_facility_tr_t10, observed).
narrative_ontology:measurement(caa111d_facility_tr_t12, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(caa111d_facility_tr_t12, observed).
narrative_ontology:measurement(caa111d_facility_tr_t14, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 14, 0.3).
narrative_ontology:measurement_basis(caa111d_facility_tr_t14, observed).
narrative_ontology:measurement(caa111d_facility_tr_t16, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(caa111d_facility_tr_t16, observed).

% Extraction over time
narrative_ontology:measurement(caa111d_facility_be_t0, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(caa111d_facility_be_t0, observed).
narrative_ontology:measurement(caa111d_facility_be_t3, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 3, 0.36).
narrative_ontology:measurement_basis(caa111d_facility_be_t3, observed).
narrative_ontology:measurement(caa111d_facility_be_t6, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 6, 0.22).
narrative_ontology:measurement_basis(caa111d_facility_be_t6, observed).
narrative_ontology:measurement(caa111d_facility_be_t8, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 8, 0.3).
narrative_ontology:measurement_basis(caa111d_facility_be_t8, observed).
narrative_ontology:measurement(caa111d_facility_be_t10, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(caa111d_facility_be_t10, observed).
narrative_ontology:measurement(caa111d_facility_be_t12, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(caa111d_facility_be_t12, observed).
narrative_ontology:measurement(caa111d_facility_be_t14, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 14, 0.58).
narrative_ontology:measurement_basis(caa111d_facility_be_t14, observed).
narrative_ontology:measurement(caa111d_facility_be_t16, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement_basis(caa111d_facility_be_t16, observed).

% Suppression requirement over time
narrative_ontology:measurement(caa111d_facility_su_t0, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(caa111d_facility_su_t0, observed).
narrative_ontology:measurement(caa111d_facility_su_t3, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 3, 0.32).
narrative_ontology:measurement_basis(caa111d_facility_su_t3, observed).
narrative_ontology:measurement(caa111d_facility_su_t6, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 6, 0.16).
narrative_ontology:measurement_basis(caa111d_facility_su_t6, observed).
narrative_ontology:measurement(caa111d_facility_su_t8, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 8, 0.26).
narrative_ontology:measurement_basis(caa111d_facility_su_t8, observed).
narrative_ontology:measurement(caa111d_facility_su_t10, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(caa111d_facility_su_t10, observed).
narrative_ontology:measurement(caa111d_facility_su_t12, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(caa111d_facility_su_t12, observed).
narrative_ontology:measurement(caa111d_facility_su_t14, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 14, 0.57).
narrative_ontology:measurement_basis(caa111d_facility_su_t14, observed).
narrative_ontology:measurement(caa111d_facility_su_t16, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement_basis(caa111d_facility_su_t16, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation__systemic_transformation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Section 111(d)' decomposes into two epsilon-invariant stories per the epsilon-invariance principle — measuring the arrangement through the facility-scoped observable yields epsilon approximately 0.62 with the polarity declared here; measuring through the systemic observable yields a different arrangement with inverted seat polarity and a different epsilon. They are two constraints sharing one kernel text, not one constraint viewed twice. Each file links the other via affects_constraints; citation traffic between the readings (each side invoking the same statutory language) travels along this edge. Sibling constraint_id assumed as caa_section_111d_delegation__systemic_transformation_reading following this file's kernel__reading naming pattern; correct at compile time if the sibling was minted under a different stem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
