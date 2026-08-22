% ============================================================================
% CONSTRAINT STORY: climate_response_action__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__degrowth_transformation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: climate_response_action__degrowth_transformation
 *   human_readable: Climate Response: Degrowth Transformation Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint is ONE READING of a contested kernel: 'climate response
 *   action.' The degrowth transformation reading holds that climate
 *   stabilization requires rejecting GDP growth as an organizing principle,
 *   restructuring toward sufficiency economics, equity redistribution, and
 *   reduced resource throughput. It competes with two sibling readings:
 *   mitigation_priority (technological solutions within growth) and
 *   adaptation_priority (resilience acceptance and protection). This JSON
 *   instantiates ONLY the degrowth reading as a structurally coherent
 *   constraint with its own ε, beneficiary/victim structure, and type — not a
 *   compromise between readings. The constraint is claimed as Tangled Rope:
 *   it coordinates a genuine transition problem (climate stabilization +
 *   equity) AND extracts from wealthy populations and incumbent industries,
 *   requiring active enforcement to suppress alternatives and manage
 *   political resistance.
 *
 * KEY AGENTS:
 *   - Wealthy populations (Global North): Payers bearing consumption reduction and wealth redistribution burden
 *   - Global South populations: Beneficiaries receiving development rights and climate reparations
 *   - Low-income households (current, global): Beneficiaries of universal basic services, also bearing transition disruption
 *   - Future generations: Beneficiaries of stabilized climate and reduced intergenerational burden
 *   - Incumbent fossil fuel industries: Payers facing managed decline and identity transformation
 *   - High-consumption economies: Payers undergoing productive restructuring toward sufficiency goods
 *   - Democratic firm advocates: Agenda-setters proposing cooperative and participatory governance models
 *   - Climate science community: Observers providing empirical warrant for the founding problem
 *   - Incumbent political economy (WTO, IMF, Bretton Woods): Excluded; structurally displaced by the constraint
 *   - International financial institutions: Excluded; their growth-financing function becomes obsolete
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, 0.68).
domain_priors:suppression_score(climate_response_action__degrowth_transformation, 0.72).
domain_priors:theater_ratio(climate_response_action__degrowth_transformation, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_action__degrowth_transformation, "Climate Response: Degrowth Transformation Reading").
narrative_ontology:topic_domain(climate_response_action__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__degrowth_transformation, 'fb051176-6f1a-41a3-896b-fa6726359624').
narrative_ontology:cs_kernel_codification('fb051176-6f1a-41a3-896b-fa6726359624', distributed).
narrative_ontology:cs_authority_grounding('fb051176-6f1a-41a3-896b-fa6726359624', extraction).
narrative_ontology:cs_interpretation_layer_present('fb051176-6f1a-41a3-896b-fa6726359624').
narrative_ontology:cs_reading_relation('fb051176-6f1a-41a3-896b-fa6726359624', climate_response_action__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('fb051176-6f1a-41a3-896b-fa6726359624', climate_response_action__adaptation_priority, influences).
narrative_ontology:cs_axiom('fb051176-6f1a-41a3-896b-fa6726359624', foundational, technological_decoupling_failure).
narrative_ontology:cs_axiom_status(technological_decoupling_failure, holdable).
narrative_ontology:cs_axiom_grounding('fb051176-6f1a-41a3-896b-fa6726359624', technological_decoupling_failure, empirically_contingent).
narrative_ontology:cs_axiom('fb051176-6f1a-41a3-896b-fa6726359624', foundational, intergenerational_equity_zero_discount).
narrative_ontology:cs_axiom_status(intergenerational_equity_zero_discount, holdable).
narrative_ontology:cs_axiom_grounding('fb051176-6f1a-41a3-896b-fa6726359624', intergenerational_equity_zero_discount, deontological).
narrative_ontology:cs_axiom('fb051176-6f1a-41a3-896b-fa6726359624', secondary, sufficiency_over_growth).
narrative_ontology:cs_axiom_status(sufficiency_over_growth, holdable).
narrative_ontology:cs_axiom_grounding('fb051176-6f1a-41a3-896b-fa6726359624', sufficiency_over_growth, instrumental).
narrative_ontology:cs_reference_frame('fb051176-6f1a-41a3-896b-fa6726359624', growth_dependent_political_economy).
narrative_ontology:cs_drift_state('fb051176-6f1a-41a3-896b-fa6726359624', climate_stabilization_deadline_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fb051176-6f1a-41a3-896b-fa6726359624', '2026-06-11T14:32:51Z').
narrative_ontology:cs_kernel_id(climate_response_action__degrowth_transformation, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, low_income_households_current).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, wealthy_populations_global_north).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, incumbent_fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, high_consumption_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, low_income_households_current).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary burden of transformation through reduced material consumption, working-time restructuring, and wealth redistribution to fund Global South adaptation and development. Their current consumption levels and carbon footprints are identified as incompatible with planetary boundaries. Exit from the constraint means climate stabilization remains unachievable at the scale required.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, wealthy_populations_global_north, payer,
    powerful, biographical, constrained, global).

% Gain development rights and climate reparations-funded adaptation infrastructure. Under this reading, they are currently bearing climate impacts (floods, droughts, sea-level rise) disproportionately despite contributing minimally to atmospheric CO2. The constraint redirects Northern consumption reductions into Southern development capacity and resilience building.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_south_populations, beneficiary,
    powerless, generational, trapped, global).

% Benefit from universal basic services (energy, healthcare, food, housing, transportation) decoupled from income and consumption; bear short-term disruption costs from energy system transition and industrial restructuring. Under degrowth framing, they are protected from market volatility through guaranteed provisioning rather than commodified access.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, low_income_households_current, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, low_income_households_current, payer).

% Inherit a climate system stabilized below the 1.5°C overshoot threshold (or constrained rise), with intact ecosystems and reduced climate cascade risks. The constraint shifts burden from them to current wealthy populations by front-loading consumption reductions now rather than imposing adaptation costs on inheritors of a destabilized planet.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Face managed decline and stranded assets under the degrowth reading. Their business model (carbon throughput) is structurally incompatible with the constraint's reduced resource throughput principle. They cannot exit because their institutional identity IS fossil-fuel extraction; transformation requires dissolution of the industry's economic form.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, incumbent_fossil_fuel_industries, payer,
    institutional, biographical, identity_locked, global).

% National economies built on high-throughput material consumption (manufacturing for export, tourism, luxury goods, finance) must restructure productive capacity toward sufficiency goods and services. GDP contraction is expected and framed as necessary, not pathological. Exit means failing to stabilize climate and losing development legitimacy.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, high_consumption_economies, payer,
    institutional, generational, constrained, national).

% Set and advocate for the constraint's institutional architecture: worker cooperatives, stakeholder governance, democratic ownership of energy and food systems. They propose the structural mechanisms (universal basic services, working-time reduction, firm ownership redistribution) that operationalize degrowth principles. Their power comes from intellectual authority and coalition-building, not incumbent control.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, democratic_firm_advocates, agenda_setter,
    organized, generational, mobile, global).

% Provides empirical warrant for the constraint's founding problem: planetary carbon budgets, climate system tipping points, and the inadequacy of technological substitution to meet Paris Agreement targets at current consumption levels. They occupy an analytical seat, not an advocacy seat, though the degrowth reading draws legitimacy partly from their findings about emissions reduction necessity.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, climate_science_community, observer,
    organized, generational, analytical, global).

% The Bretton Woods order, WTO framework, and growth-dependent financial systems (pension funds indexed to equities, debt-service requirements tied to GDP growth) are structurally incompatible with degrowth principles. They are not at the negotiating table but would be displaced by the constraint's institutionalization. Their exclusion is structural, not accidental.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, incumbent_political_economy, excluded,
    institutional, generational, trapped, global).

% IMF conditionality, World Bank debt structures, and growth-indexed lending would be obsolete under degrowth architecture. They are excluded because their core function (growth financing) contradicts the constraint's foundation. Alternative financing structures (climate reparations funds, participatory budgeting, sovereign debt jubilee) would replace them.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, international_financial_institutions, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__degrowth_transformation, diffuse).
narrative_ontology:fixing_cost_class(climate_response_action__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the intersection of three coordination problems: (1) Global emissions must contract to stabilize climate; (2) contraction must be distributed equitably, not imposed on those who contributed least to atmospheric CO2; (3) sufficiency and equity must be achieved without technological substitution as a crutch for continued high consumption. The constraint coordinates a transition from growth-dependent political economy to provisioning-based sufficiency systems.
% TRANSFER_FUNCTION: Transfers consumption rights from wealthy populations (Global North high-income households and high-throughput economies) to Global South populations and future generations. Operationally: wealth from Northern wealth taxes and consumption reductions flows to climate adaptation infrastructure in vulnerable regions, universal basic services for low-income households globally, and reduced working time for workers to offset productivity gains without consumption growth. Resources flow from current wealthy to current poor and to posterity.
% ABSENT_VOICES: Incumbent extractive industries, international financial institutions, and export-dependent high-consumption economies are structurally excluded. They would argue (1) that degrowth is economically impossible, (2) that technological substitution and carbon markets can achieve climate stability within growth frameworks, (3) that developing nations should not sacrifice growth trajectories already pursued by the North. These voices are kept out because their interests structurally conflict with the constraint's core principle — they represent the economic form the constraint exists to transform, not legitimate alternatives within the constraint's own frame.
% DISAPPEARANCE_RATIONALE: If the degrowth transformation constraint disappeared, the world would snap back to growth-priority framings (mitigation via technology, adaptation via privatized resilience). Atmospheric CO2 would continue rising beyond Paris targets. Wealth inequality would remain or widen. Vulnerable populations would continue bearing climate impacts disproportionately. The constraint's disappearance would mean the political and economic transformation it requires never happens — a different planetary trajectory emerges.
% FOUNDING_PROBLEM: Current global political economy cannot achieve climate stabilization within growth frameworks because (a) decarbonization decoupled from emissions is not happening at scale; (b) technological substitution is too slow and unreliable; (c) growth-dependent economies cannot internalize climate externalities without collapsing their legitimacy; (d) equity demands (climate reparations, development rights) are incompatible with growth-as-organizing-principle; (e) future generations will inherit a climate system destabilized by current consumption, making the burden-shift intergenerationally regressive.
% FOUNDING_PROBLEM_CORROBORATION: Climate science (IPCC, NASA GISS, et al.) corroborates that current emissions trajectories exceed carbon budgets and that technological substitution alone cannot close the gap. Ecological economists (Georgescu-Roegen, Jackson, Hickel) corroborate that decarbonization without degrowth is structurally impossible under current economic accounting. However, mainstream climate policy (UNFCCC, World Economic Forum) contests the diagnosis, arguing that efficiency and technology markets can deliver climate stability within growth. Incumbent political economy and fossil fuel industries dispute that the founding problem exists at all, or frame it as solvable without transformation. The problem's status is live for those who accept the scientific warrant (Global South negotiators, climate-justice movements) and dead/displaced for those who reject growth-incompatibility (OECD capitals, financial sector).
narrative_ontology:disappearance_verdict(climate_response_action__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__degrowth_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__degrowth_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__degrowth_transformation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint's implementation demands wealth transfer and consumption reduction from powerful populations and industries — this is not negotiable within the reading's own logic. Suppression is also high (0.72) because the constraint requires active suppression of (1) incumbent political economy institutions, (2) growth-dependent financial mechanisms, (3) technological-substitution alternatives that compete with degrowth framing. Theater rises modestly over time (0.28→0.41) as implementation includes rhetorical commitment-maintenance (green-growth greenwashing, net-zero pledges, climate theater) but the core extraction and suppression remain. Accessibility collapse is moderate (0.58): once the reading's logic is accepted, alternatives (continued growth, tech-only solutions) appear logically impossible within that frame, but the frame itself remains contested — many populations maintain genuine belief in growth-compatibility with climate. Resistance is high (0.71) because powerful incumbent institutions will actively resist degrowth reorganization; the constraint succeeds only if that resistance is overcome through political organization, not absorbed through market mechanisms. The measurement series shows extractiveness and suppression rising as implementation proceeds (time 0-28 observed, time 28-50 projected), then plateauing — the curve reflects expectation that core structural transformation would be achieved by mid-interval, with later measurements capturing maintenance state.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (Global South, future generations, low-income households) and the payer seats (wealthy populations, high-consumption economies, incumbent industries) should compute very different types from this same structural data. From the beneficiary perspective, this is coordination toward equity and planetary stability — cooperative, necessary. From the payer perspective, especially the powerful-payer seats, this is extraction enforced against their interests — coercive, unjust. The engine computes per-seat type from directionality and the four power atoms; this divergence is PREDICTED from the structural asymmetry (beneficiaries are powerless or organized at low power, payers are powerful or institutional). The claim/metric gap (claimed tangled_rope, metrics showing high extraction+suppression) reflects that this reading's own logic IS both coordination AND extraction-from-the-wealthy simultaneously — the reading does not deny or minimize extraction, it JUSTIFIES extraction from high-consumption populations as the cost of climate justice and intergenerational equity.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy populations and incumbent industries: d approaches 1.0 (full target) because they bear the extraction, have identity-locked exit (fossil fuel industries cannot exit fossil fuels and remain themselves), and lack arbitrage options (consumption-reduction requirements are universal for the North under this reading). High-consumption economies: d ~0.9 (strongly targeted) because productive restructuring is mandatory and GDP contraction is expected/enforced. Global South populations: d ~0.1-0.2 (beneficiary-end) because the constraint subsidizes their development and adaptation rights. Future generations: d ~0.0 (full beneficiary, though analytically positioned — they collect all the climate benefit). Low-income households (current): d ~0.4-0.5 (near-symmetric) — they benefit from universal basic services but also bear disruption costs from energy transition and economic restructuring. This asymmetry is the constraint's core: beneficiaries and targets are DIFFERENT populations, which is why enforcement is necessary (targets do not self-enforce constraints that extract from them).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids the mandatrophy trap by MAINTAINING its founding problem as live and contested. The founding problem (climate stabilization + equity is impossible within growth frames) remains true under the reading's own logic — the constraint does not solve the problem by making it disappear; it solves it by RESTRUCTURING political economy so the problem becomes solvable. The constraint is not a theater piece masking a solved problem; it is an active reorganization of the conditions under which climate response is possible. The theater_ratio plateau (0.40-0.42) reflects expectation that some commitment-maintenance theater will persist even after structural implementation (national governments declaring 'sustainable growth', net-zero pledges that minimize actual sacrifice, etc.), but this theater is diagnosed as a residue, not the constraint's primary function. Mandatrophy would manifest if theater_ratio rose sharply while extractiveness and suppression fell — that would indicate the constraint had become performative rather than structural. The forecast does not show this pattern; instead, theater stabilizes while extraction and suppression reach a steady operational state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_decoupling_empirical_contested,
    'Can decarbonization be achieved without absolute material/energy throughput reduction, given current decoupling evidence and future substitution potential?',
    'Continued monitoring of decoupling metrics (carbon intensity per unit GDP, emissions trend vs. growth trend). A natural experiment: jurisdictions implementing emissions-reduction targets within growth frameworks (EU, UK, etc.) show whether absolute decoupling persists or reverses over 10+ year horizons.',
    'If decoupling fails and rebounds (rebound effect dominates, absolute emissions grow despite efficiency gains), degrowth reading''s empirical warrant strengthens and the constraint''s necessity increases. If decoupling succeeds and persists, mitigation_priority reading gains warrant and degrowth becomes politically optional rather than necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_decoupling_empirical_contested, empirical, 'Whether the founding problem''s core premise (decoupling failure) is empirically true or false.').

omega_variable(
    political_feasibility_of_degrowth_restructuring,
    'Is the institutional transformation (cooperative ownership, universal basic services, working-time reduction, wealth redistribution) politically achievable against organized incumbent resistance, or does political resistance make it a theoretical proposition with zero implementation probability?',
    'Observational: (1) Do any nation-states or supranational bodies adopt core degrowth policies (wealth tax, universal basic income, mandatory cooperative firm structures) at scale? (2) Do implementation attempts face organized sabotage or normalization? (3) What proportion of the global economy would need to move toward degrowth for the climate impact to be decisive (carbon budget management)? Can that critical mass be achieved politically?',
    'If political feasibility proves extremely low (power of incumbent institutions too high), the constraint becomes a ''piton'' — theoretically sound but practically unmaintained except through theater. If feasibility is moderate-to-high (movements gain institutional power, legislation passes), the constraint remains a tangled_rope (genuine coordination + enforcement). If feasibility is very high (rapid political reorganization around degrowth), the constraint becomes closer to rope (coordination perceived as necessary, not enforced extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_of_degrowth_restructuring, preference, 'Political viability of the structural transformation degrowth requires — is it achievable or merely advocated?').

omega_variable(
    beneficiary_acceptance_paradox,
    'Will Global South and low-income household beneficiaries actually accept universal basic services and working-time reduction as preferable to current market-mediated access and growth-enabled upward mobility? Or is the degrowth reading''s assumption of beneficiary preference counterfactual?',
    'Survey and deliberative research: ask Global South populations and low-income households whether they prefer (a) access via universal provisioning without growth, or (b) current market access with growth-opportunity claims. Observe revealed preference: do populations organize politically to demand degrowth restructuring, or do they demand integration into growth-opportunity frameworks?',
    'If beneficiaries reject the reading''s framing (prefer growth-opportunity and market access), the constraint lacks its assumed coordination function and becomes pure extraction from payers imposed on resisting beneficiaries — a snare, not tangled_rope. If beneficiaries accept sufficiency provisioning as preferable, the tangled_rope classification holds. If acceptance is mixed or conditional (yes to basic services, no to working-time reduction; yes at low income, no at moderate income), the constraint becomes a hybrid with perimeter effects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_acceptance_paradox, preference, 'Whether beneficiaries actually endorse the degrowth reading''s value frame or resist it in practice.').

omega_variable(
    reading_containment_logic,
    'Does the degrowth reading logically contain and supersede mitigation_priority and adaptation_priority, or are the three readings genuinely incommensurate framings with no logical hierarchy?',
    'Formal logical analysis: can a framework hold all three readings simultaneously (degrowth restructuring + technological innovation + resilience adaptation), or does accepting degrowth''s core premises (decoupling failure, equity redistribution necessity) logically foreclose the other two? Or do the other two foreclose degrowth (if technology succeeds, degrowth becomes unnecessary; if adaptation succeeds, degrowth becomes politically impossible)? The logical containment determines the reading_relations assignment.',
    'If degrowth logically contains the others (they are special cases of a degrowth framework), the reading_relations shift from ''coexists_with'' to ''forecloses''. If they are incommensurate (different epistemic frames, no logical hierarchy), the ''coexists_with'' classification holds and the kernel remains genuinely contested without interior resolution. If technological success forecloses degrowth, then mitigation_priority forecloses this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_containment_logic, conceptual, 'Logical relations between degrowth and sibling readings: are they commensurate or incommensurate?').

omega_variable(
    carbon_reparations_legitimacy,
    'Does the Global North''s historical responsibility for atmospheric CO2 (cumulative emissions) generate a legitimate moral and economic claim for carbon reparations, or is such reparations framing anachronistic/contested?',
    'Normative analysis and political alignment: do Global South nations, Indigenous communities, and climate-justice movements organize around carbon-reparations framings? Do Global North publics or institutions accept historical responsibility and pay reparations, or do they reject the framing? The constraint''s beneficiary (Global South) status depends on whether reparations are accepted as legitimate.',
    'If reparations are widely accepted as legitimate (by courts, legislatures, global institutions), the constraint''s core transfer mechanism (wealth from North to South) gains legitimacy and the constraint becomes closer to rope (coordinated action toward shared principle). If reparations are rejected as illegitimate or retroactive (typical incumbent-institution stance), the transfer is perceived as confiscatory extraction from the North, and the constraint becomes closer to snare from the Northern perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carbon_reparations_legitimacy, preference, 'Normative legitimacy of historical carbon reparations as the warrant for North-to-South wealth transfer.').

omega_variable(
    intergenerational_discounting_logic,
    'Is present-generation sacrifice justified by future-generation benefit (zero or declining discount rate on future welfare), or should future generations'' welfare be discounted relative to present welfare (standard economic discounting)?',
    'Normative philosophical analysis: the reading assumes intergenerational equity (equal moral weight to current and future welfare) and near-zero discount rates. Mainstream economics and incumbent policy assume higher discount rates (future welfare weighted less). The resolution is institutional: which discount rate becomes law, policy, and institutional practice? Countries adopting zero-discount intergenerational frameworks (e.g., New Zealand''s Well-Being Act, some tribal governance) vs. standard-discount countries show the empirical pattern.',
    'If zero-discount intergenerational ethics becomes institutionalized, present sacrifice for climate stabilization gains legitimacy and the constraint''s burden-shift (from future to current wealthy) is accepted. If standard discounting persists (future welfare discounted), present sacrifice appears economically irrational and the constraint loses warrant, collapsing toward piton (maintained only by political coalition, not rational justification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_discounting_logic, preference, 'Discount rate applied to future-generation welfare determines the rationale for present-generation sacrifice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__degrowth_transformation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__degrowth_transformation, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t7, climate_response_action__degrowth_transformation, theater_ratio, 7, 0.32).
narrative_ontology:measurement(clim_tr_t14, climate_response_action__degrowth_transformation, theater_ratio, 14, 0.36).
narrative_ontology:measurement(clim_tr_t21, climate_response_action__degrowth_transformation, theater_ratio, 21, 0.38).
narrative_ontology:measurement(clim_tr_t28, climate_response_action__degrowth_transformation, theater_ratio, 28, 0.4).
narrative_ontology:measurement(clim_tr_t35, climate_response_action__degrowth_transformation, theater_ratio, 35, 0.41).
narrative_ontology:measurement(clim_tr_t42, climate_response_action__degrowth_transformation, theater_ratio, 42, 0.42).
narrative_ontology:measurement(clim_tr_t50, climate_response_action__degrowth_transformation, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__degrowth_transformation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(clim_be_t7, climate_response_action__degrowth_transformation, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(clim_be_t14, climate_response_action__degrowth_transformation, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(clim_be_t21, climate_response_action__degrowth_transformation, base_extractiveness, 21, 0.63).
narrative_ontology:measurement(clim_be_t28, climate_response_action__degrowth_transformation, base_extractiveness, 28, 0.66).
narrative_ontology:measurement(clim_be_t35, climate_response_action__degrowth_transformation, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(clim_be_t42, climate_response_action__degrowth_transformation, base_extractiveness, 42, 0.69).
narrative_ontology:measurement(clim_be_t50, climate_response_action__degrowth_transformation, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_action__degrowth_transformation, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(clim_su_t7, climate_response_action__degrowth_transformation, suppression_requirement, 7, 0.6).
narrative_ontology:measurement(clim_su_t14, climate_response_action__degrowth_transformation, suppression_requirement, 14, 0.66).
narrative_ontology:measurement(clim_su_t21, climate_response_action__degrowth_transformation, suppression_requirement, 21, 0.7).
narrative_ontology:measurement(clim_su_t28, climate_response_action__degrowth_transformation, suppression_requirement, 28, 0.72).
narrative_ontology:measurement(clim_su_t35, climate_response_action__degrowth_transformation, suppression_requirement, 35, 0.73).
narrative_ontology:measurement(clim_su_t42, climate_response_action__degrowth_transformation, suppression_requirement, 42, 0.73).
narrative_ontology:measurement(clim_su_t50, climate_response_action__degrowth_transformation, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__degrowth_transformation, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__degrowth_transformation, 0.18).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, planetary_boundaries_ecological_overshoot).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, global_north_consumption_redistribution).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, fossil_fuel_industry_managed_decline).

% DUAL FORMULATION NOTE:
% Part of the climate_response_action kernel family. This story represents one reading (degrowth transformation) of a contested kernel. The mitigation_priority and adaptation_priority constraint stories represent sibling readings of the same kernel. Degrowth FORECLOSES mitigation-via-technology-within-growth as a single-framework proposition (if decoupling fails, technology alone cannot deliver mitigation), but INFLUENCES adaptation by shifting resource availability. The three constraint stories are linked via network.affects_constraints; they share the same kernel (climate response action) but differ in core premise (technology feasibility, growth compatibility, intergenerational burden distribution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_action__degrowth_transformation, powerless, 0.15).
constraint_indexing:directionality_override(climate_response_action__degrowth_transformation, organized, 0.35).
constraint_indexing:directionality_override(climate_response_action__degrowth_transformation, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
