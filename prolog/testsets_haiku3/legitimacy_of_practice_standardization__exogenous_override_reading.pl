% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: State Authority Practice Standardization via Exogenous Decree
 *   domain: political/institutional
 *
 * SUMMARY:
 *   This constraint models the exogenous-override reading of
 *   legitimacy-of-practice-standardization: state authority claims the right
 *   to mandate practice change (calendar, dress, measurement) when it serves
 *   collective benefit (modernization, fiscal integration, international
 *   alignment). The reading treats the state decree as a legitimate
 *   imposition on traditional practitioners, justified by coordination
 *   outcomes. Structurally, the constraint operates as tangled rope: it
 *   solves a genuine coordination problem (enabling national administration
 *   and international trade) while simultaneously extracting from traditional
 *   practitioners who bear the cost of practice abandonment. The claim/metric
 *   independence is deliberate: the constraint is CLAIMED as tangled_rope
 *   (coordination + asymmetric enforcement required) while measurements track
 *   extractiveness rising over decades as dual-practice equilibrium proves
 *   durable—the theater ratio climbs because enforcement shifts from direct
 *   compliance-driving to performative legitimacy maintenance (school
 *   curricula, symbolic adoption, administrative theater) while underground
 *   traditional practice persists. The key structural divergence is between
 *   state authority (who sets and enforces) and traditional practitioners
 *   (trapped into identity-locked exit, bearing the cost). The reading
 *   forecloses neither the dual-practice nor endogenous-displacement readings
 *   across their own frameworks—those readings occupy different normative
 *   commitments about what legitimates practice change. This reading instead
 *   influences both by establishing exogenous state authority as a competing
 *   legitimacy claim.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: Institutional authority, sets decree, coordinates enforcement via tax timing and school curricula
 *   - modernization_advocates: Organized beneficiaries, gain alignment with international standards and unified markets
 *   - traditional_practitioners: Moderate power, identity-locked (religious obligation, cultural continuity), bear cost of practice displacement
 *   - rural_populations: Powerless, trapped geographically and institutionally, carry material costs of calendar/measure change
 *   - religious_authorities: Organized, constrained—lose institutional calendar-keeping function in public domain, maintain private authority
 *   - international_coordination_systems: Institutional beneficiary, gain standardized trading partners and interoperable measures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.72).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "State Authority Practice Standardization via Exogenous Decree").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political/institutional").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, '190081f9-9a02-4ce9-889a-ee754bd6c866').
narrative_ontology:cs_kernel_codification('190081f9-9a02-4ce9-889a-ee754bd6c866', formalized).
narrative_ontology:cs_authority_grounding('190081f9-9a02-4ce9-889a-ee754bd6c866', extraction).
narrative_ontology:cs_interpretation_layer_present('190081f9-9a02-4ce9-889a-ee754bd6c866').
narrative_ontology:cs_reading_relation('190081f9-9a02-4ce9-889a-ee754bd6c866', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('190081f9-9a02-4ce9-889a-ee754bd6c866', legitimacy_of_practice_standardization__endogenous_displacement_reading, influences).
narrative_ontology:cs_axiom('190081f9-9a02-4ce9-889a-ee754bd6c866', foundational, state_authority_legitimacy_via_collective_benefit).
narrative_ontology:cs_axiom_status(state_authority_legitimacy_via_collective_benefit, holdable).
narrative_ontology:cs_axiom_grounding('190081f9-9a02-4ce9-889a-ee754bd6c866', state_authority_legitimacy_via_collective_benefit, instrumental).
narrative_ontology:cs_axiom('190081f9-9a02-4ce9-889a-ee754bd6c866', secondary, exogenous_override_precedence_over_tradition).
narrative_ontology:cs_axiom_status(exogenous_override_precedence_over_tradition, holdable).
narrative_ontology:cs_axiom_grounding('190081f9-9a02-4ce9-889a-ee754bd6c866', exogenous_override_precedence_over_tradition, deontological).
narrative_ontology:cs_reference_frame('190081f9-9a02-4ce9-889a-ee754bd6c866', fragmented_pre_standardization_state).
narrative_ontology:cs_drift_state('190081f9-9a02-4ce9-889a-ee754bd6c866', contemporary_dual_practice_equilibrium, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('190081f9-9a02-4ce9-889a-ee754bd6c866', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, modernization_advocates).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_practitioners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, international_coordination_systems).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, religious_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees practice standardization (calendar reform, dress codes, measurement systems) in the name of collective benefit: fiscal efficiency, international commercial alignment, administrative uniformity. Enforces the new standard through bureaucratic mechanisms—tax collection aligned to reformed calendar, school curricula mandating new standards, border protocols recognizing only state-authorized measures. Justifies the decree as necessary modernization; resistance is framed as backwardness or particularism.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Support standardization as prerequisite for national integration, scientific progress, and economic participation. Often urban, educated, engaged with international standards. Gain coordinated markets, legible institutions, and narratives of progress. Face no enforcement cost—the new standards align with their preferred practices or pose minimal friction.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, modernization_advocates, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct cost of standardization: religious calendars displaced by civil calendar, customary dress codes overridden by state prescription, traditional measurement systems criminalized. The identity-lock operates through religious obligation, community legitimacy, and intergenerational transmission—exiting the traditional practice means severing ties to spiritual community, family authority, and cultural continuity. Compliance is partial: surface adoption of state standards in public/administrative domains while maintaining traditional practices in private/ritual domains.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_practitioners, payer,
    moderate, biographical, identity_locked, local).

% Lack access to state enforcement mechanisms' alternatives and lack political voice to contest the decree. Carry material costs: agricultural calendars tied to lunar cycles are displaced by solar calendars; tax timing and market participation are reorganized around state-defined dates; children educated in state-standardized measures lose fluency in traditional ones. Geographic isolation and weak institutional access make exit impossible; resistance is diffuse and atomized.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_populations, payer,
    powerless, biographical, trapped, local).

% Gain standardized trading partners, aligned timekeeping, interoperable measurement systems. The state's decree to align with international standards creates network benefits for the global commercial and diplomatic order. No direct participation in enforcement; benefit accrues through reduced transaction costs.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_coordination_systems, beneficiary,
    institutional, generational, analytical, global).

% Lose institutional authority to define calendar for religious observance. Maintain parallel calendars (lunar for ritual, solar for administration) but lose the public legitimacy that once attached to their calendar-keeping function. Excluded from decree-making; their objections are overridden as particularist or anti-modern. Maintain some authority in private/ritual domains but subordinated to state authority in public domains.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, religious_authorities, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, religious_authorities, excluded).

% Implements standardization through schools, tax collection, border control, police. Enforces public compliance while tacitly tolerating private persistence of traditional practices. Gains bureaucratic efficiency and legibility; loses the cost of total surveillance (enforcing private practice change would exceed available capacity).
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, state_enforcement_apparatus, agenda_setter,
    institutional, biographical, arbitrage, national).

% Document the constraint's operation: the abrupt imposition, the dual-practice equilibrium, the persistence of underground practice despite decades of enforcement. Measure divergence between state-declared standards and actual practice; track cost accumulation among victims.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, observer_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__exogenous_override_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes uniform measures, calendars, and administrative standards across diverse populations with heterogeneous prior practices, enabling national taxation, market integration, and diplomatic/commercial alignment with international systems.
% TRANSFER_FUNCTION: Transfers the cost of standardization—abandonment of traditional calendars, dress codes, measurement systems—from modernization advocates and international coordination systems to traditional practitioners and rural populations. Transfers administrative burden of enforcement from state apparatus onto those who must internalize or performatively adopt the new standard.
% ABSENT_VOICES: Rural populations and traditional practitioners participate in enforcement-related interactions (school attendance, tax timing, market navigation) but are excluded from decree-making. Religious authorities lose institutional voice in calendar-setting. Underground practitioners (those maintaining traditional practices despite prohibition) are structurally absent from the formal decree process.
% DISAPPEARANCE_RATIONALE: If the state decree and its enforcement vanished, traditional calendars would re-emerge publicly; administrative systems would fragment across local practices; international commercial coordination would require negotiated standards rather than imposed ones. The constraint's absence would reorganize both the administrative landscape (decentralization toward local practice) and the international system (shift from hierarchical standard-setting to negotiated alignment).
% FOUNDING_PROBLEM: National governments undertaking modernization faced fragmented calendars, measures, and administrative standards that impeded taxation, military coordination, and commercial integration. State authority asserted the power to standardize as necessary for collective benefit—fiscal stability, defensive capacity, international trade participation.
% FOUNDING_PROBLEM_CORROBORATION: State officials and modernization historians attest the founding problem was acute and standardization was necessary. Rural populations and religious authorities attest the founding problem was framed by centralists to justify suppression of alternative practices that were functionally adequate within their own domains. Anthropological and historical evidence documents that dual-practice equilibria (state standard in public domains, traditional practice in private domains) sustained for decades without fiscal collapse, suggesting the collective-benefit framing overstated necessity.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over 50 time units, tracking the constraint's maturation: initial decree meets scattered resistance and enforcement is uneven (low extractiveness), but as administrative infrastructure hardens (schools teach new standards, tax collection aligns to reformed calendar, generations grow up with state standards), extraction deepens—the cost of traditional practice becomes the cost of a double life, sustained underground at material and psychological expense. Suppression requirement tracks enforcement burden: initial imposition requires active coercion (police, school truancy enforcement, tax penalties), but as compliance becomes normalized and identity-fusion with the state standard generationally accumulates, the enforcement burden shifts toward maintaining theater (ceremonial respect for the standard, curricular ideology work) rather than direct coercion. Theater rises to 0.44 because a significant share of late-stage enforcement is performative legitimacy maintenance—states do not need to actively police every lunar calendar reference in private ritual; instead, state institutions ceremonialize state standards (national calendars in official buildings, standardized timekeeping in media) while tolerating private persistence, creating a stable equilibrium where public compliance and private tradition coexist. The dual-practice equilibrium is structurally stable, not transitional: rural populations and traditional practitioners do NOT gradually assimilate; instead, they maintain a hidden traditional calendar for decades while outwardly complying with state standards. This is the key structural delta from a Rope reading: a Rope would show extraction declining as alternatives equalize; here, extraction plateaus as dual practice stabilizes. Suppression requirement plateaus at 0.72 because enforcement has matured to a sustainable level (enforcement apparatus knows where to focus, compliance costs are distributed, resistance is diffuse).
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and modernization advocates experience this constraint as genuine coordination (solving the real problem of fragmented standards; enabling trade, taxation, military coordination). Traditional practitioners and rural populations experience it as extraction with a coordination cover story—they are forced to abandon functional traditional practices and absorb costs (relearning measures, reorganizing agricultural calendars, losing intergenerational transmission of traditional knowledge) while gaining no material benefit (markets are not more accessible to powerless rural populations; fiscal stability benefits flow to state institutions and urban merchants, not to rural taxpayers). Religious authorities experience loss of institutional authority in the public domain while retaining it in private domains, creating a constrained equilibrium. The engine computes this perspectival gap from power (institutional state apparatus vs. moderate/powerless practitioners), exit options (state has arbitrage; practitioners are identity-locked or trapped), and beneficiary/victim declarations. From the state seat, d approaches 0 (beneficiary); from the traditional practitioner seat, d approaches 1 (target). This divergence is exactly what the per-seat classification measures.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority (institutional power, arbitrage exit) benefits from standardization and enforces it: low d, low effective extraction (the constraint subsidizes their coordination function). Modernization advocates (organized, mobile exit) benefit without bearing enforcement cost: low d. Traditional practitioners (moderate power, identity-locked exit, explicit victim status) are the constraint's targets—they cannot exit without severing spiritual, community, and family ties, making their exit options severely constrained: high d, high effective extraction. Rural populations (powerless, trapped geographically) are the most vulnerable targets: highest d, highest effective extraction, least ability to negotiate or resist. Religious authorities (organized, constrained exit—they can maintain some authority in private domains but lose public authority) occupy a middle ground: moderate d. International coordination systems (institutional, analytical) benefit through network effects but participate analytically only: low d, subsidy-like effective extraction. The directionality derivation chains through beneficiary/victim declarations and power/exit atoms; no overrides are necessary—the structure is clear.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is correctly classified as Tangled Rope, not Snare: it carries genuine coordination function (solving fragmented standards, enabling national administration) AND asymmetric extraction (traditional practitioners pay the cost). A Snare reading would miss the coordination component and would misclassify the beneficiary claim (modernization advocates and state apparatus genuinely solve a collective-action problem, not mere cover story for pure extraction). Mandatrophy does not apply—the founding problem (fragmented standards impeding national coordination and international trade) remains contested in status but not dead; the constraint continues to serve the coordination function, which justifies its persistence even though victims would prefer alternative structures. If the founding problem were dead (if fragmented standards no longer impeded anything because underground practice persisted without fiscal collapse), the constraint would be a candidate for mandatrophy resolution—a structure maintaining theater and enforcement machinery for a problem that no longer exists. The data suggest the founding problem is *mostly* solved (in administrative domains; in ritual/private domains, fragmentation persists). This is the dual-practice equilibrium: problem solved in one domain (public/administrative) and unsolved in another (private/ritual). The constraint's persistence is legitimate under the exogenous-override reading because it delivers on coordination in the public domain, but that legitimacy is contested by the endogenous-displacement and dual-practice readings, which assert the legitimacy boundary should be drawn differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_benefit_distribution_ambiguity,
    'Does the claimed collective benefit (modernization, fiscal stability, international alignment) actually distribute to the population bearing the extraction cost, or does it concentrate among state apparatus and international trading partners?',
    'Comparative fiscal analysis tracking tax collection outcomes, wage changes, and market access for rural populations before and after standardization; economic mobility data; redistribution mechanisms if any.',
    'If benefits concentrate among state and urban merchants while rural populations face cost without gain, the ''collective benefit'' framing is legitimacy theater and the constraint reclassifies toward snare. If benefits distribute broadly (including rural wage gains, market access, fiscal stability enabling public goods), the tangled-rope classification holds and the constraint is a defensible trade-off.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_benefit_distribution_ambiguity, empirical, 'Whether collective-benefit framing reflects actual benefit distribution or masks concentrated extraction').

omega_variable(
    dual_practice_equilibrium_stability,
    'Is the dual-practice equilibrium (state standard in public domains, traditional practice in private domains) a stable, generationally sustainable arrangement, or a transitional phase toward complete state standardization?',
    'Longitudinal observation across 2-3 generations: do children born into dual-practice regimes maintain traditional practice privately, or do they assimilate fully to state standards? Do enforcement costs decline (suggesting stability) or increase (suggesting incomplete compliance driving escalation)?',
    'If dual practice is stable for generations, the constraint is a sustainable tangled-rope with managed extraction and internalized theater. If it is transitional, the constraint''s claimed persistence is an overestimate and eventual full standardization represents hidden mandatrophy (extraction rises until traditional practice disappears entirely).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_practice_equilibrium_stability, empirical, 'Whether the measured dual-practice equilibrium is structurally stable or decaying toward full standardization').

omega_variable(
    exogenous_override_framing_alternative,
    'Is the exogenous-override legitimacy ground defensible within a constitutional/democratic framework, or does it depend on suppressing the dual-practice and endogenous readings?',
    'Examine whether state apparatus explicitly forecloses alternative readings (suppresses dual-practice advocates, criminalizes traditional practice, prohibits dissent), or whether it coexists with acknowledged alternatives (permits private traditional practice, tolerates ritual calendars, acknowledges domain limitations).',
    'If the state actively suppresses alternatives, the exogenous-override reading is maintained coercively and its legitimacy is undercut by the suppression itself—the reading moves toward snare classification. If the state permits acknowledged coexistence, the reading''s legitimacy is more robust even though extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_override_framing_alternative, conceptual, 'Whether exogenous-override legitimacy is maintained by suppressing alternatives or by acknowledging domain limitations').

omega_variable(
    reading_contest_over_kernel,
    'The three readings (exogenous-override, dual-practice, endogenous) represent a genuine structural contest over what legitimates practice change, or do they represent three successive historical phases of a single constraint''s evolution?',
    'Historical research into institutional discourse: are all three readings articulated simultaneously by different actors, or does the reading shift as the constraint matures (decree-era speaks exogenous-override, dual-practice era speaks domain-partition, decay-era speaks voluntary-adoption)?',
    'If simultaneous, the three readings are distinct constraints linked by affect_constraints relations. If sequential, they are versions of one constraint over time, and the ''kernel'' is better understood as a problem space the state address through different framings rather than a single commitment structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_over_kernel, conceptual, 'Whether the three readings are simultaneous alternative frameworks or sequential phases of institutional evolution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t5, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(legi_tr_t5, observed).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(legi_tr_t10, observed).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(legi_tr_t20, observed).
narrative_ontology:measurement(legi_tr_t35, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 35, 0.43).
narrative_ontology:measurement_basis(legi_tr_t35, observed).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 50, 0.44).
narrative_ontology:measurement_basis(legi_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t5, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(legi_be_t5, observed).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(legi_be_t10, observed).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(legi_be_t20, observed).
narrative_ontology:measurement(legi_be_t35, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement_basis(legi_be_t35, observed).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(legi_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t5, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(legi_su_t5, observed).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(legi_su_t10, observed).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(legi_su_t20, observed).
narrative_ontology:measurement(legi_su_t35, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(legi_su_t35, observed).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(legi_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__exogenous_override_reading, 0.18).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).

% DUAL FORMULATION NOTE:
% The legitimacy_of_practice_standardization kernel decomposes into three structurally distinct constraints: (1) exogenous_override_reading treats standardization legitimacy as grounded in state authority acting for collective benefit, carries moderate extraction and tangled-rope classification; (2) dual_practice_equilibrium_reading treats legitimacy as domain-partitioned (state governs public, tradition governs private), carries lower asymmetry and approaches rope classification; (3) endogenous_displacement_reading treats legitimacy as emerging from voluntary adoption, carries low extraction and pure-rope classification. The three readings are coexisting positions held by different institutional actors, not temporal phases. ε differs substantively across readings (0.68 exogenous, ~0.45 dual-practice, ~0.25 endogenous) because each reading instantiates a different constraint: the empirical referent (the standing practice-change arrangement) stays fixed, but which costs count as extractive changes with the reading's legitimacy ground. All three are linked via affects_constraints to enable contamination-propagation analysis and constraint-family tracking.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
